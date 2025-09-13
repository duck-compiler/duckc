use std::collections::{HashMap, HashSet, VecDeque};
use tree_sitter::{Node, Parser, Range};

#[derive(Debug, Clone)]
enum DeclKind {
    Function,
    Method,
    Interface { methods: HashSet<String> },
    Type,
    Var,
    Const,
}

#[derive(Debug, Clone)]
struct Declaration {
    _name: String, // g for debuuggggggg
    kind: DeclKind,
    range: Range,
    is_exported: bool,
    dependencies: HashSet<String>,
}

pub fn cleanup_go_source(go_source: &str, remove_exported: bool) -> String {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_go::LANGUAGE.into())
        .expect("compiler error: couldn't load treesitter go parser");

    let tree = parser.parse(go_source, None).unwrap();

    let (declarations, _imports, _package_usages) =
        analyze_source(tree.root_node(), go_source.as_bytes());

    let live_set = calculate_live_set(&declarations, remove_exported);

    let mut ranges_to_delete = Vec::new();

    for (name, decl) in &declarations {
        if !live_set.contains(name) && (!decl.is_exported || remove_exported) {
            ranges_to_delete.push(decl.range);
        }
    }

    let cleaned_source = drain_ranges(go_source, ranges_to_delete);

    remove_unused_imports(&cleaned_source)
}

pub fn remove_unused_imports(go_source: &str) -> String {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_go::LANGUAGE.into())
        .expect("compiler error: couldn't load treesitter go parser");

    let tree = parser.parse(go_source, None).unwrap();

    let import_ranges = find_import_ranges(&tree);
    let used_imports = find_used_imports(&tree, go_source);

    let mut ranges_to_delete = Vec::new();
    for import_range in import_ranges {
        let import_text = &go_source[import_range.start_byte..import_range.end_byte];

        let Some(package_name) = extract_package_name_from_import(import_text) else {
            continue;
        };

        if package_name == "_" {
            ranges_to_delete.push(import_range);
            continue;
        }

        if import_text.contains(". \"") {
            let mut has_used_symbols = false;

            if !used_imports.is_empty() {
                has_used_symbols = true;
            }

            if !has_used_symbols {
                ranges_to_delete.push(import_range);
            }
        } else if !used_imports.contains(&package_name) {
            ranges_to_delete.push(import_range);
        }
    }

    drain_ranges(go_source, ranges_to_delete)
}

fn find_import_ranges(tree: &tree_sitter::Tree) -> Vec<Range> {
    let mut import_ranges = Vec::new();
    let mut stack = vec![tree.root_node()];

    while let Some(node) = stack.pop() {
        if node.kind() != "import_declaration" {
            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    stack.push(child);
                }
            }

            continue;
        }

        let has_import_spec_list = node
            .children(&mut node.walk())
            .any(|child| child.kind() == "import_spec_list");

        if !has_import_spec_list {
            import_ranges.push(node.range());
            continue;
        }

        for i in 0..node.child_count() {
            let Some(child) = node.child(i) else { continue };

            if child.kind() == "import_spec_list" {
                for j in 0..child.child_count() {
                    let Some(spec) = child.child(j) else { continue };
                    if spec.kind() == "import_spec" {
                        import_ranges.push(spec.range());
                    }
                }
            } else if child.kind() == "import_spec" {
                import_ranges.push(child.range());
            }
        }
    }

    import_ranges
}

fn find_used_imports(tree: &tree_sitter::Tree, go_source: &str) -> HashSet<String> {
    let mut used_imports = HashSet::new();
    let mut stack = vec![tree.root_node()];

    while let Some(node) = stack.pop() {
        if node.kind() == "identifier" {
            let text = &go_source[node.start_byte()..node.end_byte()];
            used_imports.insert(text.to_string());
        } else if node.kind() == "selector_expression" {
            if let Some(package_node) = node.child(0)
                && package_node.kind() == "identifier" {
                    let package_name =
                        &go_source[package_node.start_byte()..package_node.end_byte()];
                    used_imports.insert(package_name.to_string());
                }
        } else if node.kind() == "call_expression" {
            if let Some(function_node) = node.child(0)
                && function_node.kind() == "selector_expression"
                    && let Some(package_node) = function_node.child(0)
                        && package_node.kind() == "identifier" {
                            let package_name =
                                &go_source[package_node.start_byte()..package_node.end_byte()];
                            used_imports.insert(package_name.to_string());
                        }
        } else if node.kind() == "type_identifier"
            && let Some(parent) = node.parent()
                && parent.kind() == "selector_expression"
                    && let Some(package_node) = parent.child(0)
                        && package_node.kind() == "identifier" {
                            let package_name =
                                &go_source[package_node.start_byte()..package_node.end_byte()];
                            used_imports.insert(package_name.to_string());
                        }

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                stack.push(child);
            }
        }
    }

    let mut import_stack = vec![tree.root_node()];
    while let Some(node) = import_stack.pop() {
        if node.kind() == "import_declaration" {
            for spec in node
                .children(&mut node.walk())
                .filter(|c| c.kind() == "import_spec")
            {
                if let Some(name_node) = spec.child(0) {
                    if name_node.kind() == "dot" {
                        if let Some(path_node) = spec.child(spec.child_count() - 1)
                            && path_node.kind() == "interpreted_string_literal" {
                                let import_path = path_node
                                    .utf8_text(go_source.as_bytes())
                                    .unwrap()
                                    .trim_matches('"');
                                let package_name = extract_package_name_from_path(import_path);
                                used_imports.insert(package_name);
                            }
                    } else if name_node.kind() == "identifier" {
                        let alias_name = name_node.utf8_text(go_source.as_bytes()).unwrap();
                        used_imports.insert(alias_name.to_string());
                    }
                }
            }
        }

        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                import_stack.push(child);
            }
        }
    }

    used_imports
}

fn extract_package_name_from_import(import_text: &str) -> Option<String> {
    let trimmed = import_text.trim();

    if trimmed.starts_with('_') {
        if let Some(start) = trimmed.find('"')
            && let Some(end) = trimmed[start + 1..].find('"') {
                let package_path = &trimmed[start + 1..start + 1 + end];
                return Some(extract_package_name_from_path(package_path));
            }
        return None;
    }

    if trimmed.starts_with('.') {
        if let Some(start) = trimmed.find('"')
            && let Some(end) = trimmed[start + 1..].find('"') {
                let package_path = &trimmed[start + 1..start + 1 + end];
                return Some(extract_package_name_from_path(package_path));
            }
        return None;
    }

    if trimmed.starts_with('"') && trimmed.ends_with('"') {
        let package_path = &trimmed[1..trimmed.len() - 1];
        return Some(extract_package_name_from_path(package_path));
    }

    if !trimmed.starts_with("import") {
        return None;
    }

    let start = trimmed.find('"')?;
    let end = trimmed[start + 1..].find('"')?;

    let package_path = &trimmed[start + 1..start + 1 + end];
    Some(extract_package_name_from_path(package_path))
}

fn extract_package_name_from_path(package_path: &str) -> String {
    let package_path = if let Some(last_slash) = package_path.rfind('/') {
        package_path[last_slash + 1..].to_string()
    } else {
        package_path.to_string()
    };

    return if let Some(last_dash) = package_path.rfind('-') {
        package_path[last_dash + 1..].to_string()
    } else {
        package_path.to_string()
    };
}

fn analyze_source(
    root_node: Node,
    source: &[u8],
) -> (
    HashMap<String, Declaration>,
    HashMap<String, Range>,
    HashSet<String>,
) {
    let mut declarations = HashMap::new();
    let mut imports = HashMap::new();

    for node in root_node.children(&mut root_node.walk()) {
        parse_node(node, source, &mut declarations, &mut imports);
    }

    let mut package_usages = HashSet::new();
    find_all_package_usages(root_node, source, &mut package_usages);

    (declarations, imports, package_usages)
}

fn parse_node(
    node: Node,
    source: &[u8],
    declarations: &mut HashMap<String, Declaration>,
    imports: &mut HashMap<String, Range>,
) {
    match node.kind() {
        "import_declaration" => parse_imports(node, source, imports),
        "function_declaration"
        | "method_declaration"
        | "type_declaration"
        | "var_declaration"
        | "const_declaration" => {
            go_parse_declaration(node, source, declarations);
        }
        _ => {
            for child in node.children(&mut node.walk()) {
                parse_node(child, source, declarations, imports);
            }
        }
    }
}

fn calculate_live_set(
    declarations: &HashMap<String, Declaration>,
    _remove_exported: bool,
) -> HashSet<String> {
    let mut live_set = HashSet::new();
    let mut worklist: VecDeque<String> = VecDeque::new();

    for name in ["main", "init"] {
        if declarations.contains_key(name) && live_set.insert(name.to_string()) {
            worklist.push_back(name.to_string());
        }
    }

    perform_reachability_analysis(&mut live_set, &mut worklist, declarations);

    let required_methods = collect_required_methods(&live_set, declarations);
    if !required_methods.is_empty() {
        add_required_methods(
            &mut live_set,
            &mut worklist,
            declarations,
            &required_methods,
        );
        perform_reachability_analysis(&mut live_set, &mut worklist, declarations);
    }

    let receiver_types = find_receiver_types(&live_set, declarations);
    for receiver_type in receiver_types {
        if live_set.insert(receiver_type.clone()) {
            worklist.push_back(receiver_type);
        }
    }

    let type_methods = find_type_methods(&live_set, declarations);
    for method_name in type_methods {
        if live_set.insert(method_name.clone()) {
            worklist.push_back(method_name);
        }
    }

    let method_names = find_special_method_names(&live_set);
    if !method_names.is_empty() {
        add_special_methods(&mut live_set, &mut worklist, declarations, &method_names);
    }

    if !worklist.is_empty() {
        perform_reachability_analysis(&mut live_set, &mut worklist, declarations);
    }

    live_set
}

fn collect_required_methods(
    live_set: &HashSet<String>,
    declarations: &HashMap<String, Declaration>,
) -> HashSet<String> {
    live_set
        .iter()
        .filter_map(|name| declarations.get(name))
        .filter_map(|decl| match &decl.kind {
            DeclKind::Interface { methods } => Some(methods.iter().cloned()),
            _ => None,
        })
        .flatten()
        .collect()
}

fn add_required_methods(
    live_set: &mut HashSet<String>,
    worklist: &mut VecDeque<String>,
    declarations: &HashMap<String, Declaration>,
    required_methods: &HashSet<String>,
) {
    for (name, decl) in declarations {
        let DeclKind::Method = &decl.kind else {
            continue;
        };

        let (receiver_type, method_name) = if let Some(dot_pos) = name.find('.') {
            (&name[..dot_pos], &name[dot_pos + 1..])
        } else {
            continue;
        };

        if required_methods.contains(method_name)
            && live_set.contains(receiver_type)
            && live_set.insert(name.clone())
        {
            worklist.push_back(name.clone());
        }
    }
}

fn find_receiver_types(
    live_set: &HashSet<String>,
    declarations: &HashMap<String, Declaration>,
) -> HashSet<String> {
    let mut receiver_types = HashSet::new();

    for (name, decl) in declarations {
        if !live_set.contains(name) {
            continue;
        }

        let DeclKind::Method = &decl.kind else {
            continue;
        };

        if let Some(dot_pos) = name.find('.') {
            let receiver_type = &name[..dot_pos];
            receiver_types.insert(receiver_type.to_string());
        }
    }

    receiver_types
}

fn find_type_methods(
    live_set: &HashSet<String>,
    declarations: &HashMap<String, Declaration>,
) -> Vec<String> {
    let mut type_methods = Vec::new();

    for (name, decl) in declarations {
        if !live_set.contains(name) {
            continue;
        }

        let is_type_or_interface = matches!(decl.kind, DeclKind::Type | DeclKind::Interface { .. });
        if !is_type_or_interface {
            continue;
        }

        for (method_name, method_decl) in declarations {
            let DeclKind::Method = &method_decl.kind else {
                continue;
            };

            if let Some(dot_pos) = method_name.find('.') {
                let receiver_type = &method_name[..dot_pos];
                if receiver_type == name {
                    type_methods.push(method_name.clone());
                }
            }
        }
    }

    type_methods
}

fn find_special_method_names(live_set: &HashSet<String>) -> HashSet<String> {
    live_set
        .iter()
        .filter(|name| {
            name.contains("as_dgo_string")
                || name.contains("as_dgo_int")
                || name.contains("as_dgo_float32")
                || name.contains("as_dgo_bool")
                || name.contains("as_dgo_rune")
        })
        .cloned()
        .collect()
}

fn add_special_methods(
    live_set: &mut HashSet<String>,
    worklist: &mut VecDeque<String>,
    declarations: &HashMap<String, Declaration>,
    method_names: &HashSet<String>,
) {
    for (name, decl) in declarations {
        let DeclKind::Method = &decl.kind else {
            continue;
        };

        if let Some(dot_pos) = name.find('.') {
            let method_name = &name[dot_pos + 1..];
            if method_names.contains(method_name) {
                let receiver_type = &name[..dot_pos];
                if live_set.insert(receiver_type.to_string()) {
                    worklist.push_back(receiver_type.to_string());
                }
            }
        }
    }
}

fn perform_reachability_analysis(
    live_set: &mut HashSet<String>,
    worklist: &mut VecDeque<String>,
    declarations: &HashMap<String, Declaration>,
) {
    while let Some(name) = worklist.pop_front() {
        let Some(decl) = declarations.get(&name) else {
            continue;
        };

        for dep in &decl.dependencies {
            if live_set.insert(dep.clone()) {
                worklist.push_back(dep.clone());
            }
        }
    }
}

fn drain_ranges(go_source: &str, mut ranges: Vec<Range>) -> String {
    ranges.sort_by_key(|r| (r.start_byte, r.end_byte));
    ranges.dedup_by_key(|r| (r.start_byte, r.end_byte));

    ranges.sort_by_key(|r| std::cmp::Reverse(r.start_byte));

    let mut new_source = go_source.to_string();
    for range in ranges {
        if range.start_byte < new_source.len() && range.end_byte <= new_source.len() {
            new_source.drain(range.start_byte..range.end_byte);
        }
    }

    new_source
        .lines()
        .map(|line| line.trim_end())
        .collect::<Vec<_>>()
        .join("\n")
        .replace("\n\n\n", "\n\n")
}

fn go_parse_declaration(
    node: Node,
    source: &[u8],
    declarations: &mut HashMap<String, Declaration>,
) {
    let kind = match node.kind() {
        "function_declaration" => DeclKind::Function,
        "method_declaration" => DeclKind::Method,
        "type_declaration" => DeclKind::Type,
        "var_declaration" => DeclKind::Var,
        "const_declaration" => DeclKind::Const,
        _ => return,
    };

    let nodes_to_process = if matches!(
        node.kind(),
        "var_declaration" | "const_declaration" | "type_declaration"
    ) {
        find_spec_nodes(node)
    } else {
        vec![node]
    };

    for item_node in nodes_to_process {
        let Some(name_node) = item_node.child_by_field_name("name") else {
            continue;
        };
        let name = name_node.utf8_text(source).unwrap().to_string();

        let mut dependencies = HashSet::new();
        find_dependencies(item_node, source, &mut dependencies);
        dependencies.remove(&name);

        if item_node.kind() == "method_declaration"
            && let Some(receiver_type) = find_receiver_type_name(&item_node, source) {
                dependencies.insert(receiver_type.clone());

                let method_name_with_receiver = format!("{receiver_type}.{name}");
                declarations.insert(
                    method_name_with_receiver.clone(),
                    Declaration {
                        _name: method_name_with_receiver,
                        kind: DeclKind::Method,
                        range: item_node.range(),
                        is_exported: name.chars().next().unwrap_or('a').is_uppercase(),
                        dependencies: dependencies.clone(),
                    },
                );
            }

        let is_exported = name.chars().next().unwrap_or('a').is_uppercase();

        let final_kind = if let Some(type_body) = item_node.child_by_field_name("type") {
            if type_body.kind() == "interface_type" {
                let methods = extract_interface_methods(type_body, source);
                DeclKind::Interface { methods }
            } else {
                kind.clone()
            }
        } else {
            kind.clone()
        };

        let range = if matches!(
            node.kind(),
            "type_declaration" | "var_declaration" | "const_declaration"
        ) {
            node.range()
        } else {
            item_node.range()
        };

        declarations.insert(
            name.clone(),
            Declaration {
                _name: name,
                kind: final_kind,
                range,
                is_exported,
                dependencies,
            },
        );
    }
}

fn find_spec_nodes(node: Node) -> Vec<Node> {
    let mut nodes_to_process = Vec::new();
    let target_kind = node.kind().replace("declaration", "spec");

    let mut queue = VecDeque::from_iter(node.children(&mut node.walk()));

    while let Some(current) = queue.pop_front() {
        if current.kind() == target_kind {
            nodes_to_process.push(current);
        } else {
            queue.extend(current.children(&mut current.walk()));
        }
    }

    nodes_to_process
}

fn extract_interface_methods(type_body: Node, source: &[u8]) -> HashSet<String> {
    let mut methods = HashSet::new();

    let Some(list) = type_body.child_by_field_name("methods") else {
        return methods;
    };

    for method_spec in list.children(&mut list.walk()) {
        if let Some(m_name) = method_spec.child_by_field_name("name") {
            methods.insert(m_name.utf8_text(source).unwrap().to_string());
        }
    }

    methods
}

fn find_receiver_type_name(node: &Node, source: &[u8]) -> Option<String> {
    let receiver_node = node.child_by_field_name("receiver")?;
    let mut queue = VecDeque::from_iter(receiver_node.children(&mut receiver_node.walk()));

    while let Some(current) = queue.pop_front() {
        if current.kind() == "type_identifier" {
            return Some(current.utf8_text(source).unwrap().to_string());
        }
        queue.extend(current.children(&mut current.walk()));
    }
    None
}

fn find_dependencies(node: Node, source: &[u8], deps: &mut HashSet<String>) {
    match node.kind() {
        "identifier" | "type_identifier" | "field_identifier" => {
            deps.insert(node.utf8_text(source).unwrap().to_string());
        }
        "selector_expression" => {
            if let Some(operand) = node.child(0)
                && operand.kind() == "identifier" {
                    deps.insert(operand.utf8_text(source).unwrap().to_string());
                }
            if let Some(field) = node.child(1)
                && field.kind() == "field_identifier" {
                    deps.insert(field.utf8_text(source).unwrap().to_string());
                }
        }
        "type_assertion" => {
            if let Some(type_node) = node.child(1)
                && type_node.kind() == "type_identifier" {
                    deps.insert(type_node.utf8_text(source).unwrap().to_string());
                }
        }
        "type_switch_expression" => {
            if let Some(expr) = node.child(0)
                && expr.kind() == "type_assertion"
                    && let Some(type_node) = expr.child(1)
                        && type_node.kind() == "type_identifier" {
                            deps.insert(type_node.utf8_text(source).unwrap().to_string());
                        }
        }
        "type_case" => {
            for child in node.children(&mut node.walk()) {
                if child.kind() == "type_identifier" {
                    deps.insert(child.utf8_text(source).unwrap().to_string());
                }
            }
        }
        "type_switch_statement" => {
            for child in node.children(&mut node.walk()) {
                if child.kind() == "type_identifier" {
                    deps.insert(child.utf8_text(source).unwrap().to_string());
                }
            }
        }
        "composite_literal" => {
            if let Some(type_node) = node.child(0)
                && type_node.kind() == "type_identifier" {
                    deps.insert(type_node.utf8_text(source).unwrap().to_string());
                }
        }
        _ => {}
    }

    for child in node.children(&mut node.walk()) {
        find_dependencies(child, source, deps);
    }
}

fn find_all_package_usages(node: Node, source: &[u8], usages: &mut HashSet<String>) {
    if node.kind() == "selector_expression"
        && let Some(operand) = node.child(0)
            && operand.kind() == "identifier" {
                usages.insert(operand.utf8_text(source).unwrap().to_string());
            }

    for child in node.children(&mut node.walk()) {
        find_all_package_usages(child, source, usages);
    }
}

fn parse_imports(node: Node, source: &[u8], imports: &mut HashMap<String, Range>) {
    for spec in node
        .children(&mut node.walk())
        .filter(|c| c.kind() == "import_spec")
    {
        if let Some(path_node) = spec.child(spec.child_count() - 1)
            && path_node.kind() == "interpreted_string_literal" {
                let import_path = path_node.utf8_text(source).unwrap().trim_matches('"');
                if let Some(name) = get_import_package_name(&spec, import_path, source) {
                    imports.insert(name, spec.range());
                }
            }
    }
}

fn get_import_package_name(node: &Node, path: &str, source: &[u8]) -> Option<String> {
    if let Some(name_node) = node.child(0)
        && name_node.kind() != "interpreted_string_literal" {
            return match name_node.kind() {
                "identifier" => Some(name_node.utf8_text(source).unwrap().to_string()),
                "blank_identifier" => Some("_".to_string()), // handle blank imports
                "dot" => Some(path.rsplit('/').next().unwrap_or(path).to_string()), // handle dot imports
                _ => None,
            };
        }
    path.rsplit('/').next().map(|s| s.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_parser() -> Parser {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_go::LANGUAGE.into())
            .unwrap();
        parser
    }

    fn parse_go_code(code: &str) -> tree_sitter::Tree {
        let mut parser = create_test_parser();
        parser.parse(code, None).unwrap()
    }

    fn assert_cleanup_result(input: &str, expected: &str, remove_exported: bool) {
        let result = cleanup_go_source(input, remove_exported);
        let normalized_result = normalize_whitespace(&result);
        let normalized_expected = normalize_whitespace(expected);

        assert_eq!(
            normalized_result, normalized_expected,
            "input: {}\nexpected: {}\ngot: {}",
            input, normalized_expected, normalized_result
        );
    }

    fn normalize_whitespace(text: &str) -> String {
        text.split_whitespace().collect::<Vec<&str>>().join(" ")
    }

    #[test]
    fn test_cleanup_basic_function() {
        let input = r#"
            package main

            import "fmt"

            func unused() {
                fmt.Println("unused")
            }

            func main() {
                fmt.Println("hello")
            }
        "#;

        let expected = r#"
            package main

            import "fmt"

            func main() {
                fmt.Println("hello")
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_unused_imports() {
        let input = r#"
            package main

            import (
                "fmt"
                "os"
                "strings"
            )

            func main() {
                fmt.Println("hello")
            }
        "#;

        let expected = r#"
            package main

            import (
                "fmt"
            )

            func main() {
                fmt.Println("hello")
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_unused_types() {
        let input = r#"
            package main

            import "fmt"

            type UnusedType struct {
                field int
            }

            type UsedType struct {
                value string
            }

            func main() {
                var u UsedType
                fmt.Println(u.value)
            }
        "#;

        let expected = r#"
            package main

            import "fmt"

            type UsedType struct {
                value string
            }

            func main() {
                var u UsedType
                fmt.Println(u.value)
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_unused_methods() {
        let input = r#"
            package main

            import "fmt"

            type MyType struct {
                value string
            }

            func (m MyType) usedMethod() string {
                return m.value
            }

            func (m MyType) unusedMethod() int {
                return 42
            }

            func main() {
                m := MyType{value: "test"}
                fmt.Println(m.usedMethod())
            }
        "#;

        let expected = r#"
            package main

            import "fmt"

            type MyType struct {
                value string
            }

            func (m MyType) usedMethod() string {
                return m.value
            }

            func main() {
                m := MyType{value: "test"}
                fmt.Println(m.usedMethod())
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_interface_methods() {
        let input = r#"
            package main

            import "fmt"

            type Writer interface {
                Write([]byte) (int, error)
                Close() error
            }

            type MyWriter struct{}

            func (w MyWriter) Write(data []byte) (int, error) {
                return len(data), nil
            }

            func (w MyWriter) Close() error {
                return nil
            }

            func (w MyWriter) unusedMethod() {
            }

            func main() {
                var w Writer = MyWriter{}
                w.Write([]byte("test"))
                w.Close()
            }
        "#;

        let expected = r#"
            package main

            type Writer interface {
                Write([]byte) (int, error)
                Close() error
            }

            type MyWriter struct{}

            func (w MyWriter) Write(data []byte) (int, error) {
                return len(data), nil
            }

            func (w MyWriter) Close() error {
                return nil
            }

            func main() {
                var w Writer = MyWriter{}
                w.Write([]byte("test"))
                w.Close()
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_unused_interface() {
        let input = r#"
            package main

            import "fmt"

            type UnusedInterface interface {
                Method() string
            }

            type UsedInterface interface {
                DoSomething() int
            }

            type Impl struct{}

            func (i Impl) DoSomething() int {
                return 42
            }

            func main() {
                var i UsedInterface = Impl{}
                fmt.Println(i.DoSomething())
            }
        "#;

        let expected = r#"
            package main

            import "fmt"

            type UsedInterface interface {
                DoSomething() int
            }

            type Impl struct{}

            func (i Impl) DoSomething() int {
                return 42
            }

            func main() {
                var i UsedInterface = Impl{}
                fmt.Println(i.DoSomething())
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_unused_variables() {
        let input = r#"
            package main

            import "fmt"

            var unusedVar = "unused"
            var usedVar = "used"

            const unusedConst = 42
            const usedConst = 100

            func main() {
                fmt.Println(usedVar, usedConst)
            }
        "#;

        let expected = r#"
            package main

            import "fmt"

            var usedVar = "used"

            const usedConst = 100

            func main() {
                fmt.Println(usedVar, usedConst)
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_complex_dependencies() {
        let input = r#"
            package main

            import "fmt"

            type Node struct {
                value int
                next  *Node
            }

            func (n *Node) GetValue() int {
                return n.value
            }

            func (n *Node) GetNext() *Node {
                return n.next
            }

            func (n *Node) unusedMethod() {
                // unused
            }

            func createNode(value int) *Node {
                return &Node{value: value}
            }

            func processNode(n *Node) {
                if n != nil {
                    fmt.Println(n.GetValue())
                    processNode(n.GetNext())
                }
            }

            func main() {
                head := createNode(1)
                head.next = createNode(2)
                processNode(head)
            }
        "#;

        let expected = r#"
            package main

            import "fmt"

            type Node struct {
                value int
                next  *Node
            }

            func (n *Node) GetValue() int {
                return n.value
            }

            func (n *Node) GetNext() *Node {
                return n.next
            }

            func createNode(value int) *Node {
                return &Node{value: value}
            }

            func processNode(n *Node) {
                if n != nil {
                    fmt.Println(n.GetValue())
                    processNode(n.GetNext())
                }
            }

            func main() {
                head := createNode(1)
                head.next = createNode(2)
                processNode(head)
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_import_aliases() {
        let input = r#"
            package main

            import (
                "fmt"
                f "fmt"
                "os"
                "strings"
            )

            func main() {
                fmt.Println("hello")
                f.Println("world")
            }
"#;

        let expected = r#"
            package main

            import (
                "fmt"
                f "fmt"
            )

            func main() {
                fmt.Println("hello")
                f.Println("world")
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_dot_imports() {
        let input = r#"
            package main

            import (
                "fmt"
                . "strings"
                "os"
            )

            func main() {
                fmt.Println(ToUpper("hello"))
            }
        "#;

        let expected = r#"
            package main

            import (
                "fmt"
                . "strings"
            )

            func main() {
                fmt.Println(ToUpper("hello"))
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_blank_imports() {
        let input = r#"
            package main

            import (
                "fmt"
                _ "os"
                "strings"
            )

            func main() {
                fmt.Println(strings.ToUpper("hello"))
            }
        "#;

        let expected = r#"
            package main

            import (
                "fmt"
                "strings"
            )

            func main() {
                fmt.Println(strings.ToUpper("hello"))
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_empty_file() {
        let input = r#"
            package main
        "#;

        let expected = r#"
            package main
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_only_unused() {
        let input = r#"
            package main

            import "fmt"

            func unused() {
                fmt.Println("unused")
            }

            type UnusedType struct {
                field int
            }
        "#;

        let expected = r#"
            package main
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_init_function() {
        let input = r#"
            package main

            import "fmt"

            func init() {
                fmt.Println("init")
            }

            func unused() {
                fmt.Println("unused")
            }

            func main() {
                fmt.Println("main")
            }
        "#;

        let expected = r#"
            package main

            import "fmt"

            func init() {
                fmt.Println("init")
            }

            func main() {
                fmt.Println("main")
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_keep_exported_when_remove_exported_false() {
        let input = r#"
            package main

            import "fmt"

            func ExportedFunc() {
                fmt.Println("exported")
            }

            func unexportedFunc() {
                fmt.Println("unexported")
            }

            func main() {
                fmt.Println("main")
            }
        "#;

        let expected = r#"
            package main

            import "fmt"

            func ExportedFunc() {
                fmt.Println("exported")
            }

            func main() {
                fmt.Println("main")
            }
        "#;

        assert_cleanup_result(input, expected, false);
    }

    #[test]
    fn test_cleanup_remove_exported_when_remove_exported_true() {
        let input = r#"
            package main

            import "fmt"

            func ExportedFunc() {
                fmt.Println("exported")
            }

            func unexportedFunc() {
                fmt.Println("unexported")
            }

            func main() {
                fmt.Println("main")
            }
        "#;

        let expected = r#"
            package main

            import "fmt"

            func main() {
                fmt.Println("main")
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_type_assertions() {
        let input = r#"
            package main

            import "fmt"

            type Stringer interface {
                String() string
            }

            type MyString string

            func (m MyString) String() string {
                return string(m)
            }

            type UnusedType int

            func (u UnusedType) String() string {
                return "unused"
            }

            func main() {
                var s Stringer = MyString("hello")
                if str, ok := s.(MyString); ok {
                    fmt.Println(str.String())
                }
            }
"#;

        let expected = r#"
            package main

            import "fmt"

            type Stringer interface {
                String() string
            }

            type MyString string

            func (m MyString) String() string {
                return string(m)
            }

            type UnusedType int

            func (u UnusedType) String() string {
                return "unused"
            }

            func main() {
                var s Stringer = MyString("hello")
                if str, ok := s.(MyString); ok {
                    fmt.Println(str.String())
                }
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_composite_literals() {
        let input = r#"
            package main

            import "fmt"

            type Point struct {
                x, y int
            }

            type UnusedPoint struct {
                x, y int
            }

            func main() {
                p := Point{x: 1, y: 2}
                fmt.Println(p.x, p.y)
            }
        "#;

        let expected = r#"
            package main

            import "fmt"

            type Point struct {
                x, y int
            }

            func main() {
                p := Point{x: 1, y: 2}
                fmt.Println(p.x, p.y)
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_method_receiver_types() {
        let input = r#"
            package main

            import "fmt"

            type MyType struct {
                value string
            }

            func (m MyType) GetValue() string {
                return m.value
            }

            func (m MyType) unusedMethod() int {
                return 42
            }

            type UnusedType struct {
                field int
            }

            func (u UnusedType) Method() int {
                return u.field
            }

            func main() {
                m := MyType{value: "test"}
                fmt.Println(m.GetValue())
            }
        "#;

        let expected = r#"
            package main

            import "fmt"

            type MyType struct {
                value string
            }

            func (m MyType) GetValue() string {
                return m.value
            }

            func main() {
                m := MyType{value: "test"}
                fmt.Println(m.GetValue())
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_special_method_names() {
        let input = r#"
            package main

            import "fmt"

            type MyType struct {
                value string
            }

            func (m MyType) as_dgo_string() string {
                return m.value
            }

            func (m MyType) as_dgo_int() int {
                return 42
            }

            func (m MyType) unusedMethod() bool {
                return true
            }

            func main() {
                m := MyType{value: "test"}
                fmt.Println(m.as_dgo_string())
            }
        "#;

        let expected = r#"
            package main

            import "fmt"

            type MyType struct {
                value string
            }

            func (m MyType) as_dgo_string() string {
                return m.value
            }

            func main() {
                m := MyType{value: "test"}
                fmt.Println(m.as_dgo_string())
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_extract_package_name_from_import() {
        assert_eq!(
            extract_package_name_from_import(r#""fmt""#),
            Some("fmt".to_string())
        );
        assert_eq!(
            extract_package_name_from_import(r#""net/http""#),
            Some("http".to_string())
        );
        assert_eq!(
            extract_package_name_from_import(r#""github.com/user/repo""#),
            Some("repo".to_string())
        );
        assert_eq!(
            extract_package_name_from_import(r#"import "fmt""#),
            Some("fmt".to_string())
        );
        assert_eq!(
            extract_package_name_from_import(r#"import "net/http""#),
            Some("http".to_string())
        );
        assert_eq!(
            extract_package_name_from_import(r#"import "github.com/http-routing""#),
            Some("routing".to_string())
        );
        assert_eq!(extract_package_name_from_import("invalid"), None);
    }

    #[test]
    fn test_extract_package_name_from_path() {
        assert_eq!(extract_package_name_from_path("fmt"), "fmt".to_string());
        assert_eq!(
            extract_package_name_from_path("net/http"),
            "http".to_string()
        );
        assert_eq!(
            extract_package_name_from_path("github.com/user/repo"),
            "repo".to_string()
        );
        assert_eq!(
            extract_package_name_from_path("github.com/user/repo/v2"),
            "v2".to_string()
        );
        assert_eq!(
            extract_package_name_from_path("github.com/user/repo/http-routing"),
            "routing".to_string()
        );
    }

    #[test]
    fn test_find_import_ranges() {
        let code = r#"
            package main

            import "fmt"

            func main() {
                fmt.Println("hello")
            }
        "#;

        let tree = parse_go_code(code);
        let ranges = find_import_ranges(&tree);
        assert_eq!(ranges.len(), 1);
    }

    #[test]
    fn test_find_import_ranges_multiple() {
        let code = r#"
            package main

            import (
                "fmt"
                "os"
                "strings"
            )

            func main() {
                fmt.Println("hello")
            }
        "#;

        let tree = parse_go_code(code);
        let ranges = find_import_ranges(&tree);
        assert_eq!(ranges.len(), 3);
    }

    #[test]
    fn test_find_used_imports() {
        let code = r#"
            package main

            import "fmt"

            func main() {
                fmt.Println("hello")
            }
        "#;

        let tree = parse_go_code(code);
        let used = find_used_imports(&tree, code);
        assert!(used.contains("fmt"));
        assert!(used.contains("main"));
    }

    #[test]
    fn test_analyze_source() {
        let code = r#"
            package main

            import "fmt"

            func main() {
                fmt.Println("hello")
            }
        "#;

        let tree = parse_go_code(code);
        let (declarations, imports, package_usages) =
            analyze_source(tree.root_node(), code.as_bytes());

        assert!(declarations.contains_key("main"));
        assert!(imports.contains_key("fmt"));
        assert!(package_usages.contains("fmt"));
    }

    #[test]
    fn test_calculate_live_set() {
        let code = r#"
            package main

            import "fmt"

            func main() {
                fmt.Println("hello")
            }

            func unused() {
                fmt.Println("unused")
            }
        "#;

        let tree = parse_go_code(code);
        let (declarations, _, _) = analyze_source(tree.root_node(), code.as_bytes());
        let live_set = calculate_live_set(&declarations, true);

        assert!(live_set.contains("main"));
        assert!(!live_set.contains("unused"));
    }

    #[test]
    fn test_modify_source() {
        let input = "hello world test";

        let ranges = vec![Range {
            start_byte: 6,
            end_byte: 12,
            start_point: tree_sitter::Point { row: 0, column: 6 },
            end_point: tree_sitter::Point { row: 0, column: 12 },
        }];

        let result = drain_ranges(input, ranges);
        assert_eq!(result, "hello test");
    }

    #[test]
    fn test_modify_source_duplicate_ranges() {
        let input = "hello world test";
        let ranges = vec![
            Range {
                start_byte: 6,
                end_byte: 12,
                start_point: tree_sitter::Point { row: 0, column: 6 },
                end_point: tree_sitter::Point { row: 0, column: 12 },
            },
            Range {
                start_byte: 6,
                end_byte: 12,
                start_point: tree_sitter::Point { row: 0, column: 6 },
                end_point: tree_sitter::Point { row: 0, column: 12 },
            },
        ];
        let result = drain_ranges(input, ranges);
        assert_eq!(result, "hello test");
    }

    #[test]
    fn test_cleanup_large_codebase() {
        let input = r#"
            package main

            import (
                "fmt"
                "os"
                "strings"
                "strconv"
                "time"
                "net/http"
                "encoding/json"
                "io/ioutil"
            )

            type User struct {
                ID   int    `json:"id"`
                Name string `json:"name"`
            }

            func (u User) GetID() int {
                return u.ID
            }

            func (u User) GetName() string {
                return u.Name
            }

            func (u User) unusedMethod() bool {
                return false
            }

            type UserService struct {
                users []User
            }

            func (s *UserService) AddUser(user User) {
                s.users = append(s.users, user)
            }

            func (s *UserService) GetUser(id int) *User {
                for _, user := range s.users {
                    if user.GetID() == id {
                        return &user
                    }
                }
                return nil
            }

            func (s *UserService) unusedMethod() {
                // unused
            }

            type UnusedService struct {
                data string
            }

            func (s UnusedService) Method() string {
                return s.data
            }

            func processUser(user *User) {
                if user != nil {
                    fmt.Printf("User: %s (ID: %d)\n", user.GetName(), user.GetID())
                }
            }

            func main() {
                service := &UserService{}
                user := User{ID: 1, Name: "John"}
                service.AddUser(user)

                foundUser := service.GetUser(1)
                processUser(foundUser)
            }
        "#;

        let expected = r#"
            package main

            import (
                "fmt"
            )

            type User struct {
                ID   int    `json:"id"`
                Name string `json:"name"`
            }

            func (u User) GetID() int {
                return u.ID
            }

            func (u User) GetName() string {
                return u.Name
            }

            func (u User) unusedMethod() bool {
                return false
            }

            type UserService struct {
                users []User
            }

            func (s *UserService) AddUser(user User) {
                s.users = append(s.users, user)
            }

            func (s *UserService) GetUser(id int) *User {
                for _, user := range s.users {
                    if user.GetID() == id {
                        return &user
                    }
                }
                return nil
            }

            func processUser(user *User) {
                if user != nil {
                    fmt.Printf("User: %s (ID: %d)\n", user.GetName(), user.GetID())
                }
            }

            func main() {
                service := &UserService{}
                user := User{ID: 1, Name: "John"}
                service.AddUser(user)

                foundUser := service.GetUser(1)
                processUser(foundUser)
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_malformed_imports() {
        let input = r#"
            package main

            import (
                "fmt"
                "invalid-import
                "os"
            )

            func main() {
                fmt.Println("hello")
            }
        "#;

        let result = cleanup_go_source(input, true);
        assert!(result.contains("fmt"));
        assert!(result.contains("main"));
    }

    #[test]
    fn test_cleanup_performance() {
        let input = r#"
            package main

            import "fmt"

            func main() {
                fmt.Println("hello")
            }
        "#;

        let start = std::time::Instant::now();
        let _result = cleanup_go_source(input, true);
        let duration = start.elapsed();

        assert!(duration.as_secs() < 1);
    }

    #[test]
    fn test_regression_orphaned_structs() {
        let input = r#"
            package main

            import "fmt"

            type MyType struct {
                value string
            }

            func (m MyType) GetValue() string {
                return m.value
            }

            func main() {
                m := MyType{value: "test"}
                fmt.Println(m.GetValue())
            }
        "#;

        let result = cleanup_go_source(input, true);

        assert!(result.contains("type MyType struct"));
        assert!(result.contains("func (m MyType) GetValue()"));
        assert!(result.contains("func main()"));
        assert!(result.contains("struct {"));
        assert!(result.contains("value string"));
        assert!(result.contains("fmt.Println"));
    }

    #[test]
    fn test_cleanup_fasthttp_routing() {
        let input = r#"
            package main

            import (
                "fmt"
                "github.com/qiangxue/fasthttp-routing"
                "github.com/valyala/fasthttp"
                "net/http"
                "encoding/json"
            )

            type User struct {
                ID   int    `json:"id"`
                Name string `json:"name"`
            }

            func (u User) String() string {
                return fmt.Sprintf("User{ID: %d, Name: %s}", u.ID, u.Name)
            }

            func (u User) unusedMethod() string {
                return "unused"
            }

            func main() {
                router := routing.New()

                router.Get("/", func(c *routing.Context) error {
                    fmt.Fprintf(c, "Hello, world!")
                    return nil
                })

                router.Get("/users", func(c *routing.Context) error {
                    users := []User{
                        {ID: 1, Name: "Alice"},
                        {ID: 2, Name: "Bob"},
                    }

                    json.NewEncoder(c).Encode(users)
                    return nil
                })

                router.Get("/users/<id>", func(c *routing.Context) error {
                    id := c.Param("id")
                    user := User{ID: 1, Name: "Alice"}
                    fmt.Fprintf(c, "User ID: %s, User: %s", id, user.String())
                    return nil
                })

                router.Post("/users", func(c *routing.Context) error {
                    var user User
                    json.NewDecoder(c.Request.Body).Decode(&user)
                    fmt.Fprintf(c, "Created user: %s", user.String())
                    return nil
                })

                panic(fasthttp.ListenAndServe(":8080", router.HandleRequest))
            }
        "#;

        let expected = r#"
            package main

            import (
                "fmt"
                "github.com/qiangxue/fasthttp-routing"
                "github.com/valyala/fasthttp"
                "encoding/json"
            )

            type User struct {
                ID   int    `json:"id"`
                Name string `json:"name"`
            }

            func (u User) String() string {
                return fmt.Sprintf("User{ID: %d, Name: %s}", u.ID, u.Name)
            }

            func main() {
                router := routing.New()

                router.Get("/", func(c *routing.Context) error {
                    fmt.Fprintf(c, "Hello, world!")
                    return nil
                })

                router.Get("/users", func(c *routing.Context) error {
                    users := []User{
                        {ID: 1, Name: "Alice"},
                        {ID: 2, Name: "Bob"},
                    }
                    json.NewEncoder(c).Encode(users)
                    return nil
                })

                router.Get("/users/<id>", func(c *routing.Context) error {
                    id := c.Param("id")
                    user := User{ID: 1, Name: "Alice"}
                    fmt.Fprintf(c, "User ID: %s, User: %s", id, user.String())
                    return nil
                })

                router.Post("/users", func(c *routing.Context) error {
                    var user User
                    json.NewDecoder(c.Request.Body).Decode(&user)
                    fmt.Fprintf(c, "Created user: %s", user.String())
                    return nil
                })

                panic(fasthttp.ListenAndServe(":8080", router.HandleRequest))
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_complex_goroutines() {
        let input = r#"
            package main

            import (
                "fmt"
                "sync"
                "time"
                "context"
                "net/http"
                "encoding/json"
            )

            type Worker struct {
                ID   int
                Name string
            }

            func (w Worker) Process() string {
                return fmt.Sprintf("Worker %d: %s", w.ID, w.Name)
            }

            func (w Worker) unusedMethod() string {
                return "unused"
            }

            func processWorker(w Worker, wg *sync.WaitGroup, results chan<- string) {
                defer wg.Done()
                time.Sleep(10 * time.Millisecond)
                results <- w.Process()
            }

            func main() {
                workers := []Worker{
                    {ID: 1, Name: "Alice"},
                    {ID: 2, Name: "Bob"},
                    {ID: 3, Name: "Charlie"},
                }

                var wg sync.WaitGroup
                results := make(chan string, len(workers))

                for _, worker := range workers {
                    wg.Add(1)
                    go processWorker(worker, &wg, results)
                }

                go func() {
                    wg.Wait()
                    close(results)
                }()

                for result := range results {
                    fmt.Println(result)
                }
            }
        "#;

        let expected = r#"
            package main

            import (
                "fmt"
                "time"
            )

            type Worker struct {
                ID   int
                Name string
            }

            func (w Worker) Process() string {
                return fmt.Sprintf("Worker %d: %s", w.ID, w.Name)
            }

            func processWorker(w Worker, wg *sync.WaitGroup, results chan<- string) {
                defer wg.Done()
                time.Sleep(10 * time.Millisecond)
                results <- w.Process()
            }

            func main() {
                workers := []Worker{
                    {ID: 1, Name: "Alice"},
                    {ID: 2, Name: "Bob"},
                    {ID: 3, Name: "Charlie"},
                }

                var wg sync.WaitGroup
                results := make(chan string, len(workers))

                for _, worker := range workers {
                    wg.Add(1)
                    go processWorker(worker, &wg, results)
                }

                go func() {
                    wg.Wait()
                    close(results)
                }()

                for result := range results {
                    fmt.Println(result)
                }
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_complex_interfaces() {
        let input = r#"
            package main

            import (
                "fmt"
                "io"
                "os"
                "strings"
                "encoding/json"
            )

            type Reader interface {
                Read([]byte) (int, error)
            }

            type Writer interface {
                Write([]byte) (int, error)
            }

            type ReadWriter interface {
                Reader
                Writer
            }

            type FileReader struct {
                filename string
            }

            func (f FileReader) Read(data []byte) (int, error) {
                return 0, nil
            }

            func (f FileReader) unusedMethod() error {
                return nil
            }

            type StringReader struct {
                data string
                pos  int
            }

            func (s *StringReader) Read(data []byte) (int, error) {
                if s.pos >= len(s.data) {
                    return 0, io.EOF
                }
                n := copy(data, s.data[s.pos:])
                s.pos += n
                return n, nil
            }

            func (s *StringReader) Write(data []byte) (int, error) {
                s.data += string(data)
                return len(data), nil
            }

            func processReader(r Reader) {
                data := make([]byte, 1024)
                n, err := r.Read(data)
                if err != nil {
                    fmt.Printf("Read error: %v\n", err)
                    return
                }
                fmt.Printf("Read %d bytes: %s\n", n, string(data[:n]))
            }

            func main() {
                fileReader := FileReader{filename: "test.txt"}
                stringReader := &StringReader{data: "Hello, World!"}

                processReader(fileReader)
                processReader(stringReader)

                var rw ReadWriter = stringReader
                rw.Write([]byte(" Additional data"))
                processReader(rw)
            }
        "#;

        let expected = r#"
            package main

            import (
                "fmt"
                "io"
            )

            type Reader interface {
                Read([]byte) (int, error)
            }

            type Writer interface {
                Write([]byte) (int, error)
            }

            type ReadWriter interface {
                Reader
                Writer
            }

            type FileReader struct {
                filename string
            }

            func (f FileReader) Read(data []byte) (int, error) {
                return 0, nil
            }

            type StringReader struct {
                data string
                pos  int
            }

            func (s *StringReader) Read(data []byte) (int, error) {
                if s.pos >= len(s.data) {
                    return 0, io.EOF
                }
                n := copy(data, s.data[s.pos:])
                s.pos += n
                return n, nil
            }

            func (s *StringReader) Write(data []byte) (int, error) {
                s.data += string(data)
                return len(data), nil
            }

            func processReader(r Reader) {
                data := make([]byte, 1024)
                n, err := r.Read(data)
                if err != nil {
                    fmt.Printf("Read error: %v\n", err)
                    return
                }
                fmt.Printf("Read %d bytes: %s\n", n, string(data[:n]))
            }

            func main() {
                fileReader := FileReader{filename: "test.txt"}
                stringReader := &StringReader{data: "Hello, World!"}

                processReader(fileReader)
                processReader(stringReader)

                var rw ReadWriter = stringReader
                rw.Write([]byte(" Additional data"))
                processReader(rw)
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_complex_generics() {
        let input = r#"
            package main

            import (
                "fmt"
                "sort"
                "reflect"
                "unsafe"
            )

            type Comparable interface {
                LessThan(Comparable) bool
            }

            type Int int

            func (i Int) LessThan(other Comparable) bool {
                if o, ok := other.(Int); ok {
                    return i < o
                }
                return false
            }

            func (i Int) unusedMethod() bool {
                return false
            }

            type String string

            func (s String) LessThan(other Comparable) bool {
                if o, ok := other.(String); ok {
                    return s < o
                }
                return false
            }

            func sortComparables(items []Comparable) {
                sort.Slice(items, func(i, j int) bool {
                    return items[i].LessThan(items[j])
                })
            }

            func printType(v interface{}) {
                t := reflect.TypeOf(v)
                fmt.Printf("Type: %s, Kind: %s\n", t, t.Kind())
            }

            func main() {
                ints := []Comparable{Int(3), Int(1), Int(2)}
                strings := []Comparable{String("c"), String("a"), String("b")}

                sortComparables(ints)
                sortComparables(strings)

                for _, item := range ints {
                    printType(item)
                    fmt.Printf("Value: %v\n", item)
                }

                for _, item := range strings {
                    printType(item)
                    fmt.Printf("Value: %v\n", item)
                }
            }
        "#;

        let expected = r#"
            package main

            import (
                "fmt"
                "sort"
                "reflect"
            )

            type Comparable interface {
                LessThan(Comparable) bool
            }

            type Int int

            func (i Int) LessThan(other Comparable) bool {
                if o, ok := other.(Int); ok {
                    return i < o
                }
                return false
            }

            type String string

            func (s String) LessThan(other Comparable) bool {
                if o, ok := other.(String); ok {
                    return s < o
                }
                return false
            }

            func sortComparables(items []Comparable) {
                sort.Slice(items, func(i, j int) bool {
                    return items[i].LessThan(items[j])
                })
            }

            func printType(v interface{}) {
                t := reflect.TypeOf(v)
                fmt.Printf("Type: %s, Kind: %s\n", t, t.Kind())
            }

            func main() {
                ints := []Comparable{Int(3), Int(1), Int(2)}
                strings := []Comparable{String("c"), String("a"), String("b")}

                sortComparables(ints)
                sortComparables(strings)

                for _, item := range ints {
                    printType(item)
                    fmt.Printf("Value: %v\n", item)
                }

                for _, item := range strings {
                    printType(item)
                    fmt.Printf("Value: %v\n", item)
                }
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_complex_reflection() {
        let input = r#"
            package main

            import (
                "fmt"
                "reflect"
                "unsafe"
                "runtime"
                "os"
            )

            type Person struct {
                Name string `json:"name" db:"person_name"`
                Age  int    `json:"age" db:"person_age"`
            }

            func (p Person) String() string {
                return fmt.Sprintf("Person{Name: %s, Age: %d}", p.Name, p.Age)
            }

            func (p Person) unusedMethod() string {
                return "unused"
            }

            func inspectStruct(v interface{}) {
                t := reflect.TypeOf(v)
                v_val := reflect.ValueOf(v)

                fmt.Printf("Type: %s\n", t)
                fmt.Printf("Kind: %s\n", t.Kind())
                fmt.Printf("NumField: %d\n", t.NumField())

                for i := 0; i < t.NumField(); i++ {
                    field := t.Field(i)
                    fieldValue := v_val.Field(i)

                    fmt.Printf("Field %d: %s (%s) = %v\n",
                    i, field.Name, field.Type, fieldValue.Interface())

                    if tag := field.Tag.Get("json"); tag != "" {
                        fmt.Printf("  JSON tag: %s\n", tag)
                    }
                    if tag := field.Tag.Get("db"); tag != "" {
                        fmt.Printf("  DB tag: %s\n", tag)
                    }
                }
            }

            func main() {
                person := Person{Name: "Alice", Age: 30}
                inspectStruct(person)

                // Test unsafe operations
                ptr := unsafe.Pointer(&person)
                fmt.Printf("Pointer: %p\n", ptr)

                // Test runtime info
                fmt.Printf("NumCPU: %d\n", runtime.NumCPU())
                fmt.Printf("GOMAXPROCS: %d\n", runtime.GOMAXPROCS(0))
            }
        "#;

        let expected = r#"
            package main

            import (
                "fmt"
                "reflect"
                "unsafe"
                "runtime"
            )

            type Person struct {
                Name string `json:"name" db:"person_name"`
                Age  int    `json:"age" db:"person_age"`
            }

            func inspectStruct(v interface{}) {
                t := reflect.TypeOf(v)
                v_val := reflect.ValueOf(v)

                fmt.Printf("Type: %s\n", t)
                fmt.Printf("Kind: %s\n", t.Kind())
                fmt.Printf("NumField: %d\n", t.NumField())

                for i := 0; i < t.NumField(); i++ {
                    field := t.Field(i)
                    fieldValue := v_val.Field(i)

                    fmt.Printf("Field %d: %s (%s) = %v\n",
                    i, field.Name, field.Type, fieldValue.Interface())

                    if tag := field.Tag.Get("json"); tag != "" {
                        fmt.Printf("  JSON tag: %s\n", tag)
                    }
                    if tag := field.Tag.Get("db"); tag != "" {
                        fmt.Printf("  DB tag: %s\n", tag)
                    }
                }
            }

            func main() {
                person := Person{Name: "Alice", Age: 30}
                inspectStruct(person)

                // Test unsafe operations
                ptr := unsafe.Pointer(&person)
                fmt.Printf("Pointer: %p\n", ptr)

                // Test runtime info
                fmt.Printf("NumCPU: %d\n", runtime.NumCPU())
                fmt.Printf("GOMAXPROCS: %d\n", runtime.GOMAXPROCS(0))
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }

    #[test]
    fn test_cleanup_complex_channels() {
        let input = r#"
            package main

            import (
                "fmt"
                "time"
                "context"
                "sync"
                "math/rand"
            )

            type Message struct {
                ID   int
                Data string
            }

            func (m Message) Process() string {
                return fmt.Sprintf("Message %d: %s", m.ID, m.Data)
            }

            func (m Message) unusedMethod() string {
                return "unused"
            }

            func producer(ctx context.Context, messages chan<- Message, wg *sync.WaitGroup) {
                defer wg.Done()
                defer close(messages)

                for i := 0; i < 10; i++ {
                    select {
                    case <-ctx.Done():
                        return
                    case messages <- Message{ID: i, Data: fmt.Sprintf("data-%d", i)}:
                        time.Sleep(time.Duration(rand.Intn(100)) * time.Millisecond)
                    }
                }
            }

            func consumer(ctx context.Context, messages <-chan Message, wg *sync.WaitGroup) {
                defer wg.Done()

                for {
                    select {
                    case <-ctx.Done():
                        return
                    case msg, ok := <-messages:
                        if !ok {
                            return
                        }
                        fmt.Println(msg.Process())
                    }
                }
            }

            func main() {
                ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
                defer cancel()

                messages := make(chan Message, 5)
                var wg sync.WaitGroup

                wg.Add(2)
                go producer(ctx, messages, &wg)
                go consumer(ctx, messages, &wg)

                wg.Wait()
            }
        "#;

        let expected = r#"
            package main

            import (
                "fmt"
                "time"
                "context"
                "math/rand"
            )

            type Message struct {
                ID   int
                Data string
            }

            func (m Message) Process() string {
                return fmt.Sprintf("Message %d: %s", m.ID, m.Data)
            }

            func producer(ctx context.Context, messages chan<- Message, wg *sync.WaitGroup) {
                defer wg.Done()
                defer close(messages)

                for i := 0; i < 10; i++ {
                    select {
                    case <-ctx.Done():
                        return
                    case messages <- Message{ID: i, Data: fmt.Sprintf("data-%d", i)}:
                        time.Sleep(time.Duration(rand.Intn(100)) * time.Millisecond)
                    }
                }
            }

            func consumer(ctx context.Context, messages <-chan Message, wg *sync.WaitGroup) {
                defer wg.Done()

                for {
                    select {
                    case <-ctx.Done():
                        return
                    case msg, ok := <-messages:
                        if !ok {
                            return
                        }
                        fmt.Println(msg.Process())
                    }
                }
            }

            func main() {
                ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
                defer cancel()

                messages := make(chan Message, 5)
                var wg sync.WaitGroup

                wg.Add(2)
                go producer(ctx, messages, &wg)
                go consumer(ctx, messages, &wg)

                wg.Wait()
            }
        "#;

        assert_cleanup_result(input, expected, true);
    }
}
