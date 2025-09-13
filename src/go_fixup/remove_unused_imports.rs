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

    let (declarations, _imports, _package_usages) = analyze_source(
        tree.root_node(),
        go_source.as_bytes()
    );

    let live_set = calculate_live_set(&declarations, remove_exported);

    let mut ranges_to_delete = Vec::new();

    for (name, decl) in &declarations {
        if !live_set.contains(name) && (!decl.is_exported || remove_exported) {
            ranges_to_delete.push(decl.range);
        }
    }

    let cleaned_source = drain_ranges(
        go_source,
        ranges_to_delete
    );

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

            for _used_symbol in &used_imports {
                has_used_symbols = true;
                break;
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

        let has_import_spec_list = node.children(&mut node.walk()).any(|child| child.kind() == "import_spec_list");

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
            if let Some(package_node) = node.child(0) {
                if package_node.kind() == "identifier" {
                    let package_name = &go_source[package_node.start_byte()..package_node.end_byte()];
                    used_imports.insert(package_name.to_string());
                }
            }
        } else if node.kind() == "call_expression" {
            if let Some(function_node) = node.child(0) {
                if function_node.kind() == "selector_expression" {
                    if let Some(package_node) = function_node.child(0) {
                        if package_node.kind() == "identifier" {
                            let package_name = &go_source[package_node.start_byte()..package_node.end_byte()];
                            used_imports.insert(package_name.to_string());
                        }
                    }
                }
            }
        } else if node.kind() == "type_identifier" {
            if let Some(parent) = node.parent() {
                if parent.kind() == "selector_expression" {
                    if let Some(package_node) = parent.child(0) {
                        if package_node.kind() == "identifier" {
                            let package_name = &go_source[package_node.start_byte()..package_node.end_byte()];
                            used_imports.insert(package_name.to_string());
                        }
                    }
                }
            }
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
            for spec in node.children(&mut node.walk()).filter(|c| c.kind() == "import_spec") {
                if let Some(name_node) = spec.child(0) {
                    if name_node.kind() == "dot" {
                        if let Some(path_node) = spec.child(spec.child_count() - 1) {
                            if path_node.kind() == "interpreted_string_literal" {
                                let import_path = path_node.utf8_text(go_source.as_bytes()).unwrap().trim_matches('"');
                                if let Some(package_name) = extract_package_name_from_path(import_path) {
                                    used_imports.insert(package_name);
                                }
                            }
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
        if let Some(start) = trimmed.find('"') {
            if let Some(end) = trimmed[start+1..].find('"') {
                let package_path = &trimmed[start+1..start+1+end];
                return extract_package_name_from_path(package_path);
            }
        }
        return None;
    }

    if trimmed.starts_with('.') {
        if let Some(start) = trimmed.find('"') {
            if let Some(end) = trimmed[start+1..].find('"') {
                let package_path = &trimmed[start+1..start+1+end];
                return extract_package_name_from_path(package_path);
            }
        }
        return None;
    }

    if trimmed.starts_with('"') && trimmed.ends_with('"') {
        let package_path = &trimmed[1..trimmed.len()-1];
        return extract_package_name_from_path(package_path);
    }

    if !trimmed.starts_with("import") {
        return None;
    }

    let Some(start) = trimmed.find('"') else { return None; };
    let Some(end) = trimmed[start+1..].find('"') else { return None; };

    let package_path = &trimmed[start+1..start+1+end];
    extract_package_name_from_path(package_path)
}

fn extract_package_name_from_path(package_path: &str) -> Option<String> {
    if let Some(last_slash) = package_path.rfind('/') {
        Some(package_path[last_slash+1..].to_string())
    } else {
        Some(package_path.to_string())
    }
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

    (
        declarations,
        imports,
        package_usages
    )
}

fn parse_node(
    node: Node,
    source: &[u8],
    declarations: &mut HashMap<String, Declaration>,
    imports: &mut HashMap<String, Range>,
) {
    match node.kind() {
        "import_declaration" => parse_imports(node, source, imports),
        "function_declaration" | "method_declaration" | "type_declaration" | "var_declaration" | "const_declaration" => {
            go_parse_declaration(node, source, declarations);
        }
        _ => {
            for child in node.children(&mut node.walk()) {
                parse_node(child, source, declarations, imports);
            }
        }
    }
}

fn calculate_live_set(declarations: &HashMap<String, Declaration>, _remove_exported: bool) -> HashSet<String> {
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
        add_required_methods(&mut live_set, &mut worklist, declarations, &required_methods);
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

fn collect_required_methods(live_set: &HashSet<String>, declarations: &HashMap<String, Declaration>) -> HashSet<String> {
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
        let DeclKind::Method = &decl.kind else { continue; };

        let (receiver_type, method_name) = if let Some(dot_pos) = name.find('.') {
            (&name[..dot_pos], &name[dot_pos + 1..])
        } else {
            continue;
        };

        if required_methods.contains(method_name) && live_set.contains(receiver_type) && live_set.insert(name.clone()) {
            worklist.push_back(name.clone());
        }
    }
}

fn find_receiver_types(live_set: &HashSet<String>, declarations: &HashMap<String, Declaration>) -> HashSet<String> {
    let mut receiver_types = HashSet::new();

    for (name, decl) in declarations {
        if !live_set.contains(name) {
            continue;
        }

        let DeclKind::Method = &decl.kind else { continue; };

        if let Some(dot_pos) = name.find('.') {
            let receiver_type = &name[..dot_pos];
            receiver_types.insert(receiver_type.to_string());
        }
    }

    receiver_types
}

fn find_type_methods(live_set: &HashSet<String>, declarations: &HashMap<String, Declaration>) -> Vec<String> {
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
            let DeclKind::Method = &method_decl.kind else { continue; };

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
            name.contains("as_dgo_string") ||
            name.contains("as_dgo_int") ||
            name.contains("as_dgo_float32") ||
            name.contains("as_dgo_bool") ||
            name.contains("as_dgo_rune")
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
        let DeclKind::Method = &decl.kind else { continue; };

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
        let Some(decl) = declarations.get(&name) else { continue; };

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

fn go_parse_declaration(node: Node, source: &[u8], declarations: &mut HashMap<String, Declaration>) {
    let kind = match node.kind() {
        "function_declaration" => DeclKind::Function,
        "method_declaration" => DeclKind::Method,
        "type_declaration" => DeclKind::Type,
        "var_declaration" => DeclKind::Var,
        "const_declaration" => DeclKind::Const,
        _ => return,
    };

    let nodes_to_process = if matches!(node.kind(), "var_declaration" | "const_declaration" | "type_declaration") {
        find_spec_nodes(node)
    } else {
        vec![node]
    };

    for item_node in nodes_to_process {
        let Some(name_node) = item_node.child_by_field_name("name") else { continue };
        let name = name_node.utf8_text(source).unwrap().to_string();

        let mut dependencies = HashSet::new();
        find_dependencies(item_node, source, &mut dependencies);
        dependencies.remove(&name);

        if item_node.kind() == "method_declaration" {
            if let Some(receiver_type) = find_receiver_type_name(&item_node, source) {
                dependencies.insert(receiver_type.clone());

                let method_name_with_receiver = format!("{}.{}", receiver_type, name);
                declarations.insert(method_name_with_receiver.clone(), Declaration {
                    _name: method_name_with_receiver,
                    kind: DeclKind::Method,
                    range: item_node.range(),
                    is_exported: name.chars().next().unwrap_or('a').is_uppercase(),
                    dependencies: dependencies.clone(),
                });
            }
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

        let range = if matches!(node.kind(), "type_declaration" | "var_declaration" | "const_declaration") {
            node.range()
        } else {
            item_node.range()
        };

        declarations.insert(name.clone(), Declaration {
            _name: name,
            kind: final_kind,
            range,
            is_exported,
            dependencies
        });
    }
}

fn find_spec_nodes(node: Node) -> Vec<Node> {
    let mut nodes_to_process = Vec::new();
    let target_kind = node.kind()
        .replace("declaration", "spec");

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

    let Some(list) = type_body.child_by_field_name("methods") else { return methods; };

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
            if let Some(operand) = node.child(0) {
                if operand.kind() == "identifier" {
                    deps.insert(operand.utf8_text(source).unwrap().to_string());
                }
            }
            if let Some(field) = node.child(1) {
                if field.kind() == "field_identifier" {
                    deps.insert(field.utf8_text(source).unwrap().to_string());
                }
            }
        }
        "type_assertion" => {
            if let Some(type_node) = node.child(1) {
                if type_node.kind() == "type_identifier" {
                    deps.insert(type_node.utf8_text(source).unwrap().to_string());
                }
            }
        }
        "type_switch_expression" => {
            if let Some(expr) = node.child(0) {
                if expr.kind() == "type_assertion" {
                    if let Some(type_node) = expr.child(1) {
                        if type_node.kind() == "type_identifier" {
                            deps.insert(type_node.utf8_text(source).unwrap().to_string());
                        }
                    }
                }
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
            if let Some(type_node) = node.child(0) {
                if type_node.kind() == "type_identifier" {
                    deps.insert(type_node.utf8_text(source).unwrap().to_string());
                }
            }
        }
        _ => {}
    }

    for child in node.children(&mut node.walk()) {
        find_dependencies(child, source, deps);
    }
}

fn find_all_package_usages(node: Node, source: &[u8], usages: &mut HashSet<String>) {
    if node.kind() == "selector_expression" {
        if let Some(operand) = node.child(0) {
            if operand.kind() == "identifier" {
                usages.insert(operand.utf8_text(source).unwrap().to_string());
            }
        }
    }

    for child in node.children(&mut node.walk()) {
        find_all_package_usages(child, source, usages);
    }
}

fn parse_imports(node: Node, source: &[u8], imports: &mut HashMap<String, Range>) {
    for spec in node.children(&mut node.walk()).filter(|c| c.kind() == "import_spec") {
        if let Some(path_node) = spec.child(spec.child_count() - 1) {
            if path_node.kind() == "interpreted_string_literal" {
                let import_path = path_node.utf8_text(source).unwrap().trim_matches('"');
                if let Some(name) = get_import_package_name(&spec, import_path, source) {
                    imports.insert(name, spec.range());
                }
            }
        }
    }
}

fn get_import_package_name(node: &Node, path: &str, source: &[u8]) -> Option<String> {
    if let Some(name_node) = node.child(0) {
        if name_node.kind() != "interpreted_string_literal" {
            return match name_node.kind() {
                "identifier" => Some(name_node.utf8_text(source).unwrap().to_string()),
                "blank_identifier" => Some("_".to_string()), // handle blank imports
                "dot" => Some(path.rsplit('/').next().unwrap_or(path).to_string()), // handle dot imports
                _ => None,
            };
        }
    }
    path.rsplit('/').next().map(|s| s.to_string())
}
