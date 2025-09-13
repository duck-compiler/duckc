use std::collections::HashSet;
use tree_sitter::{Parser, Node, Range};

pub fn remove_unused_imports(go_source: &str) -> String {
    let mut go_parser = Parser::new();
    go_parser
        .set_language(&tree_sitter_go::LANGUAGE.into())
        .expect("compiler error: error loading go treesitter grammar");

    let tree = go_parser
        .parse(go_source, None)
        .expect("compiler error: couldn't parse emitted go source with treesitter");

    let root_node = tree.root_node();
    let source_bytes = go_source.as_bytes();

    let mut declared_imports: HashSet<(String, Range)> = HashSet::new();
    let mut used_packages: HashSet<String> = HashSet::new();

    find_imports_and_usages(
        root_node,
        source_bytes,
        &mut declared_imports,
        &mut used_packages
    );

    let mut ranges_to_comment: Vec<Range> = declared_imports
        .into_iter()
        .filter(|(name, _)| !used_packages.contains(name))
        .map(|(_, range)| range)
        .collect();

    ranges_to_comment.sort_by_key(|r| std::cmp::Reverse(r.start_byte));

    let mut new_source = go_source.to_string();

    for range in ranges_to_comment {
        new_source.insert_str(range.start_byte, "// ");
    }

    new_source
}

fn find_imports_and_usages<'a>(
    node: Node<'a>,
    source: &'a [u8],
    imports: &mut HashSet<(String, Range)>,
    usages: &mut HashSet<String>,
) {
    match node.kind() {
        "import_spec" => {
            let first_child = node.child(0);
            let path_node = node.child(node.child_count() - 1).unwrap();

            if path_node.kind() != "interpreted_string_literal" {
                // todo: do we ignore this?
                // path node should be a string like "path/to/pkg"
                return
            }

            if path_node.kind() == "interpreted_string_literal" {
                let import_path = path_node.utf8_text(source).unwrap().trim_matches('"');

                let package_name = if let Some(child) = first_child {
                    match child.kind() {
                        // import f "fmt" -> import fmt as f
                        "identifier" => Some(child.utf8_text(source).unwrap().to_string()),
                        // import _ "driver" -> side effect import always considered "used"
                        "blank_identifier" => None,
                        // import . "mypackage" -> adds symbols to current namespace.
                        "dot" => None,
                        _ => get_package_name_from_path(import_path),
                    }
                } else {
                    get_package_name_from_path(import_path)
                };

                if let Some(name) = package_name {
                    imports.insert((name, node.range()));
                }
            }
        }
        // och mensch opi
        // this is a field access
        "selector_expression" => {
            if let Some(package_node) = node.child(0) {
                // the first child is the package identifier, hopefully
                if package_node.kind() != "identifier" {
                    return
                }

                let package_name = package_node.utf8_text(source).unwrap().to_string();
                usages.insert(package_name);
            }
        }
        _ => {}
    }

    for child in node.children(&mut node.walk()) {
        find_imports_and_usages(child, source, imports, usages);
    }
}

fn get_package_name_from_path(path: &str) -> Option<String> {
    path.rsplit('/')
        .next()
        .map(|s| s.to_string())
}
