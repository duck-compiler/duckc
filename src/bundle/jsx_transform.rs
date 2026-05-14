/// Transform a raw JSX string into Preact `h()` calls.
pub fn transform_jsx(raw: &str) -> String {
    let src = raw.trim().as_bytes();
    if src.is_empty() || src[0] != b'<' {
        return raw.to_string();
    }
    let (node, _) = parse_node(src, 0);
    render_node(&node)
}

#[derive(Debug)]
enum Node {
    Element {
        tag: String,
        attrs: Vec<Attr>,
        children: Vec<Node>,
    },
    Text(String),
    Expr(String),
}

#[derive(Debug)]
struct Attr {
    name: String,
    value: AttrValue,
}

#[derive(Debug)]
enum AttrValue {
    Bool,
    Str(String),
    Expr(String),
}

fn skip_ws(src: &[u8], mut i: usize) -> usize {
    while i < src.len() && matches!(src[i], b' ' | b'\n' | b'\r' | b'\t') {
        i += 1;
    }
    i
}

/// Read content between balanced `{ }`, starting at the opening `{`.
/// Returns (inner_content, position_after_closing_brace).
fn read_braces(src: &[u8], start: usize) -> (String, usize) {
    let mut i = start + 1; // skip '{'
    let mut depth = 1usize;
    let content_start = i;
    while i < src.len() && depth > 0 {
        match src[i] {
            b'{' => {
                depth += 1;
                i += 1;
            }
            b'}' => {
                depth -= 1;
                i += 1;
            }
            b'"' | b'\'' => {
                let q = src[i];
                i += 1;
                while i < src.len() && src[i] != q {
                    if src[i] == b'\\' {
                        i += 1;
                    }
                    if i < src.len() {
                        i += 1;
                    }
                }
                if i < src.len() {
                    i += 1;
                }
            }
            _ => {
                i += 1;
            }
        }
    }
    let end = if depth == 0 { i - 1 } else { i };
    let content = String::from_utf8_lossy(&src[content_start..end]).to_string();
    (content, i)
}

fn parse_node(src: &[u8], start: usize) -> (Node, usize) {
    let i = skip_ws(src, start);
    if i >= src.len() {
        return (Node::Text(String::new()), i);
    }
    if src[i] == b'<' {
        parse_element(src, i)
    } else if src[i] == b'{' {
        let (expr, end) = read_braces(src, i);
        (Node::Expr(expr), end)
    } else {
        parse_text(src, i)
    }
}

fn parse_element(src: &[u8], start: usize) -> (Node, usize) {
    let mut i = start + 1; // skip '<'

    // Tag name
    let tag_start = i;
    while i < src.len()
        && (src[i].is_ascii_alphanumeric() || src[i] == b'-' || src[i] == b'_' || src[i] == b'.')
    {
        i += 1;
    }
    let tag = String::from_utf8_lossy(&src[tag_start..i]).to_string();

    // Attributes
    let mut attrs = Vec::new();
    loop {
        i = skip_ws(src, i);
        if i >= src.len()
            || src[i] == b'>'
            || (src[i] == b'/' && i + 1 < src.len() && src[i + 1] == b'>')
        {
            break;
        }
        // Attribute name
        let name_start = i;
        while i < src.len() && !matches!(src[i], b'=' | b' ' | b'\n' | b'\t' | b'>' | b'/') {
            i += 1;
        }
        let name = String::from_utf8_lossy(&src[name_start..i]).to_string();
        if name.is_empty() {
            break;
        }

        // Attribute value
        let value = if i < src.len() && src[i] == b'=' {
            i += 1; // skip '='
            if i < src.len() && src[i] == b'"' {
                i += 1;
                let vs = i;
                while i < src.len() && src[i] != b'"' {
                    i += 1;
                }
                let s = String::from_utf8_lossy(&src[vs..i]).to_string();
                if i < src.len() {
                    i += 1;
                }
                AttrValue::Str(s)
            } else if i < src.len() && src[i] == b'\'' {
                i += 1;
                let vs = i;
                while i < src.len() && src[i] != b'\'' {
                    i += 1;
                }
                let s = String::from_utf8_lossy(&src[vs..i]).to_string();
                if i < src.len() {
                    i += 1;
                }
                AttrValue::Str(s)
            } else if i < src.len() && src[i] == b'{' {
                let (expr, end) = read_braces(src, i);
                i = end;
                AttrValue::Expr(expr)
            } else {
                AttrValue::Bool
            }
        } else {
            AttrValue::Bool
        };
        attrs.push(Attr { name, value });
    }

    // Self-closing?
    if i < src.len() && src[i] == b'/' {
        i += 2; // skip '/>'
        return (
            Node::Element {
                tag,
                attrs,
                children: vec![],
            },
            i,
        );
    }
    if i < src.len() && src[i] == b'>' {
        i += 1;
    }

    // Children
    let mut children = Vec::new();
    loop {
        i = skip_ws(src, i);
        if i >= src.len() {
            break;
        }

        // Closing tag?
        if src[i] == b'<' && i + 1 < src.len() && src[i + 1] == b'/' {
            while i < src.len() && src[i] != b'>' {
                i += 1;
            }
            if i < src.len() {
                i += 1;
            }
            break;
        }

        if src[i] == b'<' {
            let (child, end) = parse_element(src, i);
            i = end;
            children.push(child);
        } else if src[i] == b'{' {
            let (expr, end) = read_braces(src, i);
            i = end;
            if !expr.trim().is_empty() {
                children.push(Node::Expr(expr));
            }
        } else {
            // Text content
            let ts = i;
            while i < src.len() && src[i] != b'<' && src[i] != b'{' {
                i += 1;
            }
            let text = String::from_utf8_lossy(&src[ts..i]).to_string();
            let text = text.trim().to_string();
            if !text.is_empty() {
                children.push(Node::Text(text));
            }
        }
    }

    (
        Node::Element {
            tag,
            attrs,
            children,
        },
        i,
    )
}

fn parse_text(src: &[u8], start: usize) -> (Node, usize) {
    let mut i = start;
    while i < src.len() && src[i] != b'<' && src[i] != b'{' {
        i += 1;
    }
    let text = String::from_utf8_lossy(&src[start..i])
        .to_string()
        .trim()
        .to_string();
    (Node::Text(text), i)
}

fn render_node(node: &Node) -> String {
    match node {
        Node::Text(s) => {
            let s = s.trim();
            if s.is_empty() {
                return String::new();
            }
            format!("{s:?}")
        }
        Node::Expr(s) => transform_expr(s.trim()),
        Node::Element {
            tag,
            attrs,
            children,
        } => {
            let tag_arg = if tag.chars().next().map_or(false, |c| c.is_uppercase()) {
                tag.clone()
            } else {
                format!("{tag:?}")
            };

            let attrs_arg = if attrs.is_empty() {
                "null".to_string()
            } else {
                let pairs: Vec<String> = attrs
                    .iter()
                    .map(|a| {
                        let key = if a.name == "class" {
                            "className".to_string()
                        } else {
                            a.name.clone()
                        };
                        let val = match &a.value {
                            AttrValue::Bool => "true".to_string(),
                            AttrValue::Str(s) => format!("{s:?}"),
                            AttrValue::Expr(s) => transform_expr(s.trim()),
                        };
                        format!("{key}: {val}")
                    })
                    .collect();
                format!("{{{}}}", pairs.join(", "))
            };

            let child_args: Vec<String> = children
                .iter()
                .map(render_node)
                .filter(|s| !s.is_empty())
                .collect();

            let mut args = format!("{tag_arg}, {attrs_arg}");
            if !child_args.is_empty() {
                args += &format!(", {}", child_args.join(", "));
            }
            format!("h({args})")
        }
    }
}

/// Duck's empty lambda `|| expr` -> JS arrow `(() => expr)`.
pub fn transform_expr(s: &str) -> String {
    let s = s.trim();
    if s.starts_with("||") {
        let body = s[2..].trim();
        format!("(() => {body})")
    } else {
        s.to_string()
    }
}
