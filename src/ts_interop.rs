//! TypeScript .d.ts declaration file parser.
//! Extracts exported names and simplified types. Unknown types fall back to `any`.

use std::{
    collections::HashMap,
    path::Path,
    sync::{Mutex, OnceLock},
};

use crate::go_interop::{GoFieldInfo, GoFuncInfo, GoPackageData, GoParam, GoTypeInfo};

// Cache

static TS_CACHE: OnceLock<Mutex<HashMap<String, GoPackageData>>> = OnceLock::new();

fn cache() -> &'static Mutex<HashMap<String, GoPackageData>> {
    TS_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Scan a TypeScript package from `node_modules` and cache it. Returns true on success.
pub fn scan_package(pkg_name: &str, node_modules: &Path) -> bool {
    {
        let guard = cache().lock().unwrap();
        if guard.contains_key(pkg_name) {
            return true;
        }
    }

    let Some((source, dts_path)) = find_dts(pkg_name, node_modules) else {
        return false;
    };
    let file_dir = dts_path.parent();
    let pkg = parse_dts(&source, pkg_name, file_dir);
    cache().lock().unwrap().insert(pkg_name.to_string(), pkg);
    true
}

/// Look up a package that was previously scanned.
pub fn get_package(pkg_name: &str) -> Option<GoPackageData> {
    cache().lock().unwrap().get(pkg_name).cloned()
}

// .d.ts discovery

fn find_dts(pkg_name: &str, node_modules: &Path) -> Option<(String, std::path::PathBuf)> {
    // `pkg_name` may be a subpath like `preact/hooks` - map `/` to the OS separator.
    let pkg_dir = node_modules.join(pkg_name.replace('/', std::path::MAIN_SEPARATOR_STR));

    // 1. `node_modules/<pkg>/index.d.ts`
    let index = pkg_dir.join("index.d.ts");
    if index.exists() {
        if let Ok(txt) = std::fs::read_to_string(&index) {
            return Some((txt, index));
        }
    }

    // 2. `package.json` -> "types" or "typings" field.
    let pkg_json_path = pkg_dir.join("package.json");
    if let Ok(json) = std::fs::read_to_string(&pkg_json_path) {
        if let Some(main_dts) = extract_json_string_field(&json, "types")
            .or_else(|| extract_json_string_field(&json, "typings"))
        {
            let resolved = pkg_dir.join(&main_dts);
            let with_ext = if main_dts.ends_with(".d.ts") {
                resolved
            } else {
                resolved.with_extension("d.ts")
            };
            if let Ok(txt) = std::fs::read_to_string(&with_ext) {
                return Some((txt, with_ext));
            }
        }
    }

    // 3. `@types/<base-pkg>` fallback (only for non-subpath names).
    if !pkg_name.contains('/') || pkg_name.starts_with('@') {
        let at_types_index = node_modules
            .join("@types")
            .join(pkg_name)
            .join("index.d.ts");
        if at_types_index.exists() {
            if let Ok(txt) = std::fs::read_to_string(&at_types_index) {
                return Some((txt, at_types_index));
            }
        }
    }

    None
}

/// Naively extract a top-level string field from JSON (no full JSON parser needed).
fn extract_json_string_field(json: &str, field: &str) -> Option<String> {
    let needle = format!("\"{}\"", field);
    let pos = json.find(&needle)?;
    let rest = &json[pos + needle.len()..];
    let colon = rest.find(':')? + 1;
    let after = rest[colon..].trim_start();
    if after.starts_with('"') {
        let inner = &after[1..];
        let end = inner.find('"')?;
        Some(inner[..end].to_string())
    } else {
        None
    }
}

// Tokenizer

#[derive(Debug, Clone, PartialEq)]
enum Tok {
    Ident(String),
    Str(String),
    Semi,
    Colon,
    Comma,
    Dot,
    Dot3,
    Eq,
    FatArrow, // =>
    Question,
    Bang,
    Pipe,
    Amp,
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Lt,
    Gt,
    Star,
    Eof,
}

struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src: src.as_bytes(),
            pos: 0,
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            // Skip whitespace.
            while self.pos < self.src.len() && self.src[self.pos].is_ascii_whitespace() {
                self.pos += 1;
            }
            // Single-line comment.
            if self.pos + 1 < self.src.len()
                && self.src[self.pos] == b'/'
                && self.src[self.pos + 1] == b'/'
            {
                while self.pos < self.src.len() && self.src[self.pos] != b'\n' {
                    self.pos += 1;
                }
                continue;
            }
            // Block comment.
            if self.pos + 1 < self.src.len()
                && self.src[self.pos] == b'/'
                && self.src[self.pos + 1] == b'*'
            {
                self.pos += 2;
                while self.pos + 1 < self.src.len() {
                    if self.src[self.pos] == b'*' && self.src[self.pos + 1] == b'/' {
                        self.pos += 2;
                        break;
                    }
                    self.pos += 1;
                }
                continue;
            }
            break;
        }
    }

    fn next(&mut self) -> Tok {
        self.skip_whitespace_and_comments();
        if self.pos >= self.src.len() {
            return Tok::Eof;
        }
        let c = self.src[self.pos];
        match c {
            b';' => {
                self.pos += 1;
                Tok::Semi
            }
            b':' => {
                self.pos += 1;
                Tok::Colon
            }
            b',' => {
                self.pos += 1;
                Tok::Comma
            }
            b'?' => {
                self.pos += 1;
                Tok::Question
            }
            b'!' => {
                self.pos += 1;
                Tok::Bang
            }
            b'|' => {
                self.pos += 1;
                Tok::Pipe
            }
            b'&' => {
                self.pos += 1;
                Tok::Amp
            }
            b'{' => {
                self.pos += 1;
                Tok::LBrace
            }
            b'}' => {
                self.pos += 1;
                Tok::RBrace
            }
            b'(' => {
                self.pos += 1;
                Tok::LParen
            }
            b')' => {
                self.pos += 1;
                Tok::RParen
            }
            b'[' => {
                self.pos += 1;
                Tok::LBracket
            }
            b']' => {
                self.pos += 1;
                Tok::RBracket
            }
            b'<' => {
                self.pos += 1;
                Tok::Lt
            }
            b'>' => {
                self.pos += 1;
                Tok::Gt
            }
            b'*' => {
                self.pos += 1;
                Tok::Star
            }
            b'.' => {
                if self.pos + 2 < self.src.len()
                    && self.src[self.pos + 1] == b'.'
                    && self.src[self.pos + 2] == b'.'
                {
                    self.pos += 3;
                    Tok::Dot3
                } else {
                    self.pos += 1;
                    Tok::Dot
                }
            }
            b'=' => {
                if self.pos + 1 < self.src.len() && self.src[self.pos + 1] == b'>' {
                    self.pos += 2;
                    Tok::FatArrow
                } else {
                    self.pos += 1;
                    Tok::Eq
                }
            }
            b'"' | b'\'' | b'`' => {
                let quote = c;
                self.pos += 1;
                let start = self.pos;
                while self.pos < self.src.len() {
                    if self.src[self.pos] == quote {
                        break;
                    }
                    if self.src[self.pos] == b'\\' {
                        self.pos += 1;
                    }
                    self.pos += 1;
                }
                let s = String::from_utf8_lossy(&self.src[start..self.pos]).into_owned();
                if self.pos < self.src.len() {
                    self.pos += 1;
                }
                Tok::Str(s)
            }
            c if c.is_ascii_alphabetic() || c == b'_' || c == b'@' || c == b'$' => {
                let start = self.pos;
                while self.pos < self.src.len()
                    && (self.src[self.pos].is_ascii_alphanumeric()
                        || self.src[self.pos] == b'_'
                        || self.src[self.pos] == b'$')
                {
                    self.pos += 1;
                }
                Tok::Ident(String::from_utf8_lossy(&self.src[start..self.pos]).into_owned())
            }
            _ => {
                self.pos += 1;
                // Skip unknown characters.
                self.next()
            }
        }
    }
}

// Parser

struct Parser {
    tokens: Vec<Tok>,
    pos: usize,
    /// Accumulated exported declarations.
    funcs: HashMap<String, GoFuncInfo>,
    types: HashMap<String, GoTypeInfo>,
    vars: HashMap<String, String>,
    consts: HashMap<String, String>,
    /// `export * from "..."` or `export { X } from "..."` paths to follow.
    reexports: Vec<String>,
}

impl Parser {
    fn new(src: &str) -> Self {
        let mut lex = Lexer::new(src);
        let mut tokens = Vec::new();
        loop {
            let t = lex.next();
            let done = t == Tok::Eof;
            tokens.push(t);
            if done {
                break;
            }
        }
        Self {
            tokens,
            pos: 0,
            funcs: HashMap::new(),
            types: HashMap::new(),
            vars: HashMap::new(),
            consts: HashMap::new(),
            reexports: Vec::new(),
        }
    }

    fn peek(&self) -> &Tok {
        self.tokens.get(self.pos).unwrap_or(&Tok::Eof)
    }

    fn advance(&mut self) -> &Tok {
        let t = self.tokens.get(self.pos).unwrap_or(&Tok::Eof);
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        t
    }

    fn eat_ident(&mut self, kw: &str) -> bool {
        if self.peek() == &Tok::Ident(kw.to_string()) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect_ident(&mut self) -> Option<String> {
        if let Tok::Ident(s) = self.peek().clone() {
            self.advance();
            Some(s)
        } else {
            None
        }
    }

    fn eat(&mut self, t: &Tok) -> bool {
        if self.peek() == t {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Skip a balanced group: `(...)`, `{...}`, `<...>`, `[...]`
    fn skip_balanced(&mut self, open: &Tok, close: &Tok) {
        if self.peek() != open {
            return;
        }
        self.advance();
        let mut depth = 1;
        loop {
            let t = self.peek().clone();
            if t == *open {
                depth += 1;
            }
            if t == *close {
                depth -= 1;
                if depth == 0 {
                    self.advance();
                    return;
                }
            }
            if t == Tok::Eof {
                return;
            }
            self.advance();
        }
    }

    /// Skip generic type parameters `<...>` if present.
    fn skip_generics(&mut self) {
        self.skip_balanced(&Tok::Lt, &Tok::Gt);
    }

    /// Collect a raw type string up to a set of terminator tokens.
    /// Handles balanced `<>`, `()`, `[]`, `{}` within the type.
    fn collect_type_str(&mut self) -> String {
        let mut out = String::new();
        let mut depth_angle = 0i32;
        let mut depth_paren = 0i32;
        let mut depth_bracket = 0i32;
        let mut depth_brace = 0i32;

        loop {
            let t = self.peek().clone();
            match &t {
                Tok::Lt => {
                    depth_angle += 1;
                }
                Tok::Gt if depth_angle > 0 => {
                    depth_angle -= 1;
                }
                Tok::LParen => {
                    depth_paren += 1;
                }
                Tok::RParen if depth_paren > 0 => {
                    depth_paren -= 1;
                }
                Tok::LBracket => {
                    depth_bracket += 1;
                }
                Tok::RBracket if depth_bracket > 0 => {
                    depth_bracket -= 1;
                }
                Tok::LBrace => {
                    depth_brace += 1;
                }
                Tok::RBrace if depth_brace > 0 => {
                    depth_brace -= 1;
                }

                // Terminators when at top level.
                Tok::Semi | Tok::Eof
                    if depth_angle == 0
                        && depth_paren == 0
                        && depth_bracket == 0
                        && depth_brace == 0 =>
                {
                    break;
                }
                Tok::Comma | Tok::RParen | Tok::RBracket | Tok::RBrace | Tok::Eq
                    if depth_angle == 0
                        && depth_paren == 0
                        && depth_bracket == 0
                        && depth_brace == 0 =>
                {
                    break;
                }
                _ => {}
            }
            let tok_str = tok_to_str(&t);
            if !out.is_empty()
                && !tok_str.starts_with([')', ']', '>', '}', ',', ';'])
                && !out.ends_with(['(', '[', '<', '{', ' ', '.'])
            {
                out.push(' ');
            }
            out.push_str(&tok_str);
            self.advance();
        }
        out.trim().to_string()
    }

    /// Parse a single parameter: `name?: type` or `...name: type`.
    /// Returns `(name, type_str, is_variadic)`.
    fn parse_param(&mut self) -> Option<(String, String, bool)> {
        let variadic = self.eat(&Tok::Dot3);
        let name = match self.peek().clone() {
            Tok::Ident(s) => {
                self.advance();
                s
            }
            _ => return None,
        };
        self.eat(&Tok::Question);
        self.eat(&Tok::Bang);
        let type_str = if self.eat(&Tok::Colon) {
            self.collect_type_str()
        } else {
            "any".to_string()
        };
        Some((name, type_str, variadic))
    }

    /// Parse a parameter list `(param1, param2, ...)` -> `Vec<GoParam>`.
    fn parse_params(&mut self) -> Vec<GoParam> {
        if !self.eat(&Tok::LParen) {
            return vec![];
        }
        let mut params = Vec::new();
        loop {
            // Skip index signatures `[key: K]: V`
            if self.peek() == &Tok::LBracket {
                self.skip_balanced(&Tok::LBracket, &Tok::RBracket);
                self.eat(&Tok::Colon);
                if self.peek() != &Tok::RParen {
                    self.collect_type_str();
                }
                self.eat(&Tok::Comma);
                continue;
            }
            if self.peek() == &Tok::RParen || self.peek() == &Tok::Eof {
                break;
            }
            // Skip modifiers: `public`, `private`, `protected`, `readonly`
            loop {
                let skip = matches!(self.peek(), Tok::Ident(k) if matches!(k.as_str(), "public"|"private"|"protected"|"readonly"));
                if skip {
                    self.advance();
                } else {
                    break;
                }
            }
            if let Some((name, ty, variadic)) = self.parse_param() {
                params.push(GoParam {
                    name,
                    type_str: ty,
                    variadic,
                });
            }
            if !self.eat(&Tok::Comma) {
                break;
            }
        }
        self.eat(&Tok::RParen);
        params
    }

    /// Parse return type annotation `: type` -> type string.
    fn parse_return_type(&mut self) -> Vec<String> {
        if !self.eat(&Tok::Colon) {
            return vec![];
        }
        let ty = self.collect_type_str();
        if ty.is_empty() || ty == "void" || ty == "undefined" || ty == "never" {
            vec![]
        } else {
            vec![ty]
        }
    }

    /// Skip to next `}` or `;` at top level (error recovery).
    fn skip_to_stmt_end(&mut self) {
        let mut depth = 0i32;
        loop {
            match self.peek() {
                Tok::LBrace => {
                    depth += 1;
                    self.advance();
                }
                Tok::RBrace => {
                    if depth == 0 {
                        break;
                    }
                    depth -= 1;
                    self.advance();
                    if depth == 0 {
                        break;
                    }
                }
                Tok::Semi if depth == 0 => {
                    self.advance();
                    break;
                }
                Tok::Eof => break,
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Parse an interface or class body `{ ... }` into methods and fields.
    fn parse_body(&mut self) -> (HashMap<String, GoFieldInfo>, HashMap<String, GoFuncInfo>) {
        let mut fields = HashMap::new();
        let mut methods = HashMap::new();

        if !self.eat(&Tok::LBrace) {
            return (fields, methods);
        }

        loop {
            if self.peek() == &Tok::RBrace || self.peek() == &Tok::Eof {
                break;
            }

            // Skip modifiers.
            loop {
                let skip = matches!(self.peek(), Tok::Ident(k) if
                    matches!(k.as_str(), "public"|"private"|"protected"|"readonly"|"static"|"abstract"|"override"|"declare")
                );
                if skip {
                    self.advance();
                } else {
                    break;
                }
            }

            // `new(...)` constructor signature in interface.
            if self.eat_ident("new") {
                self.skip_balanced(&Tok::LParen, &Tok::RParen);
                self.parse_return_type();
                self.eat(&Tok::Semi);
                continue;
            }

            // Index signature: `[key: K]: V`
            if self.peek() == &Tok::LBracket {
                self.skip_balanced(&Tok::LBracket, &Tok::RBracket);
                self.eat(&Tok::Colon);
                self.collect_type_str();
                self.eat(&Tok::Semi);
                continue;
            }

            // `get` / `set` accessors - consume the keyword, don't need to distinguish.
            let is_get = self.eat_ident("get");
            if !is_get {
                self.eat_ident("set");
            }

            let name = match self.expect_ident() {
                Some(n) => n,
                None => {
                    self.skip_to_stmt_end();
                    continue;
                }
            };
            self.eat(&Tok::Question);

            // Skip generic parameters.
            self.skip_generics();

            if self.peek() == &Tok::LParen {
                // Method.
                let params = self.parse_params();
                let results = self.parse_return_type();
                methods.insert(name, GoFuncInfo { params, results });
            } else {
                // Field.
                let type_str = if self.eat(&Tok::Colon) {
                    self.collect_type_str()
                } else {
                    "any".to_string()
                };
                fields.insert(name, GoFieldInfo { type_str });
            }
            self.eat(&Tok::Semi);
            self.eat(&Tok::Comma);
        }
        self.eat(&Tok::RBrace);
        (fields, methods)
    }

    /// Parse one top-level declaration. Returns false when EOF.
    fn parse_one(&mut self) -> bool {
        // Skip export/declare/abstract/default modifiers.
        loop {
            let skip = matches!(self.peek(), Tok::Ident(k) if
                matches!(k.as_str(), "export"|"declare"|"abstract"|"default"|"async")
            );
            if skip {
                self.advance();
            } else {
                break;
            }
        }

        match self.peek().clone() {
            Tok::Eof => return false,

            // `function name<G>(params): ret;`
            Tok::Ident(k) if k == "function" => {
                self.advance();
                let Some(name) = self.expect_ident() else {
                    self.skip_to_stmt_end();
                    return true;
                };
                self.skip_generics();
                let params = self.parse_params();
                let results = self.parse_return_type();
                // body or semicolon
                if self.peek() == &Tok::LBrace {
                    self.skip_balanced(&Tok::LBrace, &Tok::RBrace);
                } else {
                    self.eat(&Tok::Semi);
                }
                self.funcs.insert(name, GoFuncInfo { params, results });
            }

            // `const name: type;`
            Tok::Ident(k) if matches!(k.as_str(), "const" | "let" | "var") => {
                self.advance();
                let Some(name) = self.expect_ident() else {
                    self.skip_to_stmt_end();
                    return true;
                };
                self.eat(&Tok::Question);
                let type_str = if self.eat(&Tok::Colon) {
                    self.collect_type_str()
                } else {
                    "any".to_string()
                };
                self.eat(&Tok::Semi);
                self.vars.insert(name, type_str);
            }

            // `interface Name<G> { ... }`
            Tok::Ident(k) if k == "interface" => {
                self.advance();
                let Some(name) = self.expect_ident() else {
                    self.skip_to_stmt_end();
                    return true;
                };
                self.skip_generics();
                // extends clause
                if self.eat_ident("extends") {
                    loop {
                        self.collect_type_str();
                        if !self.eat(&Tok::Comma) {
                            break;
                        }
                    }
                }
                let (fields, methods) = self.parse_body();
                self.types.insert(
                    name,
                    GoTypeInfo {
                        kind: "interface".into(),
                        fields,
                        methods,
                    },
                );
            }

            // `class Name<G> extends B implements I { ... }`
            Tok::Ident(k) if k == "class" => {
                self.advance();
                let Some(name) = self.expect_ident() else {
                    self.skip_to_stmt_end();
                    return true;
                };
                self.skip_generics();
                // extends / implements
                loop {
                    if self.eat_ident("extends") || self.eat_ident("implements") {
                        self.collect_type_str();
                    } else {
                        break;
                    }
                }
                let (fields, methods) = self.parse_body();
                self.types.insert(
                    name,
                    GoTypeInfo {
                        kind: "struct".into(),
                        fields,
                        methods,
                    },
                );
            }

            // `type Name<G> = ...;`
            Tok::Ident(k) if k == "type" => {
                self.advance();
                let Some(name) = self.expect_ident() else {
                    self.skip_to_stmt_end();
                    return true;
                };
                self.skip_generics();
                self.eat(&Tok::Eq);
                self.collect_type_str(); // discard
                self.eat(&Tok::Semi);
                // Register as an opaque type alias.
                self.types.entry(name).or_insert_with(|| GoTypeInfo {
                    kind: "alias".into(),
                    fields: HashMap::new(),
                    methods: HashMap::new(),
                });
            }

            // `enum Name { ... }`
            Tok::Ident(k) if k == "enum" => {
                self.advance();
                let Some(name) = self.expect_ident() else {
                    self.skip_to_stmt_end();
                    return true;
                };
                self.skip_balanced(&Tok::LBrace, &Tok::RBrace);
                self.types.insert(
                    name,
                    GoTypeInfo {
                        kind: "alias".into(),
                        fields: HashMap::new(),
                        methods: HashMap::new(),
                    },
                );
            }

            // `namespace Name { ... }` or `module Name { ... }` or `module "path" { ... }`
            Tok::Ident(k) if matches!(k.as_str(), "namespace" | "module") => {
                self.advance();
                // Module path string - skip.
                if let Tok::Str(_) = self.peek() {
                    self.advance();
                } else {
                    self.expect_ident();
                }
                self.skip_generics();
                // Parse the body recursively as top-level decls.
                if self.eat(&Tok::LBrace) {
                    while self.peek() != &Tok::RBrace && self.peek() != &Tok::Eof {
                        self.parse_one();
                    }
                    self.eat(&Tok::RBrace);
                }
            }

            // `import ...` or `export * from ...` - skip.
            Tok::Ident(k) if matches!(k.as_str(), "import") => {
                self.skip_to_stmt_end();
            }

            // `export = Name;` - skip.
            Tok::Eq => {
                self.skip_to_stmt_end();
            }

            // `export { Foo, Bar } from "path"` or `export { Foo, Bar };`
            Tok::LBrace => {
                self.skip_balanced(&Tok::LBrace, &Tok::RBrace);
                self.eat_ident("from");
                if let Tok::Str(path) = self.peek().clone() {
                    self.reexports.push(path);
                    self.advance();
                }
                self.eat(&Tok::Semi);
            }

            // `* from "path"` (after `export` was consumed above).
            Tok::Star => {
                self.advance(); // eat `*`
                // Could be `* as Name from "path"` or `* from "path"`
                if self.eat_ident("as") {
                    self.expect_ident();
                }
                self.eat_ident("from");
                if let Tok::Str(path) = self.peek().clone() {
                    self.reexports.push(path);
                    self.advance();
                }
                self.eat(&Tok::Semi);
            }

            // Skip semicolons.
            Tok::Semi => {
                self.advance();
            }

            _ => {
                // Unknown top-level construct - skip to next statement boundary.
                self.skip_to_stmt_end();
            }
        }
        true
    }

    fn parse_all(&mut self) {
        while self.parse_one() {}
    }
}

fn tok_to_str(t: &Tok) -> String {
    match t {
        Tok::Ident(s) | Tok::Str(s) => s.clone(),
        Tok::Semi => ";".into(),
        Tok::Colon => ":".into(),
        Tok::Comma => ",".into(),
        Tok::Dot => ".".into(),
        Tok::Dot3 => "...".into(),
        Tok::Eq => "=".into(),
        Tok::FatArrow => "=>".into(),
        Tok::Question => "?".into(),
        Tok::Bang => "!".into(),
        Tok::Pipe => " |".into(),
        Tok::Amp => " &".into(),
        Tok::LBrace => "{".into(),
        Tok::RBrace => "}".into(),
        Tok::LParen => "(".into(),
        Tok::RParen => ")".into(),
        Tok::LBracket => "[".into(),
        Tok::RBracket => "]".into(),
        Tok::Lt => "<".into(),
        Tok::Gt => ">".into(),
        Tok::Star => "*".into(),
        Tok::Eof => "".into(),
    }
}

// Entry point

/// Parse a `.d.ts` source string into a `GoPackageData` with TypeScript type strings.
/// `file_dir` is the directory containing the `.d.ts` file, used to resolve relative re-exports.
pub fn parse_dts(src: &str, pkg_name: &str, file_dir: Option<&std::path::Path>) -> GoPackageData {
    parse_dts_inner(
        src,
        pkg_name,
        file_dir,
        &mut std::collections::HashSet::new(),
    )
}

fn parse_dts_inner(
    src: &str,
    pkg_name: &str,
    file_dir: Option<&std::path::Path>,
    visited: &mut std::collections::HashSet<std::path::PathBuf>,
) -> GoPackageData {
    let mut p = Parser::new(src);
    p.parse_all();

    // Follow `export * from "..."` and `export { X } from "..."` re-exports.
    if let Some(dir) = file_dir {
        for reexport_path in p.reexports.drain(..) {
            // Only follow relative paths (starts with `./` or `../`).
            if !reexport_path.starts_with('.') {
                continue;
            }

            // Resolve the path relative to the current file's directory.
            let base = dir.join(&reexport_path);

            // Try `path.d.ts`, `path/index.d.ts` in order.
            let candidates = [base.with_extension("d.ts"), base.join("index.d.ts")];

            for candidate in &candidates {
                let canonical = match candidate.canonicalize() {
                    Ok(p) => p,
                    Err(_) => candidate.clone(),
                };
                if visited.contains(&canonical) {
                    continue;
                }
                if let Ok(txt) = std::fs::read_to_string(candidate) {
                    visited.insert(canonical);
                    let sub = parse_dts_inner(&txt, pkg_name, candidate.parent(), visited);
                    // Merge sub-module exports into this package (non-overwriting).
                    for (k, v) in sub.funcs {
                        p.funcs.entry(k).or_insert(v);
                    }
                    for (k, v) in sub.types {
                        p.types.entry(k).or_insert(v);
                    }
                    for (k, v) in sub.vars {
                        p.vars.entry(k).or_insert(v);
                    }
                    for (k, v) in sub.consts {
                        p.consts.entry(k).or_insert(v);
                    }
                    break;
                }
            }
        }
    }

    let short_name = pkg_name
        .split('/')
        .next_back()
        .unwrap_or(pkg_name)
        .to_string();
    GoPackageData {
        import_path: pkg_name.to_string(),
        short_name,
        funcs: p.funcs,
        types: p.types,
        vars: p.vars,
        consts: p.consts,
    }
}
