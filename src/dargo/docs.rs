use colored::Colorize;
use duckwind::EmitEnv;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::{
    fs,
    path::{Path, PathBuf},
    sync::mpsc,
};

use crate::{
    cli::go_cli::GoCliErrKind, dargo::cli::DocsGenerateArgs, lex, parse_src_file, tags::Tag,
    typecheck,
};

#[derive(Debug)]
pub enum DocsErrKind {
    CorruptedFileName,
    TargetPathIsDirectory,
    FileNotFound,
    CannotReadFile,
    GoCli(GoCliErrKind),
}

lazy_static! {
    static ref COMPILE_TAG: String = " docs ".on_bright_black().bright_white().to_string();
}

pub struct DocsOutput {
    pub json_output_path: PathBuf,
    pub fn_docs: Vec<FunctionDoc>,
    pub struct_docs: Vec<StructDoc>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DocsField {
    // pub comments: Vec<String>,
    pub field_name: String,
    pub type_annotation: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ExtensionsDoc {
    pub target_type_annotation: String,
    pub comments: Vec<String>,
    pub function_docs: Vec<FunctionDoc>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct StructDoc {
    pub struct_name: String,
    pub fields: Vec<DocsField>,
    pub comments: Vec<String>,
    pub function_docs: Vec<FunctionDoc>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FunctionDoc {
    pub function_name: String,
    pub function_annotation: String,
    pub comments: Vec<String>,
}

pub fn generate(generate_args: DocsGenerateArgs) -> Result<DocsOutput, (String, DocsErrKind)> {
    let src_file: PathBuf = generate_args.file;
    if src_file.is_dir() {
        let message = format!(
            "{}{} the path you provided is a directory. You need to provide a .duck file",
            *COMPILE_TAG,
            Tag::Err,
        );

        return Err((message, DocsErrKind::TargetPathIsDirectory));
    }

    if src_file
        .extension()
        .ok_or_else(|| {
            format!(
                "{}{} couldn't extract file extension from provided source file",
                *COMPILE_TAG,
                Tag::Err,
            )
        })
        .unwrap()
        != "duck"
    {
        let message = format!(
            "{}{} the path you provided is not a valid duck source file. You need to provide a .duck file",
            *COMPILE_TAG,
            Tag::Err,
        );
        return Err((message, DocsErrKind::TargetPathIsDirectory));
    }

    let src_file_name: &'static str = src_file
        .file_name()
        .ok_or_else(|| {
            (
                format!(
                    "{}{} couldn't get the filename from given ",
                    *COMPILE_TAG,
                    Tag::Err
                ),
                DocsErrKind::CorruptedFileName,
            )
        })?
        .to_str()
        .ok_or_else(|| {
            (
                format!(
                    "{}{} the filename is an invalid utf-8 string",
                    *COMPILE_TAG,
                    Tag::Err
                ),
                DocsErrKind::CorruptedFileName,
            )
        })?
        .to_string()
        .leak();

    let src_file_file_contents: &'static str = fs::read_to_string(&src_file)
        .map_err(|err| {
            (
                format!(
                    "{}{} couldn't read file '{}'. msg='{}'",
                    *COMPILE_TAG,
                    Tag::Err,
                    src_file.to_string_lossy().bright_blue(),
                    err.to_string().bright_red()
                ),
                DocsErrKind::CannotReadFile,
            )
        })?
        .to_string()
        .leak();

    let tokens = lex(src_file_name, src_file_file_contents);
    let mut src_file_ast = parse_src_file(&src_file, src_file_name, src_file_file_contents, tokens);

    let (tailwind_worker_send, tailwind_worker_receive) = mpsc::channel::<String>();
    let (tailwind_result_send, _tailwind_result_receive) = mpsc::channel::<String>();

    let tailwind_prefix = None::<String>;

    std::thread::spawn(move || {
        let mut emit_env = EmitEnv::new_with_default_config();
        // emit_env.parse_full_string(src_file_file_contents);
        loop {
            let s = tailwind_worker_receive.recv();
            match s {
                Ok(s) => emit_env.parse_full_string(tailwind_prefix.as_deref(), s.as_str()),
                Err(_) => break,
            }
        }
        tailwind_result_send
            .send(emit_env.to_css_stylesheet(true))
            .expect("could not send css result");
    });

    let mut fn_docs = vec![];
    let mut struct_docs = vec![];
    let mut extensions_docs = vec![];

    let type_env = typecheck(&mut src_file_ast, &tailwind_worker_send);
    type_env
        .struct_definitions
        .iter()
        .for_each(|struct_definition| {
            let mut fn_docs = vec![];
            struct_definition.methods.iter().for_each(|function_def| {
                if !function_def.comments.is_empty() {
                    fn_docs.push(FunctionDoc {
                        function_name: function_def.name.clone(),
                        function_annotation: function_def
                            .type_expr()
                            .0
                            .as_clean_user_faced_type_name(),
                        comments: function_def.comments.iter().map(|c| c.0.clone()).collect(),
                    });
                }
            });

            if !(fn_docs.is_empty() && struct_definition.doc_comments.is_empty()) {
                struct_docs.push(StructDoc {
                    function_docs: fn_docs,
                    struct_name: struct_definition.name.clone(),
                    comments: struct_definition
                        .doc_comments
                        .iter()
                        .map(|c| c.0.clone())
                        .collect(),
                    fields: struct_definition
                        .fields
                        .iter()
                        .map(|field| DocsField {
                            field_name: field.name.clone(),
                            type_annotation: field.type_expr.0.as_clean_user_faced_type_name(),
                        })
                        .collect(),
                });
            }
        });

    src_file_ast
        .extensions_defs
        .iter()
        .for_each(|extensions_def| {
            let mut fn_docs = vec![];
            extensions_def
                .function_definitions
                .iter()
                .for_each(|(function_def, _)| {
                    if !function_def.comments.is_empty() {
                        fn_docs.push(FunctionDoc {
                            function_name: function_def.name.clone(),
                            function_annotation: function_def
                                .type_expr()
                                .0
                                .as_clean_user_faced_type_name(),
                            comments: function_def.comments.iter().map(|c| c.0.clone()).collect(),
                        });
                    }
                });

            if !(fn_docs.is_empty() && extensions_def.doc_comments.is_empty()) {
                extensions_docs.push(ExtensionsDoc {
                    target_type_annotation: extensions_def
                        .target_type_expr
                        .0
                        .as_clean_user_faced_type_name(),
                    function_docs: fn_docs,
                    comments: extensions_def
                        .doc_comments
                        .iter()
                        .map(|c| c.0.clone())
                        .collect(),
                });
            }
        });

    type_env
        .function_definitions
        .iter()
        .for_each(|function_def| {
            if !function_def.comments.is_empty() {
                fn_docs.push(FunctionDoc {
                    function_name: function_def.name.clone(),
                    function_annotation: function_def.type_expr().0.as_clean_user_faced_type_name(),
                    comments: function_def.comments.iter().map(|c| c.0.clone()).collect(),
                });
                println!()
            }
        });

    println!(
        "{}{}{} Successfully generated docs",
        Tag::Dargo,
        *COMPILE_TAG,
        Tag::Check,
    );

    let json_output = serde_json::to_string(&struct_docs).unwrap();
    dbg!(json_output);
    let json_output = serde_json::to_string(&fn_docs).unwrap();
    dbg!(json_output);

    let html = layout_html(&fn_docs, &struct_docs, &extensions_docs);
    // println!("{}", layout_html(&fn_docs, &struct_docs, &extensions_docs));

    let file = Path::new("./docs_output.html");
    fs::write(file, html).expect("couldn't write docs");

    return Ok(DocsOutput {
        json_output_path: Path::new("here").to_path_buf(),
        fn_docs,
        struct_docs,
    });
}

fn layout_html(
    fn_docs: &[FunctionDoc],
    struct_docs: &[StructDoc],
    extensions_docs: &[ExtensionsDoc],
) -> String {
    let sidebar_html = render_sidebar(fn_docs, struct_docs, extensions_docs);

    let structs_html = struct_docs
        .iter()
        .map(render_struct)
        .collect::<Vec<_>>()
        .join("\n");

    let extensions_html = extensions_docs
        .iter()
        .map(render_extension)
        .collect::<Vec<_>>()
        .join("\n");

    let fns_html = fn_docs
        .iter()
        .map(|f| render_function(f, false))
        .collect::<Vec<_>>()
        .join("\n");

    let empty_state_html = r#"
        <div id="no-results" class="hidden text-center py-12 text-[#a89984]">
            <p class="text-xl">No results found matching your search.</p>
        </div>
    "#;

    let body_content = format!(
        r#"
        <div class="flex flex-col md:flex-row h-screen w-full bg-[#282828] overflow-hidden font-sans text-[#ebdbb2]">

            <div class="md:hidden flex items-center justify-between p-4 bg-[#1d2021] border-b border-[#3c3836] shrink-0 z-40">
                <h1 class="text-lg font-bold text-[#fabd2f] tracking-wide">Duck<span class="text-[#ebdbb2]">Docs</span></h1>
                <button onclick="toggleSidebar()" class="text-[#ebdbb2] focus:outline-none p-2 rounded hover:bg-[#32302f]">
                    <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h16M4 18h16"></path>
                    </svg>
                </button>
            </div>

            <div id="sidebar-overlay" onclick="closeSidebar()" class="fixed inset-0 bg-[#1d2021] bg-opacity-90 z-40 hidden transition-opacity opacity-0 md:hidden glass-blur"></div>

            <aside id="sidebar" class="fixed inset-y-0 left-0 z-50 w-72 h-full bg-[#1d2021] border-r border-[#3c3836] flex flex-col transform -translate-x-full transition-transform duration-300 ease-in-out md:relative md:translate-x-0 md:inset-auto shadow-2xl md:shadow-none shrink-0">

                <div class="p-5 border-b border-[#3c3836] flex justify-between items-center bg-[#1d2021] shrink-0">
                    <div class="w-full">
                        <div class="flex justify-between items-center mb-3">
                            <h1 class="text-xl font-bold text-[#fabd2f] tracking-wide hidden md:block">Duck<span class="text-[#ebdbb2]">Docs</span></h1>
                            <h1 class="text-xl font-bold text-[#fabd2f] tracking-wide md:hidden">Menu</h1>

                            <button onclick="closeSidebar()" class="md:hidden text-[#a89984] hover:text-[#fabd2f]">
                                <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path>
                                </svg>
                            </button>
                        </div>

                        <input
                            type="text"
                            id="search-input"
                            placeholder="Search docs..."
                            class="w-full px-3 py-2 bg-[#32302f] border border-[#504945] rounded text-[#ebdbb2] placeholder-[#a89984] focus:outline-none focus:border-[#d79921] focus:ring-1 focus:ring-[#d79921] transition-all text-sm"
                            onkeyup="filterDocs()"
                        />
                    </div>
                </div>

                <nav class="flex-1 overflow-y-auto p-4 space-y-8 scrollbar-thin scrollbar-thumb-[#504945] scrollbar-track-transparent" id="sidebar-nav">
                    {sidebar_html}
                </nav>
            </aside>

            <main class="flex-1 h-full overflow-y-auto bg-[#282828] scroll-smooth scrollbar-thin scrollbar-thumb-[#504945] relative w-full">
                <div class="max-w-5xl mx-auto p-6 md:p-10 space-y-12 md:space-y-16 pb-24" id="main-content">
                    <section>
                        <h2 class="text-2xl md:text-3xl font-bold text-[#fabd2f] mb-6 md:mb-8 border-b border-[#3c3836] pb-3">Structs</h2>
                        <div class="space-y-12 md:space-y-16">
                            {structs_html}
                        </div>
                    </section>

                    <section>
                        <h2 class="text-2xl md:text-3xl font-bold text-[#8ec07c] mb-6 md:mb-8 border-b border-[#3c3836] pb-3">Extensions</h2>
                        <div class="space-y-12 md:space-y-16">
                            {extensions_html}
                        </div>
                    </section>

                    <section>
                        <h2 class="text-2xl md:text-3xl font-bold text-[#b8bb26] mb-6 md:mb-8 border-b border-[#3c3836] pb-3">Global Functions</h2>
                        <div class="space-y-8 md:space-y-10">
                            {fns_html}
                        </div>
                    </section>
                    {empty_state_html}
                </div>

                <footer class="p-10 text-center text-[#a89984] text-sm border-t border-[#3c3836] mt-20">
                    Generated by Dargo
                </footer>
            </main>
        </div>
        "#
    );

    let mut duckwind_emit_env = duckwind::EmitEnv::new_with_default_config();
    duckwind_emit_env.parse_full_string(None, &body_content);
    let output_css = duckwind_emit_env.to_css_stylesheet(true);

    let script = r#"
        <script>
            function filterDocs() {
                const input = document.getElementById('search-input');
                const filter = input.value.toLowerCase();

                const sidebarLinks = document.querySelectorAll('#sidebar-nav a');
                sidebarLinks.forEach(link => {
                    const text = link.innerText.toLowerCase();
                    link.parentElement.style.display = text.includes(filter) ? "" : "none";
                });

                const contentItems = document.querySelectorAll('.doc-item');
                let hasVisibleItems = false;

                contentItems.forEach(item => {
                    const name = item.getAttribute('data-name').toLowerCase();
                    if (name.includes(filter)) {
                        item.style.display = "";
                        hasVisibleItems = true;
                    } else {
                        item.style.display = "none";
                    }
                });

                document.getElementById('no-results').style.display = hasVisibleItems ? 'none' : 'block';
            }

            const sidebar = document.getElementById('sidebar');
            const overlay = document.getElementById('sidebar-overlay');

            function toggleSidebar() {
                const isClosed = sidebar.classList.contains('-translate-x-full');
                if (isClosed) {
                    openSidebar();
                } else {
                    closeSidebar();
                }
            }

            function openSidebar() {
                sidebar.classList.remove('-translate-x-full');
                overlay.classList.remove('hidden');
                setTimeout(() => {
                    overlay.classList.remove('opacity-0');
                }, 10);
            }

            function closeSidebar() {
                sidebar.classList.add('-translate-x-full');
                overlay.classList.add('opacity-0');
                setTimeout(() => {
                    overlay.classList.add('hidden');
                }, 300);
            }

            document.querySelectorAll('#sidebar-nav a').forEach(link => {
                link.addEventListener('click', () => {
                    if (window.innerWidth < 768) {
                        closeSidebar();
                    }
                });
            });
        </script>
    "#;

    format!(
        "<!doctype html>
        <html lang='en'>
            <head>
                <meta charset='UTF-8'>
                <meta name='viewport' content='width=device-width, initial-scale=1.0'>
                <title>Duck Documentation</title>
                <style>
                    body {{ margin: 0; background-color: #282828; color: #ebdbb2; }}
                    ::-webkit-scrollbar {{ width: 8px; }}
                    ::-webkit-scrollbar-track {{ background: #1d2021; }}
                    ::-webkit-scrollbar-thumb {{ background: #504945; border-radius: 4px; }}
                    ::-webkit-scrollbar-thumb:hover {{ background: #665c54; }}
                    {output_css}
                </style>
            </head>
            <body>
                {body_content}
                {script}
            </body>
        </html>"
    )
}

fn render_sidebar(
    fn_docs: &[FunctionDoc],
    struct_docs: &[StructDoc],
    extensions_docs: &[ExtensionsDoc],
) -> String {
    let struct_links = struct_docs.iter().map(|s| {
        format!(
            "<li><a href='#struct-{}' class='block text-[#a89984] hover:text-[#fabd2f] hover:bg-[#32302f] px-2 py-1.5 rounded transition-colors duration-200'>{}</a></li>",
            s.struct_name, s.struct_name
        )
    }).collect::<Vec<_>>().join("");

    let extension_links = extensions_docs.iter().map(|e| {
        format!(
            "<li><a href='#ext-{}' class='block text-[#a89984] hover:text-[#8ec07c] hover:bg-[#32302f] px-2 py-1.5 rounded transition-colors duration-200'>{}</a></li>",
            e.target_type_annotation, e.target_type_annotation
        )
    }).collect::<Vec<_>>().join("");

    let fn_links = fn_docs.iter().map(|f| {
        format!(
            "<li><a href='#fn-{}' class='block text-[#a89984] hover:text-[#b8bb26] hover:bg-[#32302f] px-2 py-1.5 rounded transition-colors duration-200'>{}</a></li>",
            f.function_name, f.function_name
        )
    }).collect::<Vec<_>>().join("");

    format!(
        r#"
        <div>
            <h3 class="font-bold text-[#fabd2f] uppercase tracking-wider text-xs mb-3 ml-2">Structs</h3>
            <ul class="space-y-0.5 text-sm">{struct_links}</ul>
        </div>
        <div>
            <h3 class="font-bold text-[#8ec07c] uppercase tracking-wider text-xs mb-3 ml-2">Extensions</h3>
            <ul class="space-y-0.5 text-sm">{extension_links}</ul>
        </div>
        <div>
            <h3 class="font-bold text-[#b8bb26] uppercase tracking-wider text-xs mb-3 ml-2">Functions</h3>
            <ul class="space-y-0.5 text-sm">{fn_links}</ul>
        </div>
        "#
    )
}

fn render_struct(doc: &StructDoc) -> String {
    let comments_html = doc
        .comments
        .iter()
        .map(|c| format!("<p class='text-[#ebdbb2] opacity-80 mb-2 leading-relaxed'>{c}</p>"))
        .collect::<Vec<_>>()
        .join("");

    let fields_html = if doc.fields.is_empty() {
        String::new()
    } else {
        let rows = doc.fields.iter().map(|field| {
            format!(
                "<tr class='border-b border-[#3c3836] last:border-0 hover:bg-[#32302f] transition-colors'>
                    <td class='py-3 px-4 font-mono text-sm text-[#83a598]'>{}</td>
                    <td class='py-3 px-4 font-mono text-sm text-[#d3869b]'>{}</td>
                </tr>",
                field.field_name, field.type_annotation
            )
        }).collect::<Vec<_>>().join("");

        format!(
            r#"
            <div class="mt-5 mb-8 bg-[#1d2021] rounded border border-[#3c3836] overflow-x-auto">
                <table class="w-full text-left min-w-[300px]">
                    <thead class="bg-[#32302f] border-b border-[#3c3836]">
                        <tr>
                            <th class="py-2 px-4 text-xs font-bold text-[#a89984] uppercase tracking-wider">Field</th>
                            <th class="py-2 px-4 text-xs font-bold text-[#a89984] uppercase tracking-wider">Type</th>
                        </tr>
                    </thead>
                    <tbody>
                        {rows}
                    </tbody>
                </table>
            </div>
            "#
        )
    };

    let methods_html = if doc.function_docs.is_empty() {
        String::new()
    } else {
        let methods = doc
            .function_docs
            .iter()
            .map(|f| render_function(f, true))
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            r#"
            <div class="mt-8 pl-4 md:pl-5 border-l-2 border-[#504945]">
                <h4 class="text-sm font-bold text-[#fe8019] uppercase tracking-widest mb-6">Methods</h4>
                <div class="space-y-8">
                    {methods}
                </div>
            </div>
            "#
        )
    };

    format!(
        r#"
        <div id="struct-{name}" class="doc-item scroll-mt-24" data-name="{name}">
            <div class="flex items-center gap-3 mb-4">
                <span class="text-xs font-bold text-[#1d2021] bg-[#fabd2f] px-2 py-0.5 rounded uppercase tracking-wide">Struct</span>
                <h3 class="text-xl md:text-2xl font-bold text-[#fbf1c7] tracking-tight break-all">{name}</h3>
            </div>
            <div class="prose max-w-none mb-4 text-[#ebdbb2]">
                {comments}
            </div>
            {fields}
            {methods}
        </div>
        "#,
        name = doc.struct_name,
        comments = comments_html,
        fields = fields_html,
        methods = methods_html
    )
}

fn render_extension(doc: &ExtensionsDoc) -> String {
    let comments_html = doc
        .comments
        .iter()
        .map(|c| format!("<p class='text-[#ebdbb2] opacity-80 mb-2 leading-relaxed'>{c}</p>"))
        .collect::<Vec<_>>()
        .join("");

    let methods_html = if doc.function_docs.is_empty() {
        String::new()
    } else {
        let methods = doc
            .function_docs
            .iter()
            .map(|f| render_function(f, true))
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            r#"
            <div class="mt-8 pl-4 md:pl-5 border-l-2 border-[#504945]">
                <h4 class="text-sm font-bold text-[#fe8019] uppercase tracking-widest mb-6">Extension Methods</h4>
                <div class="space-y-8">
                    {methods}
                </div>
            </div>
            "#
        )
    };

    format!(
        r#"
        <div id="ext-{name}" class="doc-item scroll-mt-24" data-name="{name}">
            <div class="flex items-center gap-3 mb-4">
                <span class="text-xs font-bold text-[#1d2021] bg-[#8ec07c] px-2 py-0.5 rounded uppercase tracking-wide">Extension</span>
                <h3 class="text-xl md:text-2xl font-bold text-[#8ec07c] tracking-tight break-all">on {name}</h3>
            </div>
            <div class="prose max-w-none mb-4 text-[#ebdbb2]">
                {comments}
            </div>
            {methods}
        </div>
        "#,
        name = doc.target_type_annotation,
        comments = comments_html,
        methods = methods_html
    )
}

fn render_function(doc: &FunctionDoc, is_method: bool) -> String {
    let comments_html = doc
        .comments
        .iter()
        .map(|c| format!("<p class='text-[#ebdbb2] opacity-80 mb-2 leading-relaxed'>{c}</p>"))
        .collect::<Vec<_>>()
        .join("");

    let (badge, title_color) = if is_method {
        (
            r#"<span class="text-xs font-bold text-[#a89984] bg-[#32302f] px-2 py-0.5 rounded border border-[#504945] uppercase tracking-wide">Method</span>"#,
            "text-[#ebdbb2]",
        )
    } else {
        (
            r#"<span class="text-xs font-bold text-[#1d2021] bg-[#b8bb26] px-2 py-0.5 rounded uppercase tracking-wide">Fn</span>"#,
            "text-[#fbf1c7]",
        )
    };

    let id_prefix = if is_method { "method" } else { "fn" };

    format!(
        r#"
        <div id="{id_prefix}-{name}" class="doc-item scroll-mt-24" data-name="{name}">
            <div class="flex flex-wrap items-center gap-3 mb-3">
                {badge}
                <h3 class="text-lg md:text-xl font-bold {title_color} font-mono tracking-tight break-all">{name}</h3>
            </div>

            <div class="bg-[#32302f] rounded p-4 font-mono text-sm overflow-x-auto shadow-sm mb-4 border border-[#3c3836]">
                <code class="text-[#8ec07c] whitespace-pre">{annotation}</code>
            </div>

            <div class="prose max-w-none text-[#ebdbb2]">
                {comments}
            </div>
        </div>
        "#,
        id_prefix = id_prefix,
        name = doc.function_name,
        badge = badge,
        title_color = title_color,
        annotation = doc.function_annotation,
        comments = comments_html
    )
}
