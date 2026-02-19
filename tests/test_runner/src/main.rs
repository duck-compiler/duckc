use clap::Parser;
use colored::Colorize;
use rayon::prelude::*;
use similar::{ChangeTag, TextDiff};
use serde::{Deserialize, Serialize};
use std::fs;
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};
use std::time::Instant;
use std::process::Command;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use walkdir::WalkDir;

struct CompileLimiter(Arc<(Mutex<usize>, Condvar)>);

impl CompileLimiter {
    fn new(n: usize) -> Self {
        Self(Arc::new((Mutex::new(n), Condvar::new())))
    }

    fn acquire(&self) -> CompilePermit {
        let (lock, cvar) = &*self.0;
        let mut g = lock.lock().unwrap();
        while *g == 0 {
            g = cvar.wait(g).unwrap();
        }
        *g -= 1;
        CompilePermit(Some(self.0.clone()))
    }
}

struct CompilePermit(Option<Arc<(Mutex<usize>, Condvar)>>);

impl Drop for CompilePermit {
    fn drop(&mut self) {
        if let Some(l) = self.0.take() {
            let (lock, cvar) = &*l;
            let mut g = lock.lock().unwrap();
            *g += 1;
            cvar.notify_one();
        }
    }
}

#[derive(Parser, Debug)]
#[command(about = "Parallel test runner for the duck compiler")]
struct Args {
    #[arg(long)]
    verbose: bool,
    #[arg(long)]
    cicd: bool,
    #[arg(long, default_value_t = true)]
    release: bool,
    #[arg(long)]
    no_build: bool,
    #[arg(short, long)]
    update: bool,
    #[arg(short, long)]
    interactive: bool,
    #[arg(short, long)]
    errors_only: bool,
    #[arg(short, long)]
    filter: Option<String>,
    #[arg(short, long)]
    jobs: Option<usize>,
    #[arg(long)]
    compile_jobs: Option<usize>,
}

#[derive(Clone, Copy, PartialEq)]
enum TestType {
    Error,   // errors/ - expect compile failure, snapshot stdout/stderr
    Invalid, // invalid_programs/ - expect compile failure, no snapshot
    Valid,   // valid_programs/ - expect compile success, run binary, snapshot
}

#[derive(Clone)]
struct TestCase {
    path: PathBuf,
    test_type: TestType,
}

#[derive(Clone)]
enum TestOutcome {
    Passed,
    Failed { message: String },
    SnapshotMismatch {
        snapshot_path: PathBuf,
        expected_stdout: String,
        expected_stderr: String,
        actual_stdout: String,
        actual_stderr: String,
    },
    Skipped { reason: String },
}

#[derive(Serialize, Deserialize)]
struct SnapshotData {
    stdout: String,
    stderr: String,
}

struct TestResult {
    path: PathBuf,
    outcome: TestOutcome,
}

fn print_snapshot_diff(label: &str, expected: &str, actual: &str) {
    if expected == actual {
        return;
    }
    eprintln!("  {}:", label.bright_black());
    let diff = TextDiff::from_lines(expected, actual);
    for change in diff.iter_all_changes() {
        let line = change.to_string();
        match change.tag() {
            ChangeTag::Delete => eprintln!("  {}", format!("-{line}").red()),
            ChangeTag::Insert => eprintln!("  {}", format!("+{line}").green()),
            ChangeTag::Equal => eprintln!("  {}", format!(" {line}").bright_black()),
        }
    }
    eprintln!();
}

fn path_to_filename(path: &Path) -> String {
    let s = path.to_string_lossy().replace(['\\', '/'], "_");
    let s: String = s
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '.' || c == '_' || c == '-' {
                c
            } else {
                '_'
            }
        })
        .collect();
    let mut s = s;
    while s.contains("__") {
        s = s.replace("__", "_");
    }
    s = s.trim_matches('_').to_string();
    s.strip_prefix("._").unwrap_or(&s).to_string()
}

fn find_duck_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    for entry in WalkDir::new(dir)
        .into_iter()
        .filter_entry(|e| e.file_name() != ".dargo")
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.is_file() && path.extension().map_or(false, |e| e == "duck") {
            files.push(path.to_path_buf());
        }
    }
    files.sort();
    files
}

fn build_dargo(project_root: &Path, release: bool, verbose: bool, cicd: bool) -> Option<PathBuf> {
    let build_type = if release { "release" } else { "debug" };
    eprintln!(
        "Starting Cargo Build for '{}' ({})",
        "dargo".yellow(),
        build_type.yellow()
    );

    let mut cmd = Command::new("cargo");
    cmd.current_dir(project_root).arg("build");
    if release {
        cmd.arg("--release");
    }

    let output = cmd.output().ok()?;
    if verbose {
        let _ = io::stderr().write_all(&output.stdout);
        let _ = io::stderr().write_all(&output.stderr);
    }

    if !output.status.success() {
        eprintln!(
            "{} cargo build failed with exit code {}",
            "✗".red(),
            output.status.code().unwrap_or(-1)
        );
        if cicd {
            std::process::exit(-1);
        }
        return None;
    }

    let exe = if cfg!(windows) { "dargo.exe" } else { "dargo" };
    let binary_src = project_root
        .join("target")
        .join(build_type)
        .join(exe);
    let binary_dst = project_root.join("tests").join(exe);

    if !binary_src.exists() {
        eprintln!("{} Binary not found at {}", "✗".red(), binary_src.display());
        if cicd {
            std::process::exit(-1);
        }
        return None;
    }

    fs::copy(&binary_src, &binary_dst).ok()?;
    eprintln!("{} Cargo build successful, binary at {}", "✔".green(), binary_dst.display());
    Some(binary_dst)
}

fn run_test(
    test: &TestCase,
    compiler_path: &Path,
    tests_dir: &Path,
    update_snapshots: bool,
    interactive: bool,
    _cicd: bool,
    _verbose: bool,
    compile_limiter: Option<&CompileLimiter>,
) -> TestResult {
    let path = &test.path;
    let _permit = compile_limiter.map(|l| l.acquire());

    match test.test_type {
        TestType::Invalid => {
            let output = match Command::new(compiler_path)
                .arg("compile")
                .arg(path)
                .current_dir(tests_dir)
                .output()
            {
                Ok(o) => o,
                Err(e) => {
                    return TestResult {
                        path: path.clone(),
                        outcome: TestOutcome::Failed {
                            message: format!("Failed to run compiler: {e}"),
                        },
                    }
                }
            };
            if output.status.success() {
                TestResult {
                    path: path.clone(),
                    outcome: TestOutcome::Failed {
                        message: "Expected compilation failure, but compilation succeeded".into(),
                    },
                }
            } else {
                TestResult {
                    path: path.clone(),
                    outcome: TestOutcome::Passed,
                }
            }
        }
        TestType::Error => {
            let output = match Command::new(compiler_path)
                .arg("compile")
                .arg(path)
                .current_dir(tests_dir)
                .output()
            {
                Ok(o) => o,
                Err(e) => {
                    return TestResult {
                        path: path.clone(),
                        outcome: TestOutcome::Failed {
                            message: format!("Failed to run compiler: {e}"),
                        },
                    }
                }
            };

            if output.status.success() {
                return TestResult {
                    path: path.clone(),
                    outcome: TestOutcome::Skipped {
                        reason: "compilation successful, expected error".into(),
                    },
                };
            }

            let actual_stdout = String::from_utf8_lossy(&output.stdout).to_string();
            let actual_stderr = String::from_utf8_lossy(&output.stderr).to_string();

            let rel_path = path.strip_prefix(tests_dir).unwrap_or(path.as_path());
            let snapshot_path = tests_dir
                .join("snapshots")
                .join(format!("{}.snap", path_to_filename(rel_path)));

            verify_snapshot_result(
                path,
                &snapshot_path,
                &actual_stdout,
                &actual_stderr,
                update_snapshots,
                interactive,
            )
        }
        TestType::Valid => {
            let rel_path = path.strip_prefix(tests_dir).unwrap_or(path.as_path());
            let output_name = path_to_filename(rel_path);
            let compile_output = Command::new(compiler_path)
                .arg("compile")
                .arg("--output-name")
                .arg(&output_name)
                .arg(path)
                .current_dir(tests_dir)
                .output();

            let compile_output = match compile_output {
                Ok(o) => o,
                Err(e) => {
                    return TestResult {
                        path: path.clone(),
                        outcome: TestOutcome::Failed {
                            message: format!("Failed to run compiler: {e}"),
                        },
                    }
                }
            };

            if !compile_output.status.success() {
                let stderr = String::from_utf8_lossy(&compile_output.stderr);
                return TestResult {
                    path: path.clone(),
                    outcome: TestOutcome::Skipped {
                        reason: format!("compilation failed: {stderr}"),
                    },
                };
            }

            let executable_path = tests_dir.join(".dargo").join(&output_name);
            let exec_path = if cfg!(windows) {
                let exe = executable_path.parent().unwrap().join(format!("{}.exe", output_name));
                if exe.exists() {
                    exe
                } else if executable_path.exists() {
                    executable_path.clone()
                } else {
                    return TestResult {
                        path: path.clone(),
                        outcome: TestOutcome::Failed {
                            message: format!(
                                "compiled output not found at {}",
                                executable_path.display()
                            ),
                        },
                    };
                }
            } else if executable_path.exists() {
                executable_path.clone()
            } else {
                return TestResult {
                    path: path.clone(),
                    outcome: TestOutcome::Failed {
                        message: format!(
                            "compiled output not found at {}",
                            executable_path.display()
                        ),
                    },
                };
            };

            let run_output = match Command::new(&exec_path)
                .current_dir(tests_dir)
                .output()
            {
                Ok(o) => o,
                Err(e) => {
                    return TestResult {
                        path: path.clone(),
                        outcome: TestOutcome::Failed {
                            message: format!("Failed to run compiled binary: {e}"),
                        },
                    }
                }
            };

            let actual_stdout = String::from_utf8_lossy(&run_output.stdout).to_string();
            let actual_stderr = String::from_utf8_lossy(&run_output.stderr).to_string();

            let rel_path = path.strip_prefix(tests_dir).unwrap_or(path.as_path());
            let snapshot_path = tests_dir
                .join("snapshots")
                .join(format!("{}.snap", path_to_filename(rel_path)));

            verify_snapshot_result(
                path,
                &snapshot_path,
                &actual_stdout,
                &actual_stderr,
                update_snapshots,
                interactive,
            )
        }
    }
}

fn verify_snapshot_result(
    path: &Path,
    snapshot_path: &Path,
    actual_stdout: &str,
    actual_stderr: &str,
    update_snapshots: bool,
    interactive: bool,
) -> TestResult {
    let snapshot_data = SnapshotData {
        stdout: actual_stdout.to_string(),
        stderr: actual_stderr.to_string(),
    };

    if update_snapshots {
        if let Ok(json) = serde_json::to_string_pretty(&snapshot_data) {
            let _ = fs::write(snapshot_path, json);
        }
        return TestResult {
            path: path.to_path_buf(),
            outcome: TestOutcome::Passed,
        };
    }

    let content = match fs::read_to_string(snapshot_path) {
        Ok(c) => c,
        Err(_) => {
            let message = format!(
                "No snapshot found. Use --update to create. Actual stdout: {:?}, stderr: {:?}",
                actual_stdout,
                actual_stderr
            );
            if interactive {
                return TestResult {
                    path: path.to_path_buf(),
                    outcome: TestOutcome::SnapshotMismatch {
                        snapshot_path: snapshot_path.to_path_buf(),
                        expected_stdout: String::new(),
                        expected_stderr: String::new(),
                        actual_stdout: actual_stdout.to_string(),
                        actual_stderr: actual_stderr.to_string(),
                    },
                };
            }
            return TestResult {
                path: path.to_path_buf(),
                outcome: TestOutcome::Failed { message },
            };
        }
    };

    let expected: SnapshotData = match serde_json::from_str(&content) {
        Ok(d) => d,
        Err(e) => {
            let message = format!("Invalid snapshot JSON: {e}");
            if interactive {
                return TestResult {
                    path: path.to_path_buf(),
                    outcome: TestOutcome::SnapshotMismatch {
                        snapshot_path: snapshot_path.to_path_buf(),
                        expected_stdout: String::new(),
                        expected_stderr: String::new(),
                        actual_stdout: actual_stdout.to_string(),
                        actual_stderr: actual_stderr.to_string(),
                    },
                };
            }
            return TestResult {
                path: path.to_path_buf(),
                outcome: TestOutcome::Failed { message },
            };
        }
    };

    if actual_stdout == expected.stdout && actual_stderr == expected.stderr {
        TestResult {
            path: path.to_path_buf(),
            outcome: TestOutcome::Passed,
        }
    } else {
        let mut msg = String::new();
        if actual_stdout != expected.stdout {
            msg.push_str(&format!(
                "stdout mismatch: expected {:?}, got {:?}",
                expected.stdout, actual_stdout
            ));
        }
        if actual_stderr != expected.stderr {
            if !msg.is_empty() {
                msg.push_str("; ");
            }
            msg.push_str(&format!(
                "stderr mismatch: expected {:?}, got {:?}",
                expected.stderr, actual_stderr
            ));
        }
        if interactive {
            TestResult {
                path: path.to_path_buf(),
                outcome: TestOutcome::SnapshotMismatch {
                    snapshot_path: snapshot_path.to_path_buf(),
                    expected_stdout: expected.stdout.clone(),
                    expected_stderr: expected.stderr.clone(),
                    actual_stdout: actual_stdout.to_string(),
                    actual_stderr: actual_stderr.to_string(),
                },
            }
        } else {
            TestResult {
                path: path.to_path_buf(),
                outcome: TestOutcome::Failed { message: msg },
            }
        }
    }
}

fn main() {
    let args = Args::parse();

    if args.verbose {
        eprintln!("{}", "Verbose output is enabled.".yellow());
    }
    if args.cicd {
        eprintln!("{}", "Running in CICD Mode.".yellow());
    }
    if args.update {
        eprintln!("{}", "Snapshot Update Mode. All snapshots will be overwritten.".yellow());
    }
    if args.interactive {
        eprintln!("{}", "Interactive mode: will prompt on snapshot mismatch to update.".yellow());
    }
    if args.errors_only {
        eprintln!("{}", "Errors only mode.".yellow());
    }
    if let Some(ref f) = args.filter {
        eprintln!("{} {}", "Filtering tests by name:".yellow(), f);
    }

    let project_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("run_tests_parallel must live under tests/");
    let tests_dir = project_root.join("tests");

    let compiler_path = if args.no_build {
        let exe = if cfg!(windows) { "dargo.exe" } else { "dargo" };
        let build_type = if args.release { "release" } else { "debug" };
        let candidates = [
            tests_dir.join(exe),
            project_root.join("target").join(build_type).join(exe),
            project_root.join("target").join("debug").join(exe),
        ];
        match candidates.iter().find(|p| p.exists()) {
            Some(p) => {
                eprintln!("{} Using existing binary: {}", "✔".green(), p.display());
                p.clone()
            }
            None => {
                eprintln!("{} No dargo binary found. Run without --no-build first.", "✗".red());
                std::process::exit(-1);
            }
        }
    } else {
        match build_dargo(project_root, args.release, args.verbose, args.cicd) {
            Some(p) => p,
            None => {
                eprintln!("{} Failed to build dargo", "✗".red());
                std::process::exit(-1);
            }
        }
    };

    let mut tests: Vec<TestCase> = Vec::new();

    let errors_dir = tests_dir.join("errors");
    if errors_dir.exists() {
        for p in find_duck_files(&errors_dir) {
            tests.push(TestCase {
                path: p,
                test_type: TestType::Error,
            });
        }
    }

    if !args.errors_only {
        let invalid_dir = tests_dir.join("invalid_programs");
        if invalid_dir.exists() {
            for p in find_duck_files(&invalid_dir) {
                tests.push(TestCase {
                    path: p,
                    test_type: TestType::Invalid,
                });
            }
        }

        let valid_dir = tests_dir.join("valid_programs");
        if valid_dir.exists() {
            for p in find_duck_files(&valid_dir) {
                tests.push(TestCase {
                    path: p,
                    test_type: TestType::Valid,
                });
            }
        }
    }

    if let Some(ref filter) = args.filter {
        let filter_lower = filter.to_lowercase();
        tests.retain(|t| {
            t.path
                .to_string_lossy()
                .to_lowercase()
                .contains(&filter_lower)
        });
    }

    tests.sort_by(|a, b| a.path.cmp(&b.path));

    let total_tests = tests.len();
    let num_jobs = args.jobs.unwrap_or_else(|| {
        std::thread::available_parallelism()
            .map(|p| (p.get() * 2).min(48))
            .unwrap_or(16)
    });
    let compile_jobs = args.compile_jobs.unwrap_or(num_jobs);

    let compile_limiter = Arc::new(CompileLimiter::new(compile_jobs));
    eprintln!(
        "{} Running {} tests with {} worker(s), max {} concurrent compile(s)...",
        "---".bright_black(),
        total_tests,
        num_jobs,
        compile_jobs
    );

    let failed = AtomicBool::new(false);
    let results = Mutex::new(Vec::<TestResult>::new());
    let completed = AtomicUsize::new(0);

    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(num_jobs)
        .build()
        .expect("rayon pool");

    let test_start = Instant::now();
    pool.install(|| {
        tests.par_iter().for_each(|test| {
            let result = run_test(
                test,
                &compiler_path,
                &tests_dir,
                args.update,
                args.interactive,
                args.cicd,
                args.verbose,
                Some(compile_limiter.as_ref()),
            );

            let done = completed.fetch_add(1, Ordering::Relaxed) + 1;
            let is_fail = matches!(
                &result.outcome,
                TestOutcome::Failed { .. } | TestOutcome::SnapshotMismatch { .. }
            );
            if is_fail {
                failed.store(true, Ordering::SeqCst);
            }

            let path_display = result.path.display().to_string();
            let rel_path = path_display
                .rsplit_once("tests")
                .map(|(_, p)| p.trim_start_matches('/'))
                .unwrap_or(&path_display);
            match &result.outcome {
                TestOutcome::Passed => {
                    println!(
                        "[{}/{}] {} {} {}",
                        done,
                        total_tests,
                        "[✔]".bright_black().green(),
                        "test".yellow(),
                        rel_path
                    );
                }
                TestOutcome::Failed { message } => {
                    println!(
                        "[{}/{}] {} {} {}",
                        done,
                        total_tests,
                        "[✗]".bright_black().red(),
                        "test".yellow(),
                        rel_path
                    );
                    eprintln!("  {}", message);
                }
                TestOutcome::SnapshotMismatch {
                    expected_stdout,
                    expected_stderr,
                    actual_stdout,
                    actual_stderr,
                    ..
                } => {
                    println!(
                        "[{}/{}] {} {} {}",
                        done,
                        total_tests,
                        "[✗]".bright_black().red(),
                        "test".yellow(),
                        rel_path
                    );
                    print_snapshot_diff("stdout (expected → actual)", expected_stdout, actual_stdout);
                    print_snapshot_diff("stderr (expected → actual)", expected_stderr, actual_stderr);
                    eprintln!("  {} (use -i/--interactive to prompt to update)", "Snapshot mismatch.".bright_black());
                }
                TestOutcome::Skipped { reason } => {
                    println!(
                        "[{}/{}] {} {} {} {} {}",
                        done,
                        total_tests,
                        "[~]".bright_black().yellow(),
                        "test".yellow(),
                        rel_path,
                        "->".bright_black(),
                        reason.red()
                    );
                }
            }

            if args.cicd && is_fail {
                std::process::exit(1);
            }
            results.lock().unwrap().push(result);
        });
    });
    let test_elapsed = test_start.elapsed();
    let mut results = results.into_inner().unwrap();

    if args.interactive {
        let stdin = io::stdin();
        let mut stdin = stdin.lock();
        let mut line = String::new();
        for result in &mut results {
            if let TestOutcome::SnapshotMismatch {
                ref expected_stdout,
                ref expected_stderr,
                ref snapshot_path,
                ref actual_stdout,
                ref actual_stderr,
                ..
            } = result.outcome
            {
                let path_str = result.path.display().to_string();
                let rel = path_str
                    .rsplit_once("tests")
                    .map(|(_, p)| p.trim_start_matches('/').to_string())
                    .unwrap_or(path_str);
                print_snapshot_diff("stdout (expected → actual)", expected_stdout, actual_stdout);
                print_snapshot_diff("stderr (expected → actual)", expected_stderr, actual_stderr);
                print!("Accept new snapshot for {}? [y/N] ", rel);
                let _ = io::stdout().flush();
                line.clear();
                if stdin.read_line(&mut line).is_ok() {
                    let answer = line.trim().to_lowercase();
                    if answer == "y" || answer == "yes" {
                        let data = SnapshotData {
                            stdout: actual_stdout.clone(),
                            stderr: actual_stderr.clone(),
                        };
                        if let Ok(json) = serde_json::to_string_pretty(&data) {
                            let _ = fs::write(snapshot_path, json);
                            eprintln!("  {} Snapshot updated.", "✔".green());
                            result.outcome = TestOutcome::Passed;
                        }
                    }
                }
            }
        }
    }

    let (total, passed, failed_count, skipped) = results.iter().fold(
        (0u64, 0u64, 0u64, 0u64),
        |(t, p, f, s), r| {
            (
                t + 1,
                p + matches!(&r.outcome, TestOutcome::Passed) as u64,
                f + matches!(
                    &r.outcome,
                    TestOutcome::Failed { .. } | TestOutcome::SnapshotMismatch { .. }
                ) as u64,
                s + matches!(&r.outcome, TestOutcome::Skipped { .. }) as u64,
            )
        },
    );

    println!("\n{}", "--- Test Summary ---".cyan());
    println!("Total tests: {}", total);
    println!("{} Passed:      {}", "".green(), passed);
    println!("{} Failed:      {}", "".red(), failed_count);
    println!("{} Skipped:     {}", "".yellow(), skipped);
    println!("{} Duration:    {:.2}s", "".bright_black(), test_elapsed.as_secs_f64());
    println!("{}", "--------------------".cyan());

    if failed_count > 0 {
        println!("\n{} {} test(s) failed.", "[✗]".red(), failed_count);
        std::process::exit(1);
    } else {
        println!("\n{} All tests passed! 💛", "[✔]".green());
    }
}
