import sys
import os
import subprocess
import shutil
import argparse
import json
import difflib
import re

VERBOSE = False
CICD = False
UPDATE_SNAPSHOTS = False

COLOR_RED = "\033[91m"
COLOR_GREEN = "\033[92m"
COLOR_YELLOW = "\033[93m"
COLOR_BLUE = "\033[94m"
COLOR_CYAN = "\033[96m"
COLOR_GRAY = "\033[90m"
COLOR_RESET = "\033[0m"

CHECK = f"{COLOR_GRAY}[{COLOR_GREEN}✔{COLOR_GRAY}]{COLOR_RESET}"
CROSS = f"{COLOR_GRAY}[{COLOR_RED}✗{COLOR_GRAY}]{COLOR_RESET}"
SKIP = f"{COLOR_GRAY}[{COLOR_YELLOW}~{COLOR_GRAY}]{COLOR_RESET}"
UPDATE = f"{COLOR_GRAY}[{COLOR_BLUE}U{COLOR_GRAY}]{COLOR_RESET}"

def indent_all_lines_with_tab(input_string):
    if not isinstance(input_string, str):
        raise TypeError("Input must be a string.")

    if not input_string:
        return ""

    lines = input_string.splitlines(keepends=True)
    indented_lines = ["\t" + line for line in lines]
    modified_string = "".join(indented_lines)

    return modified_string

def find_duck_files_in_directory(base_directory):
    duck_files = []

    if not os.path.isdir(base_directory):
        print(f"{COLOR_RED}Error: Base directory '{base_directory}' does not exist or is not a directory.{COLOR_RESET}")
        return []

    print(f"\n{COLOR_YELLOW}Searching for .dargo files in: {base_directory} and its subdirectories...{COLOR_RESET}")

    try:
        for root, _, files in os.walk(base_directory):
            print(f"  {COLOR_YELLOW}Scanning directory{COLOR_RESET}: {root}{COLOR_RESET}")
            for file_name in files:
                if file_name.endswith('.duck'):
                    full_file_path = os.path.join(root, file_name)
                    duck_files.append(full_file_path)
                    print(f"    {COLOR_GREEN}Found{COLOR_RESET}: {full_file_path}{COLOR_RESET}")
                else:
                    print(f"    {COLOR_YELLOW}Skipping{COLOR_RESET}: {os.path.join(root, file_name)}")

    except PermissionError:
        print(f"Permission denied: Could not access '{base_directory}' or one of its subdirectories.")
    except Exception as e:
        print(f"An unexpected error occurred: {e}")

    return duck_files

def build_and_move_cargo_binary(project_name, build_type="debug"):
    original_cwd = os.getcwd()
    parent_dir = os.path.abspath(os.path.join(original_cwd, os.pardir))
    binary_path_in_target = None
    moved_binary_path = None

    target_subdir = f"target/{build_type}"
    binary_name = project_name

    # windows target
    if os.name == 'nt':
        binary_name += ".exe"

    print(f"{COLOR_GRAY}--- {COLOR_CYAN} Starting Cargo Build for {COLOR_RESET}'{COLOR_YELLOW}{project_name}{COLOR_RESET}' ({COLOR_YELLOW}{build_type}{COLOR_RESET}) {COLOR_GRAY}---{COLOR_RESET}")
    print(f"{COLOR_YELLOW}Original directory{COLOR_RESET}: {original_cwd}")
    print(f"{COLOR_YELLOW}Target build directory{COLOR_RESET}: {parent_dir}")

    try:
        print(f"{COLOR_YELLOW}Changing directory to{COLOR_RESET}: {parent_dir}{COLOR_RESET}")
        os.chdir(parent_dir)

        print(f"{COLOR_YELLOW}Executing {COLOR_RESET}'{COLOR_GREEN}cargo build{COLOR_RESET}' {COLOR_YELLOW}for project {COLOR_RESET}'{COLOR_YELLOW}{project_name}{COLOR_RESET}'{COLOR_GRAY}...{COLOR_RESET}")
        cargo_command = ["cargo", "build"]
        if build_type == "release":
            cargo_command.append("--release")

        build_result = subprocess.run(
            cargo_command,
            capture_output=True,
            text=True,
            check=True
        )

        print(f"\n{COLOR_GRAY}--- {COLOR_GREEN}Cargo Build Output {COLOR_RESET}({COLOR_YELLOW}STDOUT{COLOR_RESET}) {COLOR_GRAY}---{COLOR_RESET}")
        print(build_result.stdout)

        if build_result.stderr:
            print(f"\n{COLOR_GRAY}--- {COLOR_RED}Cargo Build Output {COLOR_RESET}({COLOR_YELLOW}STDERR{COLOR_RESET}) {COLOR_GRAY}---{COLOR_RESET}")
            print(build_result.stderr)

        print(f"{CHECK} {COLOR_YELLOW}Cargo build was successful.{COLOR_RESET}")

        expected_binary_dir = os.path.join(parent_dir, target_subdir)
        binary_path_in_target = os.path.join(expected_binary_dir, binary_name)

        if not os.path.exists(binary_path_in_target):
            raise FileNotFoundError(
                f"{CROSS} {COLOR_RED}Binary not found at expected path{COLOR_RESET}: {binary_path_in_target}. "
                f"{COLOR_YELLOW}Make sure {COLOR_RESET}'{COLOR_YELLOW}project_name{COLOR_RESET}' {COLOR_YELLOW}matches your Cargo.toml package name."
            )

        print(f"{CHECK} {COLOR_YELLOW}Found binary{COLOR_RESET}: {binary_path_in_target}{COLOR_RESET}")

        destination_path = os.path.join(original_cwd, binary_name)
        shutil.move(binary_path_in_target, destination_path)
        moved_binary_path = destination_path

        print(f"{CHECK} {COLOR_YELLOW}Binary moved successfully to{COLOR_RESET}: {moved_binary_path}{COLOR_RESET}")
    except FileNotFoundError as e:
        print(f"{CROSS}{COLOR_RED}Error{COLOR_RESET}: {e}{COLOR_RESET}")

        if CICD:
            sys.exit(-1)
    except subprocess.CalledProcessError as e:
        print(f"{CROSS}{COLOR_RED}Error{COLOR_RESET}: 'cargo build' failed with exit code {e.returncode}.{COLOR_RESET}")
        print(f"    {COLOR_GREEN}STDOUT{COLOR_RESET}:\n{e.stdout}{COLOR_RESET}")
        print(f"    {COLOR_RED}STDERR{COLOR_RESET}:\n{e.stderr}{COLOR_RESET}")

        if CICD:
            sys.exit(-1)
    except shutil.Error as e:
        print(f"{CROSS} {COLOR_RED}Error moving binary{COLOR_RESET}: {e}{COLOR_RESET}")

        if CICD:
            sys.exit(-1)
    except Exception as e:
        print(f"{CROSS} {COLOR_RED}An unexpected error occurred{COLOR_RESET}: {e}{COLOR_RESET}")

        if CICD:
            sys.exit(-1)
    finally:
        os.chdir(original_cwd)

        print(f"{CHECK} {COLOR_YELLOW}Returned to original directory{COLOR_RESET}: {os.getcwd()}{COLOR_RESET}")
        print(f"{CHECK} {COLOR_GREEN}Finished Cargo Build and Move {COLOR_RESET}")

    return moved_binary_path

def compile_failure(compiler_path, invalid_program):
    if VERBOSE:
        print(f"{COLOR_YELLOW}compile_failure {COLOR_RESET}'{invalid_program}'")

    try:
        command = [compiler_path] + ["compile"] + [invalid_program];

        result = subprocess.run(command, capture_output=True, text=True, check=False)

        if VERBOSE:
            if len(result.stdout) > 1:
                print(f"{COLOR_YELLOW}  captured output of stdout{COLOR_RESET}: \n{COLOR_GRAY}{indent_all_lines_with_tab(result.stdout)}'")
            if len(result.stderr) > 1:
                print(f"{COLOR_RED}  captured output of stderr{COLOR_RESET}: \n{COLOR_GRAY}{indent_all_lines_with_tab(result.stderr)}'")

        if result.returncode != 0:
            print(f"{CHECK} {COLOR_YELLOW}test {COLOR_RESET}{invalid_program}")
        else:
            print(f"{CROSS} {COLOR_YELLOW}test {COLOR_RESET}{invalid_program}")
            if CICD:
                sys.exit(-1)
    except FileNotFoundError:
        print(f"Error: Program not found at '{compiler_path}'")
    except Exception as exception:
        print(f"An unexpected error occured for file : {exception}")
        return None
    pass

def compile_valid(compiler_path, valid_program):
    if VERBOSE:
        print(f"{COLOR_YELLOW}compile_valid {COLOR_RESET}'{valid_program}'")

    try:
        command = [compiler_path] + ["compile"] +  [valid_program];

        result = subprocess.run(command, capture_output=True, text=True, check=False)

        if VERBOSE:
            if len(result.stdout) > 1:
                print(f"{COLOR_YELLOW}  captured output of stdout{COLOR_RESET}: \n{COLOR_GRAY}{indent_all_lines_with_tab(result.stdout)}'")
            if len(result.stderr) > 1:
                print(f"{COLOR_RED}  captured output of stderr{COLOR_RESET}: \n{COLOR_GRAY}{indent_all_lines_with_tab(result.stderr)}'")

        if result.returncode == 0:
            print(f"{CHECK} {COLOR_YELLOW}test {COLOR_RESET}{valid_program}")
        else:
            print(f"{CROSS} {COLOR_YELLOW}test {COLOR_RESET}{valid_program}")
            if CICD:
                sys.exit(-1)
    except FileNotFoundError:
        print(f"Error: Program not found at '{compiler_path}'")
    except Exception as exception:
        print(f"An unexpected error occured for file : {exception}")
        return None
    pass

def path_to_filename(path: str) -> str:
    if not isinstance(path, str):
        raise TypeError("Input path must be a string.")

    sanitized = re.sub(r'[\\/]', '_', path)
    sanitized = re.sub(r'[^a-zA-Z0-9._-]', '_', sanitized)
    sanitized = re.sub(r'__+', '_', sanitized)
    sanitized = sanitized.strip('_')

    return sanitized

def verify_snapshot(test_name, actual_stdout, actual_stderr):
    snapshot_path = f".snapshots/{path_to_filename(test_name)}.snap"
    snapshot_data = {
        "stdout": actual_stdout,
        "stderr": actual_stderr,
    }

    if UPDATE_SNAPSHOTS:
        print(f"{UPDATE} {COLOR_BLUE}Updating snapshot for {COLOR_RESET}{test_name}")
        with open(snapshot_path, 'w', encoding='utf-8') as f:
            json.dump(snapshot_data, f, indent=4)
        return True

    if not os.path.exists(snapshot_path):
        print(f"{CHECK} {COLOR_CYAN}No previous snapshot found for {COLOR_RESET}{test_name}")

        if CICD:
            print(f"{CROSS} {COLOR_RED}New snapshot detected in CICD mode. Failing test.{COLOR_RESET}")
            print_diff("stdout", "", actual_stdout)
            print_diff("stderr", "", actual_stderr)
            return False

        print(f"\n{COLOR_GRAY}--- Proposed STDOUT ---{COLOR_RESET}")
        print(f"{COLOR_GREEN}{actual_stdout if actual_stdout else '<empty>'}{COLOR_RESET}")
        print(f"{COLOR_GRAY}-----------------------{COLOR_RESET}")
        print(f"\n{COLOR_GRAY}--- Proposed STDERR ---{COLOR_RESET}")
        print(f"{COLOR_RED}{actual_stderr if actual_stderr else '<empty>'}{COLOR_RESET}")
        print(f"{COLOR_GRAY}-----------------------{COLOR_RESET}\n")

        while True:
            choice = input(f"{COLOR_YELLOW}Create new snapshot with this output? (y/n): {COLOR_RESET}").lower().strip()
            if choice == 'y':
                with open(snapshot_path, 'w', encoding='utf-8') as f:
                    json.dump(snapshot_data, f, indent=4)
                print(f"{UPDATE} {COLOR_BLUE}New snapshot created.{COLOR_RESET}")
                return True
            elif choice == 'n':
                print(f"{CROSS} {COLOR_RED}Test failed. New snapshot rejected by user.{COLOR_RESET}")
                return False

    try:
        with open(snapshot_path, 'r', encoding='utf-8') as f:
            expected_data = json.load(f)
    except (json.JSONDecodeError, IOError) as e:
        print(f"{CROSS} {COLOR_RED}Error reading snapshot file '{snapshot_path}': {e}{COLOR_RESET}")
        return False

    expected_stdout = expected_data.get("stdout", "")
    expected_stderr = expected_data.get("stderr", "")

    if actual_stdout == expected_stdout and actual_stderr == expected_stderr:
        return True

    print(f"{CROSS} {COLOR_RED}Snapshot mismatch for {COLOR_RESET}{test_name}")

    print_diff("stdout", expected_stdout, actual_stdout)
    print_diff("stderr", expected_stderr, actual_stderr)

    if CICD:
        return False

    while True:
        choice = input(f"{COLOR_RED}Accept new snapshot? (y/n): {COLOR_RESET}").lower().strip()
        if choice == 'y':
            with open(snapshot_path, 'w', encoding='utf-8') as f:
                json.dump(snapshot_data, f, indent=4)
            print(f"{UPDATE} {COLOR_BLUE}Snapshot updated.{COLOR_RESET}")
            return True
        elif choice == 'n':
            print(f"{CROSS} {COLOR_RED}Test failed. Fix code or update snapshot.{COLOR_RESET}")
            return False

def print_diff(output_type, expected, actual):
    if expected == actual:
        return

    print(f"\n{COLOR_GRAY}--- Mismatch in {output_type.upper()} ---{COLOR_RESET}")
    diff = difflib.unified_diff(
        expected.splitlines(keepends=True),
        actual.splitlines(keepends=True),
        fromfile='expected',
        tofile='actual',
    )
    for line in diff:
        if line.startswith('+'):
            print(f"{COLOR_GREEN}{line}{COLOR_RESET}", end='')
        elif line.startswith('-'):
            print(f"{COLOR_RED}{line}{COLOR_RESET}", end='')
        else:
            print(line, end='')
    print(f"{COLOR_GRAY}--------------------{COLOR_RESET}\n")

def compile_and_run_with_assert(compiler_path, program_path):
    if VERBOSE:
        print(f"{COLOR_YELLOW}Running compile_and_run_with_assert for '{program_path}'{COLOR_RESET}")
    try:
        compile_command = [compiler_path, "compile", program_path]
        compile_result = subprocess.run(compile_command, capture_output=True, text=True, check=False)

        if compile_result.returncode != 0:
            print(f"{SKIP} {COLOR_YELLOW}test {COLOR_RESET}{program_path} {COLOR_GRAY}-> {COLOR_RED}compilation failed{COLOR_RESET}")
            if VERBOSE:
                print(f"  {COLOR_RED}STDERR:\n{indent_all_lines_with_tab(compile_result.stderr)}{COLOR_RESET}")
            return

        executable_path = "./.dargo/duck_out"
        if not os.path.exists(executable_path):
             print(f"{CROSS} {COLOR_RED}test {COLOR_RESET}{program_path} {COLOR_GRAY}-> {COLOR_RED}compiled output not found at '{executable_path}'{COLOR_RESET}")
             if CICD: sys.exit(1)
             return

        run_result = subprocess.run(executable_path, capture_output=True, text=True, check=False)
        actual_stdout = run_result.stdout
        actual_stderr = run_result.stderr

        has_error = False

        snapshot_passed = verify_snapshot(program_path, actual_stdout, actual_stderr)
        if not snapshot_passed:
            has_error = True

        if not has_error:
            print(f"{CHECK} {COLOR_GREEN}test {COLOR_RESET}{program_path}")
        elif CICD:
            sys.exit(1)

    except Exception as e:
        print(f"{CROSS} {COLOR_RED}An unexpected error occurred for file '{program_path}': {e}{COLOR_RESET}")
        if CICD: sys.exit(1)

def perform_tests():
    compiler_path = build_and_move_cargo_binary("dargo");

    print(f"{COLOR_YELLOW}Duck Compiler is located at {COLOR_RESET}{compiler_path}{COLOR_RESET}")

    invalid_program_files = find_duck_files_in_directory("./invalid_programs")
    print(f"\n{COLOR_YELLOW}Starting the evaluation of the invalid test cases...")
    for invalid_program in invalid_program_files:
        compile_failure(compiler_path, invalid_program)
        pass

    assert_program_files = find_duck_files_in_directory("./valid_programs")
    if assert_program_files:
        print(f"\n{COLOR_CYAN}--- Evaluating Programs with Assertions (snapshots) ---{COLOR_RESET}")
        for program in assert_program_files:
            compile_and_run_with_assert(compiler_path, program)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Test runner for the duck compiler"
    )

    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Enable verbose output for cargo build details and errors.'
    )

    parser.add_argument(
        '--cicd',
        action='store_true',
        help='Run script in cicd mode'
    )

    parser.add_argument(
        '-u', '--update',
        action='store_true',
        dest='update_snapshots',
        help='Force update all snapshot files with new output.'
    )

    args = parser.parse_args()

    VERBOSE = args.verbose
    if VERBOSE:
        print(f"{COLOR_YELLOW}Verbose output is enabled.{COLOR_RESET}")

    CICD = args.cicd
    if CICD:
        print(f"{COLOR_YELLOW}Running the script in CICD Mode.{COLOR_RESET}")

    UPDATE_SNAPSHOTS = args.update_snapshots
    if UPDATE_SNAPSHOTS:
        print(f"{COLOR_YELLOW}Running in Snapshot Update Mode. All snapshots will be overwritten.{COLOR_RESET}")

    perform_tests()
