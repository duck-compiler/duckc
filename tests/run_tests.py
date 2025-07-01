import sys
import os
import subprocess
import shutil
import argparse

VERBOSE = False
CICD = False

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

def read_meta_file(file_name):
    meta_file_path = f"{file_name}.meta"
    meta_data = {}

    if VERBOSE:
        print(f"{COLOR_YELLOW}attempting to read meta file for {file_name}{COLOR_RESET}: {meta_file_path}")

    if not os.path.exists(meta_file_path):
        print(f"{CROSS} {COLOR_RED}Error{COLOR_RESET}: {COLOR_RED}Meta file {COLOR_RESET}'{meta_file_path}' {COLOR_RED}does not exist.")
        if CICD:
            sys.exit(-1)
        return {}

    if not os.path.isfile(meta_file_path):
        print(f"{CROSS} Error: '{meta_file_path}' exists but is not a file (it might be a directory).")
        if CICD:
            sys.exit(-1)
        return {}

    try:
        with open(meta_file_path, 'r', encoding='utf-8') as f:
            for line_num, line in enumerate(f, 1):
                line = line.strip() # Remove leading/trailing whitespace, including newline
                if not line or line.startswith('#'): # Skip empty lines and comments
                    continue

                if '=' in line:
                    key, value = line.split('=', 1) # Split only on the first '='
                    meta_data[key.strip()] = value.strip()
                else:
                    print(f"{COLOR_YELLOW}Warning{COLOR_RESET}: {COLOR_YELLOW}Line {line_num} in '{meta_file_path}' is not in 'key=value' format: '{line}'")

        if not meta_data and VERBOSE:
            print(f"{COLOR_YELLOW}Info{COLOR_RESET}: Meta file '{meta_file_path}' was empty or contained no valid key-value pairs.")

        if VERBOSE:
            print(f"{COLOR_YELLOW} Successfully read meta data from '{meta_file_path}'.")

        return meta_data

    except PermissionError:
        print(f"{COLOR_RED}Error{COLOR_RESET}: Permission denied to read meta file '{meta_file_path}'.")
        return {}
    except Exception as e:
        print(f"{COLOR_RED}An unexpected error occurred while reading '{meta_file_path}': {e}")
        return {}

def compile_valid_with_assert(compiler_path, valid_program):
    if VERBOSE:
        print(f"{COLOR_YELLOW}compile_valid_with_assert {COLOR_RESET}'{valid_program}'")

    try:
        command = [compiler_path] + ["compile"] + [valid_program];

        result = subprocess.run(command, capture_output=True, text=True, check=False)

        if VERBOSE:
            if len(result.stdout) > 1:
                print(f"{COLOR_YELLOW}  captured output of stdout{COLOR_RESET}: \n{COLOR_GRAY}{indent_all_lines_with_tab(result.stdout)}'")
            if len(result.stderr) > 1:
                print(f"{COLOR_RED}  captured output of stderr{COLOR_RESET}: \n{COLOR_GRAY}{indent_all_lines_with_tab(result.stderr)}'")

        if result.returncode != 0:
            print(f"{SKIP} {COLOR_YELLOW}test {COLOR_RESET}{valid_program} {COLOR_GRAY}-> {COLOR_RED}compilation failed")
            return

        meta = read_meta_file(valid_program)
        expected_return_code = meta["expected_return_code"];
        if expected_return_code == None:
            print(f"{SKIP} {COLOR_YELLOW}test {COLOR_RESET}{valid_program} {COLOR_GRAY}-> {COLOR_RED}no expected return code defined in metadata")
            return

        expected_return_code = int(expected_return_code)

        duck_execute_result = subprocess.run("./.dargo/duck_out", capture_output=True, text=True, check=False)
        actual_return_code = duck_execute_result.returncode

        if actual_return_code != expected_return_code:
            print(f"{CROSS} {COLOR_YELLOW}test {COLOR_RESET}{valid_program} {COLOR_RESET}")
            if CICD:
                sys.exit(-1)
            print(f"    {COLOR_GRAY}-> {COLOR_RED} Expected return code to be {expected_return_code} but got {actual_return_code}")
        else:
            print(f"{CHECK} {COLOR_YELLOW}test {COLOR_RESET}{valid_program} {COLOR_RESET}")
    except FileNotFoundError:
        print(f"Error: Program not found at '{compiler_path}'")
    except Exception as exception:
        print(f"An unexpected error occured for file : {exception}")
        return None
    pass

def perform_tests():
    compiler_path = build_and_move_cargo_binary("dargo");

    print(f"{COLOR_YELLOW}Duck Compiler is located at {COLOR_RESET}{compiler_path}{COLOR_RESET}")

    invalid_program_files = find_duck_files_in_directory("./invalid_programs")
    print(f"\n{COLOR_YELLOW}Starting the evaluation of the invalid test cases...")
    for invalid_program in invalid_program_files:
        compile_failure(compiler_path, invalid_program)
        pass

    valid_program_files = find_duck_files_in_directory("./valid_programs")
    print(f"\n{COLOR_YELLOW}Starting the evaluation of the valid test cases...")
    for valid_program in valid_program_files:
        compile_valid(compiler_path, valid_program)
        pass

    valid_program_files_with_assert = find_duck_files_in_directory("./valid_programs_with_assert")
    print(f"\n{COLOR_YELLOW}Starting the evaluation of the valid test cases with assertions...")
    for valid_program in valid_program_files_with_assert:
        compile_valid_with_assert(compiler_path, valid_program)
        pass

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

    args = parser.parse_args()

    VERBOSE = args.verbose
    if VERBOSE:
        print(f"{COLOR_YELLOW}Verbose output is enabled.{COLOR_RESET}")

    CICD = args.cicd
    if CICD:
        print(f"{COLOR_YELLOW}Running the script in CICD Mode.{COLOR_RESET}")
    perform_tests()
