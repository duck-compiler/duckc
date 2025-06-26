import os
import subprocess

COLOR_RED = "\033[91m"
COLOR_GREEN = "\033[92m"
COLOR_YELLOW = "\033[93m"
COLOR_BLUE = "\033[94m"
COLOR_CYAN = "\033[96m"
COLOR_GRAY = "\033[90m"
COLOR_RESET = "\033[0m"

def find_duck_files_in_directory(base_directory):
    duck_files = []

    if not os.path.isdir(base_directory):
        print(f"{COLOR_RED}Error: Base directory '{base_directory}' does not exist or is not a directory.{COLOR_RESET}")
        return []

    print(f"{COLOR_YELLOW}Searching for .duck files in: {base_directory} and its subdirectories...{COLOR_RESET}")

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

def perform_tests():
    invalid_program_files = find_duck_files_in_directory("./invalid_programs")
    pass

def execute_with_expected_exit_code(expected_exit_code: int):
    program_path = ""

    try:
        command = [program_path];

        result = subprocess.run(command, capture_output=True, text=True, check=False)
        print(f"test ")

        print("Expect")
    except FileNotFoundError:
        print(f"Error: Program not found at '{program_path}'")
    except Exception as exception:
        print(f"An unexpected error occured: {exception}")
        return None
    pass

if __name__ == "__main__":
    perform_tests()
