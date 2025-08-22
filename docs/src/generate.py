#!/usr/bin/env python3
import os
import re
import logging
import sys


OUTPUT_DIR = ".."
MANDATORY_VARS = ["title"]
GLOBAL_VARS_FILE = "00_intro.md"

class ColoredFormatter(logging.Formatter):
    LOG_COLORS = {
        logging.DEBUG: "\033[94m",    # Blue
        logging.INFO: "\033[92m",     # Green
        logging.WARNING: "\033[93m",  # Yellow
        logging.ERROR: "\033[91m",    # Red
        logging.CRITICAL: "\033[91m\033[1m" # Bold Red
    }
    RESET_COLOR = "\033[0m"

    def format(self, record):
        log_color = self.LOG_COLORS.get(record.levelno, "")
        record.msg = f"{log_color}{record.msg}{self.RESET_COLOR}"
        return super().format(record)

def setup_logger():
    handler = logging.StreamHandler(sys.stdout)
    formatter = ColoredFormatter(
        "%(asctime)s [%(levelname)s] - %(message)s",
        "%Y-%m-%d %H:%M:%S"
    )
    handler.setFormatter(formatter)

    logger = logging.getLogger()
    logger.setLevel(logging.INFO)
    if not logger.handlers:
        logger.addHandler(handler)

def parse_header_and_content(file_path):
    log = logging.getLogger(__name__)
    log.debug(f"parsing header for: \033[36m{file_path}\033[0m")

    variables = {}
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content_lines = f.readlines()
    except FileNotFoundError:
        log.error(f"File not found: {file_path}")
        return {}, ""

    header_end_index = 0
    variable_pattern = re.compile(r'^\$(\w+)\s*=\s*"?([^"]*)"?\s*$')

    for i, line in enumerate(content_lines):
        match = variable_pattern.match(line)
        if match:
            key, value = match.groups()
            variables[key.strip()] = value.strip()
            header_end_index = i + 1
        else:
            break

    for var in MANDATORY_VARS:
        if var not in variables:
            log.warning(f"Mandatory variable '{var}' not found in {file_path}")
            variables.setdefault(var, "Untitled")

    content = "".join(content_lines[header_end_index:]).strip()
    return variables, content

def replace_variables_in_content(content, local_vars, global_vars):
    combined_vars = {**global_vars, **local_vars}

    def replace_match(match):
        var_name = match.group(1)
        return combined_vars.get(var_name, f"$${{{var_name}}}")

    return re.sub(r'\$\${(\w+)}', replace_match, content)

def build_book_from_directory(root_dir="."):
    log = logging.getLogger(__name__)
    log.info("bilding book structure from source directory...")

    file_pattern = re.compile(r"^\d{2}_.*")
    paths = sorted([p for p in os.listdir(root_dir) if file_pattern.match(p)])

    global_vars, _ = parse_header_and_content(os.path.join(root_dir, GLOBAL_VARS_FILE))
    log.info(f"Loaded {len(global_vars)} global variables from '{GLOBAL_VARS_FILE}'.")

    book = {
        "book_intro": {"name": "Introduction", "sections": []},
        "chapters": [],
    }

    for path in paths:
        full_path = os.path.join(root_dir, path)
        name_without_prefix = path[3:].replace('.md', '').replace('_', ' ').title()

        if os.path.isfile(full_path) and full_path.endswith('.md'):
            local_vars, raw_content = parse_header_and_content(full_path)
            processed_content = replace_variables_in_content(raw_content, local_vars, global_vars)
            book["book_intro"]["sections"].append({
                "name": name_without_prefix,
                "content": processed_content,
                "title": local_vars.get("title", name_without_prefix)
            })
        elif os.path.isdir(full_path):
            chapter = {
                "name": name_without_prefix,
                "sections": []
            }
            log.info(f"processing chapter: \033[35m{chapter['name']}\033[0m")

            section_files = sorted([f for f in os.listdir(full_path) if f.endswith('.md')])
            for section_file in section_files:
                section_path = os.path.join(full_path, section_file)
                section_name = section_file[3:].replace('.md', '').replace('_', ' ').title()

                local_vars, raw_content = parse_header_and_content(section_path)
                processed_content = replace_variables_in_content(raw_content, local_vars, global_vars)
                chapter["sections"].append({
                    "name": section_name,
                    "content": processed_content,
                    "title": local_vars.get("title", section_name)
                })
            book["chapters"].append(chapter)

    return book

def generate_output_files(book):
    log = logging.getLogger(__name__)
    log.info(f"generating output files in '{os.path.abspath(OUTPUT_DIR)}' directory...")
    log.warning(f"existing files in the output directory with matching names will be overwritten.")

    flat_pages = []
    page_counter = 0

    for section in book["book_intro"]["sections"]:
        filename = "README.md" if page_counter == 0 else f"{page_counter:03d}-{section['name'].lower().replace(' ', '-')}.md"
        flat_pages.append({"filename": filename, "chapter_name": "Introduction", **section})
        page_counter += 1

    for chapter in book["chapters"]:
        for section in chapter["sections"]:
            filename = f"{page_counter:03d}-{chapter['name'].lower().replace(' ', '-')}-{section['name'].lower().replace(' ', '-')}.md"
            flat_pages.append({"filename": filename, "chapter_name": chapter['name'], **section})
            page_counter += 1

    total_pages = len(flat_pages)
    for i, page in enumerate(flat_pages):
        nav_links = []
        if i > 0:
            prev_page = flat_pages[i-1]
            nav_links.append(f"[< Previous]({prev_page['filename']})")

        if i != 0:
            home_page = flat_pages[0]
            nav_links.append(f"[Home]({home_page['filename']})")

        if i < total_pages - 1:
            next_page = flat_pages[i+1]
            nav_links.append(f"[Next >]({next_page['filename']})")

        nav_bar = " | ".join(nav_links)

def main():
    setup_logger()
    log = logging.getLogger(__name__)

    log.info("starting book generation process...")
    book_data = build_book_from_directory()
    generate_output_files(book_data)
    pass

if __name__ == "__main__":
    main()
