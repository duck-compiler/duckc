#!/usr/bin/env python3
import os
import re
import logging
import sys

MANDATORY_VARS = ["title"]

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

def main():
    setup_logger()
    log = logging.getLogger(__name__)

    log.info("starting book generation process...")
    pass

if __name__ == "__main__":
    main()
