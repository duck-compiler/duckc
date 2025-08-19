#!/usr/bin/env python3
import sys
import logging

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

def main():
    setup_logger()
    log = logging.getLogger(__name__)

    log.info("starting book generation process...")
    pass

if __name__ == "__main__":
    main()
