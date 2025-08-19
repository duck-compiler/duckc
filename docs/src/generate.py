#!/usr/bin/env python3
import sys
import logging

def setup_logger():
    handler = logging.StreamHandler(sys.stdout)
    formatter = logging.Formatter(
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
