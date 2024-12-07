import logging


LOGGER_NAME = "AOC2024"
LOG_LEVEL = logging.DEBUG
FORMAT = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"


def create_logger():
    logger = logging.getLogger(LOGGER_NAME)
    logger.setLevel(LOG_LEVEL)
    logger.addHandler(create_console_handler())
    return logger


def create_console_handler():
    console_handler = logging.StreamHandler()
    console_handler.setLevel(LOG_LEVEL)
    console_handler.setFormatter(logging.Formatter(FORMAT))
    return console_handler


logger = create_logger()
