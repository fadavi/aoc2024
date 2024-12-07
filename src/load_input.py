from os import path
from logger import logger
import sys


dirname = path.dirname(__file__)
inputs_dir = path.join(dirname, '..', 'inputs')


def load_input(name: str) -> str:
    file_path = path.join(inputs_dir, name) 
    try:
        with open(file_path) as input_file:
            return input_file.read().strip()
    except FileNotFoundError:
        logger.error(f"File {file_path} not found.")
        sys.exit(1)
