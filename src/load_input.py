from os import path


dirname = path.dirname(__file__)
inputs_dir = path.join(dirname, '..', 'inputs')


def load_input(name: str) -> str:
    with open(path.join(inputs_dir, name)) as input_file:
        return input_file.read().strip()
