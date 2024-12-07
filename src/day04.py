#!/usr/bin/env python

from load_input import load_input


WORD = "XMAS"


def main():
    input_str = load_input("day04.txt")
    grid = LetterGrid(input_str)

    n_occurrences = grid.count_occurrences(WORD)
    print(f"Part1 - # of occurrences of '{WORD}':", n_occurrences)


class LetterGrid:
    def __init__(self, data_str: str):
        self._data = self._parse_data(data_str)


    @staticmethod
    def _parse_data(data_str: str):
        return [list(ds) for ds in data_str.splitlines()]


    def _at(self, x: int, y: int):
        if 0 <= y < len(self._data) and 0 <= x < len(self._data[y]):
            return self._data[y][x]
        return ''

    
    def _read(self, x: int, y: int, length: int ,dx: int, dy: int):
        return ''.join(
            self._at(x + i * dx, y + i * dy)
            for i in range(length)
        )

    def _lookup(self, x: int, y: int, word: str):
        length = len(word)
        words_found = (
            self._read(x, y, length, dx, dy)
            for dx in range(-1, 2)
            for dy in range(-1, 2)
            if dx != 0 or dy != 0
        )
        return sum(word == w for w in words_found)

    
    def count_occurrences(self, word: str):
        return sum(
            self._lookup(x, y, word)
            for y in range(len(self._data))
            for x in range(len(self._data[y]))
        )


if __name__ == "__main__":
    main()
