#!/usr/bin/env python

from collections.abc import Iterable
import re
from load_input import load_input


MUL_PATTERN = r"mul\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*\)"
MulOperands = tuple[int, int]


def main():
    input_str = load_input("day03.txt")
    mul_operands = parse_mul_operands(input_str)

    mul_sum = calc_mul_sum(mul_operands)
    print("Part1 - Sum of Mul operations:", mul_sum)


def parse_mul_operands(input_str: str) -> Iterable[MulOperands]:
    for o1, o2 in re.findall(MUL_PATTERN, input_str):
        yield int(o1), int(o2)


def calc_mul(ops: MulOperands):
    return ops[0] * ops[1]


def calc_mul_sum(ops: Iterable[MulOperands]):
    return sum(calc_mul(ops) for ops in ops)


if __name__ == "__main__":
    main()
