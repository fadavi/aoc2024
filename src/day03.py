#!/usr/bin/env python

from collections.abc import Iterable
from functools import reduce
from typing import Literal
from itertools import tee
from operator import mul
import re

from load_input import load_input


MUL_PATTERN = r"mul\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*\)|(do\(\))|(don't\(\))"
OPR1_GROUP, OPR2_GROUP, DO_GROUP, DONT_GROUP = 1, 2, 3, 4
DO, DONT = "do()", "don't()"

MulOperands = tuple[int, int]
Instruction = MulOperands | Literal["do()", "don't()"]
Instructions = Iterable[Instruction]


def main():
    memory_dump = load_input("day03.txt")
    insts1, insts2 = tee(parse_instructions(memory_dump))

    mul_sum_without_donts = sum_instructions_results(insts1)
    print("Part1 - Sum of Mul operations:", mul_sum_without_donts)

    mul_sum_with_donts = sum_instructions_results(insts2, apply_donts=True)
    print("Part2 - Sum of Mul operations with don'ts:", mul_sum_with_donts)


def parse_instructions(input_str: str) -> Instructions:
    for m in re.finditer(MUL_PATTERN, input_str):
        o1 = m.group(OPR1_GROUP)
        o2 = m.group(OPR2_GROUP)

        if o1 is not None and o2 is not None:
            yield int(o1), int(o2)
        elif m.group(DO_GROUP) == DO:
            yield DO
        elif m.group(DONT_GROUP) == DONT:
            yield DONT


def run_instructions(insts: Instructions, apply_donts=False):
    enabled = True
    for inst in insts:
        if inst in (DO, DONT):
            enabled = not apply_donts or inst == DO
        elif enabled:
            yield calc_mul(inst)


def calc_mul(operands: MulOperands):
    return reduce(mul, operands)


def sum_instructions_results(insts: Instructions, apply_donts=False):
    return sum(run_instructions(insts, apply_donts))


if __name__ == "__main__":
    main()
