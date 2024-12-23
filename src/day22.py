#!/usr/bin/env pypy

from collections import defaultdict
from time import perf_counter_ns
from typing import Iterator
from itertools import tee
from sys import argv
from os import path

DIR = path.dirname(path.realpath(__file__))
INPUT_FILE_PATH = path.join(DIR, "..", "inputs", "day22.txt")


def parse_input(file_path=INPUT_FILE_PATH):
    with open(file_path) as file:
        while (line := file.readline()):
            yield int(line)


def next_secret(secret: int):
    secret = (secret ^ (secret << 6)) & 0xFFFFFF
    secret = (secret ^ (secret >> 5)) & 0xFFFFFF
    return (secret ^ (secret << 11)) & 0xFFFFFF


def last_secret(secret: int):
    for _ in range(2000):
        secret = next_secret(secret)
    return secret


def get_prices(secret: int):
    for _ in range(2001):
        yield secret % 10
        secret = next_secret(secret)


def pack_seq(ch0: int, ch1: int, ch2: int, ch3: int):
    return (ch0 << 12) + (ch1 << 8) + (ch2 << 4) + ch3


def merge_change_sequences_into(secret: int, seqs: defaultdict):
    seen = set()
    prices = get_prices(secret)

    pprice, price = next(prices), next(prices)
    ch0 = price - pprice
    pprice, price = price, next(prices)
    ch1 = price - pprice
    pprice, price = price, next(prices)
    ch2 = price - pprice
    pprice = price

    for price in prices:
        ch3 = price - pprice
        seq = pack_seq(ch0, ch1, ch2, ch3)
        if seq not in seen:
            seen.add(seq)
            seqs[seq] += price

        ch0, ch1, ch2 = ch1, ch2, ch3
        pprice = price

    return seqs


def max_bananas(secrets: Iterator[int]):
    seqs = defaultdict(int)
    for s in secrets:
        merge_change_sequences_into(s, seqs)
    return max(seqs.values())


def main():
    secrets1, secrets2 = tee(parse_input())
    parts = argv[1] if len(argv) > 1 else "part1,part2"

    if "part1" in parts:
        part1 = (sum(last_secret(s) for s in secrets1))
        print("Part 1:", part1)
    if "part2" in parts:
        part2 = max_bananas(secrets2)
        print("Part 2:", part2)


if __name__ == "__main__":
    t0 = perf_counter_ns()
    main()
    t1 = perf_counter_ns()
    millis = (t1 - t0) / 1e6
    print(f"Elapsed Time: {millis}ms")
