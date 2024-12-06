#!/usr/bin/env python

from functools import reduce
from load_input import load_input


IntList = list[int]
DELIM = "   "


def main():
    input_str = load_input("day01.txt")
    left_list, right_list = parse_input(input_str)

    total_distance = calc_total_distance(left_list, right_list)
    print("Part1 - Total distance:", total_distance)

    similarity_score = calc_similarity_score(left_list, right_list)
    print("Part2 - Similarity score:", similarity_score)


def parse_input(input_str: str):
    left_list, right_list = [], []

    for line in input_str.splitlines():
        left, right = line.split(DELIM)
        left_list.append(int(left))
        right_list.append(int(right))

    return sorted(left_list), sorted(right_list)


def calc_total_distance(left_list: IntList, right_list: IntList):
    distances = (abs(l - r) for l, r in zip(left_list, right_list))
    return sum(distances)


def prepare_freq_dict(data: IntList):
    return reduce(
        lambda acc, n: {**acc, n: acc.get(n, 0) + 1},
        data,
        {},
    )


def calc_similarity_score(left_list: IntList, right_list: IntList):
    left_freq = prepare_freq_dict(left_list)
    right_freq = prepare_freq_dict(right_list)

    score = 0

    for val, l_freq in left_freq.items():
        r_freq = right_freq.get(val, 0)
        score += l_freq * r_freq * val

    return score


if __name__ == "__main__":
    main()
