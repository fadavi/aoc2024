#!/usr/bin/env python

from load_input import load_input


DELIM = "   "


def main():
    input_str = load_input("day01.txt")
    left_list, right_list = parse_input(input_str)

    total_distance = calc_total_distance(left_list, right_list)
    print("Part1 - Total distance:", total_distance)


def parse_input(input_str: str):
    left_list, right_list = [], []

    for line in input_str.splitlines():
        left, right = line.split(DELIM)
        left_list.append(int(left))
        right_list.append(int(right))

    return sorted(left_list), sorted(right_list)


def calc_total_distance(left_list: list[int], right_list: list[int]):
    distances = (abs(l - r) for l, r in zip(left_list, right_list))
    return sum(distances)


if __name__ == "__main__":
    main()
