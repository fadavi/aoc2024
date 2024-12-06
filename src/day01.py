#!/usr/bin/env python

from load_input import load_input


DELIM = "   "


def main():
    input_str = load_input("day01.txt")
    left_list, right_list = parse_input(input_str)

    left_list = sorted(left_list)
    right_list = sorted(right_list)

    distances = (abs(l - r) for l, r in zip(left_list, right_list))
    total_distance = sum(distances)

    print("Total distance:", total_distance)


def parse_input(input_str: str):
    left_list, right_list = [], []

    for line in input_str.splitlines():
        left, right = line.split(DELIM)
        left_list.append(int(left))
        right_list.append(int(right))

    return left_list, right_list


if __name__ == "__main__":
    main()
