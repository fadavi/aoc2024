#!/usr/bin/env python

from os import walk
from load_input import load_input
from pprint import pprint

Report = list[int]
DELIM = " "


def main():
    input_str = load_input("day02.txt")
    reports = parse_input(input_str)

    safe_reports = count_safe_reports(reports)
    print("Part1 - # of Safe Reports:", safe_reports)


def parse_input(input_str: str):
    lines = input_str.splitlines()
    reports: list[Report] = []

    for line in lines:
        report = list(map(int, line.split(DELIM)))
        reports.append(report)

    return reports


def is_safe(report):   
    increasing = report[1] > report[0]

    for cur, nxt in zip(report, report[1:]):
        if increasing != (nxt > cur):
            return False

        delta = abs(cur - nxt)
        if delta < 1 or delta > 3:
            return False

    return True


def count_safe_reports(reports: list[Report]):
    return sum(is_safe(report) for report in reports)


if __name__ == "__main__":
    main()
