#!/usr/bin/env python

from collections.abc import Iterable
from load_input import load_input


DELIM = " "
MIN_SAFE_DIFF, MAX_SAFE_DIFF = 1, 3

Level = int
Report = list[Level]


def main():
    input_str = load_input("day02.txt")
    reports = parse_input(input_str)

    safe_reports = count_safe_reports(reports)
    print("Part1 - # of Safe Reports:", safe_reports)

    safe_reports_with_tolerance = count_safe_reports(reports, tolerance=1)
    print("Part2 - # of Safe Reports with Tolerance:", safe_reports_with_tolerance)


def parse_input(input_str: str):
    lines = input_str.splitlines()
    reports: list[Report] = []

    for line in lines:
        report = list(map(int, line.split(DELIM)))
        reports.append(report)

    return reports


def is_safe(report: Report, tolerance=0):
    return is_increasing_safe(report, tolerance) or is_increasing_safe(
        reversed(report), tolerance
    )


def is_increasing_safe(report: Iterable[Level], tolerance=0):
    levels = iter(report)
    next_level = lambda: next(levels, None)

    cur, nxt = next_level(), next_level()
    head = True

    while cur is not None and nxt is not None and tolerance >= 0:
        if is_safe_diff(nxt - cur):
            head = False
            cur, nxt = nxt, next_level()
            continue

        tolerance -= 1

        new_nxt = next_level()
        if head and new_nxt is not None and not is_safe_diff(new_nxt - cur):
            cur = nxt
        nxt = new_nxt

    return tolerance >= 0


def is_safe_diff(diff: int):
    return MIN_SAFE_DIFF <= diff <= MAX_SAFE_DIFF


def count_safe_reports(reports: Iterable[Report], tolerance=0):
    return sum(is_safe(report, tolerance) for report in reports)


if __name__ == "__main__":
    main()
