from collections import Counter
from itertools import combinations
from typing import List


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day4:
    def __init__(self, fname='input'):
        self.fname = fname

    def part1(self):
        valid = 0
        for line in inputs(self.fname):
            phrase = Counter(line.split())
            if all([x < 2 for x in phrase.values()]):
                valid += 1
        print(valid)

    def part2(self):
        def valid(phrase: List[str]) -> bool:
            for a, b in combinations(phrase, 2):
                if Counter(a) == Counter(b):
                    return False
            return True

        v = 0
        for line in inputs(self.fname):
            line = line.split()
            phrase = Counter(line)
            if valid(line) and all([x < 2 for x in phrase.values()]):
                v += 1
        print(v)


obj = Day4()
obj.part1()
obj.part2()
