from collections import Counter
from itertools import combinations


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day2:
    def __init__(self, fname='input'):
        self.fname = fname

    def part1(self):
        twos = 0
        threes = 0
        for line in inputs(self.fname):
            character = set(Counter(line).values())
            if 2 in character:
                twos += 1
            if 3 in character:
                threes += 1
        print(twos * threes)

    def part2(self):
        def f(A: str, B: str):
            out = 0
            for a, b in zip(A, B):
                if a != b:
                    out += 1
            return out
        for A, B in combinations(inputs(self.fname), 2):
            if f(A, B) == 1:
                pair = A, B
                break
        ans = []
        A, B = pair
        for a, b in zip(A, B):
            if a == b:
                ans.append(a)
        print(''.join(ans))


obj = Day2()
obj.part1()
obj.part2()
