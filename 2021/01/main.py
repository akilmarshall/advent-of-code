from typing import List


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line


class Day1:
    def __init__(self, fname='input'):
        self.fname = fname

    def f(self, values: List[int]):
        '''
        given a sequence count the number of times it incremented
        '''
        inc = 0
        prev = None
        for cur in values:
            if prev is not None:
                if prev < cur:
                    inc += 1
            prev = cur
        return inc

    def part1(self):
        values = [int(x) for x in inputs(self.fname)]
        print(self.f(values))

    def part2(self):
        l = list(inputs(self.fname))
        print(self.f([int(a) + int(b) + int(c)
              for a, b, c in zip(l[0:], l[1:], l[2:])]))
