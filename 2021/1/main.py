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
        for x in values:
            cur = int(x)
            if prev is not None:
                if prev < cur:
                    inc += 1
            prev = cur
        return inc

    def part1(self):
        values = [int(x) for x in inputs(self.fname)]
        print(self.f(values))

    def part2(self):
        def windows():
            # computes a list of 3 value sums of the input
            out = []
            l = list(inputs(self.fname))
            for a, b, c in zip(l[0:], l[1:], l[2:]):
                out.append(int(a) + int(b) + int(c))
            return out

        print(self.f(windows()))
