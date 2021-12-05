from functools import partial, reduce
from itertools import combinations

from parsec import digit, many1, sepBy, space


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day2:
    def __init__(self, fname='input'):
        self.fname = fname

        self.sheet = []

        numberP = many1(digit().parsecmap(int)).parsecmap(
            partial(reduce, lambda x, y: 10 * x + y))
        parser = sepBy(numberP, space())

        for line in inputs(self.fname):
            self.sheet.append(parser.parse(line))

    def part1(self):
        x = 0
        for row in self.sheet:
            a = min(row)
            b = max(row)
            x += b - a
        print(x)

    def part2(self):
        x = 0
        for row in self.sheet:
            for h, k in combinations(row, 2):
                min_ = min(h, k)
                max_ = max(h, k)
                idiv = max_ // min_
                fdiv = max_ / min_
                if idiv == fdiv:
                    x += idiv
                    break
        print(x)


obj = Day2()
obj.part1()
obj.part2()
