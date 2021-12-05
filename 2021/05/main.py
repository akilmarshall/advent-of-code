from parsec import many1, digit, space, string
from functools import partial, reduce
from typing import List, Tuple
from math import gcd
from collections import Counter


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Segment:
    def __init__(self, a: int, b: int, c: int, d: int):
        self.a = a
        self.b = b
        self.c = c
        self.d = d

    def slope(self) -> Tuple[int, int]:
        dx = self.c - self.a
        dy = self.d - self.b
        d = gcd(dx, dy)
        dx /= d
        dy /= d
        return int(dx), int(dy)

    def parallel(self) -> bool:
        return self.a == self.c or self.b == self.d

    def points(self) -> List[Tuple[int, int]]:
        points = []
        cur = (self.a, self.b)
        points.append(cur)
        x, y = cur
        while x != self.c or y != self.d:
            if x < self.c:
                x += 1
            elif x > self.c:
                x -= 1
            if y < self.d:
                y += 1
            elif y > self.d:
                y -= 1
            points.append((x, y))
        return points


class Day5:
    def __init__(self, fname='input'):
        self.fname = fname

        numberP = many1(digit().parsecmap(int)).parsecmap(
            partial(reduce, lambda x, y: 10 * x + y))
        pairP = (numberP << string(',')) + numberP
        parserP = pairP << (space() + string('->') + space())
        parser = parserP + pairP

        self.lines = []
        for line in inputs(self.fname):
            (a, b), (c, d) = parser.parse(line)
            self.lines.append(Segment(a, b, c, d))

    def part1(self):
        grid = Counter()
        for segment in self.lines:
            if segment.parallel():
                grid.update(segment.points())
        greater_than_2 = 0
        for item in grid:
            if grid[item] >= 2:
                greater_than_2 += 1
        print(greater_than_2)

    def part2(self):
        grid = Counter()
        for segment in self.lines:
            grid.update(segment.points())
        greater_than_2 = 0
        for item in grid:
            if grid[item] >= 2:
                greater_than_2 += 1
        print(greater_than_2)


obj = Day5()
obj.part1()
obj.part2()
