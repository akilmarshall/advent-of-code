from functools import partial, reduce
from typing import List, Tuple

from parsec import letter, many1, digit, sepBy, string


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Wire:
    def __init__(self, wire: List[Tuple[str, int]], central_port: Tuple[int, int] = (1, 1)):
        '''
        :wire:  a list of (str, int) pairs describing the wires path from the origin
        '''
        self.central_port = central_port
        self.points = [self.central_port]
        for d, l in wire:
            x, y = self.points[-1]
            if d == 'R':
                self.points += [(x + i, y) for i in range(1, l + 1)]
            elif d == 'L':
                self.points += [(x - i, y) for i in range(1, l + 1)]
            elif d == 'U':
                self.points += [(x, y + i) for i in range(1, l + 1)]
            elif d == 'D':
                self.points += [(x, y - i) for i in range(1, l + 1)]


class Day3:
    def __init__(self, fname='input'):
        self.fname = fname

        self.wires = []
        for line in inputs(self.fname):
            instrP = letter() + many1(digit().parsecmap(int)
                                      ).parsecmap(partial(reduce, lambda x, y: 10 * x + y))
            parser = sepBy(instrP, string(','))
            wire = Wire(parser.parse(line))
            self.wires.append(wire)

        point_sets = []
        for wire in self.wires:
            point_sets.append(set(wire.points))
        self.crosses = set.intersection(*point_sets)

    def manhattan(self, A, B):
        Ax, Ay = A
        Bx, By = B
        return abs(Ax - Bx) + abs(Ay - By)

    def part1(self):
        d = None
        for x, y in self.crosses - {(1, 1)}:
            w = self.manhattan((1, 1), (x, y))
            if d is None or w < d:
                d = w
        print(d)

    def part2(self):
        d = None
        for x, y in self.crosses - {(1, 1)}:
            w = sum([wire.points.index((x, y)) for wire in self.wires])
            if d is None or w < d:
                d = w
        print(d)


obj = Day3()
obj.part1()
obj.part2()
