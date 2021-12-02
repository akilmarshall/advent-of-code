from collections import namedtuple
from typing import Optional, Tuple


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line


class Day1:
    def __init__(self, fname='input'):
        self.fname = fname
        # heading -> modifier -> new heading
        self.direction = {
            'n': {'R': 'e', 'L': 'w'},
            'e': {'R': 's', 'L': 'n'},
            's': {'R': 'w', 'L': 'e'},
            'w': {'R': 'n', 'L': 's'},
        }

    def part1(self):
        heading = 'n'
        x, y = (0, 0)
        f = open(self.fname)
        lines = [s.strip() for s in f.readlines()[0].split(', ')]
        for line in lines:
            direction = line[0]
            steps = int(line[1:])
            heading = self.direction[heading][direction]
            match heading:
                case 'n':
                    y += steps
                case 'e':
                    x += steps
                case 's':
                    y -= steps
                case 'w':
                    x -= steps
        print(f'({x}, {y}) -> {abs(x) + abs(y)}')

    def part2(self):
        LineSegment = namedtuple('LineSegment', 'a b c d')

        def intersect(A: LineSegment, B: LineSegment) -> Optional[Tuple[int, int]]:
            if A.a == A.c and min(A.b, A.d) <= B.b <= max(A.b, A.d):
                # x accepts y
                if min(B.a, B.c) < A.a < max(B.a, B.c):
                    # y accepts x
                    return (A.a, B.b)
            elif min(A.a, A.c) < B.a < max(A.a, A.c):
                # y accepts x
                if min(B.b, B.d) < A.b < max(B.b, B.d):
                    return (B.a, A.b)
            return None
        heading = 'n'
        x: int = 0
        y: int = 0
        # a list of the line segments previously traversed
        traveled = list()
        f = open(self.fname)
        lines = [s.strip() for s in f.readlines()[0].split(', ')]
        for line in lines:
            instr = line[0]
            step = int(line[1:])
            heading = self.direction[heading][instr]
            match heading:
                case 'n':
                    y_prev = y
                    y += step
                    segment = LineSegment(x, y_prev, x, y)
                case 's':
                    y_prev = y
                    y -= step
                    segment = LineSegment(x, y_prev, x, y)
                case 'e':
                    x_prev = x
                    x += step
                    segment = LineSegment(x_prev, y, x, y)
                case 'w':
                    x_prev = x
                    x -= step
                    segment = LineSegment(x_prev, y, x, y)
            if len(traveled) > 0:
                for path in traveled:
                    if p := (intersect(segment, path)):
                        print(f'({p[0]}, {p[1]}) -> {abs(p[0]) + abs(p[1])}')
                        return
            traveled.append(segment)


obj = Day1()
obj.part1()
obj.part2()
