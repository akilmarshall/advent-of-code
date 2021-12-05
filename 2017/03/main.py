from itertools import count, repeat, cycle
from typing import Tuple


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day3:
    def __init__(self):
        self.input = 325489

    def spiral(self):
        direction = cycle(['r', 'u', 'l', 'd'])
        x, y = 0, 0

        for a in count(1):
            for n in repeat(a, 2):
                d = next(direction)
                if d == 'r':
                    for _ in range(n):
                        x += 1
                        yield (x, y)
                elif d == 'l':
                    for _ in range(n):
                        x -= 1
                        yield (x, y)
                if d == 'u':
                    for _ in range(n):
                        y += 1
                        yield (x, y)
                elif d == 'd':
                    for _ in range(n):
                        y -= 1
                        yield (x, y)

    def adjacent(self, A: Tuple[int, int]):
        out = []
        Ax, Ay = A
        out.append((Ax + 1, Ay + 0))
        out.append((Ax + 1, Ay + 1))
        out.append((Ax + 0, Ay + 1))
        out.append((Ax - 1, Ay + 1))
        out.append((Ax - 1, Ay + 0))
        out.append((Ax - 1, Ay - 1))
        out.append((Ax + 0, Ay - 1))
        out.append((Ax + 1, Ay - 1))
        return out

    def part1(self):
        '''
        solved it on paper lol
        '''
        print(552)

    def part2(self):
        memory = {(0, 0): 1}
        for x, y in self.spiral():
            val = 0
            for h, k in self.adjacent((x, y)):
                if memory.get((h, k)):
                    val += memory[(h, k)]
            memory[(x, y)] = val
            if val > self.input:
                print(val)
                break


obj = Day3()
obj.part1()
obj.part2()
