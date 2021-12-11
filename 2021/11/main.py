from functools import partial
from itertools import product, starmap
from typing import List, Tuple


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day11:
    def __init__(self, fname='input'):
        self.fname = fname

        self.octopus_energy = []

        for line in inputs(self.fname):
            energies = [int(c) for c in line]
            self.octopus_energy.append(energies)

        self.rows = len(self.octopus_energy)
        self.cols = len(self.octopus_energy[0])
        self.indices = partial(product, range(self.rows), range(self.cols))

    def _copy(self, energies):
        out = [row for row in energies]
        return out

    def tick_energy(self, energies) -> List[List[int]]:
        out = self._copy(energies)
        for i, j in self.indices():
            out[i][j] += 1
        return out

    def neighbors(self, x, y):
        '''
        return all neighbors of a cell (x, y)
        '''
        return filter(lambda a: 0 <= a[0] < self.rows and 0 <= a[1] < self.cols, starmap(lambda a, b: (a + x, b + y), product((0, -1, 1), (0, -1, 1))))

    def step(self, energies: List[List[int]]) -> Tuple[List[List[int]], int]:
        '''
        return a tuple containing the next energy state and a number of flashes
        '''
        out = self._copy(energies)
        flash = 0

        # increase all energy by 1
        out = self.tick_energy(out)

        # for each cell check if it flashed, and its neighbors to an increment list
        neighbors = []
        for i, j in self.indices():
            if out[i][j] > 9:
                flash += 1
                neighbors += self.neighbors(i, j)
                out[i][j] = 0

        while len(neighbors) > 0:
            i, j = neighbors.pop()
            if out[i][j] != 0:
                out[i][j] += 1
            if out[i][j] > 9:
                flash += 1
                neighbors += self.neighbors(i, j)
                out[i][j] = 0
        return out, flash

    def part1(self):
        flashes = 0
        out = self._copy(self.octopus_energy)
        for _ in range(100):
            out, flash = self.step(out)
            flashes += flash
        print(flashes)

    def part2(self):
        out = self._copy(self.octopus_energy)
        step = 0
        while True:
            step += 1
            out, flash = self.step(out)
            for row in out:
                for r in row:
                    print(r, end=' ')
                print()
            print()
            if flash == 100:
                # off by 100 error lol
                print(step + 100)
                break


obj = Day11()
obj.part1()
obj.part2()
