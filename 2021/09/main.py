from typing import List, Tuple


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day9:
    def __init__(self, fname='input'):
        self.fname = fname
        # row, col
        self.height_map = []
        for line in inputs(self.fname):
            self.height_map.append(list(map(int, line)))

        self.width = len(self.height_map[0])
        self.height = len(self.height_map)

    def neighbors(self, x, y) -> List[Tuple[int, int]]:
        out = []
        if x > 0:
            out.append((x - 1, y))
        if x < self.width - 1:
            out.append((x + 1, y))
        if y > 0:
            out.append((x, y - 1))
        if y < self.height - 1:
            out.append((x, y + 1))
        return out

    def low_point(self, x, y) -> bool:
        val = self.height_map[y][x]
        for i, j in self.neighbors(x, y):
            if not val < self.height_map[j][i]:
                return False
        return True

    def low_points(self) -> List[Tuple[int, int]]:
        low_points = []
        for y in range(self.height):
            for x in range(self.width):
                if self.low_point(x, y):
                    low_points.append((x, y))
        return low_points

    def basin(self, x, y):
        seen = {(x, y)}
        expand = {(x, y)}
        while expand:
            h, k = expand.pop()
            seen.add((h, k))
            for i, j in self.neighbors(h, k):
                if (i, j) not in seen and self.height_map[j][i] != 9:
                    expand.add((i, j))
        return seen

    def part1(self):
        risk_level = 0
        for x, y in self.low_points():
            v = self.height_map[y][x]
            risk_level += v + 1
        print(risk_level)

    def part2(self):
        basin_sizes = []
        for i, j in self.low_points():
            s = len(self.basin(i, j))
            basin_sizes.append(s)
        basin_sizes.sort()
        print(basin_sizes[-1] * basin_sizes[-2] * basin_sizes[-3])


obj = Day9()
obj.part1()
obj.part2()
