from functools import lru_cache

from parsec import digit, sepBy, string


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day6:
    def __init__(self, fname='input'):
        self.fname = fname

        self.parser = sepBy(digit().parsecmap(int), string(','))

        self.fish_population = self.parser.parse(list(inputs(self.fname))[0])

    @lru_cache()
    def evolve(self, n: int, t: int):
        if n == 0 and t == 1:
            return 2
        elif t == 1:
            return 1
        if n == 0:
            return self.evolve(6, t - 1) + self.evolve(8, t - 1)
        return self.evolve(n - 1, t - 1)

    def model(self, t) -> int:
        pop = 0
        for fish in self.fish_population:
            pop += self.evolve(fish, t)
        return pop

    def part1(self):
        print(self.model(80))

    def part2(self):
        print(self.model(256))


obj = Day6()
obj.part1()
obj.part2()
