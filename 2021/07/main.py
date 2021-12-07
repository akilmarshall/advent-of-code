def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day7:
    def __init__(self, fname='input'):
        self.fname = fname

        self.crabs = [int(s) for s in list(inputs(self.fname))[0].split(',')]

    def fuel_cost(self, pos: int) -> int:
        return sum([abs(pos - x) for x in self.crabs])

    def actual_fuel_cost(self, pos: int) -> int:
        def f(x, y):
            n = abs(x - y)
            return n * (n + 1) // 2

        return sum([f(x, pos) for x in self.crabs])

    def part1(self):
        crabs = sorted(self.crabs)
        costs = []
        for crab in crabs:
            x = self.fuel_cost(crab)
            costs.append(x)
        print(min(costs))

    def part2(self):
        crabs = sorted(self.crabs)
        min_crab = min(crabs)
        max_crab = max(crabs)
        costs = []
        for crab in range(min_crab, 1 + max_crab):
            x = self.actual_fuel_cost(crab)
            costs.append(x)
        print(min(costs))


obj = Day7()
obj.part1()
obj.part2()
