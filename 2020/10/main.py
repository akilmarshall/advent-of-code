from collections import Counter


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day10:
    def __init__(self, fname='input'):
        self.fname = fname

        self.adapters = []

        for line in inputs(self.fname):
            self.adapters.append(int(line))
        self.adapters.sort()

    def valid(self, adapters) -> bool:
        d = []
        adapters = list(adapters)
        adapters.sort()
        for a, b in zip(adapters, adapters[1:]):
            d.append(abs(a - b))
        a = Counter(d)
        for k in a:
            if k not in {1, 2, 3}:
                return False
        return True

    def f(self, adapters) -> int:
        if self.valid(adapters) == False:
            return 1

    def part1(self):
        gaps = []
        self.adapters = [0] + self.adapters
        for a, b in zip(self.adapters, self.adapters[1:]):
            gaps.append(abs(a - b))
        analysis = Counter(gaps + [3])
        print(analysis[1] * analysis[3])

    def part2(self):
        import pdb
        pdb.set_trace()


obj = Day10('test')
obj.part1()
obj.part2()
