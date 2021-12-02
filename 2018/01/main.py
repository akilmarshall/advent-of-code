from itertools import cycle


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line


class Day1:
    def __init__(self, fname='input'):
        self.fname = fname

    def part1(self):
        freq = 0
        for line in inputs(self.fname):
            x = int(line)
            freq += x
        print(freq)

    def part2(self):
        freq = 0
        seen = set([freq])
        for line in cycle(inputs(self.fname)):
            x = int(line)
            freq += x
            if freq in seen:
                break
            seen.add(freq)
        print(freq)


obj = Day1()
obj.part1()
obj.part2()
