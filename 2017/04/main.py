def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class DayX:
    def __init__(self, fname='input'):
        self.fname = fname

    def part1(self):
        print(1)

    def part2(self):
        print(2)


obj = DayX()
obj.part1()
obj.part2()
