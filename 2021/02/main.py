# requires python >= 3.10
def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line


class Day2:
    def __init__(self, fname='input'):
        self.fname = fname

    def part1(self):
        horizontal = 0
        depth = 0
        # forward X -> horizontal += X
        # down X    -> depth += X
        # up X      -> depth -= X
        for line in inputs(self.fname):
            instr, val = line.split()
            x = int(val)
            match instr:
                case 'forward':
                    horizontal += x
                case 'down':
                    depth += x
                case 'up':
                    depth -= x
        print(horizontal * depth)

    def part2(self):
        horizontal = 0
        depth = 0
        aim = 0
        # down X -> aim += X
        # up X -> aim -= X
        # forward -> horizontal += X, depth += (aim * X)
        for line in inputs(self.fname):
            instr, val = line.split()
            x = int(val)
            match instr:
                case 'forward':
                    horizontal += x
                    depth += aim * x
                case 'down':
                    aim += x
                case 'up':
                    aim -= x
        print(horizontal * depth)


obj = Day2()
obj.part1()
obj.part2()
