def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line


class Day1:
    def __init__(self, fname='input'):
        self.fname = fname

    def part1(self):
        inc = 0
        prev = None
        for x in inputs(self.fname):
            cur = int(x)
            if prev is not None:
                if prev < cur:
                    inc += 1
            prev = cur
        print(inc)

    def part2(self):
        def windows():
            out = []
            l = list(inputs(self.fname))
            for a, b, c in zip(l[0:], l[1:], l[2:]):
                out.append(int(a) + int(b) + int(c))
            return out

        inc = 0
        prev = None
        for x in windows():
            cur = int(x)
            if prev is not None:
                if prev < cur:
                    inc += 1
            prev = cur
        print(inc)
