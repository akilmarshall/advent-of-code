def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day1:
    def __init__(self, fname='input'):
        self.fname = fname
        self.captcha = [int(x) for x in list(inputs(self.fname))[0]]

        self.n = len(self.captcha)

    def part1(self):
        x = 0
        for n, m in zip(self.captcha, self.captcha[1:] + [self.captcha[0]]):
            if n == m:
                x += n
        print(x)

    def part2(self):
        x = 0
        for i, n in enumerate(self.captcha):
            j = (i + (self.n // 2)) % self.n
            if n == self.captcha[j]:
                x += n
        print(x)


obj = Day1()
obj.part1()
obj.part2()
