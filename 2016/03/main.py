from parsec import digit, many1, space


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day3:
    def __init__(self, fname='input'):
        self.fname = fname
        self.parser = many1(digit()) + many1(space()) + \
            many1(digit()) + many1(space()) + many1(digit())

    def valid(self, A: int, B: int, C: int):
        if A + B > C and A + C > B and B + C > A:
            return True
        return False

    def part1(self):
        ok = 0
        for line in inputs(self.fname):
            out = self.parser.parse(line)
            A = int(''.join(out[0][0][0][0]))
            B = int(''.join(out[0][0][1]))
            C = int(''.join(out[1]))
            if self.valid(A, B, C):
                ok += 1
        print(ok)

    def part2(self):
        def assemble():
            '''
            from the input assemble a list of triangles 3 at a time
            '''
            A = list()
            B = list()
            C = list()
            for line in inputs(self.fname):
                out = self.parser.parse(line)
                a = int(''.join(out[0][0][0][0]))
                b = int(''.join(out[0][0][1]))
                c = int(''.join(out[1]))
                A.append(a)
                B.append(b)
                C.append(c)
                if len(A) == 3:
                    yield A
                    yield B
                    yield C
                    A.clear()
                    B.clear()
                    C.clear()

        ok = 0
        for A, B, C in assemble():
            if self.valid(A, B, C):
                ok += 1
        print(ok)


obj = Day3()
obj.part1()
obj.part2()
