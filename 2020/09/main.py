from itertools import combinations
from typing import List


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class XMASCracker:
    def __init__(self, numbers: List[int], preamble: int = 25):
        self.numbers = numbers
        self.preamble = preamble

    def check(self, i: int):
        '''
        check if the ith number satisfies the condition
        '''

        target = self.numbers[i]
        value = set(self.numbers[i - self.preamble: i])

        for a, b in combinations(value, 2):
            if a + b == target:
                return True
        return False


class Day9:
    def __init__(self, fname='input'):
        self.fname = fname

        self.numbers = []
        for line in inputs(self.fname):
            self.numbers.append(int(line))

    def part1(self):
        preamble = 25 if self.fname == 'input' else 5
        machine = XMASCracker(self.numbers, preamble)
        for i, number in enumerate(machine.numbers[preamble:], start=preamble):
            if machine.check(i) == False:
                self.invalid = number
                print(self.invalid)
                break

    def part2(self):
        i = 0
        while True:
            contiguous = []
            for x in self.numbers[i:]:
                contiguous.append(x)
                y = sum(contiguous)
                if y == self.invalid:
                    print(max(contiguous) + min(contiguous))
                    return
                if y > self.invalid:
                    contiguous.clear()
                    break
            i += 1


obj = Day9()
obj.part1()
obj.part2()
