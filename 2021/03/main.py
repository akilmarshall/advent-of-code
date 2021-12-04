from collections import Counter
from itertools import count
from typing import List, Tuple


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day3:
    def __init__(self, fname='input'):
        self.fname = fname
        self.inputs = [x for x in inputs(self.fname)]
        self.width = len(self.inputs[0])

    def compute(self, values: List[str]) -> Tuple[str, str]:
        '''
        compute the gamma, epsilon pair for a given list of boolean value
        '''
        col_lists = [[] for _ in range(self.width)]
        for line in values:
            for i, c in enumerate(line):
                col_lists[i].append(c)
        col_freq = [Counter(x) for x in col_lists]
        gamma = []
        epsilon = []
        for col in col_freq:
            g = '1' if col['1'] >= col['0'] else '0'
            e = '1' if col['1'] < col['0'] else '0'

            gamma.append(g)
            epsilon.append(e)
        return ''.join(gamma), ''.join(epsilon)

    def part1(self):
        values = list(inputs(self.fname))
        gamma, epsilon = self.compute(values)
        gamma = int(gamma, base=2)
        epsilon = int(epsilon, base=2)
        print(f'{gamma = } {epsilon = }\n{gamma * epsilon = }')

    def part2(self):
        def check(numbers, i, x):
            '''
            return a list of numbers that have value x at position i
            '''
            out = []
            for n in numbers:
                if n[i] == x:
                    out.append(n)
            return out

        def sift(numbers: List[str], pos: int):
            '''
            given a list of binary numbers and a pos (0 for gamma, 1 for epsilon)
            sift the values until 1 is left
            '''
            for i in count():
                if len(numbers) == 1:
                    break
                reference = self.compute(numbers)[pos]
                next_numbers = check(numbers, i, reference[i])
                numbers = next_numbers
            return int(numbers[0], base=2)

        oxygen_numbers = self.inputs
        co2_numbers = self.inputs

        oxygen_rating = sift(oxygen_numbers, 0)
        co2_rating = sift(co2_numbers, 1)
        print(f'{oxygen_rating = } {co2_rating = }\n{oxygen_rating * co2_rating = }')


obj = Day3()
obj.part1()
obj.part2()
