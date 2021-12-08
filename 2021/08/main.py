from itertools import combinations
from typing import List

from parsec import letter, many1, sepBy, space, string


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day8:
    def __init__(self, fname='input'):
        self.fname = fname

        wordP = many1(letter()).parsecmap(''.join)
        wordsP = sepBy(wordP, space())
        self.parser = wordsP + ((space() + string('|') + space()) >> wordsP)

        self.signals = []
        for line in inputs(self.fname):
            self.signals.append(self.parser.parse(line))

    def distinct_number(self, number) -> bool:
        if len(number) in {2, 4, 3, 7}:
            return True
        return False

    def get_distinct(self, numbers: List[str]):
        one, four, seven, eight = '', '', '', ''
        for n in numbers:
            l = len(n)
            if l == 2:
                one = n
            if l == 3:
                seven = n
            if l == 4:
                four = n
            if l == 7:
                eight = n
        return one, four, seven, eight

    def decode(self, signal_pattern: List[str]):
        '''
        given an input: (pattern_input, out_numbers)
        decode the wires and return a mapping from
        wire -> true wire
        this mapping is used to unscramble the out_numbers
        '''

        def two_int_five():
            fives = filter(lambda x: len(x) == 5, signal_pattern)
            for a, b in combinations(fives, 2):
                x = set(a).intersection(set(b))
                if len(x) == 3:
                    return x
            return set()

        def six_int():
            sixes = filter(lambda x: len(x) == 6, signal_pattern)
            return set.intersection(*[set(x) for x in sixes])

        m = {}
        one, four, seven, eight = self.get_distinct(signal_pattern)
        a = (set(seven) - set(one)).pop()
        m[a] = 'a'
        b = ((set(four) - set(seven) - two_int_five())).pop()
        m[b] = 'b'
        d = (set(four) - set(one) - set([b])).pop()
        m[d] = 'd'
        g = (two_int_five() - set([a, d])).pop()
        m[g] = 'g'
        f = (six_int() - set([a, b, g])).pop()
        m[f] = 'f'
        c = (set(one) - set([f])).pop()
        m[c] = 'c'
        e = (set(eight) - set(four) - set([a, g])).pop()
        m[e] = 'e'
        return m

    def classify(self, number: str) -> str:
        N = set(number)
        zero = frozenset({'a', 'b', 'c', 'e', 'f', 'g'})
        one = frozenset({'c', 'f'})
        two = frozenset({'a', 'c', 'd', 'e', 'g'})
        three = frozenset({'a', 'c', 'd', 'f', 'g'})
        four = frozenset({'b', 'c', 'd', 'f'})
        five = frozenset({'a', 'b', 'd', 'f', 'g'})
        six = frozenset({'a', 'b', 'd', 'e', 'f', 'g'})
        seven = frozenset({'a', 'c', 'f'})
        eight = frozenset({'a', 'b', 'c', 'd', 'e', 'f', 'g'})
        nine = frozenset({'a', 'b', 'c', 'd', 'f', 'g'})
        mapping = {zero: '0', one: '1', two: '2', three: '3', four: '4',
                   five: '5', six: '6', seven: '7', eight: '8', nine: '9'}
        for signals in mapping:
            if signals == N:
                return mapping[signals]
        return ''

    def unscramble(self, number: str, decoder: dict[str, str]) -> str:
        out = []
        for n in number:
            out.append(decoder[n])
        return self.classify(''.join(out))

    def part1(self):
        distinct = 0
        for _, b in self.signals:
            for x in b:
                if self.distinct_number(x):
                    distinct += 1
        print(distinct)

    def part2(self):
        ans = 0
        for pattern_signal, out in self.signals:
            number = []
            decoder = self.decode(pattern_signal)
            for o in out:
                d = self.unscramble(o, decoder)
                number.append(d)
            number = int(''.join(number))
            ans += number
        print(ans)


obj = Day8()
obj.part1()
obj.part2()
