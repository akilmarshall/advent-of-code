from collections import Counter
from functools import partial, reduce
from typing import List

from parsec import *


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Room:
    def __init__(self, name: List[str], sector_id: int, checksum: str):
        self.name = name
        self.sector_id = sector_id
        self.checksum = checksum

        self.common = Counter()
        for w in self.name:
            self.common.update(w)

    def valid(self) -> bool:
        for i, j in zip(self.checksum, self.checksum[1:]):
            if self.common[i] < self.common[j]:
                return False
            if self.common[i] == self.common[j] and i > j:
                return False
        return True


class Day4:
    def __init__(self, fname='input'):
        self.fname = fname

        wordP = many1(letter()).parsecmap(''.join)
        idP = string('-') >> many1(digit().parsecmap(int)
                                   ).parsecmap(partial(reduce, lambda x, y: 10 * x + y))
        checksumP = string('[') >> wordP << string(']')
        self.parser = sepBy(wordP, string('-')) + idP + checksumP

        self.rooms = []
        for line in inputs(self.fname):
            parse = self.parser.parse(line)
            checksum = parse[1]
            sector_id = parse[0][1]
            name = parse[0][0]
            self.rooms.append(Room(name, sector_id, checksum))

    def part1(self):
        valid = 0
        i = 0
        for room in self.rooms:
            if room.valid():
                valid += room.sector_id
                i += 1
        print(valid, i)

    def part2(self):
        print(2)


obj = Day4()
obj.part1()
obj.part2()
