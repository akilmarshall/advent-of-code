from typing import List
from itertools import product
from parsec import *
from functools import partial, reduce


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class BingoBoard:
    def __init__(self, numbers: List[List[int]]):
        self.numbers = numbers
        self.score = [[False for _ in range(5)] for _ in range(5)]

    def mark(self, x: int):
        for i, j in product(range(5), range(5)):
            if x == self.numbers[i][j]:
                self.score[i][j] = True

    def win(self) -> bool:
        # check horizontal
        for x in range(5):
            if all([self.score[x][y] for y in range(5)]):
                return True
        for y in range(5):
            if all([self.score[x][y] for x in range(5)]):
                return True
        return False

    def value(self) -> int:
        value = 0
        for i, j in product(range(5), range(5)):
            if self.score[i][j] == False:
                value += self.numbers[i][j]
        return value


class Day4:
    def __init__(self, fname='input'):
        self.fname = fname

        lines = list(inputs(self.fname))

        self.numbers = [int(x) for x in lines[0].split(',')]

        nP = many1(digit().parsecmap(int)).parsecmap(
            partial(reduce, lambda x, y: 10 * x + y))
        numberP = nP << spaces()
        parser = count(numberP, 5)

        boards = []
        board = []
        for i, line in enumerate(lines[2:]):
            if i > 0 and (i + 1) % 6 == 0:
                continue
            l = parser.parse(line.strip())
            board.append(l)
            if i > 0 and i % 5 == 0:
                bingo = BingoBoard(board)
                boards.append(bingo)
                board = []

        self.boards = boards

    def part1(self):
        for n in self.numbers:
            print(f'n:{n}')
            for i, board in enumerate(self.boards):
                print(f'board #{i}')
                board.mark(n)
                if board.win():
                    print(board.value())
                    return

    def part2(self):
        print(2)


obj = Day4('test')
obj.part1()
obj.part2()
