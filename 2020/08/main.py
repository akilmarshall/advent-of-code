from typing import Optional

from parsec import digit, letter, many1, one_of, space


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day8:
    def __init__(self, fname='input'):
        self.fname = fname

        instrP = many1(letter()).parsecmap(''.join)
        numberP = ((one_of('+-') + many1(digit()).parsecmap(''.join))
                   ).parsecmap(''.join).parsecmap(int)
        self.parser = instrP + (space() >> numberP)

        self.program = []
        for line in inputs(self.fname):
            self.program.append(self.parser.parse(line))

    def halt(self, program) -> Optional[int]:
        accumulator = 0
        executed = set()
        pc = 1
        while True:
            if pc > len(program):
                break
            instr, x = program[pc - 1]
            if pc in executed:
                return None
            executed.add(pc)
            if instr == 'acc':
                accumulator += x
                pc += 1
            elif instr == 'jmp':
                pc += x
            elif instr == 'nop':
                pc += 1
        return accumulator

    def swap(self, s):
        instr, x = s
        if instr == 'nop':
            return 'jmp', x
        elif instr == 'jmp':
            return 'nop', x

    def part1(self):
        accumulator = 0
        executed = set()
        pc = 1
        while True:
            instr, x = self.program[pc - 1]
            if pc in executed:
                print(accumulator)
                break
            executed.add(pc)
            if instr == 'acc':
                accumulator += x
                pc += 1
            elif instr == 'jmp':
                pc += x
            elif instr == 'nop':
                pc += 1

    def part2(self):
        for i, (instr, x) in enumerate(self.program):
            if instr in {'nop', 'jmp'}:
                program = self.program[0:i] + \
                    [self.swap((instr, x))] + self.program[i + 1:]
                check = self.halt(program)
                if check:
                    print(check)
                    break


obj = Day8()
obj.part1()
obj.part2()
