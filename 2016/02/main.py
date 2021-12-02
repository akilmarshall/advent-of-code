

def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line


class Day2:
    def __init__(self, fname='input'):
        self.fname = fname
        '''
        1 2 3
        4 5 6
        7 8 9
        '''
        # pos -> action -> new_pos
        self.f = {
            1: {'U': 1, 'D': 4, 'L': 1, 'R': 2},
            2: {'U': 2, 'D': 5, 'L': 1, 'R': 3},
            3: {'U': 3, 'D': 6, 'L': 2, 'R': 3},
            4: {'U': 1, 'D': 7, 'L': 4, 'R': 5},
            5: {'U': 2, 'D': 8, 'L': 4, 'R': 6},
            6: {'U': 3, 'D': 9, 'L': 5, 'R': 6},
            7: {'U': 4, 'D': 7, 'L': 7, 'R': 8},
            8: {'U': 5, 'D': 8, 'L': 7, 'R': 9},
            9: {'U': 6, 'D': 9, 'L': 8, 'R': 9},
        }
        '''
            1
          2 3 4
        5 6 7 8 9
          A B C
            D
        '''
        self.g = {
            1: {'U': 1, 'D': 3, 'L': 1, 'R': 1},
            2: {'U': 2, 'D': 6, 'L': 2, 'R': 3},
            3: {'U': 1, 'D': 7, 'L': 2, 'R': 4},
            4: {'U': 4, 'D': 8, 'L': 3, 'R': 4},
            5: {'U': 5, 'D': 5, 'L': 5, 'R': 6},
            6: {'U': 2, 'D': 'A', 'L': 5, 'R': 7},
            7: {'U': 3, 'D': 'B', 'L': 6, 'R': 8},
            8: {'U': 4, 'D': 'C', 'L': 7, 'R': 9},
            9: {'U': 9, 'D': 9, 'L': 8, 'R': 9},
            'A': {'U': 6, 'D': 'A', 'L': 'A', 'R': 'B'},
            'B': {'U': 7, 'D': 'D', 'L': 'A', 'R': 'C'},
            'C': {'U': 8, 'D': 'C', 'L': 'B', 'R': 'C'},
            'D': {'U': 'B', 'D': 'D', 'L': 'D', 'R': 'D'},
        }

    def part1(self):
        pos = 5
        code = []
        for digit in inputs(self.fname):
            for c in digit.strip():
                pos = self.f[pos][c]
            code.append(str(pos))
        print(''.join(code))

    def part2(self):
        pos = 5
        code = []
        for digit in inputs(self.fname):
            for c in digit.strip():
                pos = self.g[pos][c]
            code.append(str(pos))
        print(''.join(code))


obj = Day2()
obj.part1()
obj.part2()
