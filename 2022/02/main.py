cheats = []

with open("input") as f:
    for line in f.readlines():
        l, r = line.strip().split()
        cheats.append((l, r))

def part_1():
    player = {
            'X': 1,  # rock
            'Y': 2,  # paper
            'Z': 3,  # scissor
        }
    score = {
            ('A', 'X'): 3,
            ('A', 'Y'): 6,
            ('A', 'Z'): 0,
            ('B', 'X'): 0,
            ('B', 'Y'): 3,
            ('B', 'Z'): 6,
            ('C', 'X'): 6,
            ('C', 'Y'): 0,
            ('C', 'Z'): 3,
            }
    def compute(x):
        l, r = x
        return player[r] + score[(l, r)]
    print(sum(map(compute, cheats)))

def part_2():
    # given a (opp_choice, outcome) -> outcome_value + choice_value
    choose = {
            ('A', 'X'): 0 + 3,
            ('A', 'Y'): 3 + 1,
            ('A', 'Z'): 6 + 2,
            ('B', 'X'): 0 + 1,
            ('B', 'Y'): 3 + 2,
            ('B', 'Z'): 6 + 3,
            ('C', 'X'): 0 + 2,
            ('C', 'Y'): 3 + 3,
            ('C', 'Z'): 6 + 1,
            }
    def compute(x):
        return choose[x]
    print(sum(map(compute, cheats)))

part_1()
part_2()
