from string import ascii_letters
rucksacks = []
rucksacks_whole = []

with open("input") as f:
    for line in f.readlines():
        compartments = line.strip()
        n = len(compartments) // 2
        rucksacks.append((compartments[:n], compartments[n:]))
        rucksacks_whole.append(compartments)

def priority(c):
    return ascii_letters.index(c) + 1

def part_1():
    # same = []
    total = 0
    for (one, two) in rucksacks:
        one = set(one)
        two = set(two)
        both = one.intersection(two).pop()
        total += priority(both)
    print(total)

def part_2():
    from queue import deque
    q = deque(rucksacks_whole)
    total = 0
    while len(q) > 0:
        a = set(q.pop())
        b = set(q.pop())
        c = set(q.pop())
        badge = a.intersection(b).intersection(c)
        total += priority(badge.pop())
    print(total)

part_1()
part_2()
