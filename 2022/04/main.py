assignments = []

with open("input") as f:
    for line in f.readlines():
        a, b = line.strip().split(',')
        assignments.append((a, b))


def part_1():
    def contains(x):
        a, b = x
        al, ar = map(int, a.split('-'))
        bl, br = map(int, b.split('-'))
        if al <= bl and ar >= br or bl <= al and br >= ar:
            return 1
        return 0
    print(sum(map(contains, assignments)))

def part_2():
    def overlap(x):
        a, b = x
        al, ar = map(int, a.split('-'))
        bl, br = map(int, b.split('-'))
        if ar < bl or br < al:
            return 0
        return 1
    print(sum(map(overlap, assignments)))

part_1()
part_2()
