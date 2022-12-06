buffer = []

with open("input") as f:
    for line in f.readlines():
        buffer = line.strip()

def part_1():
    i = 4
    for X in zip(buffer, buffer[1:], buffer[2:], buffer[3:]):
        if len(set(X)) == 4:
            break
        i += 1
    print(i)

def part_2():
    i = 0
    while True:
        j = i + 14
        seg = set(buffer[i:j])
        if len(seg) == 14:
            break
        i += 1
    print(j)

part_1()
part_2()
