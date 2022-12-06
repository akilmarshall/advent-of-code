buffer = []

with open("input") as f:
    for line in f.readlines():
        buffer = line.strip()

def part_1():
    magic = 4
    i = 0
    while True:
        j = i + magic
        seg = set(buffer[i:j])
        if len(seg) == magic:
            break
        i += 1
    print(j)

def part_2():
    magic = 14
    i = 0
    while True:
        j = i + magic
        seg = set(buffer[i:j])
        if len(seg) == magic:
            break
        i += 1
    print(j)

part_1()
part_2()
