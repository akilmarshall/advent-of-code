from queue import deque
from pprint import pprint


stacks_raw = []
instructions_raw = []
with open("input") as f:
    phase = 0
    for line in f.readlines():
        line = line[:-1]  # remove the new line character without removing leading spaces
        if phase == 0:
            if line == '':
                phase = 1
            else:
                stacks_raw.append(line)
        else:
            instructions_raw.append(line)

# process stacks into a list of .. stacks
stacks_raw.reverse()
stacks_raw = stacks_raw[1:]

def stack_indicies():
    i = 1
    while True:
        yield i
        i += 4

stacks = [deque([]) for _ in range(9)]
for level in stacks_raw:
    I = stack_indicies()
    i = next(I)
    c = 0
    while i < len(level):
        item = level[i]
        if item != ' ':
            stacks[c].append(item)
        i = next(I)
        c += 1

# process the instructions
instructions = []  # list of (quantity, from, to)
for inst in instructions_raw:
    inst = inst.split()
    quantity = int(inst[1])
    A = int(inst[3]) - 1  # adjust to 0 indexing
    B = int(inst[5]) - 1  # adjust to 0 indexing
    instructions.append((quantity, A, B))

def part_1():
    for q, A, B in instructions:
        for _ in range(q):
            stacks[B].append(stacks[A].pop())

    for stack in stacks:
        print(stack.pop(), end='')
    print()

def part_2():
    for q, A, B in instructions:
        move = []
        for _ in range(q):
            move.append(stacks[A].pop())
        move.reverse()
        for item in move:
            stacks[B].append(item)

    for stack in stacks:
        print(stack.pop(), end='')
    print()

# due to manipulation of the global stacks in both part_1 and part_2
# I opted to just comment out the function call to compute each parts
# answer
# part_1()
part_2()
