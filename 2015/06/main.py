import numpy as np
from itertools import product
import re


def get_input():
    with open('input', 'r') as f:
        for line in f.readlines():
            yield line.strip()
    

def part_one():
    lights = np.zeros((1000, 1000), int)
    for instruction in get_input():
        regex = r"^(.*) (\d*),(\d*) through (\d*),(\d*)$"
        matches = re.finditer(regex, instruction)
        a_ = [x for x in matches][0]
        instruction = a_.group(1)
        a, b, c, d = a_.group(2), a_.group(3), a_.group(4), a_.group(5)
        a = int(a)
        b = int(b)
        c = int(c)
        d = int(d)

        f = lambda x: x

        if instruction == 'turn on':
            f = lambda x: 1
        elif instruction == 'turn off':
            f = lambda x: 0
        elif instruction == 'toggle':
            f = lambda x: 1 if x == 0 else 0
        for x, y in product(range(a, c + 1), range(b, d + 1)):
            lights[x][y] = f(lights[x][y])

    on_count = 0
    for x, y in product(range(1000), range(1000)):
        if lights[x][y] == 1:
            on_count += 1
    print(on_count)





def part_two():
    lights = np.zeros((1000, 1000), int)
    for instruction in get_input():
        regex = r"^(.*) (\d*),(\d*) through (\d*),(\d*)$"
        matches = re.finditer(regex, instruction)
        a_ = [x for x in matches][0]
        instruction = a_.group(1)
        a, b, c, d = a_.group(2), a_.group(3), a_.group(4), a_.group(5)
        a = int(a)
        b = int(b)
        c = int(c)
        d = int(d)

        f = lambda x: x

        if instruction == 'turn on':
            f = lambda x: x + 1
        elif instruction == 'turn off':
            f = lambda x: 0 if x - 1 <= 0 else x - 1
        elif instruction == 'toggle':
            f = lambda x: x + 2
        for x, y in product(range(a, c + 1), range(b, d + 1)):
            lights[x][y] = f(lights[x][y])

    on_count = 0
    for x, y in product(range(1000), range(1000)):
        on_count += lights[x][y]
    print(on_count)

    
part_one()
part_two()

test = 'turn on 887,9 through 959,629'
regex = r"^(.*) (\d*),(\d*) through (\d*),(\d*)$"
matches = re.finditer(regex, test)

a = [x for x in matches]

instruction = a[0].group(1)
a, b, c, d = a[0].group(2), a[0].group(3), a[0].group(4), a[0].group(5)
