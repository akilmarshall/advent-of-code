def get_input():
    with open('input', 'r') as f:
        for line in f.readlines():
            yield line.strip()
    

def part_one():
    x, y = 0, 0
    visited = {(0, 0)}
    for directions in get_input():
        for direction in directions:
            if direction == '>':
                x += 1
            elif direction == '<':
                x -= 1
            elif direction == '^':
                y += 1
            elif direction == 'v':
                y -= 1
            visited.add((x, y))
    print(len(visited))


def part_two():
    # santa in position 0, robot in position 1
    positions = [(0, 0), (0, 0)]
    visited = {(0, 0)}
    turn = 0
    for directions in get_input():
        for direction in directions:
            x, y = positions[turn % 2]
            if direction == '>':
                x += 1
            elif direction == '<':
                x -= 1
            elif direction == '^':
                y += 1
            elif direction == 'v':
                y -= 1
            visited.add((x, y))
            positions[turn % 2] = (x, y)
            turn += 1
    print(len(visited))

    
part_one()
part_two()
