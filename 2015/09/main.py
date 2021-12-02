import re


def get_input():
    with open('input', 'r') as f:
        for line in f.readlines():
            yield line.strip()
    

def part_one():
    distance_matrix = {}
    locations = set()
    visited = set()
    regex = r"^(.*) to (.*) = (.*)$"
    for string in get_input():
        matches = re.findall(regex, string)
        A, B, dist = matches[0]
        distance_matrix[(A, B)]  = dist
        distance_matrix[(B, A)]  = dist
        locations.add(A)
        locations.add(B)

    

def part_two():
    ...

    
part_one()
part_two()
