def get_input():
    with open('input', 'r') as f:
        for line in f.readlines():
            yield line.strip()
    

def day_one():
    def area(l, w, h):
        return 2 * (l*w + l*h + w*h)

    total_area = 0
    for dimension_string in get_input():
        l, w, h = sorted([int(s) for s in dimension_string.split('x')])
        total_area += area(l, w, h)
        total_area += l*w
    print(total_area)


def day_two():
    total_area = 0
    # for dimension_string in get_input():
    for dimension_string in get_input():
        l, w, h = sorted([int(s) for s in dimension_string.split('x')])
        total_area += l + l + w + w
        total_area += l * w * h

    print(total_area)

    
day_one()
day_two()
