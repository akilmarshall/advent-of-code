def get_input():
    with open('input', 'r') as f:
        yield f.readline().strip()


def part_one():
    floor:int = 0

    for steps in get_input():
        for step in steps:
            if step == '(':
                # santa goes up a floor
                floor += 1
            elif step == ')':
                # santa goes down a floor
                floor -= 1

    print(f'Part one solution:{floor}')

def part_two():
    steps_to_basement = 0
    floor = 0
    for steps in get_input():
        # pre increment steps_to_basement
        for step in steps:
            steps_to_basement += 1
            if step == '(':
                # santa goes up a floor
                floor += 1
            elif step == ')':
                # santa goes down a floor
                floor -= 1

            # check if we have entered the basement
            if floor == -1:
                break

        print(f'Part two solution:{steps_to_basement}')




def main():
    part_one()
    part_two()

if __name__ == '__main__':
    main()

