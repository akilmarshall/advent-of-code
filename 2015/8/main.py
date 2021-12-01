def get_input():
    with open('input', 'r') as f:
        for line in f.readlines():
            yield line.strip()
    

def part_one():
    def memory_length(s: str) -> int:
        '''
        length = len(s)
        escaping = False
        for c in s:
            if c == '\\':
                length -= 1
                escaping = True
                continue
            if escaping and c == 'x':
                escaping = False
            elif escaping and c == '\\':
                continue

        return length - 2
        '''

    def code_length(s: str) -> int:
        length = len(s)

        for a, b in zip(s, s[1:]):
            if a == b == '\\':
                length -= 1

        return length

    memory = 0
    code = 0
    for string in get_input():
        memory = memory_length(string)
        code = code_length(string)
        print(f'code: {code}')
        print(f'memory: {memory}')
        print(string)
        if input() == 'q':
            break

    print(code - memory)


def part_two():
    ...

    
part_one()
part_two()
