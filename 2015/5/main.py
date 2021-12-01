def get_input():
    with open('input', 'r') as f:
        for line in f.readlines():
            yield line.strip()
    

def part_one():
    def nice_string(s: str) -> bool:
        vowels = len(list(filter(lambda x: x in 'aeiou', s)))
        if vowels < 3:
            return False
        appears_twice = False

        for a, b in zip(s, s[1:]):
            if a == b:
                appears_twice = True
        if appears_twice is False:
            return False

        bad_fragments = ['ab', 'cd', 'pq', 'xy']
        for fragment in bad_fragments:
            if fragment in s:
                return False

        return True

    nice_strings = 0
    for string in get_input():
        if nice_string(string):
            nice_strings += 1
    print(nice_strings)



def part_two():
    def nice_string(s: str):
        duplicate_pair = False
        for a, b in zip(s, s[1:]):
            if s.count(a + b) > 1:
                duplicate_pair = True
        if not duplicate_pair:
            return False
                

        repeat = False
        for a, _, c in zip(s, s[1:], s[2:]):
            if a == c:
                repeat = True
        if not repeat:
            return False

        return True

    nice_strings = 0
    for string in get_input():
        if nice_string(string):
            nice_strings += 1
    print(nice_strings)

    
part_one()
part_two()
