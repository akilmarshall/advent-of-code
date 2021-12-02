import hashlib


Input = 'yzbqklnj'
a = hashlib.md5(b'abcdef609043')

def part_one():
    def check(s: str):
        assert len(s) > 5
        if s[0:5] == '00000':
            return True
        return False
    key = 0

    while True:
        input_ = bytes(Input + str(key), 'utf-8')
        hash_ = hashlib.md5(input_)
        if check(hash_.hexdigest()):
            break
        key += 1

    print(key)


def part_two():
    def check(s: str):
        assert len(s) > 6
        if s[0:6] == '000000':
            return True
        return False
    key = 0

    while True:
        input_ = bytes(Input + str(key), 'utf-8')
        hash_ = hashlib.md5(input_)
        if check(hash_.hexdigest()):
            break
        key += 1

    print(key)

    
part_one()
part_two()
