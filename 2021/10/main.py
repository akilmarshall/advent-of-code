from typing import Optional, Tuple


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day10:
    def __init__(self, fname='input'):
        self.fname = fname

        self.score = {')': 3, ']': 57, '}': 1197, '>': 25137}

        self.chunks = []
        for line in inputs(self.fname):
            self.chunks.append(line)

    def corrupted(self, chunk: str) -> Optional[Tuple[str, str]]:
        stack = []
        for c in chunk:
            if c in '([{<':
                stack.append(c)
            elif c in ')]}>':
                a = stack.pop()
                # corrupted
                if a == '(' and c != ')':
                    return c, ''.join(stack)
                elif a == '[' and c != ']':
                    return c, ''.join(stack)
                elif a == '{' and c != '}':
                    return c, ''.join(stack)
                elif a == '<' and c != '>':
                    return c, ''.join(stack)

    def complete(self, chunk: str):
        stack = []
        for s in chunk:
            if s in '{[(<':
                stack.append(s)
            else:
                stack.pop()

        mirror = {'(': ')', '{': '}', '[': ']', '<': '>'}
        out = []
        stack.reverse()
        for s in stack:
            out.append(mirror[s])
        return ''.join(out)

    def part1(self):
        score = 0
        for chunk in self.chunks:
            c = self.corrupted(chunk)
            if c:
                s, _ = c
                score += self.score[s]
        print(score)

    def part2(self):
        score = {')': 1, ']': 2, '}': 3, '>': 4}
        scores = []
        for chunk in self.chunks:
            s = 0
            c = self.corrupted(chunk)
            if c is None:
                complete = self.complete(chunk)
                for i in complete:
                    s *= 5
                    s += score[i]
            if s > 0:
                scores.append(s)
        scores.sort()
        i = len(scores) // 2
        print(scores[i])


obj = Day10()
obj.part1()
obj.part2()
