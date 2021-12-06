from parsec import *
from functools import partial, reduce


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Day7:
    def __init__(self, fname='input'):
        self.fname = fname

        numberP = many1(digit().parsecmap(int)).parsecmap(
            partial(reduce, lambda x, y: 10 * x + y))
        wordP = many1(letter()).parsecmap(''.join)
        colorP = (wordP + (space() >> wordP)).parsecmap(' '.join)
        bagP = (colorP << space()) << (string('bags') ^ string('bag'))
        nullP = space() >> string('no other bags.').parsecmap(lambda _: [])
        containP = nullP ^ sepBy(
            (space() >> numberP) + (space() >> bagP), string(','))
        self.parser = (bagP << space() + string('contain')) + containP

        self.rules = {}
        for line in inputs(self.fname):
            primary, rules = self.parser.parse(line)
            self.rules[primary] = {
                color: quantity for quantity, color in rules}

    def graph_search(self, root, target) -> bool:
        '''
        starting at root search for target
        '''
        # seen = set()
        # expand = list(self.rules[root].keys())  # stack
        queue = [root]
        explored = set(queue)
        i = 0
        while len(queue) > 0:
            v = queue.pop()
            if v == target and i != 0:
                return True
            for leaf in self.rules[v]:
                if leaf not in explored:
                    explored.add(leaf)
                    queue.append(leaf)
            i += 1
        return False

    def tree_accumulate(self, root) -> int:
        a = 0
        queue = [root]
        while len(queue) > 0:
            v = queue.pop()
            for leaf in self.rules[v]:
                a += self.rules[v][leaf]
                queue.append(leaf)
        return a

    def f(self, root):
        pop = 0
        if len(self.rules[root]) == 0:
            return 1
        for leaf in self.rules[root]:
            branch_val = self.rules[root][leaf]
            leaf_val = self.f(leaf)
            # pop +=
        return pop

    # def g(self, root, x=1):
    #     for leaf in self.rules[root]:
    #         x += x

    def part1(self):
        color_set = set(self.rules.keys())
        ans = 0
        for c in color_set:
            if self.graph_search(c, 'shiny gold'):
                ans += 1
        print(ans)

    def part2(self):
        # print(self.tree_accumulate('shiny gold'))
        print(self.f('shiny gold'))


obj = Day7('test')
obj.part1()
obj.part2()
