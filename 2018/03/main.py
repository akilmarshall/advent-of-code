from __future__ import annotations
from functools import partial, reduce
from itertools import product
from math import inf, floor
from typing import Optional, List, Tuple

from parsec import digit, many1, space, string
from tqdm import tqdm


def inputs(fname):
    with open(fname) as f:
        for line in f.readlines():
            yield line.strip()


class Region:
    def __init__(self, a, b, c, d):
        '''
        (a, b), (c, d) parameters need only be opposing corners of a rectangle
        (a, b), (c, d) members represent the  min corner and max corner respectively
        '''
        self.a = min(a, c)
        self.b = min(b, d)
        self.c = max(a, c)
        self.d = max(b, d)

    def area(self) -> int:
        return (self.c - self.a) * (self.d - self.b)

    def contain(self, x: int, y: int) -> bool:
        if self.a <= x <= self.c and self.b <= y <= self.d:
            return True
        return False

    def overlap(self, other: Region) -> Optional[Region]:
        out = None
        if other.contain(self.c, self.b):
            out = Region(other.a, other.d, self.a, self.b)
        elif other.contain(self.a, self.b):
            out = Region(self.a, self.b, other.c, other.d)
        elif other.contain(self.c, self.d):
            out = Region(other.a, other.b, self.c, self.d)
        elif other.contain(self.a, self.d):
            out = Region(self.a, self.d, other.c, other.b)
        elif self.contain(other.c, other.b):
            out = Region(self.a, self.d, other.a, other.b)
        elif self.contain(other.a, other.b):
            out = Region(other.a, other.b, self.c, self.d)
        elif self.contain(other.c, other.d):
            out = Region(self.a, self.b, other.c, other.d)
        elif self.contain(other.a, other.d):
            out = Region(other.a, other.d, self.c, self.b)
        if out is not None:
            if out.area() == 0:
                return None
        return out

    def center(self) -> Tuple[int, int]:
        return floor(self.c - self.a), floor(self.d - self.b)

    def __repr__(self):
        return f'Region({self.a}, {self.b}, {self.c}, {self.d})'


class QuadTree:
    '''
    B | A
    --|--
    C | D
    '''

    def __init__(self, a: Optional[QuadTree] = None, b: Optional[QuadTree] = None, c: Optional[QuadTree] = None, d: Optional[QuadTree] = None, points: List[Region] = [], area: Region = Region(0, 0, 1, 1), limit: int = 10):
        self.a = a
        self.b = b
        self.c = c
        self.d = d
        self.points = points
        self.area = area
        self.limit = limit

    def insert(self, r: Region):
        if self.area.overlap(r):
            if len(self.points) < self.limit:
                self.points.append(r)
            elif self.a is not None and self.a.area.overlap(r):
                self.a.insert(r)
            elif self.b is not None and self.b.area.overlap(r):
                self.b.insert(r)
            elif self.c is not None and self.c.area.overlap(r):
                self.c.insert(r)
            elif self.d is not None and self.d.area.overlap(r):
                self.d.insert(r)
            else:
                self.subdivide()
                if self.a.area.overlap(r):
                    self.a.insert(r)
                elif self.b.area.overlap(r):
                    self.b.insert(r)
                elif self.c.area.overlap(r):
                    self.c.insert(r)
                elif self.d.area.overlap(r):
                    self.d.insert(r)

    def subdivide(self):
        cx, cy = self.area.center()
        a = Region(cx, cy, self.area.c, self.area.b)
        b = Region(cx, cy, self.area.a, self.area.b)
        c = Region(cx, cy, self.area.a, self.area.d)
        d = Region(cx, cy, self.area.c, self.area.d)
        self.a = QuadTree(points=[], area=a, limit=self.limit)
        self.b = QuadTree(points=[], area=b, limit=self.limit)
        self.c = QuadTree(points=[], area=c, limit=self.limit)
        self.d = QuadTree(points=[], area=d, limit=self.limit)

    def size(self) -> int:
        out = len(self.points)
        if self.a is not None:
            out += self.a.size()
        if self.b is not None:
            out += self.b.size()
        if self.c is not None:
            out += self.c.size()
        if self.d is not None:
            out += self.d.size()
        return out

    def queryRegion(self, r: Region) -> List[Region]:
        out = []
        if r.overlap(self.area):
            for p in self.points:
                if p.overlap(self.area):
                    out.append(p)
                if self.a is not None:
                    out += self.a.queryRegion(r)
                if self.b is not None:
                    out += self.b.queryRegion(r)
                if self.c is not None:
                    out += self.c.queryRegion(r)
                if self.d is not None:
                    out += self.d.queryRegion(r)
        return out


class Day3:
    def __init__(self, fname='input'):
        self.fname = fname

        # f 1 2 = 12
        def f(x, y): return 10 * x + y
        # g :: [int] -> int
        # g [1, 2, 3] = 123
        g = partial(reduce, f)
        # digits_P is a parser from digits to int
        digits_P = many1(digit().parsecmap(int)).parsecmap(g)

        id_P = string('#') >> digits_P
        x_P = space() >> string('@') >> space() >> digits_P << string(',')
        y_P = digits_P << string(':') << space()
        width_P = digits_P << string('x')
        height_P = digits_P

        self.parser = id_P + x_P + y_P + width_P + height_P

    def part1(self):
        areas = []
        a = inf
        b = inf
        c = -inf
        d = -inf
        for line in inputs(self.fname):
            out = self.parser.parse(line)
            ID = out[0][0][0][0]
            x = out[0][0][0][1]
            y = out[0][0][1]
            w = out[0][1]
            h = out[1]

            region = Region(x, y, x + w, y + h)
            areas.append(region)
            if region.a < a:
                a = region.a
            if region.b < b:
                b = region.b
            if region.c > c:
                c = region.c
            if region.d > d:
                d = region.d

        overlap = 0
        # tree = QuadTree(area=Region(a, b, c, d))
        # for area in areas:
        #     print(tree.size())
        #     if tree.area.overlap(area) is None:
        #         import pdb
        #         pdb.set_trace()
        #     tree.insert(area)
        # print(f'{tree.size()}/{len(areas)}')
        # import pdb
        # pdb.set_trace()
        for x, y in tqdm(product(range(int(a), int(c + 1)), range(int(b), int(d + 1)))):
            included = 0
            o = Region(x - 1, y - 1, x, y)

            locations = tree.queryRegion(o)
            for area in locations:
                if included == 0 and o.overlap(area):
                    included += 1
                elif included == 1 and o.overlap(area):
                    overlap += 1
                    break
        print(overlap)

    def part2(self):
        claims = {}
        a = inf
        b = inf
        c = -inf
        d = -inf
        for line in inputs(self.fname):
            out = self.parser.parse(line)
            ID = out[0][0][0][0]
            x = out[0][0][0][1]
            y = out[0][0][1]
            w = out[0][1]
            h = out[1]

            region = Region(x, y, x + w, y + h)
            claims[ID] = region
            if region.a < a:
                a = region.a
            if region.b < b:
                b = region.b
            if region.c > c:
                c = region.c
            if region.d > d:
                d = region.d

        for ID in claims:
            for id_ in claims:
                flag = True
                if ID != id_:
                    if claims[id_].overlap(claims[ID]):
                        flag = False
                        break
            if flag:
                print(ID)

        def f(i: int):
            s = 0
            r = claims[i]
            for j in claims:
                if j != i:
                    if r.overlap(claims[j]):
                        s += 1
            return s

        import pdb
        pdb.set_trace()

        # for x, y in tqdm(product(range(int(a), int(c + 1)), range(int(b), int(d + 1)))):
        #     included = 0
        #     o = Region(x - 1, y - 1, x, y)

        #     locations = tree.queryRegion(o)
        #     for area in locations:
        #         if included == 0 and o.overlap(area):
        #             included += 1
        #         elif included == 1 and o.overlap(area):
        #             overlap += 1
        #             break
        # print(overlap)


obj = Day3('test')
# obj.part1()
'''
999999it [19:10, 868.94it/s]
101565
'''
obj.part2()

tree = QuadTree(limit=1, area=Region(0, 0, 7, 7))
A = Region(1, 3, 5, 7)
B = Region(3, 1, 7, 5)
C = Region(5, 5, 7, 7)
