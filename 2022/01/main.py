from heapq import heappushpop

reindeer = []

with open("input") as f:
    load = []
    for line in f.readlines():
        if line == '\n':
            reindeer.append(load)
            load = []
        else:
            load.append(int(line.strip()))

def part_1():
    print(max(map(sum, reindeer)))

def part_2():
    heap = []
    for load in reindeer:
        if len(heap) < 3:
            heap.append(sum(load))
        else:
            heappushpop(heap, sum(load))
    print(sum(heap))

part_1()
part_2()
