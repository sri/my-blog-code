def word(x):
    y = x.split()
    return x, y[0], y[-1]
_T = """\
hello world
world end
end of time
time to party
party of 5
asker oz
date jaz
moulin mibs"""
print _T
titles = [word(x.strip()) for x in _T.splitlines()]
begs = {}
for t, beg, end in titles:
    begs[beg] = (t, beg, end)
def travel(x):
    cost = 0
    while True:
        if x not in begs: break
        x = begs[x][2]
        cost += 1
    return cost
for t in titles:
    print t[2], '=>', travel(t[2])
