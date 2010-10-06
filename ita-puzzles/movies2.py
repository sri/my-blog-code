import random
def word(x):
    y = x.split()
    return x, y[0], y[-1]
titles = [word(x.strip()) for x in open('MOVIES.LST.txt').readlines() if x.strip()]
random.shuffle(titles)
begs = {}
for t, beg, end in titles:
    if beg not in begs:
        begs[beg] = set()
    begs[beg].add((t, beg, end))
results = []
try:
    for t, beg, end in titles:
        partial = []
        seen = set()
        states = [ [set(), (t, beg, end)] ]
        while states:
            state = states.pop(0)
            if len(state) > 100:
                print state; asdf
            t, beg, end = state[-1]
            #if t in seen: continue
            seen.add(t)
            if end not in begs: continue
            elts = begs[end] - state[0]
            if elts:
                for elt in elts:
                    newstate = state[:]
                    newstate[0] = state[0].copy()
                    newstate[0].add(elt)
                    newstate.append(elt)
                    states.append(newstate)
            else:
                state.pop(0)
                partial.append( [x[0] for x in state] )
        if partial:
            results.append(max(partial, key=len))
except KeyboardInterrupt:
    pass
results.sort(key=len, reverse=True)
f = open('Movies.Results.txt', 'w')
for x in results:
    print >>f, len(x)
    print >>f, '\n'.join(x)
    print >>f, '=' * 72
f.close()
