import time, collections
starttime = time.time()
titles = []
f = open('MOVIES.LST.txt')
for t in f:
    t = t.strip()
    w = t.split()
    if len(w) <= 1: continue
    beg, end = w[0], w[-1]
    titles.append( (t, beg, end) )
f.close()
beginnings = {}
endings = {}
for t, beg, end in titles:
    if beg not in beginnings: beginnings[beg] = []
    beginnings[beg].append((t, beg, end))
    if end not in endings: endings[end] = []
    endings[end].append((t, beg, end))
results = []
try:
    seen_mappings = set()
    for t, beg, end in titles:
        cur = (t, beg, end)
        seen_mappings.add((cur, cur))
        states = collections.deque()
        states.append( [set([cur]), [cur]] )
        while states:
            seen, state = states.popleft()
            last = state[-1]
            if last[2] not in beginnings:
                continue
            possible = beginnings[last[2]]
            for p in possible:
                if p in seen: continue
                seen2 = seen.copy()
                seen2.add(p)
                m = (last, p)
                if m in seen_mappings:
                    continue
                seen_mappings.add(m)
                newstate = [seen2, state + [p]]
                states.append(newstate)
                results.append(newstate)
except KeyboardInterrupt:
    print
best = max(results, key=lambda x: len(x[1]))
item = [x[0] for x in best[1]]
print '\n'.join(item)
print len(item)
print 'elapsed secs', time.time() - starttime
