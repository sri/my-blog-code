import collections, time

starttime = time.time()
titles = [x.strip() for x in open('MOVIES.LST.txt') if x.strip()]

# Add titles to beginnings such that, beginnings[T] = list of titles
# that start with some suffix of title T.
beginnings = collections.defaultdict(list)
ends = collections.defaultdict(list)
begs = collections.defaultdict(list)
for t in titles:
    s = t.split()
    for i in range(len(s) - 1):
        end = ' '.join(s[-i-1:])
        ends[end].append(t)
        beg = ' '.join(s[:i+1])
        begs[beg].append(t)
for k in [k for k in ends if k in begs]:
    for t in ends[k]:
        beginnings[t].extend( begs[k] )

titles.sort(key=lambda t: len(beginnings[t]), reverse=True)
for t in beginnings.keys():
    # if reverse is set to False, then the longer results don't
    # show up that quickly. 
    beginnings[t].sort(key=lambda t: len(beginnings[t]), reverse=True)

best = []
try:
    for t in titles:
        states = collections.deque()
        states.append( (set([t]), [t]) )
        while states:
            seen, state = states.popleft()
            last = state[-1]
            for x in beginnings[last]: # remember beginnings is a defaultdict
                if x in seen:
                    continue
                seen2 = seen.copy()
                seen2.add(x)
                newstate = (seen2, state + [x])
                states.appendleft(newstate) # KEY!!
                if len(newstate[1]) > len(best):
                    best = newstate[1]
except KeyboardInterrupt:
    print

print '\n'.join(best)
print '--'
print len(best)
print 'elapsed secs', time.time()-starttime
