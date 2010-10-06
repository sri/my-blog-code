titles = ['hello world', 'random', 'another random',
  'world of mine', 'mine is life', 'jackhammer', 'rodain',
  'word end'
]
def ends(x, y): # does y end w/ x
    return y.split()[-1] == x.split()[0]
count = 0
results = []
for t1 in titles:
    res = [t1]
    got1 = 1
    while got1:
        count += 1
        if count % 100000 == 0: print '.'
        got1 = 0
        for t2 in titles:
            if t2 in res: continue
            if ends(t2, res[-1]):
                new = ' '.join( t2.split()[1:] )
                if new: 
                    res.append(new)
                    got1 = 1
                break
        if not got1: break
    if len(res) > 1:
        results.append(res)
results.sort(key=len, reverse=1)
for x in results:
    print sum(len(y) for y in x), ' '.join(x)
