# non-recursive flatten
# requires Python 2.5 or above
def flatten(lists):
    from collections import deque
    items = deque(lists)
    while items:
        top = items.popleft()
        if isinstance(top, list):
            items.extendleft(reversed(top))
        else:
            yield top

def flatten_r(list_of_lists):
    if list_of_lists == []:
        return []
    elif isinstance(list_of_lists, list):
        result = []
        for x in list_of_lists:
            for y in flatten_r(x):
                result.append(y)
        return result
    else:
        return [list_of_lists]


# from skencil
from types import ListType
def flatten2(list):
    result = []
    for item in list:
        if type(item) == ListType:
            result = result + flatten2(item)
        else:
            result.append(item)
    return result

def xlen(x):
    if isinstance(x, list):
        return len(x)
    else:
        count = 0
        try:
            while True:
                x.next()
                count += 1
        except StopIteration:
            return count


#
# Test data:
#
t1 = [1, [2, 3, [4, 5, [[[6], 7], [[8, 9]]]]]]
t2 = [t1, [[t1], [[[t1]], t1, [[[[[t1, 'n']]], t1, t1, t1]], 'a']]]
t3 = [t2, [[t2, [t2, [[t2, [[t2, t2, t2,
                             [[[t2, [[t2, t2, t2, [[[[t2]]]]]]]]]]]]]]]]]
t4 = [[[t3, t3, [[t3, [[[t3, [[t3, [[[t3, [[[t3, t3]]]]]]]]]]]]]]]]
t5 = ['first', [[[t4, [[[t4, [[[[t4, [[[t4, 'mid']]], t4]]], t4]], t4, ]], t4, [[t4]]]]], 'last']


def rectest():
    global t5
    t5 = []
    for i in range(100000):
        t5 = [t5, i]

if __name__ == "__main__":
    def indent(s):
        lines = s.split("\n")
        spaces = 4 * ' '
        return "\n".join(spaces+line for line in lines)
    
    import commands

    sep = '=='*20

    # Sanity check:
    a1 = list(flatten(t5))
    a2 = flatten_r(t5)
    a3 = flatten2(t5)
    assert a1[0]=='first' and a1[-1]=='last' and a1==a2 and a2==a3
    
    cmd = ("python -mtimeit -v -n 1 -r 2 -s 'import flatten as F' '%sF.%s(F.t5)'")
    for funcname in ('flatten_r', 'flatten2', 'flatten'):
        print funcname
        out = commands.getoutput(cmd % ('', funcname))
        print indent(out)
        print sep

    print sep
    print 'recursion test'
    for funcname in ('flatten_r', 'flatten2', 'flatten'):
        print funcname
        status, out = commands.getstatusoutput(cmd % ('F.rectest(); ', funcname))
        if status:
            # recursion error (?)
            out = out.split("\n")[-1]
        print indent(out)
        print sep


"""
python -mtimeit -v -n 1 -r 2 -s 'import flatten as F'
'print F.xlen(F.flatten(F.t5))'


Output from a single run:
========================================

$ python2.5 flatten.py
flatten_r
    raw times: 3.61 3.1
    1 loops, best of 2: 3.1 sec per loop
========================================
flatten2
    raw times: 0.58 0.545
    1 loops, best of 2: 545 msec per loop
========================================
flatten
    raw times: 1.81e-05 4.05e-06
    1 loops, best of 2: 4.05 usec per loop
========================================
========================================
recursion test
flatten_r
    RuntimeError: maximum recursion depth exceeded in cmp
========================================
flatten2
    RuntimeError: maximum recursion depth exceeded in cmp
========================================
flatten
    raw times: 0.283 0.3
    1 loops, best of 2: 283 msec per loop
========================================
"""
