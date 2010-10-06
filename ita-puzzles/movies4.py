pre = {}
suf = {}
titles = [x.strip() for x in open('MOVIES.LST.txt').readlines()]

class Word(object):
    __slots__ = ('p', 's', 'w')
    def __init__(self, w):
        self.w = w
        ws = w.split()
        self.p = [ ws[:-i+1] for i in xrange(len(ws)) ]
        self.s = [ ws[i:] for i in xrange(len(ws)) ]
        print w, self.p, self.s
        
Word("89 seasons in the rain")
