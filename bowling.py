# Sat March 22, 2008 (9am)
# bowling.py -- calculates scores, doesn't enforce rules

# create new instance for each player.
# Public API: knocked_down(npins)
# just invoke that -- will automatically deduce
# which frame you are in.
class BowlingGame(object):
    def __init__(self):
        self.f = [[None, None] for x in range(12)]
        self.cf = 0 # current frame

    def knocked_down(self, npins):
        f = self.f[self.cf]
        if f[0] is None:
            f[0] = npins
        else:
            f[1] = npins
        if npins == 10 or f[1] is not None:
            self.cf += 1

    def get_score(self):
        total = 0
        complete = True
        for i, f in enumerate(self.f):
            if i > 9:
                break
            elif f[0] is None: # hasn't bowled this frame
                break
            elif f[0] == 10: # strike
                if self.f[i + 1][0] is None and \
                   (self.f[i + 1][1] is None or self.f[i + 2][0] is None):
                    complete = False
                    break
                # strike -- next 2 bowls (both of which can be in the same frame!)
                total += 10 + self.f[i + 1][0]
                if self.f[i + 1][1] is None:
                    total += self.f[i + 2][0]
                else:
                    total += self.f[i + 1][1]
            elif f[1] is None: # hasn't bowled 2nd bowl
                complete = False
                break
            elif f[0] + f[1] == 10: # spare
                if self.f[i + 1][0] is None:
                    complete = False
                    break
                total += 10 + self.f[i + 1][0]
            else:
                total += f[0] + f[1]            
        return total, complete


#
# Tests
#
def reporterror(actual, expected):
    if actual != expected:
        print 'actual: %s\nexpected: %s' % (actual, expected)
        raise Exception

def test1():
    game = BowlingGame()
    game.knocked_down(7)
    game.knocked_down(2)
    reporterror(game.get_score()[0], 9)

def test2():
    game = BowlingGame()
    for i in range(12):
        game.knocked_down(10)
    reporterror(game.get_score()[0], 300)

def test3():
    import random
    for i in range(1000):
        bowls = [random.randint(0, 4) for i in range(20)]
        game = BowlingGame()
        for b in bowls:
            game.knocked_down(b)
        reporterror(game.get_score(), (sum(bowls), True))

# wikipedia data:
# http://en.wikipedia.org/wiki/Ten-pin_bowling#Rules_of_play
def test4():
    game = BowlingGame()
    map(game.knocked_down, [10, 10, 4, 2])
    reporterror(game.get_score(), (46, True))
def test5():
    game = BowlingGame()
    map(game.knocked_down, [7, 3, 4, 2])
    reporterror(game.get_score(), (20, True))
def test6():
    game = BowlingGame()
    map(game.knocked_down, [10, 3, 6])
    reporterror(game.get_score(), (28, True))

def test7():
    game = BowlingGame()
    for i in range(10):
        game.knocked_down(10)
    game.knocked_down(7)
    game.knocked_down(1)
    reporterror(game.get_score(), (285, True))

def t():
    for i in range(100):
        try:
            name = "test%d" % (i+1)
            fn = globals()[name]
            print "testing", name
        except KeyError:
            break
        else:
            fn()

if __name__ == "__main__":
    t()
