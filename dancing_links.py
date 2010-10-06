# Knuth's Dancing Links paper:
# http://www-cs-faculty.stanford.edu/~uno/papers/dancing-color.ps.gz
#
#
# http://en.wikipedia.org/wiki/Algorithm_X
#
# "...is a recursive, nondeterministic, depth-first,
#  brute-force algorithm that finds all solutions to
#  the exact cover problem represented by a matrix A
#  consisting of 0s and 1s..."
#
# $Id: dancing_links.py,v 1.2 2006/12/06 03:43:17 sri Exp $

import random

# What is the matrix? It is a helper class
# for exact cover below.
class Matrix:
    def __init__(self, list_of_lists):
        self.m = [list_elt[:] for list_elt in list_of_lists]
        self.unique = object()

    def copy(self):
        new = Matrix(self.m)
        for r in new.m:
            for i in range(len(r)):
                if r[i] == self.unique:
                    r[i] = new.unique
        return new
    
    def delcol(self, i):
        for r in self.m:
            r[i] = self.unique

    def delrow(self, i):
        row = self.m[i]
        for j in range(len(row)):
            row[j] = self.unique

    def empty(self):
        for r in self.m:
            for x in r:
                if x != self.unique:
                    return False
        return True

    # Select first column with lowest number of 1s.
    # Also, returns the number of 1s found in the
    # column.
    def selcol(self):
        i = -1
        s = -1
        for colidx in range(len(self.m[0])):
            colitems = [r[colidx] for r in self.m]
            if all(x==self.unique for x in colitems):
                continue
            t = colitems.count(1)
            if s == -1 or t < s:
                s = t
                i = colidx
        return i, s

    def p(self):
        t = []
        for r in self.m:
            t2 = []
            for c in r:
                if c == self.unique:
                    c = "?"
                t2.append(c)
            t.append(t2)
        return t

# == Algorithm X  ======================================================
                
class ExactCover:
    def X(self, matrix):
        if matrix.empty():
            return []

        result = self.__X(matrix.copy())
        if result:
            all_answers = []
            for answer in result:
                all_answers.append([matrix.m[rowidx]
                                    for rowidx in answer])
            return all_answers
        else:
            return []

    def __X(self, matrix):
        # 1. Select 1st column with lowest
        #    number of 0s.
        c, s = matrix.selcol()
        if s == 0:
            return []

        # 2. Pick rows that have 1s in the
        #    above column.
        possible_rows = [(i, r) for (i, r) in enumerate(matrix.m) if r[c] == 1]
        random.shuffle(possible_rows)
        
        answers = []
        # 3. For each such row:
        for rowidx, r in possible_rows:
            new = matrix.copy()

            for i, item in enumerate(r):
                # - pick columns that have 1s
                #   in them
                # 4. For each such col:
                if item == 1:
                    for j, item2 in enumerate([r[i] for r in new.m]):
                        # - delete rows that have a 1
                        #   in that column
                        if item2 == 1:
                            new.delrow(j)
                    # - delete that column
                    #   [this was a bug before;
                    #    it used to be above the
                    #    for loop right after the
                    #    "item == 1" expr]
                    new.delcol(i)

            if new.empty():
                answers.append([rowidx])
            else:
                # - recur on the sub-matrix
                result = self.__X(new)
                for x in result:
                    x.append(rowidx)
                    answers.append(x)

        return answers

        
# == Tests ==========================================

def test_exact_cover():
     e =  [[1, 0, 0, 1, 0, 0, 1],
           [1, 0, 0, 1, 0, 0, 0],
           [0, 0, 0, 1, 1, 0, 1],
           [0, 0, 1, 0, 1, 1, 0],
           [0, 1, 1, 0, 0, 1, 1],
           [0, 1, 0, 0, 0, 0, 1]]
     m = Matrix(e)
     res = ExactCover().X(m)
     if res == [[[0, 1, 0, 0, 0, 0, 1],
                 [0, 0, 1, 0, 1, 1, 0],
                 [1, 0, 0, 1, 0, 0, 0]]]:
         print "test pass"
     else:
         print "test fail"
