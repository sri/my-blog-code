# sri, Sun Apr 13 15:05:56 PDT 2008
#
# A simple way to find palindromic pangrams.
# Completes in under 10 secs for the word list given
# by ITA software.
#
# The Algorithm: I find small palindromes (1-word & 2-word
# palindromes) and hook them up together. Example:
# if you have 1-word palindromes: "aa", "bb", & "cc", then
# aabbccccbbaa is a palindrome that contains all the characters
# a, b & c. It, by no means, produces an efficient
# palindrome. It also works for the dict file under OS X.
# So I really didn't need to find out 3-word palindroms,
# which seems much harder....

import string, sys

left = set(string.ascii_lowercase)
words = [x[:-1] for x in open(sys.argv[1]).readlines()]
        # use below for certain dicts that have many 1-letter words
        # if len(x) > 2]
         
wordset = set(words)
result = []
prefix = {}
suffix = {}

for word in words:
    for i in xrange(len(word)):
        pre = word[:i+1]
        if pre not in prefix:
            prefix[pre] = []
        prefix[pre].append(word)
        suf = word[-(i+1):]
        if suf not in suffix:
            suffix[suf] = []
        suffix[suf].append(word)

def ispal(s):
    i = 0
    j = len(s)-1
    while i < j:
        if s[i] != s[j]:
            return False
        i += 1
        j -= 1
    return True

def words_containing(words, chars):
    for word in words:
        if any(c in word for c in chars):
            yield word

def markpal(*words):
    result.append(' '.join(words))
    for c in result[-1]:
        if c in left: left.remove(c)

# THIS IS BROKEN:
def smallest_pal(items):
    def score(x):
        x = x.replace(' ', '')
        uniques = len(set(x))
        return int(100 * uniques / len(x))
    items = sorted(items, key=score, reverse=True)
    left = set(string.ascii_lowercase)
    result = []
    for item in items:
        if not any(c in item for c in left):
            continue
        result.append(item)
        for c in item:
            if c in left: left.remove(c)
            
    print 'shortest pal:', 2 * len(''.join(result).replace(' ', ''))
    print '|'.join(result)
    print '|'.join(reversed(result))


# we don't care abt making the palindrome small
# we'll try do that later
for word in words:
    if ispal(word): markpal(word)
print 'left over from 1-word pals:', left

# figuring out 2-word palindromes is very simple;
# try to "mirror" the characters on both sides
# and 1-st test if its forms a palindrome and
# then check to see if its in the word list
for word in words_containing(words, left):
    for i in xrange(len(word)):
        pre = ''.join(reversed(word[-(i+1):]))
        #if ispal(pre+word) and prefix.has_key(pre): BUG!
        if ispal(pre+word) and pre in wordset:
            markpal(pre, word)
        suf = ''.join(reversed(word[:i+1]))
        #if ispal(word+suf) and suffix.has_key(suf): BUG!
        if ispal(word+suf) and suf in wordset:
            markpal(word, suf)
if not left:
    # print things out nicely so i can quickly check result visually
    print 'found palindromic pangram:', 2*sum(len(x) for x in result)
    output = []
    for c in string.ascii_lowercase:
        for word in result:
            if c in word:
                output.append((c, word))
                break
    for (c, w) in output:
        print "[%s] %s" % (c, w)
    #smallest_pal(result)
else:
    print 'no solution found'
