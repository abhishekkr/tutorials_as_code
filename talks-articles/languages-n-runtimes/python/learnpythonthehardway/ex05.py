# -*- coding: utf-8 -*-
# more variables and printing
# http://docs.python.org/2/library/stdtypes.html#string-formatting-operations

alpha = 'abcde'
digit01 = 100
digit02 = 250

def abc():
    pass

print "alphabets to test are %s" % alpha
print "digits to test are %d and %d" % (digit01, digit02)
print "Base10(%d) == Base8(%o) == Base16(%x) == BASE16(%X) == (%f)" % (
    45, 45, 45, 45, 45)
print "repr(abc) %s == %r" % (repr(abc), abc)
print "str(abc) %s == %s" % (str(abc), abc)
