# -*- coding: utf-8 -*-
# printing, printing.....

formatter = "%r %r %r %r"

print formatter % (1, 2, 3, 4)
print formatter % ("one", "two", "three", "four")
print formatter % (True, True, False, False)
print formatter % (formatter, formatter, formatter, formatter)
print formatter % (
    '123',
    'abc',
    456,
    __name__)
