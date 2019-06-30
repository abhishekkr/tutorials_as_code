##/usr/bin/env python3

import sys


def translator(echo_divider, count_upto):
    for i in range(count_upto):
        msg = ""
        for echo in echo_divider:
            if i % echo_divider[echo] == 0:
                msg += echo
        if msg == "":
            msg = i
        print("%s " % (msg), end = '')


def fizzbuzz():
    echo_divider = {"fizz": 3, "buzz": 5}
    translator(echo_divider, 100)


def fizzbuzzfuzzbizzfazz():
    echo_divider = {"fizz": 3, "buzz": 5, "fuzz": 7, "bizz": 9, "fazz": 15}
    translator(echo_divider, 100)


if __name__ == "__main__":
    if len(sys.argv) > 1:
        fizzbuzzfuzzbizzfazz()
    else:
        fizzbuzz()
