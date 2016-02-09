#!/usr/bin/env python
#-*- coding: utf-8 -*-
"""
Usage: racks <numbers>...

Examples:
    racks 1 25 221 57 32 68 7

Options:
    -h, --help      Show this screen
    -v, --version   Show Version
"""

from docopt import docopt
from termcolor import cprint

#steps = u'ᚁᚂᚃᚄᚅ'
steps = [u'ᚁ', u'ᚂ', u'ᚃ', u'ᚄ', u'ᚅ']


def normalize_numbers(numbers):
    min_numbers = min(numbers)
    step_range = max(numbers) - min_numbers
    step = (step_range / float(4)) or 1
    rack = u'.'.join(steps[int( round((n - min(numbers)) / step) )] for n in numbers) 
    cprint(rack, "green")


def start(version):
    arguments = docopt(__doc__, version=version)
    numbers = arguments.get('<numbers>', None)
    if numbers:
        try:
            numbers = map(int, numbers)
        except:
            cprint("Racks only accepts integers.", "red")

        try:
            normalize_numbers(numbers)
        except:
            cprint("Normalization failed.", "red")


if __name__ == '__main__':
    #version = ".".join(str(x) for x in __version __)
    version = "0.0.1"
    start(version)

