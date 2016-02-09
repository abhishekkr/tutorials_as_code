#!/usr/bin/env python

""" it attaches itself to any exception occuring,
if pdb can start there it will """

import sys


def info(type, value, tb):
    if hasattr(sys, 'ps1') or not sys.stderr.isatty():
         # call default hook we no tty available
         sys.__excepthook__(type, value, tb)
    else:
         import traceback, pdb
         traceback.print_exception(type, value, tb)
         # post-mortem mode pdb
         pdb.pm()


sys.excepthook = info
