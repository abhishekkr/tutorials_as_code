#!/usr/bin/env python3

import time, sys, termios


def report_progress(ratio, width=50):
    filled = '=' * int(ratio * width)
    rest = '-' * (width - int(ratio*width))
    sys.stderr.write('\r['+filled+rest+']')
    sys.stderr.flush()


def demo_progress():
    for i in range(101):
        report_progress(i/100.0, 50)
        time.sleep(.01)
    sys.stderr.write('\n')
    sys.stderr.flush()


def no_echo(prompt="hidden-typing: "):
    fd = sys.stdin.fileno()
    old = termios.tcgetattr(fd)
    new = termios.tcgetattr(fd)
    new[3] = new[3] & ~termios.ECHO
    try:
        termios.tcsetattr(fd, termios.TCSADRAIN, new)
        passwd = raw_input(prompt)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old)
    print("saw nothing you typed, right")


if __name__ == "__main__":
    demo_progress()
    no_echo()
