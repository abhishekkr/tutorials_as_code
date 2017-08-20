#!/usr/bin/env python3
##  netcat -lv -s 127.0.0.1 -p 8080

import socket, time

def connect():
    s = socket.socket()
    s.connect(('localhost', 8080))
    return s

def publish_with_delay(txt):
    conn.send(b"%s" % txt)
    time.sleep(0.5)

def line_demo(conn):
    publish_with_delay("this is a text")
    publish_with_delay("\n")


def typewriter_demo(conn):
    publish_with_delay("this is a text again")
    publish_with_delay("\b")
    publish_with_delay("\b\b\b\b")
    publish_with_delay("changed")
    publish_with_delay("\r")
    publish_with_delay("this is a line changed")
    publish_with_delay("\n")

def progress_bar(conn):
    publish_with_delay("[#    ]")
    publish_with_delay("\r[##   ]")
    publish_with_delay("\r[###  ]")
    publish_with_delay("\r[#### ]")
    publish_with_delay("\r[#####]")

def change_char():
    publish_with_delay("\xc3\xb1")
    [ publish_with_delay(chr(x)) for x in range(32) ] ## \a\n\r\v\b\f\t
    publish_with_delay("\x0e")
    publish_with_delay("changed")
    publish_with_delay("\x0f")
    publish_with_delay("changed")
    publish_with_delay("\x1b[33m")
    publish_with_delay("changed")
    publish_with_delay("\x1b[0m")
    publish_with_delay("changed")
    publish_with_delay("\n")

if __name__ == "__main__":
    conn = connect()
    line_demo(conn)
    typewriter_demo(conn)
    progress_bar(conn)
    change_char()
