# -*- coding: utf-8 -*-
# prompting and passing

from sys import argv
from os import environ

script, user_name = argv
prompt = '%s> ' % environ['USER']

print "Hey, so you just ran %s like that..." % script
print "Now I think your name is '%s'." % user_name
print "What's your Nickname? "
nick = raw_input(prompt)

print "Where is your home %s?" % nick
home = raw_input(prompt)

print "Where is %s? No worries... give me your ph# instead." % home
phone = raw_input(prompt)

print """
So you mean to say, people call you %s
and you live at %s.
And if I need to reach out, I shall call at %s
""" % (nick, home, phone)
