#!/usr/bin/env python -tt

# OS and Utils

# import shutil  ##see this
import os
import commands
import sys

def os_story1():
  pwd = os.path.abspath('./')
  print pwd, "exists: ", os.path.exists( pwd )
  print "/no exists: ", os.path.exists( '/no' )

  def ls(dir):
    fylz = os.listdir(dir)
    non_dot = []
    for name in fylz:
      path = os.path.join(dir, name)
      print ">>>", path
      print ">>>>>", os.path.abspath(path)
  ls('./')

def os_story2():
  (status, output) = commands.getstatusoutput('ls ./')
  print "command exit with", status, "and output", output.replace("\n", "\n >>")

def main():
  print '\nOS Story1:'
  os_story1()
  print '\nOS Story2:'
  os_story2()
  sys.stderr.write('OK Error is here')
  #sys.exit(1)


if __name__ == '__main__':
  main()
