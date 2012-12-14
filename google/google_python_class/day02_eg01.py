#!/usr/bin/env python -tt

import re

def re_story1():
  match = re.search('pattern', 'some text with patternized text')
  print "[+]", match
  print "[+]", match.group()
  def Find(pat, text):
    match = re.search(pat, text)
    if match: print "[+]", match.group()
    else: print "[+]", 'not found'
  Find('ir', 'Piiig')
  Find('ii', 'Piiig')
  Find('...i', 'Piiig')

def re_story2():
  str = "yay,\n that was more of 1.2.3@F.ew RE examples@tutorial"
  print "[+]", re.findall(r'[\w.-_]+@[\w.-_]+', str)
  print "[+]", re.findall(r'([\w.-_]+)@([\w.-_]+)', str)
  print "[+]", re.findall(r'.*', str)
  print "[+]", re.findall(r'.*', str, re.DOTALL)
  print "[+]", re.findall(r'.*', str, re.M)
  print "[+]", re.findall(r'.*1', str, re.I + re.DOTALL)


def main():
  print 'Regular Exp'
  print "\nRegExp Story 1:"
  re_story1()
  print "\nRegExp Story 2:"
  re_story2()

if __name__ == '__main__':
  main()
