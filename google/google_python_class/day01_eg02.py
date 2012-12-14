#!/usr/bin/env python -tt

# LIST, SEQUENCE

def list_story1(lst):
  lst += [2, 3]
  print len(lst), lst
  lst2 = lst
  lst[2] = 'b,c'
  print lst, lst2
  lst2[2] = 'c,b'
  print lst, lst2
  lst3 = lst[2:4]
  lst[3] = 'd'
  print lst, lst3
  lst4 = lst[0:]
  lst[3] = 2
  print lst, lst4

def list_story2(lst):
  print lst
  for elem in lst: print elem
  print 1 in lst
  print 'A' in lst
  print lst.append('5')
  print lst
  print lst.pop(0)
  print lst
  print lst.pop(2)
  print lst
  del lst[2]
  print lst
  del lst
  try: print lst
  except: print 'No More lst in this def, but still in main'

def list_story3(lst):
  def s(str):
    return len(str)
  print sorted(lst)
  print sorted(lst, key = len)
  print sorted(lst, key = s)
  print ';'.join(lst)
  print 'A,B,C'.split(',')

def list_story4(lst):
  print lst
  print range(len(lst)), len(lst)

def tuple_story1(tpl):
  print tpl, len(tpl)
  try: tpl[0] = '11'
  except: print 'Tuples are immutable unlike lists.'
  tplst = [(1,'c'), (3, 'd'), (3, 'a'), (2, 'd')]
  print sorted(tplst)
  (x, y) = (1, 2)
  print x, y

def main():
  lst = [1, 'a', ['b', 'c']]
  print 'List Story1:'
  list_story1(lst)
  print "\n\n\nList Story2:"
  list_story2(lst)
  print "\n\n\nList Story3:"
  list_story3(lst)
  print "\n\n\nList Story4:"
  list_story4(lst)

  tpl = (1, 'a', "Alpha")
  print "\n\n\nTUPLE Story4:"
  tuple_story1(tpl)

if __name__ == '__main__':
  main()
