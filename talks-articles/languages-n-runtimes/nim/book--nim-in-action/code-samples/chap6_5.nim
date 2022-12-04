## Deal with Race Condition

import threadpool, locks
import os

## Locks
var counterLock: Lock
initLock(counterLock)

var counter {.guard: counterLock.} = 0

proc incr(x: int) =
  for i in 0..<x:
    withLock counterLock:
      counter.inc

spawn incr(10_000)
spawn incr(10_000)
sync()
echo(counter)


## Channels
var ch: Channel[string]
open(ch)

proc delayedSend(m: string, sec: int) =
  sleep(sec)
  ch.send(m)

spawn delayedSend("check check", 1000)
assert ch.recv() == "check check"            ## Blocking

spawn delayedSend("#1", 10)
spawn delayedSend("#2", 0)
sleep(5) ## without this '#2' skips this loop and gets concated in second loop
while ch.peek > 0:
  echo("> ", ch.recv())

proc nSend(m: string) =
  ch.send(m)

spawn nSend("#1")
spawn nSend("#2")
var s: string
for i in 0..<ch.peek:
  s = s & ch.recv()
echo(">> ", s)


echo "PASSED!"
