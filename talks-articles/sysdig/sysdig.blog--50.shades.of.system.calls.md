## Spectogram chisel to profile in csysdig

[source](https://sysdig.com/blog/50-shades-of-system-calls/)

Sysdig has a chisel `spectogram`
> * it captures every system call (eg open, close, read, write, socket...) and measure call latency
> * every 2 sec organizes call into bukcets from 1nano-sec to 10sec
> * uses color to indicate how many calls per bucket
> > black for no calls during last sample
> > green shades means 0-100 calls
> > yellow shades means 101-999 calls
> > red shade means 1000+ calls

The left side tends to show stuff that is fast and right for slow.

Csysdig (curses UI) also has it now.

Can be easily applied to arbitrary selections clicking on F12 (for full spectogram).
SHIFT+F12 for spectogram including file I/O activity only.
Similarly for a process, directory, file, container.
For a pod or a service [with Kubernete integration](https://sysdig.com/digging-into-kubernetes-with-sysdig/).

Can use mouse to drag over areas to focus on events in particular zone.
Isolating events that belong to latency range (width) in a given time interval (height).

Can profile similar activities with different modus operandi or just lib/sys calls.

---
---
