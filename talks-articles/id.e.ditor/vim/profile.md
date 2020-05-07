
## Vim Profiling

### When a `Vim plugin/config` is dragging down *performance* but don't know which

* start Vim, in Normal mode type below commands to enable profiling and push logs to `/tmp/vim-profile.log`

```
:profile start /tmp/vim-profile.log
:profile func *
:profile file *
```

* then do the activity which generally slows it down


* now go back to Normal mode, stop the profiling using `:profile pause` and quit vim (*hope you remember how to quit*)

* then `cat /tmp/vim-profile.log`, at end it will have `FUNCTIONS SORTED ON TOTAL TIME` where you can see list of operations consuming maximum time in order

* can also run following command to get the `top 10` culprit logs

```
cat /tmp/vim-profile.log | grep -A11 'FUNCTIONS SORTED ON TOTAL TIME'
```

* example my `Top 3` helped me figure out disabling `easytags`, which helped fix significantly slowed down save time

```
$ cat /tmp/vim-profile.log | grep -A4 'FUNCTIONS SORTED ON TOTAL TIME'
FUNCTIONS SORTED ON TOTAL TIME
count  total (s)   self (s)  function
    5  13.805628   0.006162  xolox#easytags#autoload()
    1  13.797875   0.001205  xolox#easytags#update()
    1  13.791280   0.007060  xolox#easytags#update#with_vim()
```

---
