## PDB Mode
* Script Mode
* Postmortem Mode
* Run Mode
* Trace Mode

#### Script Mode

    python -m pdb buggy.py

* Enhanced Script Mode

    python -m pdb -c continue buggy.py
    python -m pdb -c 'until 2' buggy.py

#### Postmortem Mode

    pdb.pm() # used after uncaught exception raised


#### Run Mode

    pdb.run('some.expression()') # uses current, locals, globals to interpret



#### Trace Mode

    pdb.set_trace() # stick in dev-code, conditionalize its execution



#### Commands

    ?<Enter> OR h<Enter> : help list of commands

    ? <Cmd><Enter> OR h <Cmd><Enter> : help on command

    l<Enter> : list code for current context file (default 11 lines), can give start line to end line

    w<Enter> : stacktrace, arrow is at current frame; [alias: bt]

    p<Enter> : display values; [pp: pretty print]

    args<Enter> : list values of current context args

    s<Enter> : step forward into next execution block
    n<Enter> : next steps over functions (not into like step)
    c<Enter> : continue execution until finds a break point
    unt<Enter> : until line# >= current-line (provided line#), or return
    r<Enter> : continue until current function executes and return

    u<Enter> : step up a stack frame
    d<Enter> : step down a stack frame

    b<Enter> : breaks execution at a line#
    you can enable/disable/clear a break-point; tbreak is one-time only

    commands [bpnum]<Enter> : Specify a list of commands for breakpoint number bpnum
    condition bpnum [_condition_]<Enter> : set new condition for breakpoint
    debug code<Enter> : enter recursive debugger to step through code
    jump linenum<Enter> : set next line# to be executed



#### More Links

[pdb track](http://wiki.zope.org/klm/PDBTrack)
[winpdb](http://winpdb.org/)
