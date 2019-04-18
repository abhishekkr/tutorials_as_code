
## TLA+ Resources and Tools

* [TLA+ Homepage](https://lamport.azurewebsites.net/tla/tla.html)

* [Summary of TLA+ Cheatsheet](https://lamport.azurewebsites.net/tla/summary-standalone.pdf)

* [TLA+ Book](https://lamport.azurewebsites.net/tla/book.html?back-link=learning.html#book), written before toolbox was implemented

* [TLA+ Google Group](https://groups.google.com/forum/?fromgroups#!forum/tlaplus)

* [TLA+ Toolbox, the IDE](https://lamport.azurewebsites.net/tla/toolbox.html)

* [more stuff](https://lamport.azurewebsites.net/tla/more-stuff.html)


### Using the Toolbox

* to add a new spec `'File' > 'Open Spec' > 'Add New Spec'`, add path with filename `any.tla`

* empty spec looks like

```
------------------------------------ MODULE SimpleSpec ------------------------
EXTENDS Integers
VARIABLES i, pc

Init == (pc = "start") /\ (i = 0)

Pick == /\ pc = "start"
        /\ i' \in 0 .. 65535
        /\ pc' = "middle"

Add1 == /\ pc = "middle"
        /\ i' = i + 1
        /\ pc' = "done"

Next == Pick \/ Add1
-------------------------------------------------------------------------------
/* finished like above, comments being comments ingored
```

* `Spec Status` block at bottom of IDE shall be green for no parsing errors

* `'File' > 'Produce PDF Version'` gets pretty-print version with mathematical notations on TLA+ symbols

* File tabs can be re-arranged in various ways

* Unicode option allows to display mathematical symbols instead of TLA+ symbols for them


### Running TLC

* to check if model checker is working, use menu `TLC Model Checker`

* click sub-menu `New Model`, give some name like `ModelBaseline`

* in `Model Overview` tab, provide value for defined `Init` and `Next` state and the play like button to `Run TLC on the Model`

* in case of any errors, it will show such on `Model Checking Results` once the TLC has finished running on model

* for our model on above SimpleSpec, one error will get reported with detail `Deadlock Reached` for Error Trace as

```
<Initial Predicate> (State num=1)
  i   = 0
  pc  = "start"

<Add1 line 12, col 10 to line 14, col 26 of module SimpleSpec> (State num=2)
  i   = 0
  pc  = "middle"

<Add1 line 8, col 10 to line 10, col 24 of module SimpleSpec> (State num=3)
  i   = 1
  pc  = "done"
```

* Since most systems are not supposed to stop, and not explicitly mentioned in model. Default for TLC on stop is error.

* On `Model Overview` page, un-check `deadlock` box and re-run model to pass.


### TLA+ Proof System (TLAPS)

* TLA+ has constructs to write algorithms, theorems and formal proofs for those

* TLPAS can check those proofs, can be run from toolbox


---
