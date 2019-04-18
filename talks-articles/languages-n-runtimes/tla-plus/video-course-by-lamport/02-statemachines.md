
## State Machines in TLA+

* need a precise language to specify StateMachine, TLA+ takes path of ordinary math as it forces to think in more abstract way about the system

* TLA+ source is in ASCII

> * `^` as `/\` for mathematical symbol for `&&/and`
>
> * `V` as `\/` for mathematical symbol for `||/or`
>
> * `∈` as `\in` for element of
>
> * `=` means equality in TLA+

### Describing State Machine with Math

* to describe older example must describe

> * possible inital values of variables
>
> * relation between values in current state and possible values in next state

```
int i;
void main(){i           // pc ~ program control
  i = randomInteger();  // pc = "start"; means first assignment need to execute next
  i = i + 1;            // pc = "middle"; means second assignment to execute next
}                       // pc = "done"; means program is finished
```

* possible initial values `i = 0 /\ pc = "start"`; or with parenthesis for readability

```
(i = 0) /\ (pc = "start")
```

* relation between values of current state to next state; this is where the `if...else...` from previous example gets depicted

> let's re-depict first as var-names for current values and var-names-prime (with tick) as next values

```
if pc = "start"
  then i' in {0, 1, ..., 65535}
       pc' = "middle"
  else if pc = "middle"
         then i' = i + 1
              pc' = "done"
         else no next values
```

> to turn mathematical, need to change `set membership` segment

```
IF pc = "start"
  THEN (i' \in 0 .. 65535) /\ (pc' = "middle")
  ELSE IF pc = "middle"
         THEN (i' = i + 1) /\ (pc' = "done")
         ELSE FALSE
```

* writing a formula for `i, pc, i', pc'`; formulae in `if` and `then` shall satisfy together

> * formula equals `true` for values `i=17, pc="start", i'=500, pc'="middle"`
>
> * formula equals `true` for values `i=179, pc="middle", i'=180, pc'="done"`
>
> * formula equals `false` for values `i=179, pc="middle", i'=580, pc'="done"`

* most keywords like `FALSE` are in upper-case letters


### A nicer way to write next-state formula

* generalize first

```
IF pc = "start"         | (pc = "start") /\ A is true
  THEN A                | \/
  ELSE IF pc = "middle" | (pc = "middle") /\ B is true
         THEN B         |
         ELSE FALSE     |
```

* above would expand to something like

```
(    (pc = "start")
  /\ (i' \in 0 .. 65535)
  /\ (pc' = "middle"))
\/
(    (pc = "middle")
  /\ (i' = i + 1)
  /\ (pc' = "done"))
```

* can be written formatted better as

```
(    pc = "start"
  /\ i' \in 0 .. 65535
  /\ pc' = "middle")
\/
(    pc = "middle"
  /\ i' = i + 1
  /\ pc' = "done")
```

* widely separated parenthesis reduce readability, can be eliminated by adding suffix `/\` and-symbol and formatting like bulletted formula; it gets broken at any following token to left of `/\` symbol

```
  /\ pc = "start"
  /\ i' \in 0 .. 65535
  /\ pc' = "middle"
\/
  /\ pc = "middle"
  /\ i' = i + 1
  /\ pc' = "done"
```

* similar formatting is applicable to `\/` or-symbol implying surrouding parenthesis, making it more readable

```
\/ /\ pc = "start"
   /\ i' \in 0 .. 65535
   /\ pc' = "middle"
\/ /\ pc = "middle"
   /\ i' = i + 1
   /\ pc' = "done"
```

* since it's a formula, not a program; there are mathematical properties attributed like commutative nature of `\/` or-symbol and `/\` and-symbol

```
\/ /\ pc = "middle"
   /\ pc' = "done"
   /\ i' = i + 1
\/ /\ pc = "start"
   /\ pc' = "middle"
   /\ i' \in 0 .. 65535
```

* Initial-state formula can also be written as following, but one above takes less space

```
/\ i = 0
/\ pc = "start"
```


### TLA+ Complete Spec

* has some additional stuff; TLA+ spec appears in some module like `SimpleProgram`

* `EXTENDS` to get mathematical function like `+`

```
EXTENDS Integers
VARIABLES i, pc

Init ==  (i = 0) /\ (pc = "start")

Next ==  \/ /\ pc = "middle"
            /\ pc' = "done"
            /\ i' = i + 1
          \/ /\ pc = "start"
            /\ pc' = "middle"
            /\ i' \in 0 .. 65535
```


### Decomposing Large Specs

* next-state formula could be pretty lengthy for real-life specs

* split it into smaller parts by using `definition`

```
----------------------- MODULE SimpleSpec ------------------------------------
EXTENDS Integers
VARIABLES i, pc

Init ==  (i = 0) /\ (pc = "start")

Pick ==  /\ pc = "middle"
         /\ i' = i + 1
         /\ pc' = "done"

Add1 ==  /\ pc = "start"
         /\ i' \n 0..65535
         /\ pc' = "middle"

Next ==  Pick \/ Add1
------------------------------------------------------------------------------
\* way to place comment
\* model for a increment by 1 for any integer
```

---
