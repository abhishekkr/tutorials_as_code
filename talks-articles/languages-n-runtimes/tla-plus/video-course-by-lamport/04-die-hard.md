
## Die Hard Problem

* Problem Statement

> Put exactly 4 gallons of water in a jug to the sound of bomb.
>
> Have a 3 gallon jug, a 5 gallon jug and a water faucet. No markings on jugs.
>
> [Die Hard Jugs Problem also detailed out here](https://mindyourdecisions.com/blog/2013/02/04/the-water-jug-riddle/)

* Start with spec by writing a single correct behavior first. Informally.

* Let variable `small` & `big` represent gallons of water in 3 and 5 gallon capacity jugs respectively. Initially zero.

```
(small = 0) /\ (big = 0)
```

* Try a sample behavior, not necessarily correct to ideate variables and what constructs a step

```
(small=0 /\ big=0) /* when empty
(small=3 /\ big=0) /* fill small
(small=0 /\ big=3) /* pour small in big
(small=3 /\ big=3) /* fill small
(small=1 /\ big=5) /* pour small in big
```

> steps are `filling a jug`, `pour from X jug to Y jug`, `empty a jug` on abstract level

* Next-state formular describes all permitted steps written as `F1 \/ F2 \/  .. \/ Fn` where each `Fi` allows different kind of step

* Filling and emptying Jugs are simple equalities, though pouring from one to other is something like

```
SmallToBig == IF big + small <= 5
              THEN /\ big' = big + small
                   /\ small' = 0
              ELSE /\ big' = 5
                   /\ small' = small - (5 - big)
BigToSmall == IF big + small <= 3
              THEN /\ big' = 0
                   /\ small' = big
              ELSE /\ big' = small - (3 - big)
                   /\ small' = 3
```


### The Specification

* no type declaration used in TLA+, as that means all variables have sensible values

* rather define a formula to assert type correctness, helps understand spec and TLC can type check spec by checking ever correctness of formula, like `TypeOK`

* steps must be defined before usage, hence `FillSmall` and other need precede `Next`

* running TLC model on Spec below just checks that Spec work okay, not the soluution

* to check type-correctness using `TypeOK`, go to `Model Overview` tab and expand `Invariants`, add `TypeOK` and run model again

> a formula true in every reachable stae is called an `invariant`


```
--------------------------- MODULE DieHard -----------------------------------
EXTENDS Integers

VARIABLES small, big

TypeOK == /\ small \in 0 .. 3
          /\ big   \in 0 .. 5



Init == /\ small = 0
        /\ big = 0

FillSmall == /\ small' = 3
             /\ big'   = big

FillBig == /\ small' = small
           /\ big' = 5

EmptySmall == /\ small' = 0
              /\ big' = big

EmptyBig == /\ small' = small
            /\ big' = 0

SmallToBig == IF big + small <= 5
              THEN /\ big' = big + small
                   /\ small' = 0
              ELSE /\ big' = 5
                   /\ small' = small - (5 - big)
BigToSmall == IF big + small <= 3
              THEN /\ big' = 0
                   /\ small' = big
              ELSE /\ big' = small - (3 - big)
                   /\ small' = 3

Next == \/ FillSmall
        \/ FillBig
        \/ EmptySmall
        \/ EmptyBig
        \/ SmallToBig
        \/ BigToSmall

------------------------------------------------------------------------------
/* TypeOK is not part of spec, removing it doesn't change anything but checking for value correctness of variables
```


### To Solve the Problem

* we let TLC check if `big != 4` is an invariant, if it isn't then TLC shows behavior ending in state with `big != 4` as false

* TLA+ uses `/=` or `#` for `!=` not-equals

* let's add another invariant in `Model Overview` as `big \= 4` and re-run model on spec

* new run will return an error with behavior ending in state `big=4 /\ small=3`, where invariant is false

```
Invariant big /= 4 is violated.

<Initial Predicate> State(num=1)
  big=0 /\ small = 0
<FillSmall> State(num=2)
  big=0 /\ small = 3
<SmallToBig> State(num=3)
  big=3 /\ small = 0
<FillSmall> State(num=4)
  big=3 /\ small = 3
<SmallToBig> State(num=5)
  big=5 /\ small = 1
<EmptyBig> State(num=6)
  big=0 /\ small = 1
<SmallToBig> State(num=7)
  big=1 /\ small = 0
<FillSmall> State(num=8)
  big=1 /\ small = 3
<SmallToBig> State(num=9)
  big=4 /\ small = 0
```

> have the solution above

---
