
## Introduction to TLA+

* a language for high-level modeling `critical parts` of `digital systems`

* provides tool to check these models
> * most important, TLC model checker
> * another is TLAPS, the TLA+ Proof System
> * TLA+ toolbox, an IDE

* `digital systems` include `algorithms`, `programs`, `computer sytems`; checked at design level above code level

* `critical parts` by abstracting away less-ciritical parts and low-level implementation details

* created to model concurrent and distributed systems, can help to find and correct design errors which are harder to find by testing and correct before any coding

* only by abstraction can complex systems be understood, TLA+ helps understand abstraction better in solutions to be implemented

* TLA+ can specify algorithms and high-level designs, but not generate code

* it does help coming to a cleaner architecture, describe architecture as TLA+ spec and can debug


### What TLA+ Checks

* properties to check with TLA+ are conditions on individual executions

> example: the system property `Doesn't produce wrong answer.`
>
> examining individual execution and see if it produced wrong answer, system satisfies property if every execution does
>

* TLA+ can't check properties like `99% of execution produce right answer`, as not a condition on single execution


### Basic Abstraction under TLA+

* an execution of a system is represented as a `sequence` of `discrete` `steps`

* a digital system in its continuous evolution can be abstracted as a sequence of `discrete` events

* might look strange descrbing concurrent system as `sequence` of steps, can be done simply

* `step` is a state change

* a sequence of states can be termed `behavior`, representing execution

* specifying all behaviors can be done by abstracting out a `StateMachine`

> to specify a state machine, need
>
> * all possible initial states
>
> * what possible next states are given current state
>
> StateMachine halts given no possible next state

#### Defining a StateMachine

* for example program

```
int i;
void main(){
  i = randomInteger();
  i = i + 1;
}
```

* for uncertainity of `randomInteger()`, it's not easy to declare all state transitions possible

* value of `i` is only part of program's state, the other part of state is `control state`

> * so variables: `i`, `pc` (for program control)

```
int i;
void main(){
  i = randomInteger();  // pc = "start"; means first assignment need to execute next
  i = i + 1;            // pc = "middle"; means second assignment to execute next
}                       // pc = "done"; means program is finished
```

* can now define relation between current values of `i` & `pc` and their next states

```
if current value of pc equals "start"
  then next value of i in {0, 1, ..., 65535}
       next value of pc equals "middle"
  else if current value of pc equals "middle"
         then next value of i equals current value of i + 1
              next value of pc equals "done"
         else no next values
```

* the same StateMachine as above is more readable in TLA+ as denoted as a mathematical formula

---
