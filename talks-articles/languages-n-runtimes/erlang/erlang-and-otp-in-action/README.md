
_notes from_ pg22 55of431

## Erlang and OTP in Action

> by Martin Logan, Eric Merritt, Richard Carlsson

### Introduction

* Coming to life

> Post discovering Prolog, Joe started writing Prolog meta-interpreter. That grew into a small language with processes and message passing. Built atop Prolog, was simple functional and didn't use Prolog's unification and backtracking.
>
> Named after a danish mathematician A.K.Erlang with work in communication system statistics; could also be read as 'Ericsson laguage'.
>
> First implementation in 1988 for real users was productivity boost, but way too slow.
>
> By 1990, Joe, Mike Williams and Robert Virding started first abstract machine for Erlang named `JAM`. A stack-based abstract machine written in C, 70-times faster than Prolog implementation.


* In-between years

> Over years it gained distribution, record syntax, preprocessor, lambda expressions (funs), list comprehensions, Mnesia database, binary data type, bit syntax and more.
>
> Got ported to more platforms other than Unix, as Windows, VxWorks and QNX.
>
> In 1995, Ericsson restarted a failed huge C++ project in erlang with support department of 'the OTP team'
>
> In 1998, HiPE (High Performance Erlang) research group founded at Uppsala University, eventually HiPE code compiler integrated into Erlang/OTP distribution.
>
> Compiling Erlang to C failed due to greatly increased code size. A spinoff effect was faster, register-based, threaded code abstract machine design named BEAM replaced older JAM.
>
> Erlang got banned for new projects in the hype of 'globally used languages' (in short java) in the late 90s. So Management pursuaded to release Erlang as an open source in December'1998. Soon after core developers left to form startup.


* Getting dressed for success

> External uservase started to grow, Ericsson forgot of ban with time. OTP team supported erlang, Ericsson kept sponsoring HiPE and its spinoff EDoc and Dialyzer.


### Part 1 Getting past pure Erlang: The OTP basics

* [Chapter 1 The Erlang/OTP platform](./chapter1.md)

* [Chapter 2 Erlang language essentials](#)

* [Chapter 3 Writing a TCP-based RPC service](#)

* [Chapter 4 OTP applications and supervision](#)

* [Chapter 5 Using the main graphical introspection tools](#)


### Part 2 Building a Production System

* [Chapter 6 Implementing a caching system](#)

* [Chapter 7 Logging and Event Handling the Erlang/OTP way](#)

* [Chapter 8 Introducing distributed Erlang/OTP](#)

* [Chapter 9 Adding distribution to cache with Mnesia](#)

* [Chapter 10 Packaging, services and deployment](#)


### Part 3 Integrating and Refining

* [Chapter 11 Adding an HTTP interface to the cache](#)

* [Chapter 12 Integrating with foreign code using ports and NIFs](#)

* [Chapter 13 Communication between Erlang and Java via Jinterface](#)

* [Chapter 14 Optimization and performance](#)


* [appendix.A Installing Erlang](#)

* [appendix.B Lists and referential transparency](#)

---
---
