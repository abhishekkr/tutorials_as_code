
## Racket Programming

> by Colleen Lewis, [source](https://www.youtube.com/playlist?list=PLHqz-wcqDQIEThNEXViEb1iFh9vbOtUD_)
>
> ran with Racket v7.4.

### Intro to Racket: calling and defining function, if, cond

* basic define and function calls

```
> 1
1
> (+ 1 2)
3
> (sqrt 3)
1.7320508075688772
> (+ (sqrt 4) (sqrt 9))
5
> (+)
0
> (- 2 3 5)
-6
> "oye"
"oye"
> (define two 2)
> two
2
> (define add +)
> (add 1 2)
3
```

* defining a function `(define (<signature>) (<definition>)`

```
> (define (avg x y) (quotient (+ x y) 2))
> (avg 100 25)
62
```


* if `if (<predicate>) <true case> <false case>`

```
> (if (odd? 3) "that is odd" "try again")
"that is odd"
> (if (equal? 3 2) "that is odd" "try again")
"try again"
> (if (equal? 3 2) "that is odd" (+ 1 2))
> 3

> ((if (equal? 3 2) - +) 3 4)
7
> ((if (equal? 2 2) - +) 3 4)
-1

> (define (warm temp) (if (> temp 75) "warm" "cool"))
> (warm 10)
"cool"
```


* cond `(cond [(condition1) case1] [(condition2) case2] [else last_case])`

```
> (define (xwarm temp) (cond [(> temp 75) "warm"] [(< temp 65) "cold"] [else "ok"]))
> (xwarm 10)
"cold"
```


* create new variable definitons `(let ([<var1> <val1>] [<var2> <var2>]) <body>)`

```
> (let ([one 1] [two 2]) (+ one two))
3
```

---

### Recursion, with tracing flow of execution

```
> (define (factorial N) (if (< N 2) 1 (* N (factorial (- N 1)))))
> (factorial 2)
2
> (factorial 1)
1
> (factorial 0)
1
> (factorial 3)
6
> (factorial 10)
3628800

> (require racket/trace)

> (trace factorial)
> (factorial 4)
>(factorial 4)
> (factorial 3)
> >(factorial 2)
> > (factorial 1)
< < 1
< <2
< 6
<24
24
```

---

### List functions

* lists in Racket are as `linked-lists` with each node having a first and rest pair, rest points to next node

```
> (define lst '("this" "is" "how" "a" "list" "is"))
```

* checking if list got any items

```
> (null? '())
#t
> (null? '(""))
#f
```

* `first` and `rest` always expect a minimal one element list and return error otherwise

```
> (first lst)
"this"
> (rest lst)
'("is" "how" "a" "list" "is")
> (first '(""))
""
> (rest '(""))
'()
```

* creation with `list` and `cons`

```
> (list '(1 2 3) 4)
'((1 2 3) 4)
> (list '(1 2 3) '(4 5 6))
'((1 2 3) (4 5 6))
> (list 1 3 5 7)
'(1 3 5 7)

> (cons 1 '())
'(1)
> (cons 1 '(2 3))
'(1 2 3)
> (cons '(1) '(2 3))
'((1) 2 3)
> (cons 1 2)
'(1 . 2)
;; '.' in play  here is covered in Dots section
```

* `append` needs 2+ lists

```
> (append '(1 2 3) '("a" "b" "c"))
'(1 2 3 "a" "b" "c")
> (append '(1 2 3) '("a" "b" "c") '(3 4 5))
'(1 2 3 "a" "b" "c" 3 4 5)
> (append '(1 2 3) '("a" "b" "c") '(() ()))
'(1 2 3 "a" "b" "c" () ())

> (append '(1 2 3) '("a" "b" "c") 2)
'(1 2 3 "a" "b" "c" . 2)
```

---

### Filter a list

```
> (define (evenOfList lst)
    (cond
      [(null? lst) '()]
      [(odd? (first lst)) (evenOfList (rest lst))]
      [else (cons (first lst) (evenOfList (rest lst)))]
    ))
> (evenOfList '(1 2 3 4 5))
'(2 4)
```

---

### For..Each List

```
> (define (incrList lst)
    (if (null? lst)
        '()
        (cons (+ 1 (first lst)) (incrList (rest lst)))
    )
  )
> (incrList '(1 3 5 7))
'(2 4 6 8)
```

---

### Lists are Pairs, writing copy/remove/reverse

* a line comment starts with `;`

* `'(1 2)` means `(cons 1 (cons 2 '()))`

* **In Racket, pairs can be modified, but should never be modified.**

```
" thus, to append define a new
(define a (list 1 2))
(define b (list 3))
(define c (append a b))

; this will copy first pair and point end to 'b',
; when any referenced pair modified it's copy is made
; explicit copies can be made using 'copy/1'
```

* difference for `equal?` and `eq?` for checking just values and instance respectively

```
> (define a '(1 2))
> (equal? a '(1 2))
#t
> (eq? a '(1 2))
#f
```

* remove a copy of element from list

```
> (remove 1 '(1 2 3))
'(2 3)
> (remove 1 '(1 1 2 3))
'(1 2 3)
> (remove 1 '())
'()
```

* custom copy

```
> (define (copyList lst)
    (if (null? lst)
      '()
      (cons (first lst) (copyList (rest lst)))
    )
  )
> (define a '(1 2))
> (define b (copyList a))
> b
'(1 2)
```

* custom remove

```
> (define (removeListItem item L)
    (cond
      [(null? L) '()]
      [(equal? item (first L)) (removeListItem item (rest L))]
      [else (cons (first L) (removeListItem item (rest L)))]
    )
  )
> (removeListItem 1 '(1 2 3))
'(2 3)
> (removeListItem 1 '(1 1 2 3))
'(2 3)
```

* custom reverse

```
> (define (revList lst)
    (if (null? lst)
      '()
      (append (revList (rest lst)) (list (first lst)))
    )
  )
> (revList '(1 2 3))
'(3 2 1)
```

---

### Deep-list (List-Of-List), with custom `flatten`

```
> (define lol '(((1) (2)) ((3))))
> (flatten lol)
'(1 2 3)
> (flatten (first lol))
'(1 2)
> (flatten (rest lol))
'(3)
```

* custom `flatten`

```
> (define (flattenMyList L)
    (cond
      [(null? L) '()]
      [(list? (first L)) (append (flattenMyList (first L)) (flattenMyList (rest L)))]
      [else (cons (first L) (flattenMyList (rest L)))]
    )
  )
> (flattenMyList lol)
'(1 2 3)
> (flattenMyList  (first lol))
'(1 2)
```

---

### Sublists

* using `lambda` and `map`

```
> (define add2 (lambda (n) (+ n 2)))
> (add2 8)
10

> (map add2 '(2 4 6))
'(4 6 8)
```

* a method to create all possible sublists

```
> (define (sublists L)
    (if (null? L)
      '(())
      (let*  ; to use defined var in other
        ([it (first L)]
         [lose-it (sublists (rest L))]
         [use-it (map (lambda (E) (cons it E)) lose-it)])
        (append lose-it use-it)
      )
    )
  )

 > (sublists '(1 2 3 4))
'(()
  (4)
  (3)
  (3 4)
  (2)
  (2 4)
  (2 3)
  (2 3 4)
  (1)
  (1 4)
  (1 3)
  (1 3 4)
  (1 2)
  (1 2 4)
  (1 2 3)
  (1 2 3 4))
```

---

### Embedded and tail-recursion in Factorial; with TDD

* expect can be mentioned using

```
(require htdp/testing)

(check-expect (fact 0) 1)
(check-expect (fact 1) 1)
;; more cases if needed
(check-expect (fact 5) 120)

;; (define (fact n) (definiton.....))

(generate-report)
```

* check [factorial.rkt](./sample-code/factorial.rkt) for full code

---

### Implementing `cons` without built-in pairs; passing around Function

* in [cons-l.rkt](./sample-code/cons-l.rkt), `cons-L` returns a function which is passed to `first-L` and `rest-L` as parameter

---

### Dots

* similar to lisp notation `.` denotes pair

```
'(1 2 3) ; is list wih (1 2 3 '()) just doesn't print empty list pair for 2nd last item
'(1 2 . 3) ; denotes it is not a standard list ending with '() but with 3 as pair
```

---

### Graph representation: nodes, kids, leaf?, grand-kids

* in [graph-data](sample-code/graph-data.rkt) functions `nodes`, `kids`, `leaf?` and `grand-kids` have been defined to suit list based graph data

---
