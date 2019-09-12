#lang racket
(require htdp/testing)
;; tests

(check-expect (nodes '(("A" "B") ("B" "C") ("C" "D"))) '("A" "B" "C" "D"))
(check-expect (nodes '(("A" "B"))) '("A" "B"))
(check-expect (nodes '()) '())

(check-expect (kids "B" '(("A" "B") ("B" "C") ("B" "D"))) '("C" "D"))
(check-expect (kids "B" '(("A" "B"))) '())
(check-expect (kids "B" '()) '())

(check-expect (leaf? "B" '(("A" "B") ("B" "C") ("B" "D"))) #f)
(check-expect (leaf? "B" '(("A" "B"))) #t)
(check-expect (leaf? "B" '()) #f)

(check-expect (grand-kids "B" '(("A" "B") ("B" "C") ("C" "D"))) '("D"))
(check-expect (grand-kids "B" '(("A" "B"))) '())
(check-expect (grand-kids "B" '()) '())

;; graph data

(define follows
  '(
    ("Alice" "Bob") ("Alice" "Dave")
    ("Bob" "Dave")
    ("Charlie" "Bob") ("Charlie" "Dave")
    ("Dave" "Eve")
    ("Eve" "Fiona")
    ("Fiona" "Gary")
    ("Gary" "Hans")
    ("Hans" "Eve")
    ("Hans" "Ivy")
    )
  )

(define priorityFollows
  '(
    ("Alice" "Bob" 100) ("Alice" "Dave" 73)
    ("Bob" "Dave" 24)
    ("Charlie" "Bob" 50) ("Charlie" "Dave" 50)
    ("Dave" "Eve" 10)
    ("Eve" "Fiona" 0)
    ("Fiona" "Gary" 1)
    ("Gary" "Hans" 99)
    ("Hans" "Eve" 37)
    )
  )

;; define graph func

(define (nodes Graph)
  (remove-duplicates (flatten Graph))
  )

(define (kids Node Graph)
  (map
    second
    (filter  ;; filter fun takes only one param, thus lambda as needs 2 here
      (lambda (Edge)
        (equal?
          Node
          (first Edge)
          )
        )
      Graph
      )
    )
  )

(define (leaf? Node Graph)
  (cond
    [(equal? (index-of (nodes Graph) Node) #f) #f]
    [else (null? (kids Node Graph))]
    )
  )

(define (grand-kids Node Graph)
  (flatten
    (map
      (lambda (Kid) (kids Kid Graph))
      (kids Node Graph)
      )
    )
  )

(generate-report)
;; usage

(nodes follows)

(kids "Charlie" follows)

(leaf? "Charlie" follows)
(leaf? "Ivy" follows)
(leaf? "Jack" follows)

(grand-kids "Fiona" follows)
