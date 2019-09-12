#lang racket
(require htdp/testing)

(check-expect (factorial_embedded 0) 1)
(check-expect (factorial_embedded 1) 1)
(check-expect (factorial_embedded 2) 2)
(check-expect (factorial_embedded 3) 6)
(check-expect (factorial_embedded 4) 24)
(check-expect (factorial_embedded 5) 120)

(check-expect (factorial_tail 0) 1)
(check-expect (factorial_tail 1) 1)
(check-expect (factorial_tail 2) 2)
(check-expect (factorial_tail 3) 6)
(check-expect (factorial_tail 4) 24)
(check-expect (factorial_tail 5) 120)

(define (factorial_embedded n)
  (if (< n 2)
    1
    (* n (factorial_embedded (- n 1))) ; embedded as action was needed on returned value
    )
  )

(define (factorial_tail n)
  (factorial_tail_helper n 1)
  )
(define (factorial_tail_helper n answer)
  (if (< n 1)
    answer
    (factorial_tail_helper (- n 1) (* answer n)) ;(factorial_tail_helper (- n 1) (* n answer))
    )
  )

(generate-report)

(require racket/trace)

(trace factorial_embedded)
(factorial_embedded 5)

(trace factorial_tail_helper)
(factorial_tail 5)
