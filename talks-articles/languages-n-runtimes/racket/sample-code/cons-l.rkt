#lang racket
(require htdp/testing)
(require racket/trace)

(check-expect (cons-X 1 '()) '(1))
(check-expect (cons-X 1 '(2)) '(1 2))
(check-expect (cons-X 1 '(2 3)) '(1 2 3))


(define (cons-X F R)
  (append (list F) R)
  )


(define (cons-L First Rest)
  (lambda (Option) ;; returning a func
    (cond
      [(equal? Option "first") First]
      [(equal? Option "rest") Rest]
      [else "unhandled"])
    )
  )
(define (first-L func)
  (func "first")
  )
(define (rest-L func)
  (func "rest")
  )


(generate-report)

(trace cons-L)
(define cl1 (cons-L 1 '(2 3)))
(first-L cl1)
(rest-L cl1)

(define cl2 (cons-L 1 (cons-L 2 (cons-L 3 '()))))
(first-L cl2)
(rest-L (rest-L (rest-L cl2)))
