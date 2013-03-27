; Lists: conj
; When operating on a list, the conj function will return a new list with one or more items "added" to the front.
; (= __ (conj '(2 3 4) 1))
; (= __ (conj '(3 4) 2 1))

(= (list 1 2 3 4) (conj '(2 3 4) 1))
(= (list 1 2 3 4) (conj '(3 4) 2 1))
