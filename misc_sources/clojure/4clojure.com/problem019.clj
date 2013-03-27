; last element [sequnce core function]
; Write a function which returns the last element in a sequence.
; don't use last
; (= (__ [1 2 3 4 5]) 5)
; (= (__ '(5 4 3)) 3)
; (= (__ ["b" "c" "d"]) "d")
(= (#(.get % (dec (count %))) [1 2 3 4 5]) 5)
(= (#(.get % (dec (count %))) '(5 4 3)) 3)
(= (#(.get % (dec (count %))) ["b" "c" "d"]) "d")
