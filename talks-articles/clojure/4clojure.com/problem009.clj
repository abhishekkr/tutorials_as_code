; sets: conj
; When operating on a set,
; the conj function returns a new set with one or more keys "added".
; (= #{1 2 3 4} (conj #{1 4 3} __))
(= #{1 2 3 4} (conj #{1 4 3} 2))
