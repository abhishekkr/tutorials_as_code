; just some timepass code to remind of language structs
;
; CLOJURE
;
; combination of extracts of talks
;
; * Intro To Clojure
;   -by Brian Will (http://www.youtube.com/playlist?list=PLAC43CFB134E85266)
;
; * Uncle Bob Martin presents Clojure
;   - by Uncle Bob
;
; * LearnXinYMinutes.com/doc/clojure
;

; to quick help
;(doc first)

; first call should be to set namespace
(ns clojureintro)

; tick ` :: means its data not a func so eval and place
'("I WAZ CLISP BEFORE!")
'(+ 5 10)
(println '(+ 5 10))
(eval '(+ 5 10))
(println `(+ 5 10))
(println (eval `(+ 5 10)))

(println (/ 20 5 2))
(println (repeat 2 5))
(println (apply + (repeat 2 5)))
(println (first [20 5 2]))
(println (next [20 5 2]))

(println "NOT STRING IN SINGLE-QUOTE")
(println (+ 1 2 3 4 5))
(println *clojure-version* "\n" + "\n" *file* "\n" - "\n" :KEYWORD_SYMBOL)
(println *compile-path* :key "abcde" 1 2)
(println "list" '(3 4 5)) ; list is (3 4 5) ...to data-fy it use apostrophe
(println "vector" [6 7 8 9])
(println "hashmap" {"f" "u" 1 2})
(println ({"f" "u" 1 2} "f"))
(println ({"f" "u" 1 2} 1))

;special forms list
; boolean states
(println (or))
(println (or true false false))
(println (and))
(println (and true true false))
(println true false (not true))
; equality condition and str concatenating every argument as string
(println (str (= 1 1) (= 1 2)))
; if conditional
(println ">>>>> if conditional")
(println ( if (< 1 2) "what" "now"))
(println ( if (> 1 2) "what"))
; quote
(println ">>>>> quote")
(println (quote (f (o) o)))
; def
(println ">>>>> def")
(def what {1 2 3 4}) ; var mapping
(println (what 3))
; do
(println ">>>>> do")
(println
  (do
    (def var1 "Var~1")
    (print)
    (what 1)
    )
  )
; let
(println ">>>>> let")
(def abc 1)
(def xyz 2)
(def pqr (let [abc "none" xyz 7] (println abc) (+ 3 xyz)))
(println pqr)
; fn
(println ">>>>> fn")
(def add (fn fn_add [num1, num2] (+ num1 num2) fn_add))
(def add (fn fn_add [num1, num2] (+ num1 num2) fn_add))
(println (add 1 9))
(def add (fn [num1, num2] (+ num1 num2)))
(println (add 1 9))
(def sub (fn [num1, num2] (- num1 num2)))
(println (sub 1 9))
; . (. class method argument*) (. instance method argument*)
(println ">>>>> .")
;(. System println "ABC") ; new (new class argument*)
(println ">>>>> new")
(new java.util.Date)
;(new java.util.Timer false)
(println ">>>>> clojure.core")
(println (clojure.core/+ 1 2))
(println (clojure.core/* 1 2))

; lexical scoping
(println ">>>>> lexical scoping")
(println (let [abc 1000]
              (def plus100 (fn [abc] (+ abc 100)))
              (println (plus100 1))
              (sub 1500 abc)
    )
  )

; NameSpacing Vars
(println ">>>>> clojure namespacing")
(clojure.core/in-ns (quote abk))
(def msg "welcome")
(clojure.core/println msg)
(clojure.core/in-ns (quote abionic))
(def msg "sayonara")
(clojure.core/println msg)
(clojure.core/in-ns (quote abk))
(clojure.core/println "current namespace (abk):" msg)
(clojure.core/println "abionic namespace:" abionic/msg)

(clojure.core/refer (quote clojure.core))
(println ">>>>> clojure core refer || println wouldn't have worked before refer")
(def plus (+ 3 7))
(println plus)

(ns clojureintro)
(def currentnsVar 1)
(println currentnsVar)
(ns newns)
(def currentnsVar 10)
(println currentnsVar)
(ns clojureintro)
(println currentnsVar)



; tail recursion [end-of-function recursion drop calling func frame optimization]
(println ">>>>> tail recursion")
(def showUpto100
  (fn [num]
    (print num " ")
    (if (< num 100)
      (recur (+ num 1))
      )
    )
  )
(println (showUpto100 97))
; loop
(println ">>>>> loop")
(loop [idx 5]
  (print idx " ")
  (if (< idx 15)
    (recur (+ idx 10))
    )
  )
(println)

; creating methods
;; Macros
(println ">>>>> using Macros in-built")
(defn mul [n1 n2] (* n1 n2)) ; => (def mul (fn [n1 n2] (* n1 n2)))
(println (mul 5 10))
;; also nameless function defs
(def div (fn [n1 n2] (/ n1 n2)))
(println (div 50 10))
;; also shortcut to nameless function defs
(def Div #(/ %1 %2))
(println (Div 10 5))
(def Sqr #(* % %))
(println (Sqr 10))
;(println ">>>>> creating Macros")
;(defmacro name [parameter*] expression*)

; sequence  first,rest
;
;(def lst1 (1 2 3 4 5))
;(println (first lst1)
;
;(loop [l1 (1 2 3 4 5)]
;  (print (first l1))
;  if( (< 2 1)
;  (recur (rest l1)) )
;  )
