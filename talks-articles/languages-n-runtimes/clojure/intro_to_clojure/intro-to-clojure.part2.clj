; part 2

(ns cljpart2)

; Define a structure
(defstruct nuvector :x :y)
; Define a fn with overloaded constructors
(defn make
  ([]
    (struct nuvector 0 0))
  ([x y]
    (struct nuvector x y))
)
; getters for struct
(defn zero_mag? [v]
  (and (zero? (:x v))
       (zero? (:y v))
  )
)
;; using above structs
(def var1 (struct nuvector 80 443))
(println (:x var1))
(println (:y var1))
(println var1)
(def var2 (make))
(println var2)
(def var3 (make 22 23))
(println var3)
(def var4 (make 22 23))
(def var5 (assoc var4 :x 8080))
(println var5)

(defn sumsub [o]
  (let [ ; macro declaring Vector of local variables
        a (:v1 o)
        b (:v2 o)
        c (:v3 o)
        r (:result o)
        zxy (+ a (- b c) )
        ]
    (assoc o :result zxy)
  )
)
