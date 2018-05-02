
# Chapter.1 Dipping our toes in the pool

## Truthiness

* Clojure has **one** boolean context, `if` form. Other forms as `and`, `or`, `when`, others are macros built over `if`.

* Every value looks `true` except `false` and `nil`. So even empty values are truthy.

* Can use `nil?` and `false?` to identify between two false values.

---

## Nil Punning

* since empty collections are `true`, need an idiom to identify if there's anything in a collection

```
(seq [1 2])
;=> (1 2)

(seq [])
;=> nil

(defn print-seq [s]
  (when (seq s)
    (prn (first s))
    (recur (rest s))))
```

* another non-idiomatic way is checking `empty?`

```
(when-not (empty? s) (prn (first s)))
```

* best to use `doseq`,clojure forms named with `do` (,`doseq`, `dotimes`, more) are intended for side-effects in their bodies and return `nil`

* `rest` consume the sequence on recursive call

```
(rest [1 2 3])
;=> (2 3)
(rest [1])
;=> ()
(rest [])
;=> ()
```

* `next` function return sequence of the rest i.e. `(seq (rest X))` and thus never returns empty sequence but nil instead

---

## Destructuring

* is loosely related to pattern matching from Haskell, KRC, Scala but limited in scope; for more [full featured pattern matching](https://github.com/dcolthorp/matchure)

* following does the required but not so smartly, repitition and using index number instead of locals

```
;; vector taking length 3, sort in 'last, first second' for names
(defn name-sake [fullname]
  (str (nth fullname 2) ", "
    (nth fullname 0) " "
    (nth fullname 1)
    ))

(name-sake ["fst", "snd", "lst"])
```

* destructure above problem with `let` to create locals

```
; poisitional destructuring, doesn't work on maps and sets
; it will work with anything java.util.regex.Matcher and implementing CharSequence & java.util.RandomAccess
(let [[f-name m-name l-name] fullname]
  (str l-name ", " f-name " " m-name))
```

* can also use `&` for remaining values, bound remaining values as sequence

```
(let [[a b & more] (range 10)]
  (println "a, b are" a b)
  (println "with " more))
```

* `:as` could be used to infer full collection, to be placed after `&` if any; but here all are available as is and not as sequence

```
(let [range-vec (vec (range 10))
      [a b & more :as all] range-vec]
  (println "a, b are" a b)
  (println "with " more)
  (println "all were " all))

(let [{f-name :f-name, :as whole-name} fullnamemap]
  whole-name)
```

* destructuring with a map, ugly way

```
(def fullnamemap {:f-name "fst", :m-name "snd", :l-name "lst"})

(let [{f-name :f-name, m-name :m-name, l-name :l-name} fullnamemap]
  (str l-name ", " f-name " " m-name))

;; item on the left of each pair will be a new local name, must be a symbol or destructuring form
;; it can't be keyword unless keyword is specially supported features as :keys, :strs, :syms, :as and :or
```

* `:keys` nicely handle repititiveness earlier in vector, `:strs` to lookup items in the map with string keys, `:syms` indicating symbol keys

```
; tells Clojure next form is vector of names, that it converts to keywords to look-up
(let [{:keys [f-name m-name l-name]} fullnamemap]
  (str l-name ", " f-name " " m-name))
```

* if the key looked-up is not in map, normally `nil` can be given a default suing `:or`

```
(let [{:keys [title f-name m-name l-name], :or {title "Mr."}} fullnamemap]
  (println title f-name m-name l-name))
;=> Mr. fst snd lst
```

* can destructure a vector by providing a map declaring local name as indices, associative destructuring

```
(let [{fst 0, lst 3} [1 2 3 4]]
    [fst lst])
```

* destructuring in function parameter can be used similar to `let` till now

---

## Use REPL

#### Experimenting with sequence

* to get usable sequence, `range`

```
(range 5)
;=> (0 1 2 3 4)

(range 1 10)
;=> (1 2 3 4 5 6 7 8 9)
```

* to nest sequence, `for`

```
(for [x (range 2) y (range 2)]
  [x y])
;=> ([0 0] [0 1] [1 0] [1 1])
```

* not able to find a function, clojure provides `find-doc`

```
(find-doc "xor")
; -------------------------
; clojure.core/bit-xor
; ([x y] [x y & more])
;   Bitwise exclusive or
;=> nil

(bit-xor 1 3)
;=> 2

(defn xors [max-x max-y]
  (for [x (range max-x) y (range max-y)]
    [x y (bit-xor x y)]))

(xors 2 2)
;=> ([0 0 0] [0 1 1] [1 0 1] [1 1 0])
```

#### Experimenting with graphics

* using java libraries, `frame` object marked visible using `for` macro

```
(def frame (java.awt.Frame.))
; frame

(for method (seq (.getMethods java.awt.Frame))
    :let [method-name (.getName method)]
    :when (re-find #"Vis" method-name)]         ;; #"Vis" is a regexp
  method-name)

(prn (.isVisible frame))
(.setVisible frame true)
(.setSize frame (java.awt.Dimension. 600 400))
```

* `javadoc` is available at REPL as of Clojure 1.2, usable as `(javadoc frame)`

* to draw into frame, need to fetch graphic context `(def gfx (.getGraphics frame))`

* create rectangle in frame

```
(.fillRect gfx 100 100 50 75)

(.setColor gfx (java.awt.Color. 255 128 0))
(.fillRect gfx 100 150 75 50)
```

#### Putting it all together

```
(defn xors [max-x max-y]
  (for [x (range max-x) y (range max-y)]
    [x y (bit-xor x y)]))

(def frame (java.awt.Frame.))

(.setVisible frame true)
(.setSize frame (java.awt.Dimension. 600 400))

(def gfx (.getGraphics frame))

(doseq [[x y xor] (xors 200 200)]
  (.setColor gfx (java.awt.Color. xor xor xor))
  (.fillRect gfx x y 1 1))
```

#### When things go wrong

* the result of failure is available in Var name `*e`, can view it as `(.printStackTrace *e)`

#### Just for fun

```
(defn f-values [f xs ys]
  (for [x (range xs) y (range ys)]
    [x y (rem (f x y) 256)]))

(defn draw-values [f xs ys]
  (clear gfx)
  (.setSize frame (java.awt.Dimension. xs ys))
  (doseq [[x y v] (f-values f xs ys)]
    (.setColor gfx (java.awt.Color. v v v))
    (.fillRect gfx x y 1 1)))

; (draw-values bit-and 256 256)
; (draw-values + 256 256)
; (draw-values * 256 256)
```

---

