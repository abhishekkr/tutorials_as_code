
## Racket Cheatsheet

> [source](https://docs.racket-lang.org/racket-cheat/index.html)

* [docs](https://docs.racket-lang.org/); [slack](http://racket-slack.herokuapp.com/); [irc](https://racket-lang.org/irc-chat.html); [dev-group](https://groups.google.com/forum/#!forum/racket-dev/); [user-group](https://groups.google.com/forum/#!forum/racket-users/)

* [packages](https://pkgs.racket-lang.org/)

### Primitives

#### Numbers

* Literals: integer as `1`, rational as `1/2`, complex as `1+2i`, float as `3.14`, double as `6.02e+23`, hex as `#29`, octal as `#o32`, binary as `#b01001`

* Arithmetic: `+ - * / quotient remainder modulo add1 sub1 max min round floor ceiling sqrt expt exp log sin ... atan`

* Compare: `= < <= > >=`

* Bitwise: `bitwise-ior`, `bitwise-and`, `bitwise-xor`, `bitwise-not`, `arithmetic-shift`, `integer-length`

* Format: `number->string`, `string->number`, `real->decimal-string`

* Test: `number?`, `complex?`, `exact-nonnegative-integer?`, `zero?`, `positive?`, `negative?`, `even?`, `odd?`, `exact?`, `inexact?`

* Misc: `random`

* Pattern Match: `(? number? n) 42`, `(match '(1 2 3) [(list _ _ a) a])`


#### Strings

* Literals: `"Rack", "Rack \" et"`, unicode `λx:(μα.α→α).xx`

* Create: `make-string`, `string`, `string-append`, `build-string`, `string-join`

* Observe: `string-length`, `string-ref`, `substring`, `string-split`, `in-string`

* Modify: `string-downcase`, `string-upcase`, `string-trim`, `string->list`

* Test: `string?`, `string=?`, `string<=?`, `string-ci<=?`

* Regex: `#rx"a|b"`, `#rx"^c(a|d)+r$"`, `regexp-quote`, `regexp-match`, `regexp-split`, `regexp-replace`, `regexp-replace*`

* Match: `(? string? s) "WokaWokaWow"`


#### Bytes

* Literals: `#"rawbytes\0"`

* Create: `make-bytes`, `bytes`

* Numbers: `integer->integer-bytes`, `real->floating-point-bytes`

* Observe: `bytes-length`, `bytes-ref`, `subbytes`, `in-bytes`

* Modify: `bytes-set!`, `bytes-copy!`, `bytes-fill!`

* Conversion: `bytes->string/utf-8`, `string->bytes/utf-8`

* Test: `bytes?`, `bytes=?`

* Match: `(? bytes? b) #"0xDEADBEE"`


#### Other

* Boolean: `#t`, `#f`, `not`, `equal?`

* Characters: `char?`, `char->integer`, `integer->char`, `char<=?`, `char-alphabetic?`

* Symbols: `'Racket`, `symbol?`, `eq?`, `string->symbol`, `gensym`

* Boxes: `box?`, `box`, `unbox`, `set-box!`, `box-cas!`

* Procedures: `procedure?`, `apply`, `compose`, `compose1`, `keyword-apply`, `procedure-rename`, `procedure-arity`, `curry`, `arity-includes?`

* Void: `void?`, `void`

* Undefined: `undefined`

---

### Data

#### Lists

* Create: `empty`, `list`, `list*`, `build-list`, `for/list`

* Observe: `empty?`, `list?`, `pair?`, `length`, `list-ref`, `member`, `count`, `argmin`, `argmax`

* Use: `append`, `reverse`, `map`, `andmap`, `ormap`, `foldr`, `in-list`

* Modify: `filter`, `remove`, `sort`, `take`, `drop`, `split-at`, `partition`, `remove-duplicates`, `shuffle`

* Match: `(list a b c)`, `(list* a b more.. lst)`


#### Immutable Hash

* Create: `hash`, `hashreq`

* Observe: `hash?`, `hash-ref`, `hash-has-key?`, `hash-count`, `in-hash`, `in-hash-keys`, `in-hash-values`

* Modify: `hash-set`, `hash-update`, `hash-remove`


#### Vector

* Create: `build-vector`, `vector`, `make-vector`, `list->vector`

* Observe: `vector?`, `vector-length`, `vector-ref`, `in-vector`

* Modify: `vector-set!`, `vector-fill!`, `vector-copy!`, `vector-ma!`

* Match: `(vector x y z)`


#### Streams

* Create: `stream`, `stream*`, `empty-stream`

* Observe: `stream-empty?`, `stream-first`, `stream-rest`, `in-stream`


#### Mutable Hash

* Create: `make-hash`, `make-hasheq`

* Observe: `hash?`, `hash-ref`, `hash-has-key?`, `hash-count`, `in-hash`, `in-hash-keys`, `in-hash-values`

* Modify: `hash-set!`, `hash-ref!`, `hash-update!`, `hash-remove!`

---

### Systems

#### Input/Output

* Formatting:	`~a` to string in display mode, `~v`, `~s`, `~e`, `~r`, `pretty-format`

* Input: `read`, `read-bytes`, `peek-byte`

* Output: `write`, `write-bytes`, `display`, `displayln`, `pretty-print`

* Ports and Files: `with-input-from-file`, `with-output-to-file`, `flush-output`, `file-position`, `make-pipe`, `with-output-to-string`, `with-input-from-string`, `port->bytes`, `port->lines`


#### Files

* Paths: `build-path`, `bytes->path`, `path->bytes`, `path-replace-suffix`

* Files: `file-exists?`, `rename-file-or-directory`, `copy-directory/files`, `current-directory`, `make-directory`, `delete-directory/files`, `directory-list`, `filesystem-change-evt`, `file->bytes`, `file->lines`, `make-temporary-file`


#### Miscellaneous

* Time: `current-seconds`, `current-inexact-milliseconds`, `date->string`, `date-display-format`

* Command-line Parsing: `command-line`

* FFI: `ffi-lib`, `_uint32`, `_fun`, `malloc`, `free`


#### Networking

* TCP: `tcp-listen`, `tcp-connect`, `tcp-accept`, `tcp-close`

* HTTP: `http-conn`, `http-conn-open!`, `http-conn-send!`, `http-conn-recv!`, `http-conn-sendrecv!`, `http-conn-sendrecv`

* URLs: `string->url`, `url->string`, `url->query`

* Email: `smtp-send-message`, `imap-connect`

* JSON: `write-json`, `read-json`

* XML: `read-xml`, `write-xml`, `write-xexpr`

* Databases: `postgresql-connect`, `mysql-connect`, `sqlite3-connect`, `query-exec`, `query-rows`, `prepare`, `start-transaction`


#### Security

* Custodians: `make-custodian`, `custodian-shutdown-all`, `current-custodian`

* Sandboxes: `make-evaluator`, `make-module-evaluator`


#### Concurrency

* Thread: `thread`, `kill-thread`, `thread-wait`, `make-thread-group`

* Events: `sync`, `choice-evt`, `wrap-evt`, `handle-evt`, `alarm-evt`

* Channels: `make-channel`, `channel-get`, `channel-put`

* Semaphores: `make-semaphore`, `semaphore-post`, `semaphore-wait`

* Async Channels: `make-async-channel`, `async-channel-get`, `async-channel-put`


#### Parallelism

* Futures: `future`, `touch`, `processor-count`, `make-fsemaphore`

* Places: `dynamic-place`, `place`, `place-wait`, `place-channel`

* Processes: `subprocess`, `system+`

---

### Beginner Syntax

#### Basics

* Modules: `(module+ main body ...)`, `(module+ test body ..,)`, `(require mod-path)`, `(provide id)`

* S-expressions: `quote '(a b c)`, `quasiquote unquote '(1 2, (+ 1 2))`

* Procedure Applications: `(fn arg1 arg2)`, `keyword args (fn arg1 #:key arg2)`, `(apply fn arg1 (list arg2))`

* Procedures: `(lambda (x) (+ x 1))`, `(λ (x #:req key) (+ x key))`

* Binding: `(let ([x 1] [y 2]) (+ x y))`, `(let* ([x 1] [y (+ x 1)]) (+ x y))`

* Conditionals: `(if [zero? x] 0 (/ 1 x))`, `(cond [(odd? x) x] [else 0])`, `and`, `or`

* Definitons: `(define x 1)`, `(define (foo x) (+ x 1))`

* Iteration: `for`, `for/list`, `for*`

* Blocks: `begin`, `when`, `unless`

* Require sub-forms: `prefix-in`, `only-in`, `except-in`, `rename-in`, `for-syntax`, `for-label`

* Provide Sub-forms: `all-defined-out`, `all-from-out`, `rename-out`, `contract-out`


#### Structures

* Definition: `(struct person (fname lname contact))`

* Create: `(define alice (person "Alice" "Doe"))`

* Observe: `(person? alice)`, `(person-fname alice)`, `(person-lname alice)`

* Modify: `(struct-copy person alice ([fname "Jane"]))`

* Match: `(person f l)`


#### Pattern Matching

* Basics: `(match value [pat body] ...)`

* Definitons: `(match-define pat value)`

* Patterns: `(quote datum)`, `(list lvp ...)`, `(list-no-order pat ...)`, `(vector lvp ...)`, `(struct-id pat ...)`, `(regexp rx-ecpr pat)`, `(or pat ...)`, `(and pat ...)`, `(expr pat ...)`

---

### Advanced Syntax

#### Basics

* Mutation: `set!`

* Exceptions: `error`, `with-handlers`, `raise`, `exit`

* Promises: `promise?`, `delay`, `force`

* Continuations: `let/cc`, `let/ec`, `dynamic-wind`, `call-with-continuation-prompt`, `abort-current-continuation`, `call-with-composable-continuation`

* Parametes: `make-parameter`, `parameterize`

* External Files Needed at Runtime: `define-runtime-path`

* Continuation Marks: `continuation-marks`, `with-continuation-mark`, `continuation-mark-set->list`

* Multiple Values: `values`, `let-values`, `define-values`, `call-with-values`


#### Contracts

* Basics: `any/c`, `or/c`, `and/c`, `false/c`, `integer-in`, `vector/c`, `listof`, `list/c`

* Functions: `->`, `->*`, `->i`

* Application: `contract-out`, `recontract-out`, `with-contract`, `define/contract`


#### Iteration

* Sequences: `in-range`, `in-naturals`, `in-list`, `in-vector`, `in-port`, `in-lines`, `in-hash`, `in-hash-keys`, `in-hash-values`, `in-directory`, `in-cycle`, `stop-before`, `stop-after`, `in-stream`

* Generators: `generator`, `yield`, `in-generator`


#### Structures

* Sub-Structures: `(struct 2d (x y)) (struct 3d 2d (z)) (2d-x (3d 1 2 3))`

* Mutation: `(struct monster (type [hp #:mutable])) (define healie (monster 'slime 10)) (set-monster-hp! healie 0)`

* Transparency: `(struct cash ($ ¢) #:transparent) (struct->vector (cash 5 95))`

* Printing:

```
(struct nickname
  [n v]
   #:methods
   gen:custom-write [(define (write-proc nn p mode) (fprintf p (nickname-n nn)))]
  )
(displayln (nickname "evens" (in-range 0 100 2)))
```

* Serialization: `(struct txn (who what where) #:prefab) (write (txn "Mustard" "Spatula" "Observatory"))`


#### Generics

* Definition: `define-generics`

* Instantiation:

```
(struct even-set
  () #:methods
  gen:set [(define (set-member? st i) (even? i))])
```


#### Classes

* Definition: `interface`, `class*`

* Instantiation: `make-object`, `new`, `instantiation`

* Methods: `send`, `send/apply`, `send/keyword-apply`, `send*`, `send+`

* Fields: `get-field`, `set-field!`

* Mixins: `mixin`

* Traits: `trait`, `trait-sum`, `trait-exclude`, `trait-rename`

* Contracts: `class/c`, `instanceof/c`, `is-a?/c`, `implementation?/c`, `subclass?/c`

---

### Syntax Abstractions

* Definition: `define-syntax`, `define-simple-macro`, `begin-for-syntax`, `for-syntax`

* Templates: `syntax`, `syntax/loc`, `with-syntax`

* Parsing \(\)-Syntax: `syntax-parse`, `define-syntax-class`, `pattern`

* Syntax Objects: `syntax-source`, `syntax-line`, `syntax->datum`, `datum->syntax`, `generate-temporaries`, `format-id`

* Transformers: `make-set!-transformer`, `make-rename-transformer`, `local-expand`, `syntax-local-value`, `syntax-local-name`, `syntax-local-lift-expression`

* Syntax Parameters: `define-syntax-parameter`, `syntax-parameterize`, `syntax-parameter-value`

* Parsing Raw Syntax: `lexer`, `parser`, `cfg-parser`

---

### Tools

#### Packages

* Inspection: `raco pkg show`

* Finding: `pkgs.racket-lang.org`

* Installing: `raco pkg install`

* Updating: `raco pkg update`

* Removing: `raco pkg remove`

---
