## Y NOT : Adventure in FP ~ Jim Weirich ~ RubyConf2012 Denver
## LAMBDA CLCULUS
# also see 'Programming with nothing' by Tom Stuart

puts ->() {
  fact = ->(n) { n.zero? ? 1 : n * fact.(n-1) }
  p fact.(5)

  # ->(n) { n.zero? ? 1 : n * (n-1) }
  #
  # FixPoints a==f(a)

  make_adder = ->(x) {
    ->(n) { n + x }
  }
  compose = ->(f,g) {   #Higher Order Functions
    ->(n) {f.(g.(n))}
  }

  add1 = ->(n) { n + 1 }
  add_1 = make_adder.(1)
  mul3 = ->(n) { n * 3 }

  p mul3.(add1.(10))

  mul3add1 = compose.(mul3, add1)
  p mul3add1.(10)

  # Functional Refactorings
  # (1) Tenance Corresponding Principle
  mul3 = ->(n) { ->(){ n * 3 }.() }
  p mul3.(5)
  # (2) Introduce Binding
  mul3 = ->(n) { ->(z){ n * 3 }.(1010101) }
  p mul3.(5)
  # Wrap Functions
  make_adder = ->(x) {
    ->(v) { ->(n) { n + x }.(v) }
  }
  add_1 = make_adder.(1)
  p add_1.(9)
  # Inline Definition
  puts ->(f,g) { ->(n) {f.(g.(n))} }.(  #compose
    ->(n){n*3}, #mul3
    ->(x) { ->(v) { ->(n) { n + x }.(v) } }.(1)   # add_1
  ).(10)


  # Recursion
  factx = ->(fact_gen) {
            fact_gen.(fact_gen)
          }.(
            ->(fact_gen) {
              ->(n) { n.zero? ? 1 : n * fact_gen.(fact_gen).(n-1) }
            }
          )
  p factx.(0)
  p factx.(1)
  p factx.(10)
  # recusrion => revision#1
  error = ->(n) {throw 'should never be called'}
  factx = ->(fact_improver){
            ->(fact_gen) {
                fact_gen.(fact_gen)
              }.(
                ->(fact_gen) { fact_improver.(->(v){fact_gen.(fact_gen).(v)}) }
              )
          }.(
            ->(partial){
              ->(n) { n.zero? ? 1 : n * partial.(n-1) }
            }
          )
  p factx.(10)

  # recursion => revision#2
  # Applicative Order Y Combinator
  # i.e. Z-Combinator i.e. FIXPOINT Combinator
  fact_improver = ->(partial){ ->(n) { n.zero? ? 1 : n * partial.(n-1) } }
  y = ->(improver){
            ->(fact_gen) { fact_gen.(fact_gen)
                }.( ->(fact_gen) { improver.(->(v){fact_gen.(fact_gen).(v)}) } )
      }
      # Factorial is FIXPOINT of fact_improver
      # passing factorial function to fact_improver will give factorial function
      # 'y' calculates the FIXPOINT of an improver function
      # this 'y' is the Y Combinator
  factx = y.( fact_improver )
  p factx.(10)

  # mathemtical symmetrical version
  # turn unintentional names, wrap it, symmetricize it
  y = ->(f){
        ->(x) { f.(->(v){x.(x).(v)}) }.(
        ->(x) { f.(->(v){x.(x).(v)}) } )
      }
  factx = y.( fact_improver )
  factx.(10)

}.()
