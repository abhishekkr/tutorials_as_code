Informatics 1 - Functional Programming
at University of Edinburgh
By Philip Wadler

Covered From Lecture11


---

[Video 18 on 22/November/2011]()

## Lecture.17 Monads

### Part.I The Mind-Body Problem
> Connection between mind and body.


### Part.II Commands

* Print a Char
```
putChar :: Char -> IO ()
-- putChar '!'
```

> if 'IO ()' as the type of commands, () is the type of 0-tuples
> 'putChar' yields a command, doesn't perform just produces it
> production of commands is purely functional

* Combine 2 commands sequentially
```
(>>) :: IO () -> IO () -> IO ()
-- putChar ':' >> putChar ')'
```
 > associative but not commutative, identity is command that does nothing

* Won't do anything, it's identity command for (>>)
```
done :: IO ()
-- it doesn't actually do nothin, but wouldn't do anything
```

* Print a String
```
putStr :: String -> IO ()
putStr []     = done
putStr (x:xs) = putChar x >> putStr xs
-- putStr ":)"
```

* 'putStr' using higher-order functions
```
putStr :: String -> IO ()
putStr = foldr (>>) done . map putChar
```

* Main
> using commands
```
module Confused where

main :: IO ()
main = putStr ":)\n"
```

* 'putStr' followed by newline
```
putStrLn :: String -> IO ()
putStrLn xs = putStr xs >> putChar '\n'
```


### Part.III Equational Reasoning

* Equational reasoning lost (Standard ML)
```
-- languages with side-effects
print "ha"; print "ha"        -- "haha"
-- this prints only "ha" as side-effect
let x = print "ha" in x; x
-- this again prints "haha" if side-effect
let f ()  = print "ha" in f (); f ()
```

* Equational reasoning regained (In haskell, term)
```
(1+2) * (1+2)
-- is equivalent to
let x = 1+2 in x * x

-- -- similarly

putStr "ha" >> putStr "ha"
-- entirely equivalent
let m = putStr "ha" in m >> m
```

> **Whole point of using Monads is have a property that you can abstract out.**


### Part.IV Commands with values

* Read a character
> IO () yield no value, '()' is trivial type that contains just one value '()'
> 'IO Char' for type of commands that yield a value of type 'Char'
```
getChar :: IO Char
-- it yields x and remaining input xs when input contains 'x:xs'
```

* Don't do anything and return a value
```
-- IO a for commands that return a value of type 'a'
return :: a -> IO a

-- input contains "bc" yields '[]' and an unchanged input "bc"
return [] :: IO String
```

* Combining commands with values
```
-- '>>=' is generalization of '>>' which handles values when commands performed
(>>=) :: IO a -> ( a -> IO b ) -> IO b
getChar >>= \x -> putChar (toUpper x)
-- for input "abc" produces output "A" and remaining input "bc"
```

* The "bind" operator (>>=) in detail
> if 'm :: IO a' yield a value of type a
> and, 'k :: a -> IO b' is function from value 'type a'
>     to a command yielding value of 'type b'
```
m >>= k :: IO b' is command that,
-- if ever performed behaves as
-- performs 'm' then 'k' with passing value from 'm' to 'k'
-- where 'k' gets affected by value passed
```

* Reading a line
```
getLine :: IO String    -- read input until newline
getLine = getChar >>= \x ->
          if x == '\n' then
              return []
          else
            getLine >>= \xs ->      -- recursive
            return (x:xs)
```

* Commands as a special case
```
-- general operations on command
return :: a -> IO a
(>>=)  :: IO a -> (a -> IO b) -> IO b

-- command 'done' is special case of 'return'
done :: IO ()
done = return ()

-- operator '>>' is special case of '>>='
(>>) :: IO () -> IO () -> IO ()
m >> n = m >>= \() -> n
```

* analogue of "let"
> 'let x = m in n' is another way of 'n where x =m'
```
-- standard ML let
E |- m :: a                 -- 'm' is an expression of type 'a'

E, x :: a |- n :: b         -- 'n' is an expression of type 'b' with variable 'x' of type 'a'
-------------------------
E |- let x = m in n :: b    -- then 'let x = m in n' is expression of type 'b'


-- "bind >>=" is combined with Lambda expression in a way that resembles 'let'
-- corresponding rule
E |- m :: IO a              -- 'm' returns a value of type 'a'

E, x :: a |- n :: IO b      -- 'n' returns a value of type 'b' contains variable 'x' of type 'a'
---------------------------
E |- m >>= \x -> n :: IO b  -- then 'm >>= \x -> n' does m,
                            -- uses result to do 'n', returns value type 'b'
```

* echoing input to output, example
```
echo :: IO ()
echo =  getLine >>= \line ->
        if line == "" then
          return ()
        else
          putStrLn (map toUpper line) >>
          echo

main :: IO ()
main = echo
```

* test it out
> run [lecture17.example00.hs](./lecture17.example00.hs)


### Part.V "Do" notation

> makes '>>=' bind notation look like imperative style for understanding
> 'do { x <- cmd; exp }' is same as 'cmdd >>= \x -> exp'

* Reading a line in "do" notation

```
getLine :: IO String
getLine = do {
            x <- getChar;
            if x == '\n' then
              return []
            else do {
              xs <- getLine;
              return (x:xs)
            }
          }
```

* Echoing in "do" notation

```
echo :: IO ()
echo =  do {
          line <- getLine;
          if line == "" then
            return ()
          else do {
            putStrLn (map toUpper line);
            echo
          }
        }
```

* "Do" notation in general
> every 'x <- e; ...' becomes 'e >>= \x -> ...'
> each line 'e; ..' becomes 'e >> ...'
```
do {
  x1 <- e1;
  x2 <- e2;
  e3;
  x4 <- e4;
  e5;
  e6
}
-- -- equivalent to
e1 >>= \x1 ->
e2 >>= \x2 ->
e3 >>
e4 >>= \x4 ->
e5 >>
e6
-- -- result is value returned by 'e6'
```


### Part.VI Monads

* Monoids
> 'monoid' is a pair of an operator (@@) & a value 'u',
> where operator has the value as identity and is associative
```examples
(+) and 0
(*) and 1
(||) and False
(&&) and True
(++) and []
(>>) and done
```

* Monads
> Monad is a generalized version of Monoid.
> Monad laws a closest to get to monoid laws when '>>=' & 'return' have types they have.
```
-- know that (>>) and 'done' satisfy the laws of 'monoid'
done >> m         = m
m >> done         = m
(m >> n) >> 0     = m >> (n >> o)

-- similarly (>>=) and 'return' satisfy laws of 'monad'
return v >>= \x -> m        = m[x:=v]
m >>= \x -> return x        = m
(m >>= \x -> n) >>= \y -> o = m >>= \x -> (n >>= \y -> o)
```

* Laws of Let
> 3 monad laws have analogues in "let" notation
```
let x = v in m                    = m[x:=v]
let x = m in x                    = m
let y = (let x = m in n) in o     = let x = m in (let y = n in o)
```

* "Let" in languages with/out effects
> 'v' need to be a value (not a function application) in languages with side-effects
> in Haskell one may replace a variable by any term, rather than by any value
> 'let x = n in m  =  m[x:=n]'


### Part.VII Roll your own monad - IO

* The Monad type class
> Can create an instance of TypeClass 'monad'.
> In previous typeclasses, the variable 'm' was TYPE.
> Here 'm' is TYPE CONSTRUCTOR, function which takes type and yields type.
```
class Monad m where
  return  :: a -> m a
  (>>=)   :: m a -> (a -> m b) -> m b
```

* My own IO Monad
> Definition in [lecture17.example01.hs](./lecture17.example01.hs)


[Video 19 on 28/November/2011]()


### Part.VIII The monad of lists

* The monad of lists
```
-- in standard prelude
class Monad m where
  return  ::  a -> m a
  (>>=)   :: m a -> (a -> m b) -> m b

instance Monad [] where
  return :: a -> [a]
  return x  = [x]

  (>>=)  :: [a] -> (a -> [b]) -> [b]    -- - 'a -> [b]'
  m >>= k   = [ y | x <- m, y <- k x ]  -- - modelling a
                                        -- - non-deterministic
                                        -- - computation
--- Equivalently
[] >>= k        = []
(x:xs) >>= k    = (k x) ++ (xs >>= k)

-- or
m >>= k         = concat (map k m)
```


* 'Do' notation and the monad of lists
```lecture18.example00.hs
pairs   :: Int  ->  [(Int,  Int)]
pairs n   = [(i,j) | i <- [1..n], j <- [(i+1)..n]]

-- - Equivalently
pairsWithDo  :: Int  ->  [(Int, Int)]
pairsWithDo n  = do {
              i <- [1..n];
              j <- [(i+1)..n];
              return (i,j)
            }
```
'Maybe' is lot like a list with 0 or 1 value elements.


* [Mondas with plus](https://en.wikibooks.org/wiki/Haskell/MonadPlus)
```
-- MonadPlus is a monad with extra structure
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a


instance MonadPlus [] where
  -- mzero  -- identity for plus
  -- mzero :: [a]
  mzero = []
  -- mplus
  -- mplus :: [a] -> [a] -> [a]
  mplus = (++)


guard :: MonadPlus m => Bol -> m ()
guard False   = mzero       -- non-deterministic result; 'mzero = [] = no possible result'
guard True    = return ()   -- 'return () = [()] = one possible result'

msum :: MonadPlus m => [m a] -> m a
msum  = foldr mplus mzero   -- generalization of concat
```


* Using guards
```lecture18.example00.hs
pairsGuard   :: Int  ->  [(Int,  Int)]
pairsGuard n   = [(i,j) | i <- [1..n], j <- [1..n], i < j]

-- - Equivalently
pairsWithDoGuard  :: Int  ->  [(Int, Int)]
pairsWithDoGuard n  = do {
                        i <- [1..n];
                        j <- [1..n];
                        guard (i < j);
                        return (i,j)
                      }
```

---

### Part.IX Parsers

Using Monads to build a parser.

* Parser type
> 'A parser for things is a function from strings to list of pairs Of things and strings.' -- Graham Hutton

* Creating the main Parser module used by following examples
[source](./lecture18.example01.hs)

Most important decisions for Parsers is the type.

* Creating an simple limited arithmetic expression parser and evaluator
[source](./lecture18.example02.hs)

* Creating Propositions
[source](./lecture18.example03.hs)


---

### Part.X Creating 'MicroQuickcheck'

[source](./lecture18.example04.hs)

---
---
