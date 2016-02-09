-- my own monad

module MyIO (MyIO, myPutChar, myGetChar, convert) where

import Control.Applicative          -- needed for 'instance Monad MyIO'
import Control.Monad --(liftM, ap)


type Input = String
type Output = String
type Remainder = String

data MyIO a = MyIO (Input -> (a, Remainder, Output))

apply :: MyIO a -> Input -> (a, Remainder, Output)
apply (MyIO f) = f

-- type 'MyIO' is abstract
-- only operations on it are monad operations, myPutChar, myGetChar, convert
-- apply is not exported from module

-- takes input, returns no value, leaves input unchanged & returns string "c" as input
myPutChar :: Char -> MyIO ()
myPutChar c = MyIO (\inp -> ((), inp, [c]))

-- takes input, returns first char, remainder of input is everything after that, no output
myGetChar :: MyIO Char
myGetChar = MyIO (\(ch:rem) -> (ch, rem, ""))

-----------------------------------------------------for-example
-- apply myGetChar "abc"   ==  ('a', "bc", "")
-- apply myGetChar "bc"    ==  ('b', "c", "")
-- apply (myPutChar 'A') "def"   ==  ((), "def", "A")
-- apply (myPutChar 'B') "def"   ==  ((), "def", "B")

-----------------------------------------------------instance

instance Functor MyIO where
    fmap = liftM

instance Applicative MyIO where
    pure = return
    (<*>) = ap


instance Monad MyIO where                       -- make a monad
    return x    = MyIO (\inp -> (x, inp, ""))   -- return 'x' takes input, returns 'x'
    m >>= k     = MyIO (\inp ->                 -- input unchanged, does no output
                    let (x, rem1, out1) = apply m inp in
                        let (y, rem2, out2) = apply (k x) rem1 in
                            (y, rem2, out1++out2))

-- -- example
-- apply (myGetChar >>= \x -> myGetChar >>= \y -> return [x,y]) "abc"  == ("ab", "c", "")
-- apply (myPutChar 'A' >> myPutChar 'B') "def"                        == ((), "def", "AB")
-- apply (myPutChar >>= \x -> myPutChar (toUpper x)) "abc"             == ((), "bc", "A")

--
convert :: MyIO () -> IO ()
convert m = interact (\inp ->
                     let (x, rem, out) = apply m inp in
                         out)

-- -- interact belongs to System.IO
-- -- interact :: String -> String -> IO ()

{-
module MyEcho where

import Char
import MyIO

myPutStr :: String -> MyIO ()
myPutStr = foldr (>>) (return ()) . map myPutChar

myPutStrLn :: String -> MyIO ()
myPutStrLn = myPutStr s >> myPutChar '\n'

myGetLine :: MyIO String
myGetLine = myGetChar >>= \x ->
              if x == '\n' then
                return []
              else
                myGetLine >>= \xs ->
                return  (x:xs)

myEcho :: MyIO ()
MyEcho  = myGetLine >>= \line ->
            if line == "" then
              return ()
            else
              myPutStrLn (map toUpper line) >>
              myEcho

main :: IO ()
main = convert MyEcho

-}

{-
-- could use "do" notation, 'cuz MyIO is a Monad'

myGetLine :: MyIO String
myGetLine = do {
              x <- myGetChar
              if x == '\n' then
                return []
              else do {
                xs <- myGetLine;
                return  (x:xs)
              }
            }

myEcho :: MyIO ()
MyEcho  = do {
            line <- myGetLine
            if line == "" then
              return ()
            else do {
              myPutStrLn (map toUpper line);
              myEcho
            }
          }

-}
