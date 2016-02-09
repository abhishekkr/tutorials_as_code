-- parser {copy of "lecture18.example01.hs"}
module Parser(Parser, apply, parse, char, spot,
            token, star, plus, parseInt, mplus) where

{-
[output]

Prelude> :l lecture18.example01.hs 
[1 of 1] Compiling Parser           ( lecture18.example01.hs, interpreted  )
Ok, modules loaded: Parser.
*Parser> apply (spot isDigit) "123ab"
[('1',"23ab")]
*Parser> 
*Parser> apply (spot isDigit) "23ab"
[('2',"3ab")]
*Parser> apply (spot isDigit) (tail "123ab")
[('2',"3ab")]
*Parser> apply (spot isDigit) "ab"
[]
*Parser> 
*Parser> apply (spot isAlpha) "a+b"
[('a',"+b")]
*Parser> apply (token '+') "a+b"
[]
*Parser> apply (token '+') "+b"
[('+',"b")]
*Parser> apply ( do { x <- spot (isDigit); token '+'; y <- spot (isDigit); return (digitToInt x + digitToInt y)  }  )  "1+2"
[(3,"")]
*Parser> apply ( do { x <- spot (isDigit); token '+'; y <- spot (isDigit); return (digitToInt x + digitToInt y)  }  )  "(1+2)"
[]
*Parser> apply ( do { x <- spot (isDigit); token '+'; y <- spot (isDigit); return (digitToInt x + digitToInt y)  }  )  "1+2)"
[(3,")")]
*Parser>
*Parser> apply (star (spot isDigit)) "123ab"
[("123","ab"),("12","3ab"),("1","23ab"),("","123ab")]
*Parser>
*Parser> apply (do { x <- star (token 'a' `mplus` token 'b'); y <- plus (token 'b' `mplus` token 'c'); return (x,y)  }) "aabbcc"
[(("aabb","cc"),""),(("aabb","c"),"c"),(("aab","bcc"),""),(("aab","bc"),"c"),(("aab","b"),"cc"),(("aa","bbcc"),""),(("aa","bbc"),"c"),(("aa","bb"),"cc"),(("aa","b"),"bcc")]
*Parser> apply (do { x <- star (token 'a' `mplus` token 'b'); y <- plus (token 'b' `mplus` token 'c'); return (x,y)  }) "123ab"
[]
*Parser> 
*Parser> apply parseInt "123ab"
[(123,"ab"),(12,"3ab"),(1,"23ab")]


-}

import Data.Char

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad
-- import Control.Monad (liftM, ap)
-- import Control.Applicative (Alternative())

-- The Type of Parsers
{-
First attempt, a parser of values of type 'a' takes string and produces value of type 'a'
    type Parser a = String -> a

Second attempt, at each stage you have a part that you parsed and a remainder left to parse
    type Parser a = String -> (a, String)


Third attempt, sometimes there will be more than one parse or none... so a list of possibilities
    type Parser a = String -> [(a, String)]
-}
newtype Parser a = Parser (String -> [(a, String)])

-- Apply a parser
apply :: Parser a -> String -> [(a, String)]
-- apply (Parser f) s = f s
apply (Parser f) = f

-- Return parsed value, assuming at least one successful parse
parse :: Parser a -> String -> a
-- parse m s = head [ x | (x,t) <- apply m s, t == "" ]
parse m s = one [ x | (x,t) <- apply m s, t == "" ]
    where
        one []                  = error "no parse"
        one [x]                 = x
        one xs | length xs > 1  = error "ambiguous parse"

-- Parsers from a monad
{-
class Monad m where
    return :: a -> a
    (>>=)  :: m a -> ( a -> m b ) -> m b

Now as of Applicative Monad Proposal (AMP), whenever you declare something as Monad, you also have to declare it as Applicative (and therefore Functor).
Mathematically speaking, every monad is an applicative functor, so this makes sense.
-}

-- AMP requirement for 'instance Monad Parser ..'
instance Functor Parser where
      fmap = liftM

instance Applicative Parser where
      pure  = return
      (<*>) = ap

instance Monad Parser where
    return x = Parser (\s -> [(x,s)])
    m >>= k  = Parser (\s ->
                [ (y, u) |
                  (x, t) <- apply m s,
                  (y, u) <- apply (k x) t ])

-- Monad structure gives Sequencing
-- Plus structure gives alternatives.

-- Parser form a monad with sums
{-
class MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a
-}

-- AMP requirement for 'instance MonadPlus Parser ..'
instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

instance MonadPlus Parser where
    mzero       = Parser (\s -> [])
    mplus m n   = Parser (\s -> apply m s ++ apply n s)

-- Parse one character
char :: Parser Char
char = Parser f
    where                       -- parses first char in input
        f []        = []        -- if empty input; failure: empty list of possibilities
        f (c:s)     = [(c,s)]   -- if input is non-empty; first char result, tail is remainder

-- Parse a character satisfying a predicate
spot :: (Char -> Bool) -> Parser Char
spot p = Parser f               -- requires char to satisfy a given predicate
    where                       -- 'spot isDigit' will only parse digits
        f []                = []
        f (c:s) | p c       = [(c,s)]
                | otherwise = []
-- spot p = do { c <- char; guard (p, c); return c }

-- Parse a given character
token :: Char -> Parser Char    -- token 'c' will parse the char only; fail if next char is something else
token c = spot (== c)

{-
-- Perform a list of commands, returning a list of values
sequence :: Monad m => [m a] -> m [a]
sequence []             = []
sequence (m:ms)         = do {
                                x <- m;
                                xs <- sequence ms;
                                return (x:xs)
                            }
-}

-- Parsing string
match :: String -> Parser String
match []        = return []
match (x:xs)    = do
                    y <- token x;
                    ys <- match xs;
                    return (y:ys)

-- parsing a sequence
-- match one or more occurence
plus :: Parser a -> Parser [a]
plus p = do {
            x <- p;
            xs <- star p;
            return (x:xs)
        }
-- match Zero or more occurence
star :: Parser a -> Parser [a]
star p = plus p `mplus` return []

{- example
apply (star (spot isDigit)) "123ab" = [("123", "ab"), ("12", "3ab"), ("1", "23ab"), ("", "123ab")]
-}

-- match a natural number
parseNat :: Parser Int
parseNat = do {
            s <- plus (spot isDigit);
            return (read s)
        }

-- match a negative number
parseNeg :: Parser Int
parseNeg = do {
            token '-';
            n <- parseNat;
            return (-n)
        }

-- match an integer
parseInt :: Parser Int
parseInt = parseNat `mplus` parseNeg
