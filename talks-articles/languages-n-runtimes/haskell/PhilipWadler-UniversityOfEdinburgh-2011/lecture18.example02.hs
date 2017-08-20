module Exp where

import Control.Monad
import Parser
-- import lecture18.example02

data Exp = Lit Int
         | Exp :+: Exp
         | Exp :*: Exp
         deriving (Eq, Show)

evalExp :: Exp -> Int
evalExp (Lit n)     = n
evalExp ( e :+: f)  = evalExp e + evalExp f
evalExp ( e :*: f)  = evalExp e * evalExp f

parseExp :: Parser Exp
parseExp = parseLit `mplus` parseAdd `mplus` parseMul
    where
        parseLit    = do {
                        n <- parseInt;      -- parse an integer 'n'
                        return (Lit n)      -- return Lit n
                    }
        parseAdd    = do {
                        token '(';          -- parse (
                        d <- parseExp;      -- parse an Exp d
                        token '+';          -- parse +
                        e <- parseExp;      -- parse an Exp e
                        token ')';          -- parse )
                        return (d :+: e)
                    }
        parseMul    = do {
                        token '(';          -- parse (
                        d <- parseExp;      -- parse an Exp d
                        token '*';          -- parse *
                        e <- parseExp;      -- parse an Exp e
                        token ')';          -- parse )
                        return (d :*: e)
                    }

-- expressions can be nested, recursion handling that
