{- copy of "lecture18.example03.hs"
handling Propositions

[output]

*Main> 
*Main> read "((~p)&q)" :: Proposition
((~p)&q)
*Main> 

-}

import Data.Char
import Parser

type Name = String
data Proposition = Var Name
                    | F
                    | T
                    | Not Proposition
                    | Proposition :|: Proposition
                    | Proposition :&: Proposition
                    deriving (Eq, Ord)

type Names = [Name]

instance Show Proposition where
    show (Var x)        = x
    show F              = "F"
    show T              = "T"
    show (Not p)        = par ("~" ++ show p)
    show (p :|: q)      = par (show p ++ "|" ++ show q)
    show (p :&: q)      = par (show p ++ "&" ++ show q)

par :: String -> String
par s = "(" ++ s ++ ")"

propP, varP, falseP, trueP, notP, orP, andP :: Parser Proposition

propP   = varP  `mplus` falseP `mplus` trueP `mplus`
            notP `mplus` orP `mplus` andP

varP    = do { x <- spot isLower; return (Var [x]) }
falseP  = do { x <- token 'F'; return F }
trueP   = do { x <- token 'T'; return T  }
notP    = parP( do { token '~'; p <- propP; return (Not p)  } )
orP     = parP( do { p <- propP; token '|'; q <- propP; return (p :|: q)  } )
andP    = parP( do { p <- propP; token '&'; q <- propP; return (p :&: q)  } )

parP :: Parser a -> Parser a
parP parser = do { token '('; x <- parser; token ')'; return x }


instance Read Proposition where
    readsPrec _ = apply propP
