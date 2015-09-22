import Data.List
import System.IO

{-
 - tuple: store a list of multiple different datatypes
 -}

tuple01 = (1, "Random")

bobSmith = ("Bob S", 52)
bobName = fst bobSmith
bobAge = snd bobSmith

--- 2 different list into matched tuples
names = ["alice", "bob", "eve"]
addresses = ["1 Street", "2 Street", "3 Wakowaka"]
namesAndAddresses = zip names addresses

--- haskell04.hs will have let, function onwards
--- time: 28:40
