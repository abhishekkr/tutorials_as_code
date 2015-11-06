
### 9. Input and Output

Reading from and writing to terminal are side-effects as they can change result of same operation.
Separate purely functional code from IO via Type System. The IO Type constructor produces types with IO actions wrapped around a value.
They can be performed by evaluating them inside a main function.
Special steps are necessary for conversion. Return returns a regular value into an IO action, a '<-' left arrow converts an IO action into a regular value.


* code sample calculating number of words in [enable1.txt](http://norvig.com/ngrams/enable1.txt)

```h9.1.hs
import System.IO

main = do
       handle <- openFile "enable1.txt" ReadMode
       contents <- hGetContents handle
       putStrLn ("There are " ++ show (length (lines contents)) ++ " words.")
       hClose handle
```

> usage

```
± % runhaskell h9.1.hs                                                                                                            !1154
There are 172820 words.
```
---

* printing a random word from word-list

```
import System.IO
import System.Random

main = do
       handle <- openFile "enable1.txt" ReadMode
       contents <- hGetContents handle
       --- getStdGen returns seed for a random number generator wrapped in an IO action
       gen <- getStdGen
       --- map was used to get rid of last char if it comes of as newline
       --- randomR has type spec as can return alphabet and more, return random Value and new seed
       let allWords = map init (lines contents)
           (n, _) = randomR (0, (length allWords) - 1) gen :: (Int, StdGen)
       play (allWords !! n)
       hClose handle

play word = do
            putStrLn ("The word is " ++ word ++ ".")
```

> usage

```
± % runhaskell h9.1.hs
The word is musings.

± % runhaskell h9.1.hs
The word is miscalculates.
```
---

* guess if a letter is in word

```
import System.IO
import System.Random

main = do
       handle <- openFile "enable1.txt" ReadMode
       contents <- hGetContents handle
       gen <- getStdGen
       let allWords = map init (lines contents)
           (n, _) = randomR (0, (length allWords) - 1) gen :: (Int, StdGen)
       play (allWords !! n)
       hClose handle

play word = do
            putStrLn word
            putStrLn "Enter a letter to guess:"
            line <- getLine
            putStrLn (handle (head line) word)
            play word

handle letter word
       | letter `elem` word = "Yup"
       | otherwise          = "Nope"
```

> usage

```
± % runhaskell h9.1.hs
subsaturated
Enter a letter to guess:
s
Yup
subsaturated
Enter a letter to guess:
w
Nope
subsaturated
Enter a letter to guess:
h9.1.hs: <stdin>: hGetLine: end of file

```

---

* full-on Hangman style game

```
import System.IO
import System.Random

main = do
       handle <- openFile "enable1.txt" ReadMode
       contents <- hGetContents handle
       gen <- getStdGen
       let allWords = map init (lines contents)
           (n, _) = randomR (0, (length allWords) - 1) gen :: (Int, StdGen)
           word = allWords !! n
       play word (map (\x -> '_') word)
       hClose handle

play word known = do
            putStrLn known
            putStrLn "Enter a letter to guess:"
            line <- getLine
            play word (handle (head line) word known)

handle letter word known
  | letter `elem` word = zipWith (\w k -> if w == letter then w else k) word known
  | otherwise          = known
```

> usgae

```
± % runhaskell h9.1.hs
__________
Enter a letter to guess:
a
___a_____a
Enter a letter to guess:
i
___a_____a
Enter a letter to guess:
s
s__a_____a
Enter a letter to guess:
t
s__a_____a
Enter a letter to guess:
r
s__a_____a

```
---

try 
* [stanford.edu !! schwarz-evil-hangman](http://nifty.stanford.edu/2011/schwarz-evil-hangman)

---
