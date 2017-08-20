
### 8a. Defining Type

* code sample used later

```
import Data.List

data Rank = Ace | Two | Three | Four | Five | Six
            | Seven | Eight | Nine | Ten | Jack | Queen | King
            deriving(Eq, Ord, Bounded, Enum, Show, Read)

data Suit = Spades | Hearts | Diamonds | Clubs
            deriving(Eq, Enum, Show, Read)

data Card = Card Rank Suit deriving(Eq, Show, Read)

type Hand = [Card]

--- return True if all consecutive pair satisfy f
allPairs f [] = True
allPairs f [x] = True
allPairs f (x:y:yx) = f x y && allPairs f (y:yx)

--- returns true if a sorted hand contains n sets, each of size at most k
setCount n k hand = let sets = groupBy (\(Card r1 _) (Card r2 _) -> r1 == r2) hand
                    in (length sets == n) && (maximum (map length sets) == k)

--- 5 card hand with four different Ranks, one of them appearing twice
pair = setCount 4 2

-- 5 card hand with 3 different Ranks
twoPair = setCount 3 2

threeOfAKind = setCount 3 3

--- consecutive Rank-ed cards
--- get to use 'pred' here as Rank is an instance of Enum
straight = allPairs (\(Card r1 _) (Card r2 _) -> r1 == pred r2)

--- all have the same suite
flush = allPairs (\(Card _ s1) (Card _ s2) -> s1 == s2)

fullHouse = setCount 2 3

fourOfAKind = setCount 2 4

straightFlush hand = straight hand && flush hand
```

---

* usage

deriving Eq gives equality support and Ord provides an Order
Bounded enables use of minBound, maxBound like calls
Enum lets you make them behave as a series
Show lets you print a Rank and Read enables read a Rank from its String

```
*Main> Two == Three
False
*Main> Two == Two
True
*Main> 
*Main> Two > Three
False
*Main> Two < Three
True
*Main> 
*Main> minBound :: Rank
Ace
*Main> maxBound :: Rank
King
*Main> 
*Main> [Two .. Ten]
[Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten]
*Main>
*Main> Jack :: Rank
Jack
*Main>
*Main> read "Jack" :: Rank
Jack
*Main>
*Main> Card Seven Hearts
Card Seven Hearts
*Main> 
*Main> [Card r s | s <- [Spades .. Clubs], r <- [Ace .. King]]
[Card Ace Spades,Card Two Spades,Card Three Spades,Card Four Spades,Card Five Spades,Card Six Spades,Card Seven Spades,Card Eight Spades,Card Nine Spades,Card Ten Spades,Card Jack Spades,Card Queen Spades,Card King Spades,Card Ace Hearts,Card Two Hearts,Card Three Hearts,Card Four Hearts,Card Five Hearts,Card Six Hearts,Card Seven Hearts,Card Eight Hearts,Card Nine Hearts,Card Ten Hearts,Card Jack Hearts,Card Queen Hearts,Card King Hearts,Card Ace Diamonds,Card Two Diamonds,Card Three Diamonds,Card Four Diamonds,Card Five Diamonds,Card Six Diamonds,Card Seven Diamonds,Card Eight Diamonds,Card Nine Diamonds,Card Ten Diamonds,Card Jack Diamonds,Card Queen Diamonds,Card King Diamonds,Card Ace Clubs,Card Two Clubs,Card Three Clubs,Card Four Clubs,Card Five Clubs,Card Six Clubs,Card Seven Clubs,Card Eight Clubs,Card Nine Clubs,Card Ten Clubs,Card Jack Clubs,Card Queen Clubs,Card King Clubs]
*Main> 
*Main> allPairs (<) [1,2,3,4]
True
*Main> 
*Main> allPairs (<) [1,2,4,3]
False
*Main> 
*Main> group [1,1,1,2,3,3,3,3,4,5,5]
[[1,1,1],[2],[3,3,3,3],[4],[5,5]]
*Main>
```
---

