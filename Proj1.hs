--  File     : Proj1.hs
--  Author   : YuKun
--  Student ID   : 776991
--  Purpose  : Proj1 project guess card game

module Proj1 (feedback, initialGuess, nextGuess, GameState)  where

import Card
import Data.List

-- By giving guess cards and answer card, program can give the feedback about 1. how many card are same in two cards stack 
-- 2. how many cards in the answer cards have lower rank than the lowest card in the guess cards 3. how many of cards in answer cards have the same rank 
-- as a card in the guess cards 4. how many cards in the answer cards have higher rank than the highest card in the guess cards 5. how many of cards 
-- in the answer cards have the same suit as a card in the guess cards

feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback [] [] =(0,0,0,0,0)
feedback [_] [] =(0,0,0,0,0)
feedback [] [_] =(0,0,0,0,0)
feedback xs ys = let rangeR = findOutRangeRanks xs (findMaxRank ys) (findMinRank ys) in 
   (findSameCard xs ys,rangeR!!0,countSameRanks xs ys,rangeR!!1,countSameSuits xs ys)
   
-- these function are used to help get the feedback of the guess cards
-- used to find the max rank in the cards
findMaxRank :: [Card] -> Rank
findMaxRank [] = error "no element!"
findMaxRank [(Card s1 r1)] = r1
findMaxRank ((Card s1 r1):(Card s2 r2):xs)
  | r1>=r2    = findMaxRank ((Card s1 r1):xs)
  | otherwise = findMaxRank ((Card s2 r2):xs)
  
-- Used to find the min rank in the cards
findMinRank :: [Card] -> Rank
findMinRank []=error "no element!"
findMinRank [(Card s1 r1)] = r1
findMinRank ((Card s1 r1):(Card s2 r2):xs)
  | r1<=r2    = findMinRank ((Card s1 r1):xs)
  | otherwise = findMinRank ((Card s2 r2):xs)

-- Used to find the same suit in two card stacks, each card will be count once, like for "AD 6D" "5D 7S" ,we will state that they have 1 same suit 
countSameSuits :: [Card]->[Card]->Int
countSameSuits [] []=0
countSameSuits [_] []=0
countSameSuits [] [_]=0
countSameSuits xs ys = (length xs)-(length ((map suit xs)\\(map suit ys)))

-- Used to find the same rank in two card stacks, each card will be count once, like for "6S 6D" "6D 7S" ,we will state that they have 1 same rank 
countSameRanks :: [Card]->[Card]->Int
countSameRanks [] []=0
countSameRanks [_] []=0
countSameRanks [] [_]=0
countSameRanks xs ys =(length xs)-(length((map rank xs)\\(map rank ys)))

-- Used to find the out rank range cards in answer cards, which will count how many of cards in the answer cards have higher rank than the highest 
-- card in the guess cards  and how many cards in the answer cards have lower rank than the lowest card in the guess cards
findOutRangeRanks :: [Card] -> Rank -> Rank -> [Int]
findOutRangeRanks [] _ _ = [0,0]
findOutRangeRanks ((Card s1 r1):xs) maxRank minRank 
  | r1 > maxRank = map (\ (a,b) -> a+b)$zip [0,1]$findOutRangeRanks xs maxRank minRank
  | r1 < minRank = map (\ (a,b) -> a+b)$zip [1,0]$findOutRangeRanks xs maxRank minRank
  | otherwise    = findOutRangeRanks xs maxRank minRank
  
-- Used to find the same card in two card stacks , each card will be count once, like for "5D 5D" "5D 7S" ,we will state that they have 1 same card 
findSameCard :: [Card] -> [Card] -> Int
findSameCard [] [] =0
findSameCard [] [_] =0
findSameCard [_] [] =0
findSameCard xs ys = (length xs)-(length (xs\\ys))



-- Give a initial guess according to the number we will guess in the game. To gauranteeing the first guess will cover as many cards as possible,
-- which will reduce the possible guess in the later process, we will separate the cards with (13/(n+1)) distance
-- "fromIntegral" is used to translate the number type for supporting the function "floor"
initialGuess :: Int -> ([Card],GameState)
initialGuess 0 = ([], (GameState []))
initialGuess n = initialGuess' n n where
  initialGuess' 0 _ = ([], (GameState []))
  initialGuess' n total = let step =(total-n+1)*(floor (13/(fromIntegral (total+1))))
                          in (((Card (toEnum (mod (total-n) 4)::Suit) (toEnum (mod step 13)::Rank)):(fst (initialGuess' (n-1) total))), (GameState [] ))

-- Main program for get the next guess according to the feedback and last guess, at the same time, the GameState will be transmitted. We
-- will use function "getGoodPossibleCards" to get the most possible answer. This function will use four arguments, including two guess cards,the feedback 
-- and all guessed cards with its feedback. we will just use the first element of the possible answer to make program more efficiently,
-- instead of storing all the possible cards. We use function "insertData" to ensure that the head element of old guess list have the best feedback state.
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (xs,GameState os) (corC,lo,corR,hi,corS) = let goodPossipleCards = getGoodPossibleCards xs xs (corC,lo,corR,hi,corS) os
                                                     in (head goodPossipleCards,GameState (insertData (xs,(corC,lo,corR,hi,corS)) os))

-- This insert rule aims to compare the new guess with the head element of old guess list,and put the better one in front.
insertData x []=x:[]
insertData (xs,(corC1,lo1,corR1,hi1,corS1)) ((ys,(corC2,lo2,corR2,hi2,corS2)):os)
   | (corC1+corR1+corS1)>(corC2+corR2+corS2) = ((xs,(corC1,lo1,corR1,hi1,corS1)):((ys,(corC2,lo2,corR2,hi2,corS2)):os))
   | otherwise = ((ys,(corC2,lo2,corR2,hi2,corS2)):((xs,(corC1,lo1,corR1,hi1,corS1)):os))

-- In this function, we will recursively check the element from function "getGoodPossibleCards'". The check method is in function "compareState".
-- What we will do is to compare the possible guesses which come from the function "getGoodPossibleCards'" with the old guess data.
-- If we regard the possible guess card as the guess and the old guess as answer, the feedback we get from the function "feedback" should be same as
-- what we get from using function "feedback" on the old guess and the true answer. For example, for the answer "6C JD", if our first guess and 
-- feedback are "6C TD" and (1,0,1,1,2), then our new guess should have 1 same card,1 correct rank,1 higher rank and 2 correct suit with the old guess 
-- which is just like the first feedback. So new guess will in ("6C JD","6C QD","6C KD","6C AD"). In addition, we also need to remove invalid
-- answers from the possible guess. To speed up the process,we will compare the current guess state with the head element of old guess list.
-- If the current guess state is worse than old one, we just exchange them, which mean that 
-- we pretend the better state cards as the current guess and put the worse one in the old guess list.
getGoodPossibleCards [] [] (_,_,_,_,_) []=[]
getGoodPossibleCards [_] [_] (_,_,_,_,_) []=[]
getGoodPossibleCards xs zs (corC,lo,corR,hi,corS) os
  | (not (null os)) && (compareWithOldState (corC,lo,corR,hi,corS) os) = getGoodPossibleCards (fst (head os)) (fst (head os)) (snd (head os)) ((xs,(corC,lo,corR,hi,corS)):(delete (head os) os))
  | (length os)>1 = filter (compareState (fst (head os)) (snd (head os)))$getGoodPossibleCards xs zs (corC,lo,corR,hi,corS) (tail os)
  | (length os)==1 = filter (compareState (fst (head os)) (snd (head os)))
                        $filter (\sel->sel == nub sel)$getGoodPossibleCards' xs zs (corC,lo,corR,hi,corS)
  | otherwise = filter (\sel->sel == nub sel)$getGoodPossibleCards' xs zs (corC,lo,corR,hi,corS)

-- This function is used to compare the current guess state with the head element of old guess list. If the current guess state is worse than 
-- old one, we return True
compareWithOldState (corC,lo,corR,hi,corS) ((xs,(corC1,lo1,corR1,hi1,corS1)):os) = (corC1+corR1+corS1)>(corC+corR+corS)
  
-- This comparing rule which mentioned above.  
compareState xs (corC,lo,corR,hi,corS) ys = (feedback ys xs) == (corC,lo,corR,hi,corS)

-- In this function, I want to remove the possible correct cards recursively (if have) and guess the rest cards' possibility. For example, for the 
-- answer "6C JD", if our first guess and feedback are "6C TD" and (1,0,1,1,2), then we have 1 correct card. Even we can not guarantee which card is correct,
-- we can just pretend "6C" or "TD" is correct card, so the next guess must be like "6C _" or "TD _". The rest guess will come from function
-- "getGoodPossibleCards''". Also, we need to change the state (1,0,1,1,2) to (0,0,0,1,1),and pass it to function "getGoodPossibleCards''"
getGoodPossibleCards' [] [_] (_,_,_,_,_)=[]
getGoodPossibleCards' xs zs (corC,lo,corR,hi,corS)
  | corC>0 = foldl (++) [] [(map (\a->x:a) (getGoodPossibleCards' (delete x xs) zs (corC-1,lo,corR-1,hi,corS-1))) | x <- xs ]
  | otherwise = filter (\sel->sel == nub sel && (compareState' sel corS xs))$getGoodPossibleCards'' xs zs (lo,corR,hi)

-- The compare function is used to gaurantee that the guess of the rest cards will satisfy the state
compareState' xs corS ys = (countSameSuits xs ys) == corS

-- In this function, I will guess the possible cards according to the lower, higher and correct rank state. Firstly, I make the possible right rank
-- card and the rest cards' possible guess in pair, like ("6D",[["7D"],["8D"]]). Then I combine them and return the possible guesses 
-- like [["6D","7D"],["6D","8D"]]. The state is used to reduce possible guess number. After that, if we still have cards needed to be guessed
-- we will use function "getGoodPossibleCards'''" to guess them.
getGoodPossibleCards'' [] [_] (_,_,_)=[]
getGoodPossibleCards'' xs zs (lo,corR,hi)
  | corR>0 = let pairLists = [([(Card (toEnum p::Suit) (rank x))| p<- (delete (fromEnum (suit x)) [0..3])], (getGoodPossibleCards'' (delete x xs) zs (lo,corR-1,hi)))| x <- xs]
             in foldl (++) [] [[(a:b)| a <- (fst c),b <-(snd c)]| c<- pairLists]
  | hi>0 =let pairLists = [([(Card (toEnum p::Suit) (toEnum q::Rank))| q<-[((fromEnum (findMaxRank zs))+1)..12], p<- [0..3]],(getGoodPossibleCards'' (delete x xs) zs (lo,corR,hi-1)))| x <- xs]
          in foldl (++) [] [[(a:b)| a <- (fst c),b <-(snd c)]|c<- pairLists]
  | lo>0 =let pairLists = [([(Card (toEnum p::Suit) (toEnum q::Rank))| q<-[0..((fromEnum (findMinRank zs))-1)], p<- [0..3]],(getGoodPossibleCards'' (delete x xs) zs (lo-1,corR,hi)))| x <- xs]
          in foldl (++) [] [[(a:b)| a <- (fst c),b <-(snd c)]|c<- pairLists]
  | otherwise = filter (\sel->sel == nub sel )$getGoodPossibleCards''' xs zs

-- In this function, we just need to get any possible cards' combination with right number.For example, if we still need to guess one card, then we just need to return 
-- all one card possibilities' list back.
getGoodPossibleCards''' [] [_]=[[]]
getGoodPossibleCards''' xs zs
  | not (null xs) = [(z:y)|z<-[(Card (toEnum p::Suit) (toEnum q::Rank))| q<-(delete (fromEnum (rank (head xs))) [(fromEnum (findMinRank zs))..(fromEnum (findMaxRank zs))]), p<- [0..3]] , y<-(getGoodPossibleCards''' (delete (head xs) xs) zs)]
  | otherwise = [[]] 

-- This is the definition of GameState which contains one data, the guessed data.
type OldData = [([Card],(Int,Int,Int,Int,Int))]
data GameState = GameState OldData
        deriving (Show)