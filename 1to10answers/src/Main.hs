-- solutions for:
-- https://wiki.haskell.org/99_questions/1_to_10

module Main (
    main
) where

main :: IO ()
main = print $ removeDuplicates [1,1,1,1,2,1,2,3,45,6,54,3,1,2345,3,21,1]

last' [] = error "empty list"
last' [x] = x
last' (x:xs) = last' xs

lastButOne [] = error "to short list"
lastButOne [x] = error "to short list"
lastButOne [x,y] = x
lastButOne (x:xs) = lastButOne xs

kTh _ [] = error " to short list"
kTh 1 (x:xs) = x
kTh n (x:xs) = kTh (n-1) xs

length' [] = 0
length' (x:xs) = succ $ length' xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

isPalindrome list = list == reverse' list

data NestedList a = Elem a | List [NestedList a]

myFlatten (Elem a) = [a]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeAll x xs

removeAll _ [] = []
removeAll y (x:xs) = if y == x then removeAll y xs else x : removeAll y xs
