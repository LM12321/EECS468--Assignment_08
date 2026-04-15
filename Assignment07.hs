

{-
Program Name: Assignment07.hs

Collaborators: N/A
External Sources: {

}
Author: Logan Whitt
KU ID: 3152587
Creation Date: 04/10/26 -- 04/XX/26

PART 1 --
Description: a haskell file of various functions
Inputs: whatever the user inputs for each function
Outputs: {
    - replicate 
        - returns a list of length n of a given element
    - perfects
        - returns a list of all perfect numbers up to given number
    - find
        - returns the values that have the given key in a list
    - positions
        - returns the indexes for which a key is found (using find)
    - scalarproduct
        - returns the scalar product of two same sized lists
        - using zip
    }
-}

replicate' :: Int -> a -> [a]
replicate' a b = [b | a <- [1..a]]


perfects :: Int -> [Int]
-- x is each number we check
-- y is the factor check (from 1 to x//2)
-- zs is the sum of all factors
perfects n = [x | x <- [1..n], let factors = [y | y <- [1..(x `div` 2)], x `mod` y == 0], x == (sum factors)]


find :: Eq a => a -> [(a, b)] -> [b]
{-
    -iterate through all tuples in list
    -get tuple in list
    -if key == first element
        -add it to list
-}
find key list_of_tuples = [snd tuple| tuple <- list_of_tuples,  key == fst tuple]


positions :: Eq a => a -> [a] -> [Int]
{-
    -create a list of tuples of item and index
        -[(item0, index0), (item1, index1), ... ,(item_n, index_n)]
    - use find with key and said list
    - output will be answer
-}

positions key as = find key (zip as [0 .. length as])


scalarproduct :: [Int] -> [Int] -> Int
{-
    - take in two lists
    - get each pair out of the list (with the same index) using zip
    - multiply their results together and put it into a new list
    - sum that list up
-}
scalarproduct as bs = sum [fst tuple * snd tuple | tuple <- zip as bs]

{-
PART 2 --
Description: a haskell file of various functions for different permutations
of n objects into k boxes
Inputs: whatever the user inputs for each function
Outputs: {
    - dodb 
        - returns the number of distiguishable objects into distiguishable boxes
    - iodb
        - returns the number of indistiguishable objects into distiguishable boxes
    - doib
        - returns the number of distiguishable objects into indistiguishable boxes
    - ioib
        - returns the number of indistiguishable objects into indistiguishable boxes
    }

-}
factorial :: Integer -> Integer
factorial n = product [1..n]

permutation :: Integer -> Integer -> Integer
permutation n r = factorial n `div` factorial (n - r)

combination :: Integer -> Integer -> Integer
combination n r = factorial n `div` ( factorial (n - r) * (factorial r))

-- dodb n k m
-- distiguishable objects into distiguishable boxes
-- n = total number of things
-- k = number of distinguishable boxes
-- m = number of items in each box
dodb :: Integer -> Integer -> Integer -> Integer
dodb n 1 m = combination n m
dodb n k m = (combination n m) * (dodb (n - m) (k - 1) m)


-- iodb n k
-- indistiguishable objects into distiguishable boxes
-- n = number of indistiguishable objects
-- k = number of distinguishable boxes
iodb :: Integer -> Integer -> Integer
iodb n k = combination (n + k - 1) n


-- doib n k
-- distiguishable objects into indistiguishable boxes
-- n = number of distinguishable objects
-- k = number of indistiguishable boxes

stirling :: Integer -> Integer -> Integer
stirling n j = sum [(-1)^i * (combination j i) * (j - i)^n| i <- [0..j-1]] `div` (factorial j)

-- get sum of all stirling numbers from 1 to k 
doib :: Integer -> Integer -> Integer
doib n k = sum [stirling n j| j <- [1..k]]


-- ioib n k 
-- indistiguishable objects into indistiguishable boxes
-- n = number of indistiguishable objects
-- k = number of indistiguishable boxes

ioib :: Integer -> Integer -> Integer
ioib n k | k == 1 || n == 0     = 1
         | k < 1  || n < 0      = 0
         | otherwise            = (ioib (n - k) k) + (ioib n (k - 1))