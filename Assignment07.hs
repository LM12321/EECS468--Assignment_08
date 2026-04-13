{-

Program Name: Assignment07.hs

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

Collaborators: N/A
External Sources: {

}
Author: Logan Whitt
KU ID: 3152587
Creation Date: 04/10/26 -- 04/XX/26

-}

replicate' :: Int -> a -> [a]
replicate' a b = [b | a <- [1..a]]

perfects :: Int -> [Int]
perfects n = [x if x == sum zs | x <- [1..n], y <- [1..x], zs <- x `mod` y == 0]
