

{-
Program Name: Assignment08.hs

Collaborators: N/A
External Sources: {

}
Author: Logan Whitt
KU ID: 3152587
Creation Date: 04/24/26 -- 04/XX/26

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


data Op = Add | Sub | Mul | Div | Mod | Exp
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show Exp = "**"

data Token = Num Int | Op Op | LeftParen | RightParen
instance Show Token where 
    show (Num n)    = show n
    show (Op o)     = show o
    show LeftParen  = "("
    show RightParen = ")"

parseTokens :: [Char] -> [Token]
parseTokens [] = []

--operations
parseTokens ('*' : '*' : xs) = Op Exp : parseTokens xs  -- exponent
parseTokens ('%' : xs)  = Op Mod : parseTokens xs   --modulus
parseTokens ('*' : xs) = Op Mul : parseTokens xs  -- multiplication
parseTokens ('/' : xs)  = Op Div : parseTokens xs   --division
parseTokens ('+' : xs) = Op Add : parseTokens xs  -- addition
parseTokens ('-' : xs)  = Op Sub : parseTokens xs   --subtraction

--parentheses
parseTokens ('(' : xs)  = LeftParen : parseTokens xs
parseTokens (')' : xs)  = RightParen : parseTokens xs


--numbers 
parseTokens ('9': xs) = Num 9: parseTokens xs
parseTokens ('8': xs) = Num 8: parseTokens xs
parseTokens ('7': xs) = Num 7: parseTokens xs
parseTokens ('6': xs) = Num 6: parseTokens xs
parseTokens ('5': xs) = Num 5: parseTokens xs
parseTokens ('4': xs) = Num 4: parseTokens xs
parseTokens ('3': xs) = Num 3: parseTokens xs
parseTokens ('2': xs) = Num 2: parseTokens xs
parseTokens ('1': xs) = Num 1: parseTokens xs
parseTokens ('0': xs) = Num 0: parseTokens xs

--space
parseTokens (' ': xs) = parseTokens xs
--error
parseTokens xs = error ("Invalid Characters: unrecognized token starting with " ++ xs)


parseNumbers :: [Token] -> [Token]
{- 
    -have a list of tokens
    - if it's anything but a number (except for minus)
        -ignore it
    - if it's a number
        -continue until other token or end
    - if it's a minus, check if the next sign is a minus
        -if they're both minus, add it to the next number
-}
parseNumbers [] = []
--parseNumbers (Op Sub: Op Sub: Num n: xs) = n : parseNumbers xs
--parseNumbers (Num n: xs) = n : parseNumbers xs
parseNumbers (x: xs) = x : parseNumbers xs


combineDigitsIntoNumber :: [Num] -> Num
combineDigitsIntoNumber [] = 0
combineDigitsIntoNumber (n: ns) = n * 10 ^ length (ns) + combineDigitsIntoNumber ns