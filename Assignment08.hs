

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
parseNumbers (Op Sub: Op Sub: Num n: xs) = 
    let (num_list, remaining_tokens) = extractNumberFromList([n], xs)   -- extract the entire number in token list
        number = combineDigitsIntoNumber num_list   -- get that actual number by combining the list together
    in Op Sub : Num (-number) : parseNumbers remaining_tokens    -- set the number to negative because it is
    --also add Op Sub to the top because it's subtraction of a negative number

parseNumbers (Num n: xs) = 
    let (num_list, remaining_tokens) = extractNumberFromList([n], xs)   --same thing as above
        number = combineDigitsIntoNumber num_list
    in Num number : parseNumbers remaining_tokens   --it's not negative

parseNumbers (x : xs) = x : parseNumbers xs --it's not a number, we don't care



extractNumberFromList :: ([Int], [Token]) -> ([Int], [Token])
-- we will reverse nums because that way it ends in proper numeric order
extractNumberFromList (nums, []) = (reverse nums, [])   --out of tokens; reverse
extractNumberFromList (nums, (Num n: tokens)) = extractNumberFromList (n:nums, tokens) -- if next token is number, add it to nums list
    --NUMS WILL BE IN REVESRE NUMERIC ORDER
    --I.E 238 becomes 832, etc.
extractNumberFromList (nums, tokens) = (reverse nums, tokens)   -- end the recursion as there are no more sequential numbers


combineDigitsIntoNumber :: [Int] -> Int
combineDigitsIntoNumber [] = 0  --empty list, add 0
combineDigitsIntoNumber (n: ns) = n * 10 ^ length ns + (combineDigitsIntoNumber ns)  
-- works because length of list will always be -1 what it was, so [1,2,3] becomes
-- 1 * 10 ^length[2,3] = 1 * 10^2 = 100

