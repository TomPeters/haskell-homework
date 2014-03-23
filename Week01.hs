{-# OPTIONS_GHC -Wall #-}
module Week01 where

-- |
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigits 1
-- [1]
-- >>> toDigitsRev 1234
-- [4,3,2,1]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []

toDigitsRev :: Integer -> [Integer]
toDigitsRev i = reverseDigits (toDigits i)

toDigits :: Integer -> [Integer]
toDigits i
	| i <= 0 = []
	| otherwise = toDigits (i `div` 10) ++ [i `mod` 10]

reverseDigits :: [Integer] -> [Integer]
reverseDigits [] = []
reverseDigits (x:xs) = reverseDigits xs ++ [x]

-- | 
-- >>> doubleEveryOther [8, 7, 6, 5]
-- [16,7,12,5]
-- >>> doubleEveryOther [1, 2, 3]
-- [1,4,3]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverseDigits (doubleEveryOtherFromStart (reverseDigits x))

doubleEveryOtherFromStart :: [Integer] -> [Integer]
doubleEveryOtherFromStart [] = []
doubleEveryOtherFromStart [x] = [x]
doubleEveryOtherFromStart (x:y:zs) = x : (2*y) : doubleEveryOtherFromStart zs

-- |
-- >>> sumDigits [] 
-- 0
-- >>> sumDigits [16] 
-- 7
-- >>> sumDigits [16,7,12,5] 
-- 22

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumDigitsOfSingleNumber x + sumDigits xs

sumDigitsOfSingleNumber :: Integer -> Integer
sumDigitsOfSingleNumber i
	| i <= 0 = 0
	| otherwise = i `mod`10 + sumDigitsOfSingleNumber (i `div` 10)


-- |
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882
-- False
validate :: Integer -> Bool
validate x = getChecksum x `mod` 10 == 0

getChecksum :: Integer -> Integer
getChecksum x = sumDigits (doubleEveryOther (toDigits x))