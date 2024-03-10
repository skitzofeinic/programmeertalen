module Puzzels where

-- 1. length'
length' :: [a] -> Int
length' = foldr (\_ acc -> acc + 1) 0

-- 2. or'
or' :: [Bool] -> Bool
or' = foldr (||) False

-- 3. elem'
elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\y acc -> y == x || acc) False

-- 4. map'
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- 5. plusplus
plusplus :: [a] -> [a] -> [a]
plusplus = foldr (:) 

-- 6. reverseR
reverseR :: [a] -> [a]
reverseR = foldr (\x acc -> acc ++ [x]) []

-- 7. reverseL
reverseL :: [a] -> [a]
reverseL = foldl (flip (:)) []

-- 8. (!!)
(!!) :: [a] -> Int -> a
(!!) xs n = foldl (\acc (index, x) -> if index == n then x else acc) (error "Index out of bounds") (zip [0..] xs)

-- 9. isPalindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 10. fibonacci
fibonacci :: [Integer]
fibonacci = scanl (+) 0 (1:fibonacci)
