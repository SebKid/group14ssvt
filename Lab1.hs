
module Lab1 where
import Data.List
import Test.QuickCheck    



infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Q1

sumOfNat1' :: Integer -> Integer
sumOfNat1' n = sum [n^2 | n <- [1..n]]

sumOfNat1 :: Integer -> Integer 
sumOfNat1 n = (n*(n+1)*(2*n + 1)) `div` 6

testSumOfNat1 :: Integer -> Bool
testSumOfNat1 n = (>0) n --> sumOfNat1 n == sumOfNat1' n 


sumOfNat2' :: Integer -> Integer
sumOfNat2' n = sum [n^3 | n <- [1..n]]

sumOfNat2 :: Integer -> Integer 
sumOfNat2 n = ((n*(n+1)) `div` 2)^2

testSumOfNat2 :: Integer -> Bool
testSumOfNat2 n = (>0) n --> sumOfNat2 n == sumOfNat2' n 

-- Q2

powerSet :: Integer -> Bool
powerSet n = (>0) n --> (2 ^ (length [1..n])) == length (subsequences [1..n])

-- Q4

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

limitedPrimes :: [Integer]
limitedPrimes = filter prime [13..1000] 

reversal :: Integer -> Integer
reversal = read . reverse . show


reversedPrimes :: [Integer]
reversedPrimes = map reversal limitedPrimes

containedPrimes :: [Integer]
containedPrimes = filter prime reversedPrimes

result4:: [Integer]
result4 = map reversal containedPrimes