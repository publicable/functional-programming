import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 1
-- The steps needed are k+1

-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power1: negative argument"
power1 n 0 = 1
power1 n k = product m
   where m = [n | i <- [1..k]]

-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power2: negative argument"
power2 n 0 = 1
power2 n k | even k = power2 (n*n) (div k 2)
           | otherwise = n * power2 n (k-1) 

{-
  Part 4A
  Test case 1: k is zero
  Test case 2: k is positive
  Test case 3: k is odd
  Test case 4: k is even
  
  The two most interesting test cases are zero and positive k. 
  For the power2 function we would be interested in odd and even k as input.
  Since negative k are not permitted as input they're not interesting.
  As for n any number negative, positive and zero are interesting.
-}

-- Part 4B
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n l == power1 n l) && (power1 n l == power2 n l)
    where l = abs(k) {- since we're only interested in positive k -}

-- Part 4C
test_prop_powers :: [(Integer, Integer)] -> Bool  
test_prop_powers [(n,k)]  = prop_powers n k
test_prop_powers ((n,k):tuples) = prop_powers n k && test_prop_powers tuples


main = do 
  print (test_prop_powers [(n,k) | n <- [-5..5], k <- [0..5]])

  quickCheck prop_powers