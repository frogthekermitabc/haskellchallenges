-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||), (&&), gcd)

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True = True
False || True = True
True || False = True
False || False = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a

fact :: Int -> Int
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)
  | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
  | n == 0 = 0
  | n > 0 = m + mult (n - 1) m
  | otherwise = - mult (- n) m

divide :: Int -> Int -> Int
divide n m
  | n < m = 0
  | otherwise = 1 + divide (n - m) m

-- nor :: Bool -> Bool -> Bool
-- nor False False = True
-- nor False True = False
-- nor True False = False
-- nor True True = False

-- nor :: Bool -> Bool -> Bool
-- nor False x = not x
-- nor True False = False
-- nor True True = False

-- We can use a wildcard, "_" (undercsore) to demonstarte that the second input does not matter when the first one is True:
-- nor :: Bool -> Bool -> Bool
-- nor False x = not x
-- nor True = False

----------------------------------------------------------------

fibonacci :: Int -> Int
--fibonacci n 
    -- | n == 0 = 0
    -- | n == 1 = 1
    -- | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n =  fibonacci (n - 1) + fibonacci (n - 2)

----------------------------------------------------------------

-- Question 1
(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False


-- Question 2
ex0r :: Bool -> Bool -> Bool
ex0r False False = False
ex0r False True = True
ex0r True False = True
ex0r True True = False

--Question 3
ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x _ = x;
ifThenElse False _ y = y

--Question 4
daysInMonth :: Int -> Int
daysInMonth 1 = 31
daysInMonth 2 = 28
daysInMonth 3 = 31
daysInMonth 4 = 30
daysInMonth 5 = 31
daysInMonth 6 = 30
daysInMonth 7 = 31
daysInMonth 8 = 31
daysInMonth 9 = 30
daysInMonth 10 = 31
daysInMonth 11 = 30
daysInMonth 12 = 31




validDate :: Int -> Int -> Bool
validDate day month = day <= daysInMonth month




--Question 5
sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

-- 11
-- sumNumbers :: Int -> Int
-- sumNumbers n 
--     | n == 0 = 0
--     | otherwise = n + sumNumbers (n - 1)



--Question 6
sumSquare :: Int -> Int
sumSquare 0 = 0
sumSquare n = n^2 + sumSquare (n - 1)

-- 11
-- sumSquare :: Int -> Int
-- sumSquare n
--     | n == 0 = 0
--     | otherwise = n^2 + sumSquare (n - 1)



--Question 7
power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n - 1)

-- 11
-- power :: Int -> Int -> Int
-- power x n
--     | n == 0 = 1
--     | otherwise = x * power x (n - 1)


--Question 8
sumFromTo :: Int -> Int -> Int
sumFromTo x y
    | y < x     = 0
    | x == y    = x
    | otherwise = x + sumFromTo (x + 1) y

--Question 9
gcd :: Int -> Int -> Int
gcd x y
    | x == y = x
    | x > y = gcd (x - y) y
    | otherwise = gcd x (y - x)

--Question 10
intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n 

findRoot :: Int -> Int -> Int
findRoot n s
    | s * s <= n = s
    | otherwise = findRoot n (s - 1)






