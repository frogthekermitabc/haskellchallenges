import Data.Char
import Week1 (divisibleBy)

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1, m1) (s2, m2)
  | m1 >= m2 = s1
  | otherwise = s2

marks :: [StudentMark] -> [Int]
marks stmks = [mk | (st, mk) <- stmks]

pass :: [StudentMark] -> [String]
pass stmks = [st | (st, mk) <- stmks, mk >= 40]

-- An example list of student marks
testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]

addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [i + j | (i, j) <- pairList]

minAndMax :: Int -> Int -> (Int, Int)
minAndMax x y
  | x <= y = (x, y)
  | otherwise = (y, x)

----------------------------------------------
--Worked Example 1
sumEvenNumbersBetween :: Int -> Int -> Int
sumEvenNumbersBetween x y = sum [i | i <- [x .. y], mod i 2 == 0]
    -- | x > y = 0
    -- | mod x 2 == 0 = x + sumEvenNumbersBetween (x+2) y
    -- | otherwise = x + sumEvenNumbersBetween (x+1) y

--Worked Example 2
averageMark :: [StudentMark] -> Float
averageMark [] = 0
averageMark stmks = fromIntegral sumMarks / fromIntegral numberOfStudents
  where
    sumMarks = sum [mk | (_ , mk) <- stmks]
    numberOfStudents = length stmks

sumMarks :: [StudentMark] -> Int
sumMarks stmks = sum [mk | (_, mk) <- stmks]

numberOfStudents :: [StudentMark] -> Int
numberOfStudents stmks = length stmks

----------------------------------------------

--1
sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

--2
grade :: StudentMark -> Char
grade (_, m)
    | m >= 70 = 'A'
    | m >= 60 = 'B'
    | m >= 50 = 'C'
    | m >= 40 = 'D'
    | otherwise = 'F'

--3
capMark :: StudentMark -> StudentMark
capMark (s, m)
    | m > 40 = (s, 40)
    | otherwise = (s, m)

--4
firstNumbers :: Int -> [Int]
firstNumbers n = [1..n]

--5
firstSquares :: Int -> [Int]
firstSquares n = [i^2 | i <- [1..n]]

--6
capitalise :: String -> String
capitalise str = [toUpper i | i <- str]

--7
onlyDigits :: String -> String
onlyDigits str = [i | i <- str, isDigit i]

--8
capMarks :: [StudentMark] -> [StudentMark]
capMarks stmks = [(s, min m 40) | (s, m) <- stmks]

--9
gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents stmks = [(s, grade (s, m)) | (s, m) <- stmks]

--10
duplicate :: String -> Int -> String
duplicate str n = concat [str | _ <- [1..n]]

--11
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

--12
isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2

--13
split :: [(a, b)] -> ([a], [b])
