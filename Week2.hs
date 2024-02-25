module Week2 where


heartMonitor :: Int -> Int -> String
heartMonitor age bpm
    | age > 80 && bpm > 100 = "High heart rate for 81+!"
    | age > 60 && bpm > 130 = "High heart rate for 61-80!"
    | age > 40 && bpm > 140 = "High heart rate for 41-60!"
    | age > 20 && bpm > 155 = "High heart rate for 21-40!"
    | age >= 0 && bpm > 170 = "High heart rate for 0-20!"
    | otherwise = "Normal heart rate"

-- --pizzaCalories :: Int -> String -> Float
-- --pizzaCalories diameter toppings
--     | toppings == "pepperoni" = (11.5 + 6) * pi * (fromIntegral diameter / 2) ^ 2
--     | toppings == "tuna" = (11.5 + 4) * pi * (fromIntegral diameter / 2) ^ 2
--     | toppings == "veggie" = (11.5 + 2.5) * pi * (fromIntegral diameter / 2) ^ 2
--     | otherwise = 11.5 * pi * (fromIntegral diameter / 2) ^ 2

-- pizzaCalories :: Int -> String -> Float
-- pizzaCalories diameter toppings
--     | toppings == "pepperoni" = (11.5 + 6) * area
--     | toppings == "tuna" = (11.5 + 4) * area
--     | toppings == "veggie" = (11.5 + 2.5) * area
--     | otherwise = 11.5 * area
--     where area = pi * (fromIntegral diameter / 2) ^ 2

pizzaCalories :: Int -> String -> Float
pizzaCalories diameter toppings = (11.5 + toppingCalories) * area
    where
        area = pi * (fromIntegral diameter / 2) ^ 2
        toppingCalories
            | toppings == "pepperoni" = 6
            | toppings == "tuna" = 4
            | toppings == "veggie" = 2.5
            | otherwise = 0

--Question 1
absolute :: Int -> Int
absolute a
    | a < 0 = -a 
    | otherwise = a

--Question 2
sign :: Int -> Int
sign a 
    | a < 0 = -1
    | a == 0 = 0
    | otherwise = 1

--Question 3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | a == b && b==c = 3
    | a == b || b == c || a == c = 2
    | otherwise = 0

--Question 4
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths a b c = diagonal a + diagonal b + diagonal c
    where
        diagonal x = sqrt (2 * x ^ 2)

--Question 5
taxiFare :: Int -> Float
taxiFare distance = 2.20 + rest
    where
        rest
            | distance <= 10 = 0.5 * fromIntegral distance
            | otherwise =  5 + (fromIntegral distance - 10) * 0.30


--Question 6
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c
    | f a > avg && f b > avg && f c > avg = 3
    | f a > avg && f b > avg || f b > avg && f c > avg || f a > avg && f c > avg = 2
    | f a > avg || f b > avg || f c > avg = 1
    | otherwise = 0
    where
        avg = fromIntegral (a + b + c) / 3
        f x = fromIntegral x

--Question 7
validDate :: Int -> Int -> Bool
validDate day month
    | month == 1 && day <= 31 = True
    | month == 2 && day <= 28 = True
    | month == 3 && day <= 31 = True
    | month == 4 && day <= 30 = True
    | month == 5 && day <= 31 = True
    | month == 6 && day <= 30 = True
    | month == 7 && day <= 31 = True
    | month == 8 && day <= 31 = True
    | month == 9 && day <= 30 = True
    | month == 10 && day <= 31 = True
    | month == 11 && day <= 30 = True
    | month == 12 && day <= 31 = True
    | otherwise = False

--Question 8
daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | month == 1 = 31
    | month == 2 = if isLeapYear then 29 else 28
    | month == 2 = 28
    | month == 3 = 31
    | month == 4 = 30
    | month == 5 = 31
    | month == 6 = 30
    | month == 7 = 31
    | month == 8 = 31
    | month == 9 = 30
    | month == 10 = 31
    | month == 11 = 30
    | month == 12 = 31
    | otherwise = error "Invalid month"
    where isLeapYear = year `mod` 4 == 0 