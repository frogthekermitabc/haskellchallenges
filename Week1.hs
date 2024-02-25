module Week1 where

circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h

canDrink :: Int -> Bool
canDrink age = age >= 18

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c = canDrink a && canDrink b && canDrink c

--Question 1
timesTen :: Int -> Int 
timesTen a = a * 10

--Question 2
sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

--Question 3
areaOfCircle :: Float -> Float
areaOfCircle r = pi * r ^ 2

--Question 4
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder r h = pi * r ^ 2 * h -----------------------------

--Question 5
distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt (((y1 - y2)^2) + ((x1 - x2)^2))

--Question 6
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && a /= c && b /= c

--Question 7
divisibleBy :: Int -> Int -> Bool
divisibleBy a b = a `mod` b == 0 

--Question 8
isEven :: Int -> Bool
isEven a = a `mod` 2 == 0

--Question 9
averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral (a + b + c) / 3


applyDiscount :: Float -> Int -> Float
applyDiscount price percent = price * (1 - fromIntegral percent / 100)

--Question 10
absolute :: Int -> Int
absolute a = if a < 0 then -a  else a  -------------------------
