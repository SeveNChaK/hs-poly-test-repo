module Part1.Tasks where

import Util(notImplementedYet)

factEndless = 1 : zipWith (*) factEndless [1..]
fact :: Double -> Double
fact i = factEndless !! (round i)

normalize :: Double -> Double
normalize x = x - overflow
  where
    overflow = circle * (fromIntegral $ floor $ x / circle)
    circle = 2 * pi

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sinTaylor normalizedX 0 125
    where normalizedX = normalize x

sinTaylor :: Double -> Double -> Double -> Double
sinTaylor x k m
  | k > m = 0
  | otherwise = ((-1)**(k)) * (x**(2 * k + 1)) / (fact $ 2 * k + 1) + sinTaylor x (k + 1) m

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = cosTaylor normalizedX 0 125
  where
    normalizedX = normalize x

cosTaylor :: Double -> Double -> Double -> Double
cosTaylor x k m
  | k > m = 0
  | otherwise = ((-1)**(k)) * (x**(2 * k)) / (fact $ 2 * k) + cosTaylor x (k + 1) m

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b
    | a == 0 = absB
    | b == 0 = absA
    | otherwise = myGCD (maxAB `mod` minAB) minAB
    where
        absA = abs a
        absB = abs b
        maxAB = max absA absB
        minAB = min absA absB

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isFebruaryCorrectDays :: Integer -> Integer -> Bool
isFebruaryCorrectDays day year
    | day < 1 || day > 29 = False
    | day == 29 = year `mod` 400 == 0 || year `mod` 4 == 0 && year `mod` 100 /= 0
    | otherwise = True

isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
    | year < 1 = False
    | month < 1 || month > 12 = False
    | day < 1 || day > 31 = False
    | month == 2 = isFebruaryCorrectDays day year
    | otherwise = day >= 1 && day <= correctDays
    where
        daysInMonths = [31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        correctDays = daysInMonths !! (fromInteger (month - 1))

powIter a curRes curLvl
    | curLvl == 1 = curRes
    | otherwise = powIter a (curRes * a) (curLvl - 1)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow a lvl = case lvl of
    0 -> 1
    1 -> a
    otherwise -> powIter a (a * a) (lvl - 1)

isModZero a divider
    | a `mod` divider == 0 = False
    | divider > (a `div` 2 + 1) = True
    | otherwise = isModZero a (divider + 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime a
    | a == 1 = True
    | a == 2 = True
    | otherwise = isModZero a 2

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = 0.5 * abs (left - right)
    where
        left = gaussLeft points 0 1
        right = gaussRight points 1 0

gaussLeft :: [Point2D] -> Int -> Int -> Double
gaussLeft points xi yi
    | xi == length points = 0
    | yi == length points = gaussLeft points xi 0
    | otherwise = (fst (points !! xi)) * (snd (points !! yi)) + gaussLeft points (xi + 1) (yi + 1)

gaussRight :: [Point2D] -> Int -> Int -> Double
gaussRight points xi yi
    | yi == length points = 0
    | xi == length points = gaussRight points 0 yi
    | otherwise = (fst (points !! xi)) * (snd (points !! yi)) + gaussRight points (xi + 1) (yi + 1)

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он остроугольный
--  1, если он прямоугольный
--  2, если он тупоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | a + b <= c || a + c <= b || b + c <= a = -1
    | a >= b && a >= c = triangleKind' a b c
    | b >= a && b >= c = triangleKind' b a c
    | c >= a && c >= b = triangleKind' c a b
    | otherwise = -1

triangleKind' :: Double -> Double -> Double -> Integer
triangleKind' a b c
    | a * a < b * b + c * c = 0
    | a * a == b * b + c * c = 1
    | a * a > b * b + c * c = 2
    | otherwise = -1
