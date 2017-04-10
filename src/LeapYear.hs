module LeapYear (isLeapYear) where

isN x n = (mod x n) == 0

f4 (_, x) = ((isN x 4), x)
f100 (b, x) = (b && (not (isN x 100)), x)
f400 (b, x) = (b || (isN x 400), x)
identity a = a

isLeapYear :: Integer -> Bool
isLeapYear year = fst ((foldr (.) identity [f400, f100, f4]) (True, year))
