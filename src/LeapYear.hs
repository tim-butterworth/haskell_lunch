module LeapYear (isLeapYear) where

isN x n = (mod x n) == 0

f4 (_, x) = ((isN x 4), x)
f100 (b, x) = (b && (not (isN x 100)), x)
f400 (b, x) = (b || (isN x 400), x)

invoker f accume = (f accume)

isLeapYear :: Integer -> Bool
isLeapYear year = fst (foldr invoker (True, year) [f400, f100, f4])
