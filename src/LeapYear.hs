module LeapYear (isLeapYear) where
import Data.List

isLeapYear :: Integer -> Bool
isLeapYear year = year `elem` [y | y <- ((fours \\ hundreds) ++ fourhund) ]
    where fours = [0,4..year]
          hundreds = [0,100..year]
          fourhund = [0,400..year]
