module Pangram (isPangram) where

upperLower = zip ['a'..'z'] ['A'..]

find :: (a -> Bool) -> [a] -> Maybe a
find f [] = Nothing
find f (x:xs) = nextMove (f x) f x xs
  where nextMove True _ x _ = Just x
        nextMove False f _ xs = find f xs

getMatch :: Char -> Maybe (Char, Char)
getMatch c = find (\(a,b) -> c == a || c == b) upperLower

resolveMaybe :: a -> (Maybe (a, a)) -> (a, a)
resolveMaybe _ (Just x) = x
resolveMaybe x Nothing = (x, x)

pickUpper :: a -> (Maybe (a, a)) -> a
pickUpper d m = snd (resolveMaybe d m)

pickLower :: a -> (Maybe (a, a)) -> a
pickLower d m = fst (resolveMaybe d m)

changeCase :: (Char -> Maybe (Char, Char) -> Char) -> [Char] -> [Char]
changeCase f list = map (\x -> (f x (getMatch x))) list

upCase chars = changeCase pickUpper chars

downCase chars = changeCase pickLower chars

isPangram :: String -> Bool
isPangram text = all (`elem` text) alphabet
  where alphabet = ['a'..'z']
