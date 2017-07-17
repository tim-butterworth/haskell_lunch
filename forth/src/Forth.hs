{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , nextToken
  , evalText
  , toList
  , empty
  ) where

import Data.Text (Text, uncons, pack)
import Data.Char (toLower)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthToken
  = Num Int
  | Plus
  | Minus
  | Times
  | Divide
  | Dup
  | Drop
  | Swap
  | Over
  | InvalidToken
  deriving (Show, Eq)

data ForthState = Push ForthToken ForthState | Empty
  deriving (Show, Eq)

data ReaderState = Done | Reading | Break
  deriving (Show)

validChars = ['0'..'9'] ++ ['a'..'z'] ++ ['+', '-', '*', '/'] ++ ['A'..'Z']

empty :: ForthState
empty = Empty

hasDepth :: Int -> ForthState -> Bool
hasDepth 0 _ = True
hasDepth v Empty = False
hasDepth v (Push _ s) = (hasDepth (v-1) s)

sufficientlyDeep :: Int -> ForthState -> Either ForthError ForthState 
sufficientlyDeep v stack = successOrFailure (hasDepth v stack) stack
  where successOrFailure True s = Right s
        successOrFailure False _ = Left StackUnderflow

stackBinaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
stackBinaryOp op (Push (Num a) (Push (Num b) s)) = Right (Push (Num (op b a)) s)
stackBinaryOp _ _ = Left StackUnderflow

divide :: ForthState -> Either ForthError ForthState
divide (Push (Num 0) (Push (Num b) s)) = Left DivisionByZero
divide stack = stackBinaryOp (div) stack
 
duplicateLast :: ForthState -> Either ForthError ForthState
duplicateLast Empty = Left StackUnderflow
duplicateLast stack =
  let (Push token s) = stack
  in Right (Push token stack)

-- stackDropLast :: ForthState -> ForthState
-- stackDropLast (Push token remainder) = Right remainder
-- 
-- dropLast :: ForthState -> Either ForthError ForthState
-- dropLast stack = (validStack stack ((flip exceedesMinDepth) ))

dropLast :: ForthState -> Either ForthError ForthState
dropLast (Push token remainder) = Right remainder
dropLast _ = Left StackUnderflow

swapLastTwo :: ForthState -> Either ForthError ForthState
swapLastTwo (Push token1 (Push token2 s)) = Right (Push token2 (Push token1 s))
swapLastTwo _ = Left StackUnderflow

over :: ForthState -> Either ForthError ForthState
over Empty = Left StackUnderflow
over (Push token Empty) = Left StackUnderflow
over stack =
  let (Push _ (Push token s)) = stack
  in Right (Push token stack)
  
asToken :: [Char] -> ForthToken
asToken tokenStr = findMatch (map toLower tokenStr)
  where findMatch ['+'] = Plus
        findMatch ['-'] = Minus
        findMatch ['*'] = Times
        findMatch ['/'] = Divide
        findMatch "dup" = Dup
        findMatch "drop" = Drop
        findMatch "swap" = Swap
        findMatch "over" = Over
        findMatch lst =
          if (all (\x -> (elem x ['0'..'9'])) lst)
          then (Num (read lst :: Int))
          else InvalidToken

update :: [Char] -> Char -> [Char]
update accume v = accume ++ [v]

mergeReads :: [Char] -> Maybe (Char, Text) -> ([Char], Maybe Text, ReaderState)
mergeReads accume Nothing = (accume, Nothing, Done)
mergeReads accume (Just (c, txt)) = if (elem c validChars)
                                    then (accume ++ [c], (Just txt), Reading)
                                    else (accume, (Just txt), Break)

nextToken :: Text -> (ForthToken, (Maybe Text))
nextToken txt = extractToken ([], (Just txt), Reading)
  where extractToken (accume, _, Done) = ((asToken accume), Nothing)
        extractToken (accume, txt, Break) = ((asToken accume), txt)
        extractToken (accume, (Just txt), Reading) = extractToken (mergeReads accume (uncons txt))

updateStack :: ForthToken -> ForthState -> Either ForthError ForthState
updateStack Plus stack = stackBinaryOp (+) stack
updateStack Minus stack = stackBinaryOp (-) stack
updateStack Times stack = stackBinaryOp (*) stack
updateStack Divide stack = divide stack
updateStack Dup stack = duplicateLast stack
updateStack Drop stack = dropLast stack
updateStack Swap stack = swapLastTwo stack
updateStack Over stack = over stack
updateStack token stack = Right (Push token stack)

evalText' :: (ForthToken, (Maybe Text)) -> ForthState -> Either ForthError ForthState
evalText' (token, Nothing) stack = updateStack token stack
evalText' (token, (Just txt)) stack = proceed txt (updateStack token stack)
  where proceed txt (Left e) = Left e
        proceed txt (Right stack) = evalText' (nextToken txt) stack

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText txt stack = evalText' (nextToken txt) stack
  
toList :: ForthState -> [Int]
toList (Push (Num n) stack) = (toList stack) ++ [n]
toList Empty = []

-- (>>=) (Right 3) (\x -> (Right (x + 7))) = Right 10
-- (>>=) (nextToken text) updateStack

-- validate (Either) -> transform
-- validate :: [(Stack -> Either)]
-- 
-- validStack :: ForthState -> (ForthState -> Bool) -> (ForthState -> ForthState) -> Either ForthError ForthState
-- validStack stack validator op =
--   if (validator stack)
--   then Right stack
--   else Left StackUnderflow
