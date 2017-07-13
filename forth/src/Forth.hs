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

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthToken = Num Int | Plus | Minus | Times | Divide | InvalidToken
  deriving (Show, Eq)

data ForthState = Push ForthToken ForthState | Empty
  deriving (Show, Eq)

data ReaderState = Done | Reading | Break
  deriving (Show)

empty :: ForthState
empty = Empty

stackBinaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
stackBinaryOp op (Push (Num a) (Push (Num b) s)) = Right (Push (Num (op b a)) s)
stackBinaryOp _ Empty = Left StackUnderflow
stackBinaryOp _ (Push a Empty) = Left StackUnderflow

divide :: ForthState -> Either ForthError ForthState
divide (Push (Num 0) (Push (Num b) s)) = Left DivisionByZero
divide stack = stackBinaryOp (div) stack

asToken :: [Char] -> ForthToken
asToken ['+'] = Plus
asToken ['-'] = Minus
asToken ['*'] = Times
asToken ['/'] = Divide
asToken lst = if (all (\x -> (elem x ['0'..'9'])) lst) then (Num (read lst :: Int)) else InvalidToken

validChars = ['0'..'9'] ++ ['a'..'z'] ++ ['+', '-', '*', '/']

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
