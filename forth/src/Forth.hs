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
  | UserDefined [Char]
  deriving (Show, Eq)

data ForthStack = Push ForthToken ForthStack | Empty
  deriving (Show, Eq)

data FunctionSpaceMode = Reading | Writing
  deriving (Show, Eq)

data UserDefinedFunction = Fun (ForthStack -> Either ForthError ForthStack)
  deriving (Show, Eq)

data FunctionSpace = FunSpace ([ForthToken, UserDefinedFunction], FunctionSpaceMode)
  deriving (Show, Eq)

data ForthState = State (ForthStack, FunctionSpace)
  deriving (Show, Eq)
--data ForthState = Push ForthToken ForthState
--  | Empty
--  deriving (Show, Eq)

data ReaderState = Done
  | Reading
  | Break
  deriving (Show)

push :: ForthToken -> ForthStack -> ForthStack
push token stack = Push token stack

pop :: ForthStack -> Maybe (ForthToken, ForthStack)
pop Empty = Nothing
pop (Push token stack) = Just (token, stack)

validChars = ['0'..'9'] ++ ['a'..'z'] ++ ['+', '-', '*', '/'] ++ ['A'..'Z']

hasDepth :: Int -> ForthState -> Bool
hasDepth 0 _ = True
hasDepth v Empty = False
hasDepth v (Push _ s) = (hasDepth (v-1) s)

sufficientlyDeep :: Int -> ForthState -> Either ForthError ForthState 
sufficientlyDeep v stack = successOrFailure (hasDepth v stack) stack
  where successOrFailure True s = Right s
        successOrFailure False _ = Left StackUnderflow

nonZeroDivisor :: ForthState -> Either ForthError ForthState
nonZeroDivisor (Push (Num 0) (Push (Num b) s)) = Left DivisionByZero
nonZeroDivisor stack = Right stack

stackBinaryOp :: (Int -> Int -> Int) -> ForthState -> ForthState
stackBinaryOp op (Push (Num a) (Push (Num b) s)) = (Push (Num (op b a)) s)

divide :: ForthState -> ForthState
divide stack = stackBinaryOp (div) stack
 
duplicateLast :: ForthState -> Either ForthError ForthState
duplicateLast Empty = Left StackUnderflow
duplicateLast stack =
  let (Push token s) = stack
  in Right (Push token stack)

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
  where findMatch "+" = Plus
        findMatch "-" = Minus
        findMatch "*" = Times
        findMatch "/" = Divide
        findMatch "dup" = Dup
        findMatch "drop" = Drop
        findMatch "swap" = Swap
        findMatch "over" = Over
        findMatch lst =
          if (all (\x -> (elem x ['0'..'9'])) lst)
          then (Num (read lst :: Int))
          else UserDefined lst

mergeReads :: [Char] -> Maybe (Char, Text) -> ([Char], Maybe Text, ReaderState)
mergeReads accume Nothing = (accume, Nothing, Done)
mergeReads accume (Just (c, txt)) = if (elem c validChars)
                                    then (accume ++ [c], (Just txt), Reading)
                                    else (accume, (Just txt), Break)

nextToken :: Text -> ([Char], (Maybe Text))
nextToken txt = extractToken ([], (Just txt), Reading)
  where extractToken (accume, _, Done) = (accume, Nothing)
        extractToken (accume, txt, Break) = (accume, txt)
        extractToken (accume, (Just txt), Reading) = extractToken (mergeReads accume (uncons txt))

updateStack :: [ForthToken] -> ForthState -> Either ForthError ForthState
updateStack (Plus:ts) stack = (>>=) (fmap (stackBinaryOp (+)) (sufficientlyDeep 2 stack)) (updateStack ts)
updateStack (Minus:ts) stack = (>>=) (fmap (stackBinaryOp (-)) (sufficientlyDeep 2 stack)) (updateStack ts)
updateStack (Times:ts) stack = (>>=) (fmap (stackBinaryOp (*)) (sufficientlyDeep 2 stack)) (updateStack ts)
updateStack (Divide:ts) stack = (>>=) (fmap divide ((>>=) (sufficientlyDeep 2 stack) nonZeroDivisor)) (updateStack ts)
updateStack (Dup:ts) stack = (>>=) (duplicateLast stack) (updateStack ts)
updateStack (Drop:ts) stack = (>>=) (dropLast stack) (updateStack ts)
updateStack (Swap:ts) stack = (>>=) (swapLastTwo stack) (updateStack ts)
updateStack (Over:ts) stack = (>>=) (over stack) (updateStack ts)
updateStack (token:ts) stack = (>>=) (Right (Push token stack)) (updateStack ts)
updateStack [] stack = Right stack

evalText' :: ([Char], (Maybe Text)) -> ForthState -> Either ForthError ForthState
evalText' (str, Nothing) stack = updateStack [(asToken str)] stack
evalText' (str, (Just txt)) stack = (>>=) (updateStack [(asToken str)] stack) (evalText' (nextToken txt))

empty :: ForthState
empty = Empty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText txt stack = evalText' (nextToken txt) stack
  
toList :: ForthState -> [Int]
toList (Push (Num n) stack) = (toList stack) ++ [n]
toList Empty = []
