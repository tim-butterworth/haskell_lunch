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
import Data.List (find)

data ForthError = DivisionByZero | StackUnderflow | InvalidWord | UnknownWord Text
     deriving (Show, Eq)

data ForthToken = Num Int
  | Plus
  | Minus
  | Times
  | Divide
  | StartDefiningFun
  | FinishDefiningFun
  | UserDefined [Char]
  deriving (Show, Eq)

data ForthStack = Push ForthToken ForthStack | Empty
  deriving (Show, Eq)

data ForthMode = Recording ForthStack | Execute
  deriving (Show, Eq)

data Functions = FunSpace [(ForthToken, (Functions -> ForthStack -> (Either ForthError ForthStack)))]

data ForthState = State (ForthStack, Functions, ForthMode)

data ReaderState = Done | Reading | Break
  deriving (Show, Eq)

validChars = ['0'..'9'] ++ ['a'..'z'] ++ ['+', '-', '*', '/', ':', ';'] ++ ['A'..'Z']

hasDepth :: Int -> ForthStack -> Bool
hasDepth 0 _ = True
hasDepth v Empty = False
hasDepth v (Push _ s) = (hasDepth (v-1) s)

sufficientlyDeep :: Int -> ForthStack -> Either ForthError ForthStack
sufficientlyDeep v stack = successOrFailure (hasDepth v stack) stack
  where successOrFailure True s = Right s
        successOrFailure False _ = Left StackUnderflow

nonZeroDivisor :: ForthStack -> Either ForthError ForthStack
nonZeroDivisor (Push (Num 0) (Push (Num b) s)) = Left DivisionByZero
nonZeroDivisor stack = Right stack

stackBinaryOp :: (Int -> Int -> Int) -> ForthStack -> ForthStack
stackBinaryOp op (Push (Num a) (Push (Num b) s)) = (Push (Num (op b a)) s)

divide :: ForthStack -> ForthStack
divide stack = stackBinaryOp (div) stack
 
duplicateLast :: ForthStack -> Either ForthError ForthStack
duplicateLast Empty = Left StackUnderflow
duplicateLast stack =
  let (Push token s) = stack
  in Right (Push token stack)

dropLast :: ForthStack -> Either ForthError ForthStack
dropLast (Push token remainder) = Right remainder
dropLast _ = Left StackUnderflow

swapLastTwo :: ForthStack -> Either ForthError ForthStack
swapLastTwo (Push token1 (Push token2 s)) = Right (Push token2 (Push token1 s))
swapLastTwo _ = Left StackUnderflow

over :: ForthStack -> Either ForthError ForthStack
over Empty = Left StackUnderflow
over (Push token Empty) = Left StackUnderflow
over stack =
  let (Push _ (Push token s)) = stack
  in Right (Push token stack)
  
asToken :: [Char] -> ForthToken
asToken tokenStr = findMatch (map toLower tokenStr)
  where findMatch ":" = StartDefiningFun
        findMatch ";" = FinishDefiningFun        
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

assembleState :: Functions -> ForthMode -> ForthStack -> ForthState
assembleState funs mode stack = State (stack, funs, mode)

applyUserDefinedFunction :: ForthToken -> Functions -> ForthStack -> Either ForthError ForthStack
applyUserDefinedFunction token (FunSpace funs) stack = fromJust ((fmap (applyFun (FunSpace funs) stack)) (fmap (\(name, fun) -> fun) (find (\(name, stack) -> token == name) funs)))
  where applyFun funSp stack fun = fun funSp stack
        fromJust (Just a) = a

updateStack :: ForthToken -> Functions -> ForthStack -> Either ForthError ForthStack
updateStack (Num n) _ stack = Right (Push (Num n) stack)
updateStack userDefined functions stack = (applyUserDefinedFunction userDefined functions stack)

updateRecording :: ForthToken -> ForthStack -> ForthMode
updateRecording token stack = (Recording (Push token stack))

invertStack' :: ForthStack -> ForthStack -> ForthStack
invertStack' (Push top stack) accume = invertStack' stack (Push top accume)
invertStack' Empty accume = accume

invertStack :: ForthStack -> ForthStack
invertStack stack = invertStack' stack Empty

funWrapUserDefined :: ForthStack -> Functions -> ForthStack -> Either ForthError ForthStack
funWrapUserDefined funStack funs stateStack = applyMovesFromStack funs funStack stateStack
  where applyMovesFromStack funSpace (Push token funStack) stack = (>>=) (updateStack token funSpace stack) (applyMovesFromStack funSpace funStack)
        applyMovesFromStack funSpace Empty stack = Right stack

createUserDefinedFun :: ForthStack -> (ForthToken, (Functions -> ForthStack -> (Either ForthError ForthStack)))
createUserDefinedFun stack = (top, (funWrapUserDefined actions))
  where (Push top actions) = invertStack stack

updateUserDefinedFuns :: ForthMode -> Functions -> Either ForthError Functions
updateUserDefinedFuns (Recording stack) (FunSpace funs) = Right (FunSpace ((createUserDefinedFun stack):funs))

updateState :: ForthToken -> ForthState -> Either ForthError ForthState
updateState StartDefiningFun (State (stack, funSp, Execute)) = Right (State (stack, funSp, (Recording Empty)))
updateState FinishDefiningFun (State (stack, funSp, mode)) = fmap ((flip ((flip assembleState) Execute)) stack) (updateUserDefinedFuns mode funSp)
updateState token (State (stack, funSp, (Recording funStack))) = Right (assembleState funSp (updateRecording token funStack) stack)
updateState token (State (stack, funSp, Execute)) = fmap (assembleState funSp Execute) (updateStack token funSp stack)

predefinedFun :: String -> (ForthStack -> Either ForthError ForthStack) -> (ForthToken, (Functions -> ForthStack -> Either ForthError ForthStack))
predefinedFun name fun = ((UserDefined name), (\_ -> fun))

predefinedFuns :: Functions
predefinedFuns = FunSpace [
  (predefinedFun "swap" swapLastTwo)
  ,(predefinedFun "over" over)
  ,(predefinedFun "dup" duplicateLast)
  ,(predefinedFun "drop" dropLast)
  ,(predefinedFun "+" (\stack -> (fmap (stackBinaryOp (+)) (sufficientlyDeep 2 stack))))
  ,(predefinedFun "-" (\stack -> (fmap (stackBinaryOp (-)) (sufficientlyDeep 2 stack))))
  ,(predefinedFun "/" (\stack -> (fmap divide ((>>=) (sufficientlyDeep 2 stack) nonZeroDivisor))))
  ,(predefinedFun "*" (\stack -> (fmap (stackBinaryOp (*)) (sufficientlyDeep 2 stack))))
  ]

empty :: ForthState
empty = State (Empty, predefinedFuns, Execute)

evalText' :: ([Char], (Maybe Text)) -> ForthState -> Either ForthError ForthState
evalText' (str, Nothing) stack = updateState (asToken str) stack
evalText' (str, (Just txt)) stack = (>>=) (updateState (asToken str) stack) (evalText' (nextToken txt))

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText txt state = evalText' (nextToken txt) state

toList' :: ForthStack -> [Int]
toList' (Push (Num n) stack) = (toList' stack) ++ [n]
toList' Empty = []

toList :: ForthState -> [Int]
toList (State (stack, _, _))  = toList' stack
