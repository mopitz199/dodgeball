{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import System.Random (randomRIO)
import qualified Data.Text as T

maxNum = 50

data CustomWord = Attempt String | MainWord String deriving Show
data CustomState = CustomState String

instance Eq CustomWord where
    (Attempt word1) == (MainWord word2) = word1 == word2
    (MainWord word1) == (Attempt word2) = word1 == word2
    (Attempt word1) == (Attempt word2) = word1 == word2
    (MainWord word1) == (MainWord word2) = word1 == word2

getLengthCustomWord :: CustomWord -> Int
getLengthCustomWord (Attempt word) = length word
getLengthCustomWord (MainWord word) = length word

getWords :: [String]
getWords = [
    "ask",
    "be",
    "become",
    "begin",
    "call",
    "can",
    "come",
    "could",
    "do",
    "feel",
    "find",
    "get",
    "give",
    "go",
    "have",
    "hear",
    "help",
    "keep",
    "know",
    "leave",
    "let",
    "like",
    "live",
    "look",
    "make",
    "may",
    "mean",
    "might",
    "move",
    "need",
    "play",
    "put",
    "run",
    "say",
    "see",
    "seem",
    "should",
    "show",
    "start",
    "take",
    "talk",
    "tell",
    "think",
    "try",
    "turn",
    "use",
    "want",
    "will",
    "work",
    "would"]


buildStringLogs :: String -> Writer [String] String
buildStringLogs word = writer (word, ["Attempt: " ++ word])


setState :: String -> State CustomState CustomState
setState word = do
  put $ CustomState word
  return $ CustomState word


compareLetter :: Char -> Char -> String
compareLetter letter1 letter2 = if letter1 == letter2 then (letter2:[]) else "_"


compareStrings :: String -> String -> String
compareStrings (letter1:rest1) (letter2:rest2) = (compareLetter letter1 letter2) ++ (compareStrings rest1 rest2)
compareStrings [] [] = ""


compareAttempt :: CustomWord -> CustomWord -> String
compareAttempt (MainWord word) (Attempt attempt) = compareStrings word attempt


setStateWithLog :: CustomWord -> CustomWord -> Writer [String] CustomState
setStateWithLog word attemptWord = do
    let compareResult = compareAttempt word attemptWord
    let state = evalState (setState compareResult) (CustomState "")
    logResult <- buildStringLogs compareResult
    return state


getBool :: (Bool, [String]) -> Bool
getBool (bool, list) = bool


getStateFromLog :: (CustomState, [String]) -> CustomState
getStateFromLog (state, list) = state


getLog :: (CustomState, [String]) -> [String]
getLog (_, list) = list


getReaderLog :: (Bool, [String]) -> [String]
getReaderLog (_, list) = list


printLogs :: [String] -> IO ()
printLogs list = do
    putStrLn $ show list


getState :: State CustomState CustomState
getState = do
  game <- get
  return game


processGuessWordWithLog :: CustomWord -> Reader String (CustomState, [String])
processGuessWordWithLog mainWord = do
    attemptWord <- ask
    let result = runWriter $ setStateWithLog mainWord (Attempt attemptWord)
    return result


isComplete :: CustomState -> Bool
isComplete (CustomState state) = (not) $ elem '_' state 


processGuessWord :: CustomWord -> Reader String (Bool, [String])
processGuessWord mainWord = do
    result <- processGuessWordWithLog mainWord
    let state = evalState getState (getStateFromLog result)
    let complete = isComplete state
    return (complete, getLog result)


guessWord :: CustomWord -> Int -> IO Bool
guessWord mainWord numAttemps = do
    attemptWord <- getLine

    if numAttemps == 0 then return False
    else if (length attemptWord) /= (getLengthCustomWord mainWord) then do
        putStrLn $ "You must insert a word with the same length"
        guessWord mainWord (numAttemps)
    else do
        let result = runReader (processGuessWord mainWord) attemptWord
        let boolResult = getBool result
        let logResult = getReaderLog result
        putStrLn $ show logResult

        if (not)boolResult then do
            putStrLn $ "Wrong attempt, " ++ show numAttemps ++ " left"
            guessWord mainWord (numAttemps - 1)
        else    
            return True


getRandomNumber :: IO (Int)
getRandomNumber = do
    r  <- randomRIO (0,49)
    return r

showFinalMessage :: Bool -> String -> String
showFinalMessage True _ = "You have won!"
showFinalMessage False word = "You have lost, the word was: " ++ word


main :: IO ()
main = do
    randomNumber  <- getRandomNumber
    let words = getWords
    let attempts = 5
    let word = words !! randomNumber
    putStrLn $ "Guess the word with " ++ show (length word) ++ " letters:"
    result <- guessWord (MainWord word) attempts
    putStrLn $ show (showFinalMessage result word)