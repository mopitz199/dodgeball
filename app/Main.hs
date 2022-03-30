{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import System.Random (randomRIO)
import qualified Data.Text as T

data CustomWord = Attempt String | WordToGuess String deriving Show
data CustomState = CustomState String

instance Eq CustomWord where
    (Attempt word1) == (WordToGuess word2) = word1 == word2
    (WordToGuess word1) == (Attempt word2) = word1 == word2
    (Attempt word1) == (Attempt word2) = word1 == word2
    (WordToGuess word1) == (WordToGuess word2) = word1 == word2


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

-- Get functions
getRandomNumber :: IO (Int)
getRandomNumber = do
    r  <- randomRIO (0,49)
    return r

getLengthCustomWord :: CustomWord -> Int
getLengthCustomWord (Attempt word) = length word
getLengthCustomWord (WordToGuess word) = length word

getBool :: (Bool, [String]) -> Bool
getBool (bool, list) = bool

getStateFromLog :: (CustomState, [String]) -> CustomState
getStateFromLog (state, list) = state

getLog :: (CustomState, [String]) -> [String]
getLog (_, list) = list

getReaderLog :: (Bool, [String]) -> [String]
getReaderLog (_, list) = list

getState :: State CustomState CustomState
getState = do
  game <- get
  return game

-- Set functions
setState :: String -> State CustomState CustomState
setState word = do
  put $ CustomState word
  return $ CustomState word


-- Other functions
buildStringLogs :: String -> Writer [String] String
buildStringLogs word = writer (word, ["Attempt: " ++ word])

compareLetter :: Char -> Char -> String
compareLetter letter1 letter2 = if letter1 == letter2 then (letter2:[]) else "_"

compareStrings :: String -> String -> String
compareStrings (letter1:rest1) (letter2:rest2) = (compareLetter letter1 letter2) ++ (compareStrings rest1 rest2)
compareStrings [] [] = ""

compareAttemptWord :: CustomWord -> CustomWord -> String
compareAttemptWord (WordToGuess word) (Attempt attempt) = compareStrings word attempt

verifyAttemptWord :: CustomWord -> CustomWord -> Writer [String] CustomState
verifyAttemptWord wordToGuess attemptWord = do
    let compareResult = compareAttemptWord wordToGuess attemptWord
    let state = evalState (setState compareResult) (CustomState "")
    logResult <- buildStringLogs compareResult
    return state

verifyAttemptWordWithLog :: CustomWord -> Reader String (CustomState, [String])
verifyAttemptWordWithLog wordToGuess = do
    attemptWord <- ask
    let result = runWriter $ verifyAttemptWord wordToGuess (Attempt attemptWord)
    return result

isComplete :: CustomState -> Bool
isComplete (CustomState state) = (not) $ elem '_' state 

processVerifyAttemptWord :: CustomWord -> Reader String (Bool, [String])
processVerifyAttemptWord wordToGuess = do
    result <- verifyAttemptWordWithLog wordToGuess
    let state = evalState getState (getStateFromLog result)
    let complete = isComplete state
    return (complete, getLog result)

guessWord :: CustomWord -> Int -> IO Bool
guessWord wordToGuess numberOfAttempts = do
    attemptWord <- getLine

    if numberOfAttempts == 0 then return False
    else if (length attemptWord) /= (getLengthCustomWord wordToGuess) then do
        putStrLn $ "You must insert a word with the same length"
        guessWord wordToGuess numberOfAttempts
    else do
        let result = runReader (processVerifyAttemptWord wordToGuess) attemptWord
        let boolResult = getBool result
        let logResult = getReaderLog result
        putStrLn $ show logResult

        if (not)boolResult then do
            putStrLn $ "Wrong attempt, " ++ show numberOfAttempts ++ " left"
            guessWord wordToGuess (numberOfAttempts - 1)
        else    
            return True

showFinalMessage :: Bool -> String -> String
showFinalMessage True _ = "You have won!"
showFinalMessage False word = "You have lost, the word was: " ++ word

main :: IO ()
main = do
    randomNumber  <- getRandomNumber
    let words = getWords
    let numberOfAttempts = 5
    let wordToGuess = words !! randomNumber
    putStrLn $ "Guess the word with " ++ show (length wordToGuess) ++ " letters:"
    result <- guessWord (WordToGuess wordToGuess) numberOfAttempts
    putStrLn $ show (showFinalMessage result wordToGuess)