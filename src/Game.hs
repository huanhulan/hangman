module Game where

import Control.Monad (forever, when)
import Data.List(intersperse)
import System.Exit (exitSuccess)
import Data.Maybe (isJust)

data Puzzle = Puzzle String [Maybe Char] String
--                    [1]        [2]       [3]
-- [1] The word we are trying to guess
-- [2] The characters we've filled in so far
-- [3] The letters we've guessed so far

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = 
    intersperse ' ' (fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle xs (map (const Nothing) xs) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) key = key `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) key = key `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just word) = word

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar guessedHistory) c =
    Puzzle word newFilledInSoFar (reverse (c : guessedHistory))
    where 
      newFilledInSoFar = zipWith (zipper c) word filledInSoFar
      zipper guessed wordChar guessChar = 
      --       [1]     [2]       [3]
      -- [1] The character that the player guessed on this turn
      -- [2] The characters supposed to be guessing
      -- [3] The list that keeps track of the characters the player has guessed so far
        if wordChar == guessed
        then Just wordChar
        else guessChar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
      (_, True) -> do
        putStrLn "You already guessed that\
                  \ character, pick something else!"
        return puzzle
      (True, _) -> do
        putStrLn "This character was in the word,\
                  \ filling in the word accordingly"
        return (fillInCharacter puzzle guess)
      (False, _) -> do
        putStrLn "This character wasn't in the word,\
                  \ try again"
        return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) = 
    when (length guessed > length wordToGuess) $
      do
        putStrLn "You lose!"
        putStrLn $ "The word was: " ++ wordToGuess
        exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    when (all isJust filledInSoFar) $
      do 
        putStrLn "You win!"
        exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _   -> putStrLn "Your guess must be a single character!"