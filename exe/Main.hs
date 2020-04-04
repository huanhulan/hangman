module Main where

-- import Data.Char (toLower)
import RandomWords(randomWord)
import Game(runGame, freshPuzzle)

main = do
  word <- randomWord
  let puzzle = freshPuzzle word
  runGame puzzle