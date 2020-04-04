module RandomWords where

import System.Random(randomRIO)

type WordList = [String]

getMinWordLength :: [String] -> Int
getMinWordLength list = minimum (fmap length list)

getMaxWordLength :: [String] -> Int
getMaxWordLength list = maximum (fmap length list)


allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

gameWords :: IO WordList
gameWords = 
  do
    aw <- allWords
    return (filter (filterWords aw) aw)
    where
      filterWords list = gameLength (getMinWordLength list) (getMaxWordLength list)
      gameLength :: Int -> Int -> String -> Bool
      gameLength minLen maxLen w =
        l >= minLen && l <= maxLen
          where l = length w

randomWord' :: WordList -> IO String
randomWord' wl = do
  randomIndex <- randomRIO (0 , length wl - 1)
  return $ wl !!randomIndex

randomWord :: IO String
randomWord = gameWords >>= randomWord'