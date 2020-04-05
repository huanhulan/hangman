module RandomWords where

import System.Random(randomRIO)

type WordList = [String]

getMinWordLength :: [String] -> Maybe Int
getMinWordLength []   = Nothing
getMinWordLength list = Just $ minimum (fmap length list)

getMaxWordLength :: [String] -> Maybe Int
getMaxWordLength []   = Nothing
getMaxWordLength list = Just $ maximum (fmap length list)


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
      gameLength :: Maybe Int -> Maybe Int -> String -> Bool
      gameLength Nothing _ _ = False
      gameLength _ Nothing _ = False
      gameLength (Just minLen) (Just maxLen) w =
        l >= minLen && l <= maxLen
          where l = length w

randomWord' :: WordList -> IO String
randomWord' [] = return ""
randomWord' wl = do
  randomIndex <- randomRIO (0 , length wl - 1)
  return $ wl !!randomIndex

randomWord :: IO String
randomWord = gameWords >>= randomWord'