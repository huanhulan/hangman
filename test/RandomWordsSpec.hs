module RandomWordsSpec where
import SpecHelper
import Test.QuickCheck
import RandomWords
import Data.Maybe(isNothing)

getMinSpec :: Spec
getMinSpec =
    describe "getMinWordLength" $ do
      it "getMinWordLength with ['a', \"ab\", \"abc\" , \"abcd\", \"abcde\"] is 1" $
        getMinWordLength ["a","ab", "abc" , "abcd", "abcde"] `shouldBe` Just 1
      it "getMinWordLength will always bigger or equal than zero" $
        property testGetMin where
          testGetMin [] = isNothing(getMinWordLength [])
          testGetMin x = res >= 0 where Just res = getMinWordLength x

getMaxSpec :: Spec
getMaxSpec =
    describe "getMaxWordLength" $
      it "getMaxWordLength with ['a', \"ab\", \"abc\" , \"abcd\", \"abcde\"] is 5" $
        getMaxWordLength ["a","ab", "abc" , "abcd", "abcde"] `shouldBe` Just 5

spec :: Spec
spec =
  describe "RandomWords" $ do
    getMinSpec
    getMaxSpec

main :: IO ()
main = hspec spec