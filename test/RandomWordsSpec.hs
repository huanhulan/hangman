module RandomWordsSpec where
import SpecHelper
import RandomWords

getMinSpec :: Spec
getMinSpec =
    describe "getMinWordLength" $
      it "getMinWordLength with ['a', \"ab\", \"abc\" , \"abcd\", \"abcde\"] is 1" $
        getMinWordLength ["a","ab", "abc" , "abcd", "abcde"] `shouldBe` 1

getMaxSpec :: Spec
getMaxSpec =
    describe "getMaxWordLength" $
      it "getMaxWordLength with ['a', \"ab\", \"abc\" , \"abcd\", \"abcde\"] is 5" $
        getMaxWordLength ["a","ab", "abc" , "abcd", "abcde"] `shouldBe` 5

spec :: Spec
spec =
  describe "RandomWords" $ do
    getMinSpec
    getMaxSpec

main :: IO ()
main = hspec spec