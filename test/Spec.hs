import Data.Char(isLower, isAlphaNum)
import Data.Foldable
import Test.Hspec
import qualified Tf
import qualified Idf

exampleText1 :: String
exampleText1 = "This is another example, another example, example."

exampleText2 :: String
exampleText2 = "This is a sample A."

roundTo :: Int -> Float -> Float
roundTo n x = fromIntegral (round (x * 10^n)) / 10^n

main :: IO ()
main = hspec $ do
  describe "tf(t,d)" $ do
    let docs = (foldMap Tf.newBag $ Tf.tokenize exampleText1)
    it ("The term frequency of `this` in [" ++ exampleText1 ++ "] is 0.14") $ do
      let termFreq1 = Tf.tf "this" docs
      roundTo 2 termFreq1 `shouldBe` (0.14 :: Float)

    it ("The term frequency of `yourmom` in [" ++ exampleText1 ++"] is 0") $ do
      let termFreq2 = Tf.tf "yourmom" docs
      roundTo 2 termFreq2 `shouldBe` (0.00 :: Float)

  describe "idf(t,D)" $ do
    it ("The idf of `this` in [exampleText1, exampleText2] is 0") $ do
      Idf.idf "this" (Tf.newDocuments [exampleText1, exampleText2]) `shouldBe` (0.00 :: Float)

    it ("The idf of `example` in [exampleText1, exampleText2] is 0.405") $ do
      roundTo 3 (Idf.idf "example" (Tf.newDocuments [exampleText1, exampleText2])) `shouldBe` (0.405 :: Float)
