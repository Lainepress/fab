
import           Data.Sort
import           Test.Tasty
import           Test.Tasty.HUnit


main :: IO ()
main = defaultMain $
  testGroup "Sort Tests"
    [ testCase "foo" $ do
        assertEqual "bar" [1..10] $ sort ([10,9..1] :: [Int])
    ]
