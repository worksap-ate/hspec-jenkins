module ExampleSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Example

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sub" $ do
    it "subtracts" $
      sub 1 5 == 1 - 5
  describe "add" $ do
    prop "truth" $ True
    prop "adds" $ \n m -> add n m == n + m
  describe "exn&" $ do
    describe "<desc>" $ do
      it "throws IOException" $
        exn `shouldThrow` anyIOException
  describe "foo" $ do
    it "bar" $ do
      pendingWith "baz"
    it "hoge" $ do
      pending
