module Main where

import Test.Hspec
import Data.Conduit
import Data.Conduit.Require
import Control.Dependency
import qualified Data.Conduit.List as CL
import Control.Monad.Identity
import Control.Applicative

runTest :: (Show b, Show a, Ord a) => [a] -> [(RunMode, Require a a b)] -> [b]
runTest inp ana = runIdentity (CL.sourceList inp =$ withRequirement ana id return $$ CL.consume)

main :: IO ()
main = hspec $ do
    describe "once tests" $ do
        let getn n = (Once, require n)
        it "runs once with 1 test" $ do
            runTest [1..] [getn 5] `shouldBe` [5 :: Int]
            runTest [1..] [(Once, require 5 <|> require 2)] `shouldBe` [2 :: Int]
        it "runs once with 2 tests" $ do
            runTest [1..] [getn 5, getn 2, getn 1000] `shouldBe` [2,5,1000 :: Int]
            runTest [1..] [getn 5, getn 2, getn 1000, getn 5] `shouldBe` [2,5,5,1000 :: Int]
            runTest [1..] [getn 5, getn 2, (Once, require 5 <|> require 2)] `shouldBe` [2,2,5 :: Int]
    describe "reset tests" $ do
        it "runs once with 1 test" $ do
            runTest [1..10] [(Reset, snd <$> requireFilter even)] `shouldBe` filter even [1..10 :: Int]
            runTest [1..10] [(Reset, snd <$> (requireFilter even <|> requireFilter odd))] `shouldBe` [1..10 :: Int]
        it "runs once with 2 tests" $ do
            runTest [1..10] [(Reset, snd <$> requireFilter even), (Reset, require 4)] `shouldBe` [2,4,4,6,8,10 :: Int]
            runTest [1..10] [(Reset, snd <$> requireFilter even), (Reset, snd <$> requireFilter odd)] `shouldBe` [1..10 :: Int]
