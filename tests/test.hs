module Main where

import Test.Hspec
import Data.Conduit
import Data.Conduit.Require
import Control.Dependency
import qualified Data.Conduit.List as CL
import Control.Monad.Identity
import Control.Applicative

runTest :: (Show b, Show a, Ord a) => [a] -> [(RunMode, Require a a b)] -> [b]
runTest inp ana = runIdentity (runConduit (CL.sourceList inp .| withRequirement ana id return .| CL.consume))

main :: IO ()
main = hspec $ do
    describe "once tests" $ do
        let getn n = (Once, require n)
        it "runs once with 1 test" $ do
            runTest [1..] [getn 5] `shouldMatchList` [5 :: Int]
            runTest [1..] [(Once, require 5 <|> require 2)] `shouldMatchList` [2 :: Int]
        it "runs once with 2 tests" $ do
            runTest [1..] [getn 5, getn 2, getn 1000] `shouldMatchList` [2,5,1000 :: Int]
            runTest [1..] [getn 5, getn 2, getn 1000, getn 5] `shouldMatchList` [2,5,5,1000 :: Int]
            runTest [1..] [getn 5, getn 2, (Once, require 5 <|> require 2)] `shouldMatchList` [2,2,5 :: Int]
    describe "reset tests" $ do
        it "runs once with 1 test" $ do
            runTest [1..10] [(Reset, snd <$> requireFilter even)] `shouldMatchList` filter even [1..10 :: Int]
            runTest [1..10] [(Reset, snd <$> (requireFilter even <|> requireFilter odd))] `shouldMatchList` [1..10 :: Int]
        it "runs once with 2 tests" $ do
            runTest [1..10] [(Reset, snd <$> requireFilter even), (Reset, require 4)] `shouldMatchList` [2,4,4,6,8,10 :: Int]
            runTest [1..10] [(Reset, snd <$> requireFilter even), (Reset, snd <$> requireFilter odd)] `shouldMatchList` [1..10 :: Int]
    describe "guard results" $ do
        it "drops the first results" $ do
            runTest [1..] [(Once, guardResult (==3) (require 5) <|> require 7)] `shouldMatchList` [7 :: Int]
        it "drops the second result" $ do
            runTest [1..] [(Once, require 5 <|> guardResult (==3) (require 7))] `shouldMatchList` [5 :: Int]
        it "drops all results" $ do
            runTest [1..100] [(Once, guardResult (==3) (require 5) <|> guardResult (==3) (require 7))] `shouldMatchList` ([] :: [Int])

