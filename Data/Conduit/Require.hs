{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{- |
Runs computations depending on some values coming from a conduit. The computations are defined in applicative fashion.

> test :: IO [Int]
> test = inp =$ cnd $$ CL.consume
>     where
>         inp = sourceDirectory "/etc"
>         cnd :: Conduit String IO Int
>         cnd = withRequirement (map Once comps) id (fmap length . readFile)
>         comps :: [Require String Int Int]
>         comps = [ (+) <$> require "/etc/passwd" <*> require "/etc/passwd"
>                 , (-) <$> require "/etc/resolv.conf" <*> require "/etc/nonexistent"
>                 , require "/etc/hosts"
>                 ]
-}

module Data.Conduit.Require (withRequirement, RunMode(..)) where

import Data.Conduit
import Control.Dependency
import Control.Monad
import Control.Monad.Trans

-- | This allows the user to parameter what happends once a requirement is
-- fulfilled.
data RunMode = Reset -- ^ The requirement will be reset, and can be run multiple times
             | Once -- ^ The requirement can only run once
             deriving Show

-- | Given a stream of values, from which an identifier and a content can
-- be extracted, runs a list of computation that depend on these.
--
-- Each computation's output is `yield`ed downstream.
--
-- When all computations have been run, the conduit finishes processing.
withRequirement :: (Ord identifier, Eq identifier, Monad m, Functor m, Show x, Show content, Show identifier, Show a)
                => [(RunMode, Require identifier content x)] -- ^ The list of dependent computations
                -> (a -> identifier)              -- ^ Extracting the identifier
                -> (a -> m content)               -- ^ Extracting the content, possibly with effects
                -> Conduit a m x
withRequirement computations getIdentifier getContent = run getIdentifier getContent compmap
    where
        compmap = [ (rm, req, []) | (rm, req) <- computations ]

run :: (Ord identifier, Eq identifier, Monad m, Functor m, Show a, Show x, Show content, Show identifier)
    => (a -> identifier)
    -> (a -> m content)
    -> [(RunMode, Require identifier content x, [(identifier, content)])]
    -> Conduit a m x
run getIdentifier getContent computationList = do
    mi <- await
    case mi of
        Nothing -> return ()
        Just streamElement -> do
            let ident = getIdentifier streamElement
                tryComputeReq nc (runmode, req, contents) =
                    let previouscomputation = [(runmode, req, ncontents)]
                        droppedcontent = [(runmode, req, [])]
                        ncontents = (ident, nc) : contents
                    in  case (computeRequire ncontents req, runmode) of
                            (Just rs, Once)  -> ([], [rs])
                            (Just rs, Reset) -> (droppedcontent, [rs])
                            (Nothing, _)     -> (previouscomputation, [])
                checkComputation (curCompList, curResults, curContent) requirement@(_,req,_) =
                    if triggersAnalyzer ident req
                        then do
                            nc <- case curContent of
                                      Just x -> return x
                                      _ -> getContent streamElement
                            let (resultingCompList, rcomp) = tryComputeReq nc requirement
                            return (resultingCompList ++ curCompList, rcomp ++ curResults, Just nc)
                        else return (requirement : curCompList, curResults, curContent)
            (newcomputationList, results, _) <- lift (foldM checkComputation ([], [], Nothing) computationList)
            mapM_ yield results
            unless (null newcomputationList) (run getIdentifier getContent newcomputationList)
