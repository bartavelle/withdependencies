{-# LANGUAGE ScopedTypeVariables #-}
{- |
Runs computations depending on some values coming from a conduit. The computations are defined in applicative fashion.

> test :: IO [Int]
> test = inp =$ cnd $$ CL.consume
>     where
>         inp = sourceDirectory "/etc"
>         cnd :: Conduit String IO Int
>         cnd = withRequirement comps id (fmap length . readFile)
>         comps :: [Require String Int Int]
>         comps = [ (+) <$> require "/etc/passwd" <*> require "/etc/passwd"
>                 , (-) <$> require "/etc/resolv.conf" <*> require "/etc/nonexistent"
>                 , require "/etc/hosts"
>                 ]
-}

module Data.Conduit.Require (withRequirement) where

import Data.Conduit
import Control.Dependency
import qualified Data.Map.Strict as M
import Data.Monoid
import Control.Monad
import Control.Monad.Trans

-- | Given a stream of values, from which an identifier and a content can
-- be extracted, runs a list of computation that depend on these.
--
-- Each computation can only be run once, and its output is `yield`ed
-- downstream.
--
-- When all computations have been run, the conduit finishes processing.
withRequirement :: (Ord identifier, Eq identifier, Monad m)
                => [Require identifier content x] -- ^ The list of dependent computations
                -> (a -> identifier)              -- ^ Extracting the identifier
                -> (a -> m content)               -- ^ Extracting the content, possibly with effects
                -> Conduit a m x
withRequirement computations getIdentifier getContent = run compmap imap getIdentifier getContent (fmap (const mempty) compmap)
    where
        compmap = M.fromList (zip [0..] computations)
        imap = M.fromListWith (++) $ do
            (n, c) <- M.toList compmap
            i <- getIdentifiers c
            return (i, [n])

run :: (Ord identifier, Eq identifier, Monad m)
    => M.Map Int (Require identifier content x)
    -> M.Map identifier [Int]
    -> (a -> identifier)
    -> (a -> m content)
    -> M.Map Int (M.Map identifier content)
    -> Conduit a m x
run compmap imap getIdentifier getContent curmap = do
    mi <- await
    case mi of
        Nothing -> return ()
        Just i -> do
            let ident = getIdentifier i
                matchedComputations = M.findWithDefault [] ident imap
                checkComputation (cmap, ccomp) cid =
                    case M.lookup cid cmap of
                        Just cnt -> do
                            extractedContent <- getContent i
                            let ncnt  = M.insert ident extractedContent cnt
                                nmap  = M.insert cid ncnt cmap
                            return $ case M.lookup cid compmap >>= computeRequire ncnt of
                                         Just v -> (M.delete cid cmap, v : ccomp)
                                         Nothing -> (nmap, ccomp)
                        Nothing -> return (cmap, ccomp) -- this should not happen !
            (newmap, comps) <- lift (foldM checkComputation (curmap, []) matchedComputations)
            mapM_ yield comps
            unless (M.null compmap) $ run compmap imap getIdentifier getContent newmap

