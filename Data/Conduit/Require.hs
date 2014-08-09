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
import qualified Data.Set as S
import Data.Monoid
import Control.Monad
import Control.Monad.Trans

-- | Given a stream of values, from which an identifier and a content can
-- be extracted (for example a)
--
withRequirement :: (Ord identifier, Eq identifier, Monad m)
                => [Require identifier content x] -- ^ The list of dependent computations
                -> (a -> identifier)              -- ^ Extracting the identifier
                -> (a -> m content)               -- ^ Extracting the content
                -> Conduit a m x
withRequirement computations getIdentifier getContent = run compmap imap getIdentifier getContent (fmap initialState compmap)
    where
        compmap = M.fromList (zip [0..] computations)
        imap = M.fromListWith (++) $ do
            (n, c) <- M.toList compmap
            i <- getIdentifiers c
            return (i, [n])

        -- run :: M.Map Int (S.Set identifier, M.Map identifier content) -> Conduit a m x
run :: (Ord identifier, Eq identifier, Monad m)
    => M.Map Int (Require identifier content x)
    -> M.Map identifier [Int]
    -> (a -> identifier)
    -> (a -> m content)
    -> M.Map Int (S.Set identifier, M.Map identifier content)
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
                        Just (todo, cnt) -> do
                            extractedContent <- getContent i
                            let ntodo = S.delete ident todo
                                ncnt  = M.insert ident extractedContent cnt
                                nmap  = M.insert cid (ntodo, ncnt) cmap
                            return $ if S.null ntodo
                                then case M.lookup cid compmap >>= computeRequire ncnt of
                                         Just v -> (M.delete cid cmap, v : ccomp)
                                         Nothing -> (nmap, ccomp)
                                else (nmap, ccomp)
                        Nothing -> return (cmap, ccomp) -- this should not happen !
            (newmap, comps) <- lift (foldM checkComputation (curmap, []) matchedComputations)
            mapM_ yield comps
            run compmap imap getIdentifier getContent newmap

initialState :: (Ord identifier, Eq identifier) => Require identifier content x -> (S.Set identifier, M.Map identifier content)
initialState r = (S.fromList (getIdentifiers r), mempty)
