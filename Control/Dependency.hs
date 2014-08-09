{-# LANGUAGE GADTs #-}
-- | This module is a glorified wrapper over `Data.Map.Strict.lookup`. It
-- let you define computations in an applicative way that "require" some
-- optional values, defined by an identifier.
--
-- Once defined, it is possible to extract the list of identifiers, and
-- also to evaluate the computation.
--
-- > let computation = ( (+) <$> require "a" <*> require "b" )
-- > > computeRequire M.empty computation :: Maybe Int
-- > Nothing
-- > > computeRequire (M.fromList [("a", 12), ("b", 15)]) computation :: Maybe Int
-- > Just 27
-- > > computeRequire (M.fromList [("a", 12), ("c", 15)]) computation :: Maybe Int
-- > Nothing
--
module Control.Dependency (Require, require, getIdentifiers, computeRequire) where

import Control.Applicative
import qualified Data.Map.Strict as M

-- | The main data type, used to model a computation that requires a list
-- of named parameters (the "identifier"), that are linked to a "content",
-- and that will yield a result of type "a".
data Require identifier content a where
    Require :: identifier -> Require identifier content content
    Pure    :: a          -> Require identifier content a
    Ap      :: Require identifier content (a -> b) -> Require identifier content a -> Require identifier content b

instance Functor (Require identifier content) where
    fmap f = Ap (Pure f)

instance Applicative (Require identifier content) where
    pure  = Pure
    (<*>) = Ap

-- | This operator let you "require" a value in a computation.
require :: identifier -> Require identifier content content
require = Require

-- | Given a computation, extract the list of required identifiers.
getIdentifiers :: Require identifier content a -> [identifier]
getIdentifiers (Require i) = [i]
getIdentifiers (Pure _)    = []
getIdentifiers (Ap r1 r2)  = getIdentifiers r1 ++ getIdentifiers r2

-- | Evaluate a computation, given a map of key/values for possible
-- parameters.
computeRequire :: (Ord identifier, Eq identifier, Applicative f, Alternative f) => M.Map identifier content -> Require identifier content a -> f a
computeRequire _ (Pure x) = pure x
computeRequire s (Require i) = maybe empty pure (M.lookup i s)
computeRequire s (Ap r1 r2) = computeRequire s r1 <*> computeRequire s r2

