module NaturalTransformations where

import Control.Applicative (Applicative(liftA2))
alphaA :: (Functor f, Functor g) => f a -> g a
alphaA = undefined

-- alphaA
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x


checkNat :: (Eq (g a), Functor f, Functor g) => (a -> a) -> (f a -> g a) -> f a -> Bool
checkNat f alpha = liftA2 (==) fmapAlpha alphaFmap where
  fmapAlpha = fmap f . alpha
  alphaFmap = alpha . fmap f


-- | 10.6 1
-- | checkNat succ maybeList Nothing == True
-- | checkNat succ maybeList $ Just 3 == True
maybeList :: Maybe a -> [a]
maybeList (Just x) = [x]
maybeList Nothing  = []

-- | 10.6 2
newtype Reader r a = Reader (r -> a)

obviousU :: Reader () a -> [a]
obviousU (Reader r) = [r ()]

dumbU1 :: Reader () a -> [()]
dumbU1 _ = [()]

dumbU2 :: Reader () a -> [()]
dumbU2 _ = []


-- | 10.6 3
obviousB :: Reader Bool a -> [a]
obviousB (Reader r) = [r x| x <- [False ..]]

obviousMb :: Maybe a -> [a]
obviousMb (Just x) = [x]
obviousMb Nothing  = []

-- | 10.6 4

rsh :: Reader () a -> Maybe a
rsh = safeHead . obviousU