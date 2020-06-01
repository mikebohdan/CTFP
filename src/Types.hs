{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Types where

import Control.Concurrent (threadDelay)
import Control.Monad.State (MonadState, runState, get, gets, put, unless)
import Data.Map (Map, (!), member, insert)


data Nat = Zero | Succ Nat

instance Show Nat where
  show n = "Nat " ++ ns where
    ns = show $ fromEnum n

instance Enum Nat where
  toEnum n
    | n <= 0 = Zero
    | otherwise = Succ . toEnum . pred $ n
  fromEnum Zero = 0
  fromEnum (Succ n) = 1 + fromEnum n


one :: Nat
one = toEnum 1

instance Num Nat where
  Zero + y = y
  (Succ x) + y = x + Succ y

  Zero * _ = one
  _ * Zero = one
  x * y = foldr (+) Zero ys where
    ys = replicate (fromEnum x) y


newtype Sum = Sum { getSum :: Nat }
newtype Product = Product { getProduct :: Nat }

instance Semigroup Sum where
  (Sum x) <> (Sum y) = Sum $ x + y

instance Monoid Sum where
  mempty = Sum Zero

instance Semigroup Product where
  (Product x) <> (Product y) = Product $ x * y

instance Monoid Product where
  mempty = Product one

seven :: Nat
seven = toEnum 7

factNat :: Nat -> Nat
factNat Zero = one
factNat x@(Succ x') = x * factNat x'

fact n = product [1..n]

memoize :: (MonadState (Map k a) m, Ord k) => (k -> a) -> k -> m a
memoize f x = do
  s <- get
  unless (member x s) $ put $ insert x (f x) s
  gets (! x)


inc :: Int -> Int
inc x = x + 1

-- memInc = memoize inc


-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- ma >=> mb = \a -> ma a >>= mb

data Optional a = Some a | Nil


(>=>) :: (a -> Optional b) -> (b -> Optional c) -> a -> Optional c
f >=> g = \x -> case f x of
  Some b -> g b
  Nil -> Nil

safeRoot :: (Ord a, Floating a) => a -> Optional a
safeRoot x
  | x >= 0 = Some $ sqrt x
  | otherwise = Nil

safeReciprocal :: (Eq a, Fractional a) => a -> Optional a
safeReciprocal x
  | x /= 0 = Some $ 1 / x
  | otherwise = Nil

safeRootResiprocal :: Double -> Optional Double
safeRootResiprocal = safeReciprocal >=> safeRoot