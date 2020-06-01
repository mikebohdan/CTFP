{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Product where

import Control.Arrow ((&&&))
import Control.Monad (when)
import Data.List

-- p :: Int -> Int
-- p x = x

-- q :: Int -> Bool
-- q _ = True

p :: (Int, Int, Bool) -> Int
p x = fst $ m x

q :: (Int, Int, Bool) -> Bool
q x = snd $ m x

m (x, _, b) = (x, b)

-- PRODUCT
factorizer :: (c -> a) -> (c -> b) -> c -> (a, b)
factorizer p q x = (p x, q x)


-- COPRODUCT

-- i :: a -> c
-- j :: b -> c
-- i' = m . i
-- j' = m . j

cofactorizer :: (a -> c) -> (b -> c) -> Either a b -> c
cofactorizer i j = \case
  Left a -> i a
  Right b -> j b

-- Void === 0
-- () === 1
-- Either a b = a + b
-- (a, b) == a * b
-- Bool === 2
-- 1 + a ~= data Maybe a = Nothing | Just a

-- false == Void
-- true == ()
-- a || b == Either a b
-- a && b == (a, b)

-- Maybe a ~= Either () a isomorphism 

m2e :: Maybe a -> Either () a
m2e Nothing = Left ()
m2e (Just x) = Right x

e2m :: Either () a -> Maybe a
e2m (Right x) = Just x
e2m _ = Nothing


data Shape
  = Circle Float
  | Rect Float Float
  | Sq Float

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y
area (Sq l) = l ^ 2

circ :: Shape -> Float
circ (Circle r) = 2 * pi * r
circ (Rect l h) = 2 * (l + h)
circ (Sq l) = 4 * l


-- a + a = 2 * a
-- a + a = Either a a = Left a | Right a
-- 2 * a = (Bool, a) = (False, a) | (True, a)

-- Functoir Laws
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

-- fmap id (Reader a) ~= fmap id r = id . r = r
-- id r = r

-- fmap (f . g) r = (f . g) . r = f . g . r
-- fmap f . fmap g = \x = fmap f (fmap g x) ;; def composition 
-- (\x -> fmap f (fmap g x)) r = fmap f (fmap g r) = fmap f (g . r) = f . (g . r) = f . g . r


-- fmap id xs = id xs
-- fmap id (x:xs) = id (x:xs)
-- id x : fmap id xs = id x : id xs
-- x : fmap id xs = x : id xs
-- fmap id xs = id xs

-- fmap (f . g) xs = fmap f . fmap g $ xs
-- fmap (f . g) (x: xs) = ((f. g) x) : fmap (f . g) xs = f (g x) : fmap (f . g) xs
-- fmap f . fmap g $ (x : xs) = fmap f (fmap g (x : xs)) = fmap f ((g x) : fmap g xs)
-- = f (g x) : fmap f (fmap g xs)
-- f (g x) === f (g x)

class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
  bimap g h = first g . second h
  first :: (a -> c) -> f a b -> f c b
  first g = bimap g id
  second ::  (b -> d) -> f a b -> f a d
  second h = bimap id h
  {-# MINIMAL bimap | first, second #-}

instance Bifunctor (,) where
  bimap g h (x, y) = (g x, h y)

instance Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ f (Right x) = Right $ f x

newtype BiComp bf fu gu a b = BiComp (bf (fu a) (gu b))

instance (Bifunctor bf, Functor fu, Functor gu)
  => Bifunctor (BiComp bf fu gu) where
    bimap f g (BiComp x) = BiComp (bimap (fmap f) (fmap g) x)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq, Functor)


newtype Op r a = Op (a -> r)

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

instance Contravariant (Op r) where
  -- contramap :: (b -> a) -> (a -> r) -> (b -> r)
  contramap f (Op x) = Op $ x . f


class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  rmap :: (c -> d) -> p a c -> p a d
  rmap = dimap id
  {-# MINIMAL dimap | lmap, rmap #-}


instance Profunctor (->) where
  -- dimap :: (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
  dimap g h f = h . f . g 
  lmap = flip (.)
  rmap = (.)


data Pair a b = Pair a b

instance Bifunctor Pair where
  bimap :: (a -> b) -> (c -> d) -> Pair a c -> Pair b d
  bimap fab fcd (Pair xa xc) = Pair (fab xa) (fcd xc)
  first :: (a -> b) -> Pair a c -> Pair b c
  first f (Pair a c) = Pair (f a) c
  second :: (a -> b) -> Pair c a -> Pair c b
  second f (Pair c a) = Pair c $ f a

newtype Const a b = Const a
newtype Identity a = Identity a

type Maybe' a = Either (Const () a) (Identity a)

m2m' :: Maybe a -> Maybe' a
m2m' (Just x) = Right $ Identity x
m2m' Nothing = Left $ Const ()

m'2m :: Maybe' a -> Maybe a
m'2m (Right (Identity x)) = Just x
m'2m _ = Nothing

-- data List a = Nil | Cons a (List a)

data PreList a b = Nil | Cons a b

instance Bifunctor PreList where
  bimap f g (Cons a b) = Cons (f a) (g b)
  bimap _ _ Nil = Nil

newtype K2 c a b = K2 c

instance Bifunctor (K2 c) where
  bimap _ _ (K2 c) = K2 c

newtype Fst a b = Fst a

instance Bifunctor Fst where
  bimap f _ (Fst x) = Fst $ f x

newtype Snd a b = Snd b

instance Bifunctor Snd where
  bimap _ f (Snd x) = Snd $ f x

-- sum :: [a] -> a
-- sum :: (a -> a -> a) -> [a] -> a
-- sum :: (a -> a -> a) -> a -> [a] -> a
-- sum :: ((a, a) -> a) -> a -> [a] -> a
-- sum :: ((a -> a, a -> a)) -> a -> [a] -> a || a ^ a * a ^ a = a ^ (a + a)
-- sum :: (Either a a -> a) -> a -> [a] -> a
-- sum :: (Either a a -> a, a) -> [a] -> a

foo :: [[a]] -> [a]
foo xs = xs !! go (zip [0..] xs) where
  go ixs = case filter (null . snd) ixs of
    [] -> go $ fmap (fmap tail) ixs
    ((i,_):_) -> i


flipOrder :: Ordering -> Ordering
flipOrder GT = LT
flipOrder LT = GT
flipOrder EQ = EQ

who :: [Int] -> [Int] -> Int -> [Int]
who initWeights specWeights n 
  = fmap fst
  . take n
  . sort'
  . sortOn fst
  . fmap addSpec
  . zip (cycle specWeights)
  . sort'
  . zip [1..]
  $ initWeights
  where
    sort' = sortBy cmp
    addSpec (sw, (i, w)) = (i, sw + w)
    cmp (_, w1) (_, w2) = flipOrder $ compare w1 w2


-- [1 4 7]
-- [(1 1) (2 4) (3 7)]
-- [(3 7) (2 4) (1 1)]
-- + [2,5,8,0,0,0,0,0,0,0]
-- [(3 9) (2 9) (3 9)]

far xs = do
  i <- reverse xs
  case filter (< i) xs of
    [] -> return i
    (j:_) -> return $ i - j

baz :: (Ord a, Num a) => [a] -> [a]
baz xs = let xs' = far xs in if xs == xs'
  then xs
  else baz xs'

smallestPossibleSum :: [Integer] -> Integer
smallestPossibleSum = (*) <$> genericLength <*> gcd' where
  gcd' = uncurry gcd . (head &&& last) . sort