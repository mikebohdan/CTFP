{-# LANGUAGE ExistentialQuantification #-}
module Composition () where

f :: a -> b
f = undefined

g :: b -> c
g = undefined

h :: c -> d
h = undefined

-- h . (g . f) = (h . g) . f = h . g . f
