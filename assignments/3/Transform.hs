{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

module Transform
   ( Transform(..)
   , next
   , runTransform
   , evalTransform
   , Tree(..)
   , tFoldl
   , tToListWith
   ) where

import Control.Monad

newtype Transform a b = Transform { getTransform :: (b, a -> a) }
  deriving Functor

instance Applicative (Transform a) where
   pure = return
   (<*>) = liftM2 ($)

-- | Problem 2
instance Monad (Transform a) where
   -- return :: b -> Transform a b
   return b = Transform (b, id)

   -- (>>=) :: Transform a b -> (b -> Transform a c) -> Transform a c
   Transform (b, a) >>= f = Transform (fst(getTransform(f b)), snd(getTransform(f b)).a)


next :: (a -> a) -> Transform a ()
next f = Transform ((), f)

evalTransform :: Transform a b -> b
evalTransform = fst . getTransform

runTransform :: Transform a b -> a -> a
runTransform = snd . getTransform

countedFibonacci :: Int -> Transform Int Int
countedFibonacci 0 = return 0
countedFibonacci 1 = return 1
countedFibonacci n = do
   a <- countedFibonacci (n - 1)
   next (+1)
   b <- countedFibonacci (n - 2)
   return $ a + b

data Tree a = Leaf | Branch (Tree a) a (Tree a)

-- | Problem 3

t = Branch (Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)) 4 (Branch (Branch Leaf 5 Leaf) 6 Leaf)

tFoldl :: (b -> a -> b) -> b -> Tree a -> Transform [a] b
tFoldl func str tree =  Transform (foldl func str (inOrderTraversal tree), \[] -> inOrderTraversal tree)

tToListWith :: (b -> a -> b) -> Tree a -> Transform b [a]
tToListWith func tree = Transform (inOrderTraversal tree, \lt -> foldl func lt (inOrderTraversal tree))

inOrderTraversal :: Tree a -> [a]
inOrderTraversal Leaf = []
inOrderTraversal (Branch l n r) = inOrderTraversal l ++ [n] ++ inOrderTraversal r
