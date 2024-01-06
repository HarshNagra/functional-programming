{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

module ListMaybe
  ( LM(..)
  , ML(..)
  ) where
import Control.Applicative
import Control.Monad

import Data.Maybe ()
import Data.List

newtype LM a = LM { getLM :: [Maybe a] }
  deriving Functor

instance Applicative LM where
  pure = return
  (<*>) = liftM2 ($)

-- | Problem 4.1
instance Monad LM where
  -- return :: a -> LM a
  return l =  LM [Just l]
  -- (>>=) :: LM a -> (a -> LM b) -> LM b
  LM l >>= f = lMHelp l f

lMHelp:: [Maybe a] -> (a -> LM b) -> LM b
lMHelp [] _ = LM []
lMHelp (Nothing:xs) f = LM (Nothing : getLM (lMHelp xs f))
lMHelp ((Just a):xs) f = LM(getLM(f a) ++ getLM(lMHelp xs f))


newtype ML a = ML { getML :: Maybe [a] }
  deriving Functor

instance Applicative ML where
  pure = return
  (<*>) = liftM2 ($)

-- | Problem 4.2
instance Monad ML where
  -- return :: a -> ML a
  return ml = ML (Just [ml])

  -- (>>=) :: ML a -> (a -> ML b) -> ML b
  ML ml >>= f = case ml of
                  Nothing -> ML Nothing
                  (Just []) -> ML (Just [])
                  (Just l) -> ML (Just (mLHelp (map (getML.f) l)))

mLHelp :: [Maybe [a]] -> [a]
mLHelp [] = []
mLHelp (Just x:xs) = x ++ mLHelp xs

-- instance Monad Maybe where
--   return x = Just x
  
--   Just x  >>= f  = f x
--   Nothing >>= _  = Nothing

-- instance Monad [] where
--   return x = [x]
--   xs >>= f = [y | x<-xs, y<-f x]

