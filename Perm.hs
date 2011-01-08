{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Perm where

import Prelude
import Control.Applicative
import Data.Foldable

-- A list of effectful computations. Only the final result is visible in the type.
data Linear p a where
  Nil  :: a -> Linear p a
  (:-) :: p b -> Linear p (b -> a) -> Linear p a

infixr 5 :-

-- | Map over the final result type.
instance Functor (Linear p) where
  fmap f (Nil x) = Nil (f x)
  fmap f (p :- ps) = p :- fmap (fmap f) ps

-- 'pure' represents the empty list of computations while '<*>' acts like '++'.
instance Applicative (Linear p) where
  pure = Nil
  Nil g <*> y = fmap g y
  (f :- x) <*> y = f :- (flip <$> x <*> y)

-- | Compute the length of a list of computations.
len :: Linear p a -> Int
len (Nil _)     = 0
len (_ :- xs) = 1 + len xs

-- | Lift a parser, making sure it is run once in each permutation.
once :: p a -> Linear p a
once p = p :- Nil id

-- | Execute the list of computations in order.
flatten :: Applicative p => Linear p a -> p a
flatten (Nil x) = pure x
flatten (p :- ps) = (\x f -> f x) <$> p <*> flatten ps

-- | Build a tree (using <|> for choice) of all permutations of the computations.
perms :: Alternative p => Linear p a -> p a
perms (Nil x) = pure x
perms ps = asum . map permTail . firsts $ ps
  where
    permTail (p :- ps') = flip ($) <$> p <*> perms ps'
    permTail _          = undefined

-- | Extract each element once and make it the head of the list.
firsts :: Linear p a -> [Linear p a]
firsts (Nil _) = []
firsts (p :- ps) = (p :- ps) : map (\ps' -> swap (p :- ps')) (firsts ps)

-- | Swaps the first two elements of the list, if they exist.
swap :: Linear p a -> Linear p a
swap (p0 :- p1 :- ps) = p1 :- p0 :- fmap flip ps
swap ps = ps
