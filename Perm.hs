{-# LANGUAGE GADTs #-}

module Perm
  ( Effects(..), length, sequence, swap, firsts, perms

  -- * Lifting computations
  , once

  ) where

import Prelude hiding (length, sequence)
import Control.Applicative
import Data.Foldable

-- | A chain of effectful @p@-computations with final result @a@. Individual
-- computations have their own result types, which fit together in standard
-- 'Applicative' fashion. Although these result types are existentially
-- quantified, the computations can still be moved around within the list
-- (e.g. 'swap', 'firsts'). This allows their permutations to be computed.
data Effects p a where
  Nil  :: a -> Effects p a
  (:-) :: p b -> Effects p (b -> a) -> Effects p a

infixr 5 :-

-- | Map over the final result type.
instance Functor (Effects p) where
  fmap f (Nil x) = Nil (f x)
  fmap f (p :- ps) = p :- fmap (fmap f) ps

-- | 'pure' represents the empty list of computations while '<*>' acts like
-- '++'.
instance Applicative (Effects p) where
  pure = Nil
  Nil g <*> y = fmap g y
  (f :- x) <*> y = f :- (flip <$> x <*> y)

-- | Compute the length of a list of computations.
length :: Effects p a -> Int
length (Nil _)     = 0
length (_ :- xs) = 1 + length xs

-- | Lift a computation, making sure it is run once in each permutation.
once :: p a -> Effects p a
once p = p :- Nil id

-- | Execute the computations in order.
sequence :: Applicative p => Effects p a -> p a
sequence (Nil x) = pure x
sequence (p :- ps) = (\x f -> f x) <$> p <*> sequence ps

-- | Build a tree (using '<|>' for branching) of all permutations of the
-- computations. The tree shape allows permutations to share common prefixes.
-- This allows clever computations to quickly prune uninteresting branches of
-- permutations.
perms :: Alternative p => Effects p a -> p a
perms (Nil x) = pure x
perms ps      = asum . map permTail . firsts $ ps
  where
    permTail (p :- ps') = p <**> perms ps'
    permTail _          = undefined

-- | Give each effect a chance to be the first effect in the chain, producing
-- @n@ new chains where @n@ is the 'length' of the input chain. In each case
-- the relative order of the effects is preserved with exception of the effect
-- that was moved to the front.
firsts :: Effects p a -> [Effects p a]
firsts (Nil _) = []
firsts (p :- ps) = (p :- ps) : map (\ps' -> swap (p :- ps')) (firsts ps)

-- | Swaps the first two elements of the list, if they exist.
swap :: Effects p a -> Effects p a
swap (p0 :- p1 :- ps) = p1 :- p0 :- fmap flip ps
swap ps = ps
