{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Applicative.Permute ( Effects, perms, (*.) ) where

import Prelude hiding (length, sequence)
import Control.Applicative hiding (some, many)
import Data.Foldable
import Control.Replicate hiding (Nil, Cons)
import qualified Control.Replicate as R

-- | A chain of effectful @f@-computations with composite result @a@.
-- Individual computations (lifted into @Effects@ using '*.' below) have their
-- own result types, which fit together in standard 'Applicative' fashion.
-- Although these result types are lost in the composite type, the
-- computations can still be moved around within the list (see 'swap' and
-- 'firsts' in the source code for examples). This allows their permutations
-- to be computed.
data Effects f a where
  Nil  :: a -> Effects f a
  Cons :: f x -> Replicate x y -> Effects f (y -> z) -> Effects f z

runEffects :: Alternative f => Effects f a -> f a
runEffects (Nil x) = pure x
runEffects (Cons act freq fs) = run freq act <**> runEffects fs

-- | Map over the final result type.
instance Functor (Effects f) where
  fmap f (Nil x) = Nil (f x)
  fmap f (Cons a r ps) = Cons a r (fmap (fmap f) ps)

-- | 'pure' represents the empty list of computations while '<*>' acts like
-- '++'.
instance Applicative (Effects f) where
  pure = Nil
  Nil g <*> y = fmap g y
  Cons a r x <*> y = Cons a r (flip <$> x <*> y)

-- | Compute the length of a list of computations.
length :: Effects f a -> Int
length (Nil _)     = 0
length (Cons _ _ xs) = 1 + length xs

-- | Allow a computation to be occur so many times in each permutation.
(*.) :: Replicate a b -> f a -> Effects f b
freq *. act = Cons act freq (Nil id)

-- | If all the effects in the chain allow frequency 0, we can execute them
-- all 0 times and get a result.
effectsMatchEpsilon :: Effects f a -> Maybe a
effectsMatchEpsilon eff =
  case eff of
    Nil x                -> Just x
    Cons  _ (R.Cons mz _) ps -> mz <**> effectsMatchEpsilon ps

-- | Build a tree (using '<|>' for branching) of all permutations of the
-- computations. The tree shape allows permutations to share common prefixes.
-- This allows clever computations to quickly prune away uninteresting
-- branches of permutations.
perms :: forall f a. Alternative f => Effects f a -> f a
perms (Nil x) = pure x
perms ps      = eps . asum . map split . firsts $ ps
  where
    split :: Effects f a -> f a
    split (Cons _ R.Nil _) = empty
    split (Cons _ (R.Cons (Just z) R.Nil) ps') = perms (($ z) <$> ps')
    split (Cons act (R.Cons _ s) ps') = act <**> perms (Cons act s ((.) <$> ps'))

    eps :: f a -> f a
    eps =
      -- If none effects are required (i.e. all effects allow frequency 0), 
      -- also allow a pure action.
      case effectsMatchEpsilon ps of
        Just x   -> (<|> pure x)
        Nothing  -> id

-- | Give each effect a chance to be the first effect in the chain, producing
-- @n@ new chains where @n@ is the 'length' of the input chain. In each case
-- the relative order of the effects is preserved with exception of the effect
-- that was moved to the front.
firsts :: Effects f a -> [Effects f a]
firsts (Nil _) = []
firsts (Cons a r ps) =
  (Cons a r ps) : map (\ps' -> swap (Cons a r ps')) (firsts ps)

-- | Swaps the first two elements of the list, if they exist.
swap :: Effects f a -> Effects f a
swap (Cons a1 r1 (Cons a2 r2 ps)) = Cons a2 r2 (Cons a1 r1 (fmap flip ps))
swap ps = ps
