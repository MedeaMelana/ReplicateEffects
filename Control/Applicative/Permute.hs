{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Applicative.Permute ( Effects, perms, (*.) ) where

import Prelude hiding (length, sequence)
import Control.Applicative hiding (some, many)
import Data.Foldable
import Control.Replicate

-- | A chain of effectful @f@-computations with final result @a@. Individual
-- computations (lifted into @Effects@ using one of the frequency combinators
-- below) have their own result types, which fit together in standard
-- 'Applicative' fashion. Although these result types are existentially
-- quantified, the computations can still be moved around within the list (see
-- 'swap' and 'firsts' in the source code for examples). This allows their
-- permutations to be computed.
data Effects f a where
  Nil  :: a -> Effects f a
  (:-) :: (f x, Freq x y) -> Effects f (y -> z) -> Effects f z

infixr 5 :-

runEffects :: Alternative f => Effects f a -> f a
runEffects (Nil x) = pure x
runEffects ((act, freq) :- fs) = run freq act <**> runEffects fs

-- | Map over the final result type.
instance Functor (Effects f) where
  fmap f (Nil x) = Nil (f x)
  fmap f (p :- ps) = p :- fmap (fmap f) ps

-- | 'pure' represents the empty list of computations while '<*>' acts like
-- '++'.
instance Applicative (Effects f) where
  pure = Nil
  Nil g <*> y = fmap g y
  (f :- x) <*> y = f :- (flip <$> x <*> y)

-- | Compute the length of a list of computations.
length :: Effects f a -> Int
length (Nil _)     = 0
length (_ :- xs) = 1 + length xs

-- | Allow a computation to be occur so many times in each permutation.
(*.) :: Freq a b -> f a -> Effects f b
freq *. act = (act, freq) :- Nil id

-- | If all the effects in the chain allow frequency 0, we can execute them
-- all 0 times and get a result.
effectsMatchEpsilon :: Effects f a -> Maybe a
effectsMatchEpsilon eff =
  case eff of
    Nil x                -> Just x
    (_, Freq mz _) :- ps -> mz <**> effectsMatchEpsilon ps

-- | Build a tree (using '<|>' for branching) of all permutations of the
-- computations. The tree shape allows permutations to share common prefixes.
-- This allows clever computations to quickly prune away uninteresting
-- branches of permutations.
perms :: forall f a. Alternative f => Effects f a -> f a
perms (Nil x) = pure x
perms ps      = eps . asum . map split . firsts $ ps
  where
    split :: Effects f a -> f a
    split ((_, Freq Nothing Nothing) :- _)    = empty
    split ((_, Freq (Just z) Nothing) :- ps') = perms (($ z) <$> ps')
    split ((act, Freq _ (Just s)) :- ps')     = act <**> perms ((act, s) :- ((.) <$> ps'))

    eps :: f a -> f a
    eps =
      -- If none effects are required (i.e. all effects allow frequency 0), 
      -- also allow the empty string.
      case effectsMatchEpsilon ps of
        Just x   -> (<|> pure x)
        Nothing  -> id

-- | Give each effect a chance to be the first effect in the chain, producing
-- @n@ new chains where @n@ is the 'length' of the input chain. In each case
-- the relative order of the effects is preserved with exception of the effect
-- that was moved to the front.
firsts :: Effects f a -> [Effects f a]
firsts (Nil _) = []
firsts (freq :- ps) =
  (freq :- ps) : map (\ps' -> swap (freq :- ps')) (firsts ps)

-- | Swaps the first two elements of the list, if they exist.
swap :: Effects f a -> Effects f a
swap (p0 :- p1 :- ps) = p1 :- p0 :- fmap flip ps
swap ps = ps
