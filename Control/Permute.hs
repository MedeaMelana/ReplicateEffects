{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Permute
  ( Effects(..), Freq(..), length, runEffects, runFreq, swap, firsts, perms

  -- * Lifting computations
  , once, opt, atLeast, between, exactly, many, some

  ) where

import Prelude hiding (length, sequence)
import Control.Applicative hiding (some, many)
import Data.Foldable

-- | A chain of effectful @f@-computations with final result @a@. Individual
-- computations have their own result types, which fit together in standard
-- 'Applicative' fashion. Although these result types are existentially
-- quantified, the computations can still be moved around within the list
-- (e.g. 'swap', 'firsts'). This allows their permutations to be computed.
data Effects f a where
  Nil  :: a -> Effects f a
  (:-) :: Freq f b -> Effects f (b -> a) -> Effects f a

infixr 5 :-

-- | Used to indicate the frequency of a computation in a single permutation,
-- changing the result type accordingly.
data Freq f a where
  Once    ::               f a -> Freq f a
  Opt     ::               f a -> Freq f (Maybe a)
  AtLeast :: Int ->        f a -> Freq f [a]
  Between :: Int -> Int -> f a -> Freq f [a]

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

-- | Run a computation with a certain frequency.
runFreq :: Alternative f => Freq f a -> f a
runFreq freq =
  case freq of
    Once p        -> p
    Opt  p        -> Just <$> p <|> pure Nothing
    AtLeast 0 _   -> pure []
    AtLeast n p   -> (:) <$> p <*> runFreq (AtLeast (n - 1) p)
    Between 0 0 _ -> pure []
    Between 0 m p -> runFreq (Between 0 (m - 1) p) <|> pure []
    Between n m p -> (:) <$> p <*> runFreq (Between (n - 1) (m - 1) p)

freqMatchesEpsilon :: Freq f a -> Maybe a
freqMatchesEpsilon freq =
  case freq of
    Opt  _         -> Just Nothing
    AtLeast 0 _    -> Just []
    Between 0 _ _  -> Just []
    _              -> Nothing

effectsMatchEpsilon :: Effects f a -> Maybe a
effectsMatchEpsilon eff =
  case eff of
    Nil x -> Just x
    freq :- ps -> freqMatchesEpsilon freq <**> effectsMatchEpsilon ps

-- | Splits a frequency such that the first effect in the result always occurs
-- exactly 'once'.
split :: Freq f a -> Effects f a
split freq =
  case freq of
    Once f        -> once f
    Opt  f        -> Just  <$> once f
    AtLeast n f   -> (:)   <$> once f <*> atLeast (0 `max` (n - 1)) f
    Between _ 1 f -> (:[]) <$> once f
    Between n m f -> (:)   <$> once f <*> between (0 `max` (n - 1)) (m - 1) f

lift :: Freq f a -> Effects f a
lift freq = freq :- Nil id

-- | Run the computation exactly once in each permutation.
once :: f a -> Effects f a
once = lift . Once

-- | Run the computation exactly zero or one times in each permutation.
opt :: f a -> Effects f (Maybe a)
opt = lift . Opt

-- | Run the computation at least so many times in each permutation.
atLeast :: Int -> f a -> Effects f [a]
atLeast n = lift . AtLeast n

-- | Run the computation between so and so many times (inclusive) in each
-- permutation.
between :: Int -> Int -> f a -> Effects f [a]
between n m = lift . Between n m

-- | Run the computation exactly @n@ times in each permutation.
exactly :: Int -> f a -> Effects f [a]
exactly n = between n n

-- | Run the computation zero or more times in each permutation.
many :: f a -> Effects f [a]
many = atLeast 0

-- | Run the computation one or more times in each permutation.
some :: f a -> Effects f [a]
some = atLeast 1

-- | Run the effects in order, respecting their frequencies.
runEffects :: Alternative f => Effects f a -> f a
runEffects (Nil x) = pure x
runEffects (freq :- ps) = runFreq freq <**> runEffects ps

-- | Build a tree (using '<|>' for branching) of all permutations of the
-- computations. The tree shape allows permutations to share common prefixes.
-- This allows clever computations to quickly prune away uninteresting
-- branches of permutations.
perms :: forall f a. Alternative f => Effects f a -> f a
perms (Nil x) = pure x
perms ps      = asum . eps . map (permTail . splitHead) . firsts $ ps
  where
    permTail :: Effects f a -> f a
    permTail (p :- ps') = runFreq p <**> perms ps'
    permTail _          = undefined

    eps :: [f a] -> [f a]
    eps =
      -- If none effects are required (i.e. all effects allow frequency 0), 
      -- also allow the empty string.
      case effectsMatchEpsilon ps of
        Just x   -> (++ [pure x])
        Nothing  -> id

    splitHead :: Effects f a -> Effects f a
    splitHead (p :- ps') = split p <**> ps'
    splitHead _          = undefined

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
