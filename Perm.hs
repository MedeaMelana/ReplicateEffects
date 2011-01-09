{-# LANGUAGE GADTs #-}

module Perm
  ( Effects(..), length, swap, firsts, perms

  -- * Lifting computations
  , once, opt, atLeast, between, exactly, many, some

  ) where

import Prelude hiding (length, sequence)
import Control.Applicative hiding (some, many)
import Data.Foldable

-- | A chain of effectful @p@-computations with final result @a@. Individual
-- computations have their own result types, which fit together in standard
-- 'Applicative' fashion. Although these result types are existentially
-- quantified, the computations can still be moved around within the list
-- (e.g. 'swap', 'firsts'). This allows their permutations to be computed.
data Effects p a where
  Nil  :: a -> Effects p a
  (:-) :: Freq p b -> Effects p (b -> a) -> Effects p a

infixr 5 :-

data Freq f a where
  Once    ::               f a -> Freq f a
  Opt     ::               f a -> Freq f (Maybe a)
  AtLeast :: Int ->        f a -> Freq f [a]
  Between :: Int -> Int -> f a -> Freq f [a]

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

runFreq :: Alternative p => Freq p a -> p a
runFreq freq =
  case freq of
    Once p        -> p
    Opt  p        -> Just <$> p <|> pure Nothing
    AtLeast 0 _   -> pure []
    AtLeast n p   -> (:) <$> p <*> runFreq (AtLeast (n - 1) p)
    Between 0 0 _ -> pure []
    Between 0 m p -> runFreq (Between 0 (m - 1) p) <|> pure []
    Between n m p -> (:) <$> p <*> runFreq (Between (n - 1) (m - 1) p)

freqMatchesEpsilon :: Freq p a -> Maybe a
freqMatchesEpsilon freq =
  case freq of
    Opt  _         -> Just Nothing
    AtLeast 0 _    -> Just []
    Between 0 _ _  -> Just []
    _              -> Nothing

effectsMatchEpsilon :: Effects p a -> Maybe a
effectsMatchEpsilon eff =
  case eff of
    Nil x -> Just x
    freq :- ps -> freqMatchesEpsilon freq <**> effectsMatchEpsilon ps

-- | Splits a frequency such that the first effect in the result always occurs
-- exactly 'once'.
split :: Freq p a -> Effects p a
split freq =
  case freq of
    Once p        -> once p
    Opt  p        -> Just  <$> once p
    AtLeast n p   -> (:)   <$> once p <*> atLeast (0 `max` (n - 1)) p
    Between _ 1 p -> (:[]) <$> once p
    Between n m p -> (:)   <$> once p <*> between (0 `max` (n - 1)) (m - 1) p

lift :: Freq p a -> Effects p a
lift freq = freq :- Nil id

-- | Run the computation exactly once in each permutation.
once :: p a -> Effects p a
once = lift . Once

-- | Run the computation exactly 0 or 1 times in each permutation.
opt :: p a -> Effects p (Maybe a)
opt = lift . Opt

-- | Run the computation at least so many times in each permutation.
atLeast :: Int -> p a -> Effects p [a]
atLeast n = lift . AtLeast n

-- | Run the computation between so and so many times (inclusive) in each
-- permutation.
between :: Int -> Int -> p a -> Effects p [a]
between n m = lift . Between n m

-- | Run the computation exactly @n@ times in each permutation.
exactly :: Int -> p a -> Effects p [a]
exactly n = between n n

many :: p a -> Effects p [a]
many = atLeast 0

some :: p a -> Effects p [a]
some = atLeast 1

-- runEffects :: Alternative p => Effects p a -> p a
-- runEffects (Nil x) = pure x
-- runEffects (freq :- ps) = runFreq freq <**> runEffects ps

-- | Build a tree (using '<|>' for branching) of all permutations of the
-- computations. The tree shape allows permutations to share common prefixes.
-- This allows clever computations to quickly prune away uninteresting
-- branches of permutations.
perms :: Alternative p => Effects p a -> p a
perms (Nil x) = pure x
perms ps      = asum . eps . map (permTail . splitHead) . firsts $ ps
  where
    permTail (p :- ps') = runFreq p <**> perms ps'
    permTail _          = undefined
    
    eps =
      -- If no occurrence of any of the effects are required, also allow the 
      -- empty string.
      case effectsMatchEpsilon ps of
        Just x   -> (pure x :)
        Nothing  -> id

    splitHead (p :- ps') = split p <**> ps'
    splitHead _          = undefined

-- | Give each effect a chance to be the first effect in the chain, producing
-- @n@ new chains where @n@ is the 'length' of the input chain. In each case
-- the relative order of the effects is preserved with exception of the effect
-- that was moved to the front.
firsts :: Effects p a -> [Effects p a]
firsts (Nil _) = []
firsts (freq :- ps) =
  (freq :- ps) : map (\ps' -> swap (freq :- ps')) (firsts ps)

-- | Swaps the first two elements of the list, if they exist.
swap :: Effects p a -> Effects p a
swap (p0 :- p1 :- ps) = p1 :- p0 :- fmap flip ps
swap ps = ps
