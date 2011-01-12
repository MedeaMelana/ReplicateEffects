-- | Composable replication schemes of applicative actions.
--
-- This module separates common combinators such as @some@ and @many@ (from
-- @Control.Applicative@) from any actual applicative actions. It offers
-- composable building blocks for expressing the number (or numbers) of times
-- an action should be executed. The building blocks themselves are composed
-- using the standard 'Applicative' and 'Alternative' combinators.
--
-- Some examples help see how this works:
--
-- > two :: Replicate a (a, a)
-- > two = (,) <$> one <*> one
-- >
-- > many :: Replicate a [a]
-- > many = zero [] <|> some
-- >
-- > some :: Replicate a [a]
-- > some = (:) <$> one <*> many
--
-- Replication schemes can then be 'run' to produce actual actions.
module Control.Replicate (
  -- * Type constructor @Replicate@
  Replicate(..), run, sizes,
  
  -- * Common replication schemes
  one, two, three, opt, many, some, exactly, atLeast, atMost, between
  ) where

import Data.Monoid
import Control.Applicative hiding (many, some)


-- | A set of frequencies which with an applicative action is allowed to
-- occur. @a@ is the result type of a single atomic action. @b@ is the
-- composite result type after executing the action a number of times allowed
-- by this set.
data Replicate a b = Replicate
  {
    -- | Whether zero occurrences are allowed. If so, a composite result is
    -- available immediately.
    rZero :: Maybe b,

    -- | Whether at least one occurrence is allowed. If so, the new frequency 
    -- set models all the old (positive) frequencies decreased by one. Its 
    -- composite result accepts the result of executing that one action, to return
    -- the final composite result.
    rSucc :: Maybe (Replicate a (a -> b))
  }

-- | Map over the composite result type.
instance Functor (Replicate a) where
  fmap f (Replicate mzer msuc) = Replicate (f <$> mzer) (fmap (f .) <$> msuc)

-- | 'pure' is the singleton set of exactly zero occurrences {0}. '<*>'
-- produces the set of occurrences that are the sums of all possible pairs
-- from the two operands. 
-- 
-- An example: sequencing @'exactly' 2@ {2} with @'exactly' 3@ {3} produces
-- {2+3} = {5}.
-- 
-- Another example: sequencing the set {0, 1} ('opt') with itself produces
-- {0+0, 0+1, 1+0, 1+1} = {0, 1, 1, 2} = {0, 1, 2}. In case of overlap, like
-- in this example, '<*>' favors the 'rZero's from the left operand.
instance Applicative (Replicate a) where
  pure = zero
  
  -- lowerBound (f1 <*> f2) = lowerBound f1 + lowerBound f2
  -- upperBound (f1 <*> f2) = upperBound f1 + upperBound f2
  Replicate mz1 ms1 <*> fr  =  maybe empty (<$> fr) mz1
                      <|> maybe empty (\s1 -> Replicate Nothing $ Just (flip <$> s1 <*> fr)) ms1


-- | 'empty' is the empty set {} of allowed occurrences. Not even performing
-- an action zero times is allowed in that case.
--
-- '<|>' computes the union of the two sets. For example, @'between' 2 4 '<|>'
-- 'between' 3 5@ is equivalent to @'between' 2 5@. Again, in case of overlap,
-- 'rZero's from the left operand are favored.
instance Alternative (Replicate a) where
  empty = Replicate Nothing Nothing
  
  Replicate mz1 ms1 <|> Replicate mz2 ms2 =
    -- Kinda tricky. <|> on Maybes discards the right operand if the left is a
    -- Just. mappend, however, uses both operands if it can.
    Replicate (mz1 <|> mz2) (ms1 `mappend` ms2)

-- | Behaves exactly as the 'Alternative' instance.
instance Monoid (Replicate a b) where
  mempty = empty
  mappend = (<|>)

-- And maybe even instance Monad (Replicate a) ??


-- | Run an action a certain number of times, using '<|>' to branch (at the
-- deepest point possible) if multiple frequencies are allowed.
run :: Alternative f => Replicate a b -> f a -> f b
run (Replicate mzer msuc) p  =  maybe empty (\f -> p <**> run f p) msuc
                       <|> maybe empty pure mzer

-- | Enumerate all the numbers of allowed occurrences encoded by the
-- replication scheme.
sizes :: Num num => Replicate a b -> [num]
sizes = sizes' 0
  where
    -- Type signature is mandatory here.
    sizes' :: Num num => num -> Replicate a b -> [num]
    sizes' n (Replicate mz ms) =
      maybe [] (const [n]) mz ++
      maybe [] (sizes' (n + 1)) ms



-- | Perform an action exactly zero times.
zero :: b -> Replicate a b
zero x = Replicate (Just x) Nothing

-- | Perform an action exactly one time.
one :: Replicate a a
one = Replicate Nothing (Just (zero id))

-- | Perform an action exactly two times.
two :: Replicate a (a, a)
two = (,) <$> one <*> one

-- | Perform an action exactly three times.
three :: Replicate a (a, a, a)
three = (,,) <$> one <*> one <*> one

-- | Perform an action zero or one times.
opt :: Replicate a (Maybe a)
opt = zero Nothing <|> Just <$> one

-- | Perform an action zero or more times.
many :: Replicate a [a]
many = zero [] <|> some

-- | Perform an action one or more times.
some :: Replicate a [a]
some = (:) <$> one <*> many

-- | Perform an action exactly so many times.
exactly :: Int -> Replicate a [a]
exactly 0 = zero []
exactly n = (:) <$> one <*> exactly (n - 1)

-- | Perform an action at least so many times.
atLeast :: Int -> Replicate a [a]
atLeast n = (++) <$> exactly n <*> many

-- | Perform an action at most so many times.
atMost :: Int -> Replicate a [a]
atMost 0 = zero []
atMost n = zero [] <|> (:) <$> one <*> atMost (n - 1)

-- | Allow an action to be performed between so and so many times (inclusive).
between :: Int -> Int -> Replicate a [a]
between m n = (++) <$> exactly m <*> atMost (n - m)
