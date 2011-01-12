-- | Composable frequencies of applicative actions.
module Control.Replicate (
  Replicate(..), run,
  
  -- * Common replication schemes
  one, two, three, opt, many, some, exactly, atLeast, atMost, between
  ) where

import Data.Monoid
import Control.Applicative hiding (many, some)


-- | A set of frequencies which with an action is allowed to occur. @a@ is the
-- result type of a single atomic action. @b@ is the composite result type
-- after executing the action a number of times allowed by this set.
data Replicate a b = Replicate
  {
    -- | Whether zero occurrences are allowed. If so, a composite result is
    -- available immediately.
    rZero :: Maybe b,

    -- | Whether at least one occurrence is allowed. If so, the new frequency 
    -- set models all the old (positive) frequencies decreased by one. Its 
    -- composite result accepts the result of executing one action to return 
    -- the final composite result.
    rSucc :: Maybe (Replicate a (a -> b))
  }

occ :: Replicate a b -> [Int]
occ = occ' 0
  where
    -- Type signature is mandatory here.
    occ' :: Int -> Replicate a b -> [Int]
    occ' n (Replicate mz ms) =
      maybe [] (const [n]) mz ++
      maybe [] (occ' (n + 1)) ms

instance Functor (Replicate a) where
  fmap f (Replicate mzer msuc) = Replicate (f <$> mzer) (fmap (f .) <$> msuc)

instance Applicative (Replicate a) where
  pure = zero
  
  -- Sequence two sets of frequencies: pairwise sums.
  -- lowerBound (f1 <*> f2) = lowerBound f1 + lowerBound f2
  -- upperBound (f1 <*> f2) = upperBound f1 + upperBound f2
  Replicate mz1 ms1 <*> fr  =  maybe empty (<$> fr) mz1
                      <|> maybe empty (\s1 -> Replicate Nothing $ Just (flip <$> s1 <*> fr)) ms1

instance Alternative (Replicate a) where
  -- Empty set of allowed frequencies.
  empty = Replicate Nothing Nothing
  
  -- Union of two sets of allowed frequencies.
  Replicate mz1 ms1 <|> Replicate mz2 ms2 =
      Replicate (mz1 <|> mz2) (ms1 `mappend` ms2)

instance Monoid (Replicate a b) where
  mempty = empty
  mappend = (<|>)

-- And maybe even instance Monad (Replicate a) ??


-- | Run an action a certain number of times, using '<|>' to branch if
-- multiple frequencies are allowed.
run :: Alternative f => Replicate a b -> f a -> f b
run (Replicate mzer msuc) p  =  maybe empty (\f -> p <**> run f p) msuc
                       <|> maybe empty pure mzer



-- Some common frequencies.

-- | Allow an action exactly zero times.
zero :: b -> Replicate a b
zero x = Replicate (Just x) Nothing

-- | Allow an action exactly one time.
one :: Replicate a a
one = Replicate Nothing (Just (zero id))

-- | Allow an action exactly two times.
two :: Replicate a (a, a)
two = (,) <$> one <*> one

-- | Allow an action exactly three times.
three :: Replicate a (a, a, a)
three = (,,) <$> one <*> one <*> one

-- | Allow an action zero or one times.
opt :: Replicate a (Maybe a)
opt = zero Nothing <|> Just <$> one

-- | Allow an action zero or more times.
many :: Replicate a [a]
many = zero [] <|> some

-- | Allow an action one or more times.
some :: Replicate a [a]
some = (:) <$> one <*> many

-- | Allow an action exactly so many times.
exactly :: Int -> Replicate a [a]
exactly 0 = zero []
exactly n = (:) <$> one <*> exactly (n - 1)

-- | Allow an action at least so many times.
atLeast :: Int -> Replicate a [a]
atLeast n = (++) <$> exactly n <*> many

-- | Allow an action at most so many times.
atMost :: Int -> Replicate a [a]
atMost 0 = zero []
atMost n = zero [] <|> (:) <$> one <*> atMost (n - 1)

-- | Allow an action to be run between so and so many times (inclusive).
between :: Int -> Int -> Replicate a [a]
between m n = (++) <$> exactly m <*> atMost (n - m)
