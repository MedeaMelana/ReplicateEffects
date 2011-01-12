-- | Composable frequencies of applicative actions.
module Control.Frequency (
  Freq(..),
  
  -- * Common frequency sets
  one, two, three, opt, many, some, exactly, atLeast, between
  ) where

import Control.Applicative hiding (many, some)


-- | A set of frequencies which with an action is allowed to occur. @a@ is the
-- result type of a single atomic action. @b@ is the composite result type
-- after executing the action a number of times allowed by this set.
data Freq a b = Freq
  {
    -- | Whether zero occurrences are allowed. If so, a composite result is
    -- available immediately.
    fZero :: Maybe b,

    -- | Whether at least one occurrence is allowed. If so, the new frequency 
    -- set models all the old (positive) frequencies decreased by one. Its 
    -- composite result accepts the result of executing one action to return 
    -- the final composite result.
    fSucc :: Maybe (Freq a (a -> b))
  }
    

instance Functor (Freq a) where
  fmap f (Freq mzer msuc) = Freq (f <$> mzer) (fmap (f .) <$> msuc)

instance Applicative (Freq a) where
  pure = zero
  
  -- Sequence two sets of frequencies: pairwise sums.
  -- lowerBound (f1 <*> f2) = lowerBound f1 + lowerBound f2
  -- upperBound (f1 <*> f2) = upperBound f1 + upperBound f2
  (<*>) = undefined

instance Alternative (Freq a) where
  -- Empty set of allowed frequencies.
  empty = Freq Nothing Nothing
  
  -- Union of two sets of allowed frequencies.
  Freq zer1 suc1 <|> Freq zer2 suc2 = undefined


-- And maybe even instance Monad (Freq a) ??



-- Some common frequencies.

-- | Allow an action exactly zero times.
zero :: b -> Freq a b
zero x = Freq (Just x) Nothing

-- | Allow an action exactly one time.
one :: Freq a a
one = Freq Nothing (Just (zero id))

-- | Allow an action exactly two times.
two :: Freq a (a, a)
two = (,) <$> one <*> one

-- | Allow an action exactly three times.
three :: Freq a (a, a, a)
three = (,,) <$> one <*> one <*> one

-- | Allow an action zero or one times.
opt :: Freq a (Maybe a)
opt = zero Nothing <|> Just <$> one

-- | Allow an action zero or more times.
many :: Freq a [a]
many = zero [] <|> some

-- | Allow an action one or more times.
some :: Freq a [a]
some = Freq Nothing (Just (flip (:) <$> many))

-- | Allow an action exactly so many times.
exactly :: Int -> Freq a [a]
exactly 0 = zero []
exactly n = (:) <$> one <*> exactly (n - 1)

-- | Allow an action at least so many times.
atLeast :: Int -> Freq a [a]
atLeast n = (++) <$> exactly n <*> many

-- | Allow an action to be run between so and so many times (inclusive).
between :: Int -> Int -> Freq a [a]
between 0 0 = zero []
between 0 m = zero [] <|> (:) <$> one <*> between 0 (m - 1)
between n m = (++) <$> exactly n <*> between 0 (m - n)
