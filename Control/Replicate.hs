-- | Composable replication schemes of applicative actions.
--
-- This module separates common combinators such as @some@ and @many@ (from
-- @Control.Applicative@) from any actual applicative actions. It offers
-- composable building blocks for expressing the number (or numbers) of times
-- an action should be executed. The building blocks themselves are composed
-- using the standard 'Applicative', 'Alternative' and 'Category' combinators.
-- Replication schemes can then be 'run' to produce actual actions.
--
-- Some examples help see how this works. One of the simplest schemes is
-- 'one':
--
-- > one :: Replicate a a
--
-- @run one p@ is equivalent to just @p@.
--
-- Schemes can be summed by composing them in applicative fashion. In the
-- following example, the resulting tuple type makes it clear that the action
-- has been run twice and no information is lost:
--
-- > two :: Replicate a (a, a)
-- > two = (,) <$> one <*> one
--
-- @run two p@ is equivalent to @(,) \<$\> p \<*\> p@.
--
-- Things get more interesting if we use the choice combinator @\<|\>@ to form
-- the union of two schemes.
--
-- > oneOrTwo :: Replicate a (Either a (a, a))
-- > oneOrTwo = Left <$> one <|> Right <$> two
--
-- Running schemes that allow multiple frequencies expand to actions that
-- always use @\<|\>@ as late as possible. Since @oneOrTwo@ runs an action at
-- least once, we can start by running the action once immediately and
-- only then choose whether we want to stop there or run it a second time.
-- Running it expands to:
--
-- > \p -> p <**>  (  -- Either run the action again and yield Right ... 
-- >                  (\y x -> Right (x, y)) <$> p
-- >              <|> -- ... or stop here and yield Left.
-- >                  pure Left
-- >               )
--
-- Replication schemes can be thought of as sets of Peano numerals. If there
-- is overlap between the two operands to @\<|\>@, the overlap collapses and
-- is lost in the result. For example, @'between' 3 5 \<|\> between 4 6@ is
-- equivalent to @between 3 6@, a scheme that runs an action 3, 4, 5 or 6
-- times.
--
-- Finally, schemes can be multiplied by composing them with the dot operator
-- '.' from @Control.Category@.
--
-- > twiceThree :: Replicate a ((a, a, a), (a, a, a))
-- > twiceThree = two . three
-- >
-- > thriceTwo :: Replicate a ((a, a), (a, a), (a, a))
-- > thriceTwo = three . two
--
-- If @.@'s operands allow multiple frequencies, the result will allow the
-- products of all pairs of frequencies from the operands. We can use this to
-- e.g. produce all even numbers of occurrences:
--
-- > even :: Replicate a [(a, a)]
-- > even = many . two
--
-- In this example @many@ behaves like the standard Applicative @many@,
-- allowing an action to be run any number of {0, 1, ..} times.
module Control.Replicate (
  -- * Type constructor @Replicate@
  Replicate(..), (*!), (*?), sizes,
  
  -- * Common replication schemes
  zero, one, two, three, opt, many, some, exactly, atLeast, atMost, between, forever
  ) where

import Prelude hiding (even, odd, id, (.))
import Data.Monoid
import Control.Applicative hiding (many, some)
import Control.Category

-- | A set of frequencies which with an applicative action is allowed to
-- occur. @a@ is the result type of a single atomic action. @b@ is the
-- composite result type after executing the action a number of times allowed
-- by this set.
data Replicate a b
  = Nil
  | Cons (Maybe b) (Replicate a (a -> b))

-- | Map over the composite result type.
instance Functor (Replicate a) where
  fmap _ Nil = Nil
  fmap f (Cons mx xs) = Cons (f <$> mx) (fmap (f .) xs)

-- | Pairwise addition.
-- 
-- 'pure' is the singleton set of exactly zero occurrences {0}. It is
-- synonymous with 'zero'.
--
-- '<*>' produces the set of occurrences that are the sums of all possible
-- pairs from the two operands. 
-- 
-- An example: sequencing @'exactly' 2@ {2} with @'exactly' 3@ {3} produces
-- {2+3} = {5}.
-- 
-- Another example: sequencing the set {0, 1} ('opt') with itself produces
-- {0+0, 0+1, 1+0, 1+1} = {0, 1, 1, 2} = {0, 1, 2}. In case of overlap, like
-- in this example, '<*>' favors the heads (of type @Maybe b@) from the left
-- operand.
instance Applicative (Replicate a) where
  pure = zero
  
  -- lowerBound (f1 <*> f2) = lowerBound f1 + lowerBound f2
  -- upperBound (f1 <*> f2) = upperBound f1 + upperBound f2
  Nil <*> _ = Nil
  Cons mx xs <*> ys =  -- 0 + n = n
                       maybe empty (<$> ys) mx
                   <|> -- (1 + m) + n = 1 + (m + n)
                       Cons Nothing (flip <$> xs <*> ys)

-- | 'empty' is the empty set {} of allowed occurrences. Not even performing
-- an action zero times is allowed in that case.
--
-- '<|>' computes the union of the two sets. For example, @'between' 2 4 '<|>'
-- 'between' 3 5@ is equivalent to @'between' 2 5@. Again, in case of overlap,
-- head values from the left operand are favored.
instance Alternative (Replicate a) where
  empty = Nil
  
  Nil <|> ys = ys
  xs <|> Nil = xs
  Cons mx xs <|> Cons my ys =
    -- <|> on Maybes discards the right operand if the left is a Just.
    Cons (mx <|> my) (xs <|> ys)

-- | Behaves exactly as the 'Alternative' instance.
instance Monoid (Replicate a b) where
  mempty  = empty
  mappend = (<|>)

-- | Pairwise multiplication.
--
-- 'id' is the singleton set of exactly one occurrence {1}. It is synonymous
-- with 'one'.
--
-- '.' produces the set of occurrences that are the products of all possible
-- pairs from the two operands.
instance Category Replicate where
  id = one
  Nil        . _   = Nil
  Cons mx xs . ys  =  -- 0 * n = 0
                      maybe empty zero mx
                  <|> -- (m + 1) * n = m * n + n
                      ys <**> (xs . ys)


-- | Run an action a certain number of times, using '<|>' to branch (at the
-- deepest point possible) if multiple frequencies are allowed. Use greedy
-- choices: always make the longer alternative the left operand of @\<|\>@.
(*!) :: Alternative f => Replicate a b -> f a -> f b
Nil        *! _ = empty
Cons mx xs *! p = p <**> (xs *! p) <|> maybe empty pure mx

-- | Run an action a certain number of times, using '<|>' to branch (at the
-- deepest point possible) if multiple frequencies are allowed. Use lazy
-- choices: always make the 'pure' alternative the left operand of @\<|\>@.
(*?) :: Alternative f => Replicate a b -> f a -> f b
Nil        *? _ = empty
Cons mx xs *? p = maybe empty pure mx <|> p <**> (xs *? p)

-- | Enumerate all the numbers of allowed occurrences encoded by the
-- replication scheme.
sizes :: Num num => Replicate a b -> [num]
sizes = sizes' 0
  where
    -- Type signature is mandatory here.
    sizes' :: Num num => num -> Replicate a b -> [num]
    sizes' _ Nil = []
    sizes' n (Cons mx xs) = maybe [] (const [n]) mx ++ sizes' (n + 1) xs



-- | Perform an action exactly zero times.
zero :: b -> Replicate a b
zero x = Cons (Just x) Nil

-- | Perform an action exactly one time.
one :: Replicate a a
one = Cons Nothing (zero id)

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

-- | Perform an action any even number of times.
even :: Replicate a [a]
even = zero [] <|> (\x y zs -> x : y : zs) <$> one <*> one <*> even

-- | Perform an action any odd number of times.
odd :: Replicate a [a]
odd = (:) <$> one <*> even

-- | Repeat an action forever.
forever :: Replicate a b
forever = Cons Nothing (const <$> forever)
