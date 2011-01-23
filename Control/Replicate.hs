{-# LANGUAGE GADTs, Rank2Types #-}
-- | Composable replication schemes of applicative actions.
--
-- This module separates common combinators such as @some@ and @many@ (from
-- @Control.Applicative@) from any actual applicative actions. It offers
-- composable building blocks for expressing the number (or numbers) of times
-- an action should be executed. The building blocks themselves are composed
-- using the standard 'Applicative', 'Alternative' and 'Category' combinators.
-- Replication schemes can then be run with '*!' and '*?' to produce actual
-- actions.
--
-- Some examples help see how this works. One of the simplest schemes is
-- 'one':
--
-- > one :: Replicate a a
--
-- @one *! p@ is equivalent to just @p@.
--
-- Schemes can be summed by composing them in applicative fashion. In the
-- following example, the resulting tuple type makes it clear that the action
-- has been run twice and no information is lost:
--
-- > two :: Replicate a (a, a)
-- > two = (,) <$> one <*> one
--
-- @two *! p@ is equivalent to @(,) \<$\> p \<*\> p@.
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
-- Running it with '*!' expands to:
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
-- The example above made the second @p@ the first choice and the @pure@
-- option the second choice to @\<|\>@. In some cases the other way around is
-- preferred. This is what '*?' is for; it prefers running an action fewer
-- times over more times. Running @oneOrTwo@ with it is equivalent to:
--
-- > \p -> p <**>  (  -- Either stop here and yield Left ...
-- >                  pure Left
-- >              <|> -- ... or run the action again and yield Right.
-- >                  (\y x -> Right (x, y)) <$> p
-- >               )
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

import Prelude hiding (even, odd, id, (.), foldr)
import Data.Monoid
import Data.Foldable
import Control.Applicative hiding (many, some)
import Control.Category
import Control.Arrow


-- | A set of frequencies which with an applicative action is allowed to
-- occur. @a@ is the result type of a single atomic action. @b@ is the
-- composite result type after executing the action a number of times allowed
-- by this set.
data Replicate a b where
  Nil :: Replicate a b
  Cons :: (c -> b) -> Maybe c -> Replicate a (a -> c) -> Replicate a b

-- Fold the Replicate list given:
-- * an "empty" value
-- * a function to combine the Cons value into the result
-- * a function to convert the value of the recursive call to the expected type
foldReplicate :: (forall c. f c) 
              -> (forall c. c -> f c -> f c) 
              -> (forall c. f (a -> c) -> f c) 
              -> Replicate a b -> f b
foldReplicate e _ _ Nil = e
foldReplicate e f g (Cons fx mx xs) = 
  foldr (f . fx) (g . foldReplicate e f g . fmap (fx .) $ xs) mx


-- | Map over the composite result type.
instance Functor (Replicate a) where
  fmap _ Nil = Nil
  fmap f (Cons fx mx xs) = Cons (f . fx) mx xs

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
  Cons fx mx xs <*> ys =  -- 0 + n = n
                       foldMap ((<$> ys) . fx) mx
                   <|> -- (1 + m) + n = 1 + (m + n)
                       Cons id empty ((\x y z -> fx (x z) y) <$> xs <*> ys)

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
  Cons fx mx xs <|> Cons fy my ys =
    -- <|> on Maybes discards the right operand if the left is a Just.
    Cons id (fx <$> mx <|> fy <$> my) (fmap fx <$> xs <|> fmap fy <$> ys)

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
  id  = one
  (.) = (*?)

-- | As 'Replicate' is both 'Applicative' and 'Category', it is also an 'Arrow'.
instance Arrow Replicate where
  arr f    = f   <$> id
  f &&& g  = (,) <$> f <*> g
  f *** g  = f . arr fst &&& g . arr snd
  first  f = f  *** id
  second f = id *** f

-- | Behaves exactly as the 'Alternative' instance.
instance ArrowZero Replicate where
  zeroArrow = empty
-- | Behaves exactly as the 'Alternative' instance.
instance ArrowPlus Replicate where
  (<+>) = (<|>)


-- | Run an action a certain number of times, using '<|>' to branch (at the
-- deepest point possible) if multiple frequencies are allowed. Use greedy
-- choices: always make the longer alternative the left operand of @\<|\>@.
(*!) :: Alternative f => Replicate a b -> f a -> f b
r *! p = foldReplicate empty (\x xs -> xs <|> pure x) (p <**>) r

-- | Run an action a certain number of times, using '<|>' to branch (at the
-- deepest point possible) if multiple frequencies are allowed. Use lazy
-- choices: always make the 'pure' alternative the left operand of @\<|\>@.
(*?) :: Alternative f => Replicate a b -> f a -> f b
r *? p = foldReplicate empty (\x xs -> pure x <|> xs) (p <**>) r


-- | Enumerate all the numbers of allowed occurrences encoded by the
-- replication scheme.
sizes :: Replicate a b -> [Int]
sizes = ($ 0) . getConst . sizesFold where
  sizesFold = foldReplicate 
    (                Const (\_ -> []))
    (\_ (Const g) -> Const (\n -> n : g n))
    (\  (Const g) -> Const (\n -> g (n + 1)))


-- | Perform an action exactly zero times.
zero :: b -> Replicate a b
zero x = Cons id (pure x) Nil

-- | Perform an action exactly one time.
one :: Replicate a a
one = Cons id empty (zero id)

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

-- | Repeat an action forever.
forever :: Replicate a b
forever = Cons id empty (const <$> forever)
