module ParsecEx where

import Control.Frequency
import Control.Applicative.Permute

import Data.Traversable
import Control.Applicative hiding (some, many)

import Text.Parsec hiding (many, between)


-- | Expect exactly one of each of the 26 letters in the alphabet, in any
-- order, but return them rearranged in sorted order.
alphabet :: String -> Either ParseError String
alphabet = runParser (perms p <* eof) () ""
  where
    p = for ['a'..'z'] (\c -> one *. char c)

-- | Parse the input, collecting the 26 letters from the alphabet in 26
-- buckets.
buckets :: String -> Either ParseError [String]
buckets = runParser (perms p <* eof) () ""
  where
    p = for ['a'..'z'] (\c -> many *. char c)

-- The example from Monad Reader issue 17, page 15
-- http://themonadreader.wordpress.com/2011/01/09/issue-17/
-- A crucial difference is that in our case the individual parsers can have
-- different types.
exampleInterleaveT :: String -> Either ParseError ([String], [String], String)
exampleInterleaveT = runParser (perms p <* eof) () ""
  where
    p = (,,) <$> many     *. string "a"
             <*> atMost 6 *. string "b"
             <*> one      *. string "c"
