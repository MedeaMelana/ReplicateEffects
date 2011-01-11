module ParsecEx where

import Control.Applicative.Permute

import Data.Traversable
import Control.Applicative hiding (some, many)

import Text.Parsec hiding (many, between)

alphabet :: [String]
alphabet = map pure ['a'..'z']

hoogeveen :: [String]
hoogeveen = ["aap", "noot", "mies", "wim"]

example :: [String] -> String -> Either ParseError [String]
example opts = runParser (perms (for opts (once . string)) <* eof) () ""
