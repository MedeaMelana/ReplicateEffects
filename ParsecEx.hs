module ParsecEx where

import Perm

import Data.Traversable
import Control.Applicative

import Text.Parsec

alphabet :: [String]
alphabet = map pure ['a'..'z']

hoogeveen :: [String]
hoogeveen = ["aap", "noot", "mies", "wim"]

example :: [String] -> String -> Either ParseError [String]
example opts = runParser (perms $ for opts (once . string)) () ""
