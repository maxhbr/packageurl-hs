module Purl.Purl.Helper
  ( normalisePath
  , stringToLower
  ) where

import qualified Data.Char                     as Char
import qualified System.FilePath               as FP

stringToLower :: String -> String
stringToLower = map Char.toLower

normalisePath :: FilePath -> FilePath
normalisePath ('/' : path) = normalisePath path
normalisePath path         = (FP.normalise . FP.dropTrailingPathSeparator) path
