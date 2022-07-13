-- SPDX-FileCopyrightText: Maximilian Huber <oss@maximilian-huber.de>
--
-- SPDX-License-Identifier: BSD-3-Clause

module Purl.Purl.Helper
  ( normalisePath
  , stringToLower
  ) where

import qualified Data.Char                     as Char
import qualified System.FilePath               as FP

stringToLower :: String -> String
stringToLower = map Char.toLower

normalisePath :: FilePath -> FilePath
normalisePath ""           = ""
normalisePath ('/' : path) = normalisePath path
normalisePath path         = (FP.normalise . FP.dropTrailingPathSeparator) path
