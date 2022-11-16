-- SPDX-FileCopyrightText: Maximilian Huber <oss@maximilian-huber.de>
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Purl.Purl.Internal
  ( normalisePath
  , stringToLower
  , PurlType
  , parsePurlType
  , purlTypeGeneric
  , Purl(..)
  , nullPurl
  , purlNamespace
  , purlScheme
  ) where

import qualified Data.Aeson                    as A
import qualified Data.Aeson.KeyMap             as A
import qualified Data.Char                     as Char
import           Data.List                      ( intercalate )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import qualified Data.String                   as String
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import qualified Network.URI                   as URI
import qualified Network.URI.Encode            as URI
import qualified System.FilePath               as FP

import           Purl.Purl.Helper

type PurlType = String
purlTypeGeneric :: PurlType
purlTypeGeneric = "generic"
parsePurlType :: String -> PurlType
parsePurlType "" = purlTypeGeneric
parsePurlType t  = stringToLower t

data Purl = Purl
  { purlType       :: PurlType
  , purlNamespace' :: [String]
  , purlName       :: String
  , purlVersion    :: String
  , purlQualifiers :: Map.Map String String
  , purlSubpath    :: FilePath
  }
  deriving (Eq, Ord)
nullPurl :: Purl
nullPurl = Purl purlTypeGeneric [] "" "" mempty ""

purlScheme :: String
purlScheme = "pkg:"

purlNamespace :: Purl -> String
purlNamespace = FP.joinPath . purlNamespace'

renderPurlQualifiers :: Map.Map String String -> String
renderPurlQualifiers pQualifier = if Map.null pQualifier
  then ""
  else
    ( ('?' :)
      . intercalate "&"
      . map (\(k, v) -> k ++ "=" ++ URI.encodeWith (/= ' ') v)
      . Map.toList
      )
      pQualifier

instance Show Purl where
  show purl =
    let
      uri = URI.URI
        { URI.uriScheme    = purlScheme
        , URI.uriAuthority = Nothing
        , URI.uriPath      =
          let
            beforeVersion = FP.joinPath
              ( purlType purl
              : map URI.encode (purlNamespace' purl ++ [purlName purl])
              )
            version = case purlVersion purl of
              ""       -> ""
              pVersion -> '@' : URI.encode pVersion
          in
            beforeVersion ++ version
        , URI.uriQuery     = (renderPurlQualifiers . purlQualifiers) purl
        , URI.uriFragment  = case purlSubpath purl of
                               ""       -> ""
                               pSubpath -> '#' : pSubpath
        }
    in  show uri
