{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Purl.Purl.Internal
  ( normalisePath
  , stringToLower
  , PurlType(..)
  , parsePurlType
  , Purl(..)
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

stringToLower :: String -> String
stringToLower = map Char.toLower

normalisePath :: FilePath -> FilePath
normalisePath ('/' : path) = normalisePath path
normalisePath path         = (FP.normalise . FP.dropTrailingPathSeparator) path

newtype PurlType
  = PurlType String
  deriving (Eq, Generic, Ord)
instance Show PurlType where
  show (PurlType t) = t
instance String.IsString PurlType where
  fromString = PurlType . stringToLower
parsePurlType :: String -> PurlType
parsePurlType = String.fromString
instance A.ToJSON PurlType where
  toJSON (PurlType t) = A.toJSON t
instance A.FromJSON PurlType where
  parseJSON = A.withText "PurlType" $ return . parsePurlType . T.unpack

purlTypeGeneric :: PurlType
purlTypeGeneric = PurlType "generic"

data Purl = Purl
  { _PurlType       :: Maybe PurlType
  , _PurlNamespace  :: Maybe String
  , _PurlName       :: String
  , _PurlVersion    :: Maybe String
  , _PurlQualifiers :: Map.Map String String
  , _PurlSubpath    :: Maybe FilePath
  }
  deriving Eq

purlScheme :: String
purlScheme = "pkg:"

instance Show Purl where
  show (Purl pType pNamespace pName pVersion pQualifier pSubpath) =
    let
      uri = URI.URI
        { URI.uriScheme    = purlScheme
        , URI.uriAuthority = Nothing
        , URI.uriPath      = FP.joinPath
          (  (map
               URI.encode
               (show (purlTypeGeneric `fromMaybe` pType) : maybeToList pNamespace)
             )
          ++ [ (URI.encode pName)
                 ++ (maybe "" ('@' :) (fmap URI.encode pVersion))
             ]
          )
        , URI.uriQuery     = if Map.null pQualifier
          then ""
          else
            ( ('?' :)
              . intercalate "&"
              . map (\(k, v) -> k ++ "=" ++ (URI.encodeWith (/= ' ') v))
              . Map.toList
              )
              pQualifier
        , URI.uriFragment  = "" `fromMaybe` (fmap ('#' :) pSubpath)
        }
    in  show uri
