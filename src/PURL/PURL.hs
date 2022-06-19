{-# LANGUAGE DeriveGeneric #-}

module PURL.PURL
  ( PURL_Type(..)
  , parsePURL_Type
  , PURL(..)
  , parsePURL
  ) where

import qualified Data.Aeson         as A
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromMaybe, maybeToList)
import qualified Data.Text          as T
import           GHC.Generics       (Generic)
import qualified Network.URI        as URI
import qualified Network.URI.Encode as URI
import qualified System.FilePath    as FP
import qualified Data.String as String

data PURL_Type
  = PURL_TypeBitbucket
  | PURL_TypeComposer
  | PURL_TypeDebian
  | PURL_TypeDocker
  | PURL_TypeGem
  | PURL_TypeGeneric
  | PURL_TypeGithub
  | PURL_TypeGolang
  | PURL_TypeMaven
  | PURL_TypeNPM
  | PURL_TypeNuget
  | PURL_TypePyPi
  | PURL_TypeRPM
  | PURL_Type String

instance Show PURL_Type where
  show PURL_TypeBitbucket = "bitbucket"
  show PURL_TypeComposer  = "composer"
  show PURL_TypeDebian    = "debian"
  show PURL_TypeDocker    = "docker"
  show PURL_TypeGem       = "gem"
  show PURL_TypeGeneric   = "generic"
  show PURL_TypeGithub    = "github"
  show PURL_TypeGolang    = "golang"
  show PURL_TypeMaven     = "maven"
  show PURL_TypeNPM       = "npm"
  show PURL_TypeNuget     = "nuget"
  show PURL_TypePyPi      = "pypi"
  show PURL_TypeRPM       = "rpm"
  show (PURL_Type s)      = s

instance Eq PURL_Type where
  (==) p1 p2 = (show p1) == (show p2)

parsePURL_Type :: String -> PURL_Type
parsePURL_Type "bitbucket" = PURL_TypeBitbucket
parsePURL_Type "composer"  = PURL_TypeComposer
parsePURL_Type "debian"    = PURL_TypeDebian
parsePURL_Type "docker"    = PURL_TypeDocker
parsePURL_Type "gem"       = PURL_TypeGem
parsePURL_Type "generic"   = PURL_TypeGeneric
parsePURL_Type "github"    = PURL_TypeGithub
parsePURL_Type "golang"    = PURL_TypeGolang
parsePURL_Type "maven"     = PURL_TypeMaven
parsePURL_Type "npm"       = PURL_TypeNPM
parsePURL_Type "nuget"     = PURL_TypeNuget
parsePURL_Type "pypi"      = PURL_TypePyPi
parsePURL_Type "rpm"       = PURL_TypeRPM
parsePURL_Type s           = PURL_Type s

instance String.IsString PURL_Type where
  fromString = parsePURL_Type

instance A.ToJSON PURL_Type where
  toJSON = A.toJSON . show

instance A.FromJSON PURL_Type where
  parseJSON = A.withText "" $ return . parsePURL_Type . T.unpack

data PURL =
  PURL
    { _PURL_scheme     :: Maybe String
    , _PURL_type       :: Maybe PURL_Type
    , _PURL_namespace  :: Maybe String
    , _PURL_name       :: String
    , _PURL_version    :: Maybe String
    , _PURL_qualifiers :: Maybe String
    , _PURL_subpath    :: Maybe String
    }
  deriving (Eq, Generic)

instance Show PURL where
  show (PURL pScheme pType pNamespace pName pVersion pQualifier pSubpath) =
    let uri =
          URI.URI
            { URI.uriScheme = ("pkg" `fromMaybe` pScheme) ++ ":"
            , URI.uriAuthority = Nothing
            , URI.uriPath =
                FP.joinPath
                  ((map
                      URI.encode
                      (maybeToList (fmap show pType) ++ maybeToList pNamespace)) ++
                   [ (URI.encode pName) ++
                     (maybe "" ('@' :) (fmap URI.encode pVersion))
                   ])
            , URI.uriQuery = "" `fromMaybe` pQualifier
            , URI.uriFragment = "" `fromMaybe` (fmap ('#' :) pSubpath)
            }
     in show uri

instance A.ToJSON PURL

instance A.FromJSON PURL

parsePURL :: String -> Maybe PURL
parsePURL uriStr =
  case URI.parseURI uriStr of
    Just uri ->
      let pScheme = Just (filter (/= ':') (URI.uriScheme uri))
          (pType, pNamespace, (pName, pVersion)) =
            let parseNameAndVersion :: String -> (String, Maybe String)
                parseNameAndVersion pNameAndVersion =
                  case splitOn "@" pNameAndVersion of
                    [pName, pVersion] ->
                      (URI.decode pName, Just (URI.decode pVersion))
                    _ -> (URI.decode pNameAndVersion, Nothing)
                path = URI.uriPath uri
             in case FP.splitPath path of
                  [pNameAndVersion] ->
                    (Nothing, Nothing, parseNameAndVersion pNameAndVersion)
                  [pType, pNameAndVersion] ->
                    ( Just (FP.dropTrailingPathSeparator pType)
                    , Nothing
                    , parseNameAndVersion pNameAndVersion)
                  (pType:ps) ->
                    let pNameAndVersion = last ps
                        pNamespace =
                          (FP.dropTrailingPathSeparator . FP.joinPath . init) ps
                     in ( Just (FP.dropTrailingPathSeparator pType)
                        , (Just . URI.decode) pNamespace
                        , parseNameAndVersion pNameAndVersion)
          pQualifier =
            case URI.uriQuery uri of
              [] -> Nothing
              qs -> Just $ qs
          pSubpath =
            case (URI.uriFragment uri) of
              ""       -> Nothing
              fragment -> Just (tail fragment)
       in Just $
          PURL
            pScheme
            (fmap parsePURL_Type pType)
            pNamespace
            pName
            pVersion
            pQualifier
            pSubpath
    Nothing -> Nothing
