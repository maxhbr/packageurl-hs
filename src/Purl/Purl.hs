-- SPDX-FileCopyrightText: Maximilian Huber <oss@maximilian-huber.de>
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Purl.Purl
  ( module X
  , purlTypeGeneric
  , normalisePurl
  , isPurlValid
  , parsePurl
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

import Purl.Purl.Helper
import           Purl.Purl.Internal as X
import           Purl.Purl.KnownTypes as X

purlTypeGeneric :: PurlType
purlTypeGeneric = PurlType "generic"

normalisePurl :: Purl -> Purl
normalisePurl =
  let
    normalisePurlType (p@Purl { purlType = t }) = p { purlType = fmap (parsePurlType . show) t }
    normalisePurlPaths (p@Purl { purlNamespace = ns, purlName = n, purlSubpath = sp })
      = p { purlNamespace = fmap normalisePath ns
          , purlName      = normalisePath n
          }
    normaliseSubpath (p@Purl { purlSubpath = sp }) = p{purlSubpath = fmap normalisePath sp}
    normaliseQualifiers (p@Purl { purlQualifiers = q }) =
      p { purlQualifiers = Map.mapKeys stringToLower q }
    normaliserFromType :: PurlType -> Purl -> Purl
    normaliserFromType t = case t `Map.lookup` knownPurlTypeMap of
      Just kpt -> getKptNormalizer kpt
      Nothing -> id
    normaliseFromType :: Purl -> Purl
    normaliseFromType (p@Purl { purlType = Just t }) = normaliserFromType t p
    normaliseFromType p = p
  in
    normaliseFromType
    . normaliseQualifiers
    . normaliseSubpath
    . normalisePurlPaths
    . normalisePurlType

isPurlValid :: Purl -> Bool
isPurlValid (Purl { purlName = "" }) = False
isPurlValid (Purl { purlType = Nothing }) = False
isPurlValid (Purl { purlType = Just (PurlType "") }) = False
isPurlValid (p@Purl {purlType = Just t}) = let
    validatorFromType = case t `Map.lookup` knownPurlTypeMap of
      Just kpt -> getKptValidator kpt
      Nothing -> const True
    validatorForQualifiers (Purl {purlQualifiers = qs}) = all (all (\c -> and [Char.isAlpha c, Char.isLower c])) $ Map.keys qs
  in and [validatorFromType p, validatorForQualifiers p]

parsePurl :: String -> Maybe Purl
parsePurl ('p' : 'k' : 'g' : ':' : '/' : rest) = parsePurl ("pkg:" ++ rest)
parsePurl uriStr                               = case URI.parseURI uriStr of
  Just uri ->
    let
      pScheme = URI.uriScheme uri
      (pType, pNamespace, (pName, pVersion)) =
        let
          parseNameAndVersion :: String -> (String, Maybe String)
          parseNameAndVersion pNameAndVersion =
            case splitOn "@" pNameAndVersion of
              [pName, pVersion] ->
                (URI.decode pName, Just (URI.decode pVersion))
              _ -> (URI.decode pNameAndVersion, Nothing)
          path = URI.uriPath uri
          prefixToTypeAndNamespace prefix =
            case FP.splitPath (normalisePath prefix) of
              []      -> (Nothing, Nothing)
              [pType] -> (Just (FP.dropTrailingPathSeparator pType), Nothing)
              (pType : ps) ->
                let pNamespace = (normalisePath . FP.joinPath) ps
                in  ( Just (FP.dropTrailingPathSeparator pType)
                    , (Just . URI.decode) pNamespace
                    )
        in
          case FP.splitFileName path of
            ("./", pNameAndVersion) ->
              (Nothing, Nothing, parseNameAndVersion pNameAndVersion)
            (prefix, pNameAndVersion) ->
              let (t, ns) = prefixToTypeAndNamespace prefix
              in  (t, ns, parseNameAndVersion pNameAndVersion)

      pQualifier =
        let
          parseQualifiersFindKey accum ('=' : qs') =
            parseQualifiersFindValue accum "" qs'
          parseQualifiersFindKey accum ('&' : qs') =
            (accum, "") : (parseQualifiersFindKey "" qs')
          parseQualifiersFindKey accum (c : qs') =
            parseQualifiersFindKey (accum ++ [c]) qs'
          parseQualifiersFindKey ""    [] = []
          parseQualifiersFindKey accum [] = (accum, "") : []
          parseQualifiersFindValue key accum ('&' : qs') =
            (key, URI.decode accum) : (parseQualifiersFindKey "" qs')
          parseQualifiersFindValue key accum (c : qs') =
            (parseQualifiersFindValue key (accum ++ [c]) qs')
          parseQualifiersFindValue key accum [] = (key, URI.decode accum) : []
        in
          case URI.uriQuery uri of
            ""         -> mempty
            ('?' : qs) -> Map.fromList $ parseQualifiersFindKey "" qs
      pSubpath = case (URI.uriFragment uri) of
        ""       -> Nothing
        fragment -> Just (tail fragment)
    in
      if pScheme == purlScheme
        then Just $ Purl (fmap parsePurlType pType)
                         pNamespace
                         pName
                         pVersion
                         pQualifier
                         pSubpath
        else Nothing
  Nothing -> Nothing

instance A.ToJSON Purl where
  toJSON (Purl ty ns n v qs sp) = A.object
    [ "type" A..= ty
    , "namespace" A..= ns
    , "name" A..= n
    , "version" A..= v
    , "qualifiers" A..= qs
    , "subpath" A..= sp
    ]

instance A.FromJSON Purl where
  parseJSON (A.String t) = case (parsePurl . T.unpack) t of
    Just p -> return p
    Nothing ->
      return $ Purl Nothing Nothing (T.unpack t) Nothing mempty Nothing
  parseJSON (A.Object o) =
    let parseQualifiers (Just (  A.String t')) = pure . Just $ T.unpack t'
        parseQualifiers (Just v@(A.Object _ )) = do
          qualifiersMap <- A.parseJSON v
          ( return
            . Just
            . ('?' :)
            . intercalate "&"
            . map (\(k, v) -> stringToLower k ++ "=" ++ v)
            . Map.toList
            )
            qualifiersMap
        parseQualifiers _ = pure Nothing
    in  do
          purl <-
            Purl
            <$>   o
            A..:? "type"
            <*>   o
            A..:? "namespace"
            <*>   o
            A..:  "name"
            <*>   o
            A..:? "version"
            <*>   ((mempty `fromMaybe`) <$> o A..:? "qualifiers")
            <*>   o
            A..:? "subpath"
          pure purl
  parseJSON (A.Array _) = fail "can not parse Array to Purl"
  parseJSON (A.Number _) = fail "can not parse Number to Purl"
  parseJSON (A.Bool _) = fail "can not parse Bool to Purl"
  parseJSON (A.Null) = fail "can not parse Null to Purl"
