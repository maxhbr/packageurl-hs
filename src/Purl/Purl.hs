-- SPDX-FileCopyrightText: Maximilian Huber <oss@maximilian-huber.de>
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Purl.Purl
  ( module X
  , normalisePurl
  , isPurlValid
  , parsePurlQualifiers
  , parsePurl
  , tryToExtractPurlType
  , heuristicallyRefinePurl
  ) where

import qualified Data.Aeson                    as A
import qualified Data.Aeson.KeyMap             as A
import qualified Data.Aeson.Types              as A
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
import           Purl.Purl.Internal            as X
import           Purl.Purl.KnownTypes          as X

normalisePurl :: Purl -> Purl
normalisePurl =
  let
    normalisePurlType p@Purl {purlType = t} =
      p { purlType = parsePurlType t }
    normalisePurlPaths p@Purl {purlNamespace' = ns, purlName = n} =
      let
        normaliseNamespace :: [String] -> String -> ([String], String)
        normaliseNamespace namespace name =
          case
              ( filter (/= "")
                . map FP.dropTrailingPathSeparator
                . concatMap FP.splitPath
                )
                (namespace ++ [name])
            of
              []            -> (namespace, name)
              redistributed -> (init redistributed, last redistributed)
        (newNS, newN) = normaliseNamespace ns n
      in
        p { purlNamespace' = newNS, purlName = newN }
    normaliseSubpath p@Purl {purlSubpath = ""} = p
    normaliseSubpath p@Purl {purlSubpath = sp} =
      p { purlSubpath = normalisePath sp }
    normaliseQualifiers p@Purl {purlQualifiers = q} =
      p { purlQualifiers = Map.mapKeys stringToLower q }
    normaliseFromType :: Purl -> Purl
    normaliseFromType p@Purl {purlType = t} =
      let normaliserFromType :: PurlType -> Purl -> Purl
          normaliserFromType t = maybe id getKptNormalizer (t `Map.lookup` knownPurlTypeMap)
      in  normaliserFromType t p
  in
    normaliseFromType
    . normaliseQualifiers
    . normaliseSubpath
    . normalisePurlPaths
    . normalisePurlType

isPurlValid :: Purl -> Bool
isPurlValid (Purl { purlName = "" }        ) = False
isPurlValid (Purl { purlNamespace' = [""] }) = False
isPurlValid (Purl { purlType = "" }        ) = False
isPurlValid p@Purl {purlType = t} =
  let validatorFromType = case t `Map.lookup` knownPurlTypeMap of
        Just kpt -> getKptValidator kpt
        Nothing  -> const True
      validatorForQualifiers (Purl { purlQualifiers = qs }) =
        all (all (\c -> Char.isAlpha c && Char.isLower c)) $ Map.keys qs
  in  (validatorFromType p && validatorForQualifiers p)

parsePurlFromPath :: String -> Purl
parsePurlFromPath path
  = let 
      (pType, ((pNamespace, pName), pVersion)) =
        let
          getNameAndNamespace :: String -> ([String], String)
          getNameAndNamespace prev =
            case
                (map (URI.decode . FP.dropTrailingPathSeparator) . FP.splitPath)
                  prev
              of
                []    -> ([], "")
                parts -> (init parts, last parts)
          getVersion :: String -> (([String], String), String)
          getVersion prev = case splitOn "@" prev of
            [next, pVersion] -> (getNameAndNamespace next, URI.decode pVersion)
            _                -> (getNameAndNamespace prev, "")
          getPurlType :: String -> (PurlType, (([String], String), String))
          getPurlType prev =
            case (map FP.dropTrailingPathSeparator . FP.splitPath) prev of
              []  -> (purlTypeGeneric, (([], ""), ""))
              [t] -> (parsePurlType t, (([], ""), ""))
              t : nextParts ->
                (parsePurlType t, getVersion (FP.joinPath nextParts))
        in getPurlType path
    in  nullPurl { purlType       = pType
                 , purlNamespace' = pNamespace
                 , purlName       = pName
                 , purlVersion    = pVersion
                 }

parsePurlQualifiers :: String -> Map.Map String String
parsePurlQualifiers =
  let parseQualifiersFindKey accum ('=' : qs') =
        parseQualifiersFindValue accum "" qs'
      parseQualifiersFindKey accum ('&' : qs') =
        (accum, "") : parseQualifiersFindKey "" qs'
      parseQualifiersFindKey accum (c : qs') =
        parseQualifiersFindKey (accum ++ [c]) qs'
      parseQualifiersFindKey ""    [] = []
      parseQualifiersFindKey accum [] = [(accum, "")]
      parseQualifiersFindValue key accum ('&' : qs') =
        (key, URI.decode accum) : parseQualifiersFindKey "" qs'
      parseQualifiersFindValue key accum (c : qs') =
        parseQualifiersFindValue key (accum ++ [c]) qs'
      parseQualifiersFindValue key accum [] = [(key, URI.decode accum)]
  in  \case
        ""         -> mempty
        ('?' : qs) -> Map.fromList $ parseQualifiersFindKey "" qs
        qs         -> Map.fromList $ parseQualifiersFindKey "" qs

parsePurl :: String -> Maybe Purl
parsePurl ('p' : 'k' : 'g' : ':' : '/' : rest) = parsePurl ("pkg:" ++ rest)
parsePurl uriStr                               = case URI.parseURI uriStr of
  Just uri ->
    let
      pScheme = URI.uriScheme uri
      pQualifier = parsePurlQualifiers (URI.uriQuery uri)
      pSubpath   = case URI.uriFragment uri of
        ""       -> ""
        fragment -> tail fragment
      purlFromPath = parsePurlFromPath (URI.uriPath uri)
    in
      if pScheme == purlScheme
        then Just $ purlFromPath{purlQualifiers = pQualifier, purlSubpath = pSubpath}
        else Nothing
  Nothing -> Nothing

instance A.ToJSON Purl where
  toJSON (Purl ty ns n v qs sp) = A.object
    [ "type" A..= ty
    , "namespace" A..= (if null ns
                        then Nothing
                        else Just (FP.joinPath ns))
    , "name" A..= n
    , "version" A..= (if v == ""
                      then Nothing
                      else Just v)
    , "qualifiers" A..= (if qs == mempty
                         then Nothing
                         else Just (A.toJSON qs))
    , "subpath" A..= (if sp == ""
                      then Nothing
                      else Just sp)
    ]

instance A.FromJSON Purl where
  parseJSON (A.String t) = case (parsePurl . T.unpack) t of
    Just p  -> return p
    Nothing -> return $ Purl purlTypeGeneric [] (T.unpack t) "" mempty ""
  parseJSON (A.Object o) =
    let (.::?) :: (A.FromJSON a) => A.Object -> [A.Key] -> A.Parser (Maybe a)
        (.::?) object [] = pure Nothing
        (.::?) object (key:keys) = (object A..:? key) >>= \case
          Nothing -> object .::? keys
          res     -> pure res
      
        parseQualifiers (Just (  A.String t')) = pure . Just $ T.unpack t'
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
    in  Purl
          <$>  (o .::? ["type", "Type"] A..!= purlTypeGeneric)
          <*>  (     ([] `fromMaybe`)
               .     fmap (map FP.dropTrailingPathSeparator . FP.splitPath)
               <$>   o
               .::? ["namespace","Namespace"]
               )
          <*>  (o .::? ["name","Name"] A..!= "")
          <*>  (o .::? ["version","Version"] A..!= "")
          <*>  (o .::? ["qualifiers","Qualifiers"] A..!= mempty)
          <*>  (o .::? ["subpath","Subpath"] A..!= "")
  parseJSON (A.Array  _) = fail "can not parse Array to Purl"
  parseJSON (A.Number _) = fail "can not parse Number to Purl"
  parseJSON (A.Bool   _) = fail "can not parse Bool to Purl"
  parseJSON (A.Null    ) = fail "can not parse Null to Purl"


tryToExtractPurlType :: Purl -> Purl
tryToExtractPurlType purl@Purl {purlType = "generic"} =
  case purlNamespace' purl of
    potentialType : rest -> case parseKnownPurlType potentialType of
      Just knownType -> purl { purlType = knownType, purlNamespace' = rest }
      Nothing        -> purl
    [] -> purl
tryToExtractPurlType purl = purl
heuristicallyRefinePurl :: Purl -> Purl
heuristicallyRefinePurl = normalisePurl . tryToExtractPurlType . normalisePurl
