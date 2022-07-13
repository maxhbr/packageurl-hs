-- SPDX-FileCopyrightText: Maximilian Huber <oss@maximilian-huber.de>
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import qualified Data.ByteString.Lazy          as B
import           Data.Char                      ( isSpace )
import           Data.FileEmbed                 ( embedFile )
import           Data.List                      ( intercalate
                                                , isPrefixOf
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                )
import           Test.Hspec
import           Test.QuickCheck

import           Purl.Purl
import           Purl.Purl.Internal             ( normalisePath
                                                , stringToLower
                                                )



npmPurl = "pkg:npm/%40angular/animation@12.3.1"
purls =
  [ "pkg:bitbucket/birkenfeld/pygments-main@244fd47e07d1014f0aed9c"
  , "pkg:deb/debian/curl@7.50.3-1?arch=i386&distro=jessie"
  , "pkg:docker/cassandra@sha256%3A244fd47e07d1004f0aed9c"
  , "pkg:docker/customer/dockerimage@sha256%3A244fd47e07d1004f0aed9c?repository_url=gcr.io"
  , "pkg:gem/jruby-launcher@1.1.2?platform=java"
  , "pkg:gem/ruby-advisory-db-check@0.12.4"
  , "pkg:github/package-url/purl-spec@244fd47e07d1004f0aed9c"
  , "pkg:golang/google.golang.org/genproto#googleapis/api/annotations"
  , "pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1?packaging=sources"
  , "pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1?repository_url=repo.spring.io/release"
  , npmPurl
  , "pkg:npm/foobar@12.3.1"
  , "pkg:nuget/EnterpriseLibrary.Common@6.0.1304"
  , "pkg:pypi/django@1.11.1"
  , "pkg:rpm/fedora/curl@7.50.3-1.fc25?arch=i386&distro=fedora-25"
  , "pkg:rpm/opensuse/curl@7.56.1-1.1.?arch=i386&distro=opensuse-tumbleweed"
  ]

data TestSuiteCase = TestSuiteCase
  { _desrciption    :: String
  , _input_purl     :: String
  , _canonical_purl :: String
  , _parsed_purl    :: Purl
  , _is_invalid     :: Bool
  }
  deriving Show
instance A.FromJSON TestSuiteCase where
  parseJSON = A.withObject "TestSuiteCase" $ \c -> do
    nameIsNotNull <- isJust <$> (c A..:? "name" :: A.Parser (Maybe String))
    TestSuiteCase
      <$>  c
      A..: "description"
      <*>  c
      A..: "purl"
      <*>  (("$null" `fromMaybe`) <$> c A..:? "canonical_purl")
      <*>  (if nameIsNotNull then A.parseJSON (A.Object c) else pure undefined)
      <*>  c
      A..: "is_invalid"

purlTestSuite =
  let testSuiteData :: B.ByteString
      testSuiteData =
        B.fromStrict
          $(embedFile "purl-spec/test-suite-data.json")
  in case A.eitherDecode testSuiteData :: Either String [TestSuiteCase] of
    Right cs -> mapM_ (\c -> do
        let parsedInputPurl = parsePurl (_input_purl c)
        let prefix = "[" ++ _input_purl c ++ "]: " ++ _desrciption c ++ ": "
        if (_is_invalid c)
          then do
            case parsedInputPurl of
              Just p -> it (prefix ++ "should be invalid: " ++ (show p)) $ do
                 not (isPurlValid p)

              Nothing ->
                it (prefix ++ "should be invalid") $ do
                  True `shouldBe` True
          else do
            it (show (purlType (_parsed_purl c)) ++ " of " ++ _input_purl c ++ " should be a known type") $
              (isTypeKnown (purlType (_parsed_purl c))) `shouldBe` True
            it (prefix ++ "should successfully parse " ++ (_input_purl c)) $ do
              parsedInputPurl `shouldNotBe` Nothing
            it (prefix ++ "should successfully parse " ++ (_canonical_purl c)) $ do
              parsePurl (_canonical_purl c) `shouldNotBe` Nothing
            case parsedInputPurl of
              Just parsedInputPurl' -> do
                it (prefix ++ "provided parsed should match ") $ do
                  A.toJSON (normalisePurl parsedInputPurl') `shouldBe` A.toJSON (_parsed_purl c)
                  normalisePurl parsedInputPurl' `shouldBe` normalisePurl (_parsed_purl c)
                it (prefix ++ "should not be changed by tryToExtractPurlType") $ do
                  tryToExtractPurlType parsedInputPurl' `shouldBe` parsedInputPurl'
      ) cs
    Left err -> it "fail on failure of parsing :)" $ do
      err `shouldBe` ""

purlKnownTypeSpec kpt =
  let
    pt   = getKptPurlType kpt
    desc = getKptDescription kpt
    examplesFromDescription =
      (map (dropWhile isSpace) . filter ("      pkg:" `isPrefixOf`) . lines)
        desc
  in
    do
      it (pt ++ " should be in description") $ do
        desc `shouldContain` pt
      case getKptDefaultRepository kpt of
        Just d -> it (d ++ " should be in description") $ do
          desc `shouldContain` d
        Nothing -> pure ()
      it ("there should be examples for " ++ pt) $ do
        length examplesFromDescription > 0 `shouldBe` True
      mapM_
        (\e -> do
          let parsedE = parsePurl e
          case parsedE of
            Just parsedE' -> do
              it (e ++ " should be a valid example for " ++ pt) $ do
                purlType parsedE' `shouldBe` pt
            _ -> do
              it (e ++ " should be parseable") $ do
                parsedE `shouldNotBe` Nothing
        )
        examplesFromDescription

purlTypesSpec = describe "PurlType" $ do
  mapM_ (purlKnownTypeSpec) knownPurlTypes

main :: IO ()
main = hspec $ do
  purlTypesSpec
  describe "Purl" $ do
    it "path normalization should work as expected" $ do
      mapM_
        (\(raw, normal) -> normalisePath raw `shouldBe` normal)
        [ ("test/some/normal/path", "test/some/normal/path")
        , ("test/some//path"      , "test/some/path")
        , ("test/some///path"     , "test/some/path")
        , ("/test/some/path"      , "test/some/path")
        , ("//test/some/path"     , "test/some/path")
        , ("///test/some/path"    , "test/some/path")
        , ("test/some/path/"      , "test/some/path")
        ]
    it "all example puls should be paseable" $ do
      mapM_ (\purl -> parsePurl purl `shouldNotBe` Nothing) purls
    it "all example puls should adhere some identity" $ do
      mapM_
        (\purl -> do
          (show . fromJust . parsePurl) purl `shouldBe` purl
        )
        purls
    it "it should decode Purl corectly" $ do
      (purlType <$> (parsePurl "pkg:npm/%40angular/animation@12.3.1"))
        `shouldBe` (Just "npm")
      (purlNamespace <$> (parsePurl "pkg:npm/%40angular/animation@12.3.1"))
        `shouldBe` (Just "@angular")
      (purlName <$> (parsePurl "pkg:npm/%40angular/animation@12.3.1"))
        `shouldBe` (Just "animation")
      (purlVersion <$> (parsePurl "pkg:npm/%40angular/animation@12.3.1"))
        `shouldBe` (Just "12.3.1")
    purlTestSuite
