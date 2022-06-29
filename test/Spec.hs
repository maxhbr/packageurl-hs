{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import           Data.FileEmbed                 ( embedFile )
import           Data.Maybe                     ( fromJust, fromMaybe )
import           Test.Hspec
import           Test.QuickCheck
import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy          as B

import           PURL.PURL



npmPurl = "pkg:npm/%40angular/animation@12.3.1"
purls =
  [ "pkg:bitbucket/birkenfeld/pygments-main@244fd47e07d1014f0aed9c"
  , "pkg:deb/debian/curl@7.50.3-1?arch=i386&distro=jessie"
--   , "pkg:docker/cassandra@sha256:244fd47e07d1004f0aed9c"
--   , "pkg:docker/customer/dockerimage@sha256:244fd47e07d1004f0aed9c?repository_url=gcr.io"
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

data TestSuiteCase
  = TestSuiteCase
  { _desrciption :: String
  , _input_purl :: String
  , _canonical_purl :: String
  , _parsed_purl :: PURL
  , _is_invalid :: Bool
  } deriving (Show)
instance A.FromJSON TestSuiteCase where
  parseJSON = A.withObject "TestSuiteCase" $ \c -> do
    ty <- c A..:? "type"
    ns <- c A..:? "namespace"
    n <- ("$null" `fromMaybe`) <$> c A..:? "name"
    v <- c A..:? "version"
    -- q <- c A..:? "qualifiers"
    let q' = Nothing
    s <- c A..:? "subpath"

    TestSuiteCase <$> c A..: "description"
                   <*> c A..: "purl"
                   <*> (("$null" `fromMaybe`) <$> c A..:?"canonical_purl")
                   <*> (pure (PURL (Just defaultPurlScheme) ty ns n v q' s))
                   <*> c A..: "is_invalid"



purlTestSuite =
  let testSuiteData :: B.ByteString
      testSuiteData =
        B.fromStrict
          $(embedFile "purl-spec/test-suite-data.json")
  in case A.eitherDecode testSuiteData :: Either String [TestSuiteCase] of
    Right cs -> mapM_ (\c -> do
        it ((_desrciption c) ++ ": " ++ (_input_purl c)) $ 
          if (_is_invalid c)
          then do
            parsePURL (_input_purl c) `shouldBe` Nothing
          else do
            let parsedInputPurl = parsePURL (_input_purl c)
            parsedInputPurl `shouldNotBe` Nothing
            parsePURL (_canonical_purl c) `shouldNotBe` Nothing
            parsedInputPurl `shouldBe` (Just (_parsed_purl c))
      ) cs
    Left err -> it "fail on failure of parsing :)" $ do
      err `shouldBe` ""

main :: IO ()
main = hspec $ describe "PURL" $ do
  it "all example puls should be paseable" $ do
    mapM_ (\purl -> parsePURL purl `shouldNotBe` Nothing) purls
  it "all example puls should adhere some identity" $ do
    mapM_
      (\purl -> do
        (show . fromJust . parsePURL) purl `shouldBe` purl
      )
      purls
  it "it should decode PURL corectly" $ do
    (_PURL_type =<< (parsePURL "pkg:npm/%40angular/animation@12.3.1"))
      `shouldBe` (Just PURL_TypeNPM)
    (_PURL_namespace =<< (parsePURL "pkg:npm/%40angular/animation@12.3.1"))
      `shouldBe` (Just "@angular")
    (fmap _PURL_name (parsePURL "pkg:npm/%40angular/animation@12.3.1"))
      `shouldBe` (Just "animation")
    (_PURL_version =<< (parsePURL "pkg:npm/%40angular/animation@12.3.1"))
      `shouldBe` (Just "12.3.1")
  purlTestSuite
