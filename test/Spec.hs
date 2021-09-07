{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import           Test.Hspec
import           Test.QuickCheck
import           Data.Maybe (fromJust)

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

main :: IO ()
main = hspec $ describe "PURL" $ do
  it "all example puls should be paseable" $ do
    mapM_ (\purl -> parsePURL purl `shouldNotBe` Nothing) purls
  it "all example puls should adhere some identity" $ do
    mapM_ (\purl -> do
        (show . fromJust . parsePURL) purl `shouldBe` purl) purls
  it "it should decode PURL corectly" $ do
    (_PURL_type =<< (parsePURL "pkg:npm/%40angular/animation@12.3.1")) `shouldBe` (Just PURL_TypeNPM)
    (_PURL_namespace =<< (parsePURL "pkg:npm/%40angular/animation@12.3.1")) `shouldBe` (Just "@angular")
    (fmap _PURL_name (parsePURL "pkg:npm/%40angular/animation@12.3.1")) `shouldBe` (Just "animation")
    (_PURL_version =<< (parsePURL "pkg:npm/%40angular/animation@12.3.1")) `shouldBe` (Just "12.3.1")
