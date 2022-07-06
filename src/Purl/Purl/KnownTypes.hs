{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Purl.Purl.KnownTypes
  ( KnownPurlType (..)
  , knownPurlTypes
  , knownPurlTypeMap
  , isTypeKnown
  ) where

import qualified Data.Aeson                    as A
import qualified Data.Aeson.KeyMap             as A
import qualified Data.Char                     as Char
import           Data.List                      ( intercalate )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import qualified Data.String                   as String
import qualified Data.Text                     as T
import           Data.Typeable
import           GHC.Generics                   ( Generic )
import qualified Network.URI                   as URI
import qualified Network.URI.Encode            as URI
import qualified System.FilePath               as FP
import Text.RawString.QQ

import Purl.Purl.Helper
import           Purl.Purl.Internal

data KnownPurlType
  = KPT
  { getKptPurlType :: PurlType
  , getKptDefaultRepository :: Maybe String
  , getKptNormalizer :: Purl -> Purl
  , getKptValidator :: Purl -> Bool
  , getKptQualifierDefaults :: Map.Map String String
  , getKptDescription :: String
  }
mkKPT :: String -> (KnownPurlType -> KnownPurlType) -> String -> KnownPurlType
mkKPT t f description = f $ KPT (PurlType t) Nothing id (const True) mempty description

defaultRepository :: String -> KnownPurlType -> KnownPurlType
defaultRepository r kpt = kpt{getKptDefaultRepository = Just r}
addNormalizer :: (Purl -> Purl) -> KnownPurlType -> KnownPurlType
addNormalizer n kpt = kpt{getKptNormalizer = n . (getKptNormalizer kpt) } 
addValidator :: (Purl -> Bool) -> KnownPurlType -> KnownPurlType
addValidator v kpt = kpt{getKptValidator = \p -> and [v p, getKptValidator kpt p]} 
addQualifierDefaults :: [(String,String)] -> KnownPurlType -> KnownPurlType
addQualifierDefaults qds kpt = kpt{getKptQualifierDefaults = (getKptQualifierDefaults kpt) <> Map.fromList qds}
addDescription :: String -> KnownPurlType -> KnownPurlType
addDescription d kpt = kpt{getKptDescription = d} 

namespaceCaseInsensitive =
  let namespacesToLowercase (p@Purl { purlNamespace = namespace })
        = p { purlNamespace = fmap stringToLower namespace }
  in  addNormalizer namespacesToLowercase

nameCaseInsensitive =
  let namesToLowercase (p@Purl { purlName = name })
        = p { purlName      = stringToLower name }
  in  namespaceCaseInsensitive . (addNormalizer namesToLowercase)

namespaceMandatory =
  let namespaceMandatoryFun (p@Purl { purlNamespace = Nothing }) = False
      namespaceMandatoryFun (p@Purl { purlNamespace = Just "" }) = False
      namespaceMandatoryFun (p@Purl { purlNamespace = _ }) = True
  in  addValidator namespaceMandatoryFun

versionMandatory =
  let versionMandatoryFun (p@Purl {purlVersion = Nothing}) = False
      versionMandatoryFun (p@Purl {purlVersion = Just ""}) = False
      versionMandatoryFun (p@Purl {purlVersion = _}) = False
  in  addValidator versionMandatoryFun

knownPurlTypes =
  [ mkKPT "bitbucket"
          (defaultRepository "https://bitbucket.org" . nameCaseInsensitive . namespaceMandatory)
          [r|
bitbucket
---------
``bitbucket`` for Bitbucket-based packages:
- The default repository is ``https://bitbucket.org``
- The ``namespace`` is the user or organization. It is not case sensitive and
  must be lowercased.
- The ``name`` is the repository name. It is not case sensitive and must be
  lowercased.
- The ``version`` is a commit or tag
- Examples::

      pkg:bitbucket/birkenfeld/pygments-main@244fd47e07d1014f0aed9c
|]
  , mkKPT "cocoapods" (defaultRepository "https://cdn.cocoapods.org/")
          [r|
cocoapods
---------
``cocoapods`` for Cocoapods:
- The default repository is ``https://cdn.cocoapods.org/``
- The ``name`` is the pod name and is case sensitive, cannot contain whitespace, a plus (+) character, or begin with a period (.).
- The ``version`` is the package version.
- The purl subpath is used to represent a pods subspec (if present)
- Examples::

      pkg:cocoapods/AFNetworking@4.0.1
      pkg:cocoapods/MapsIndoors@3.24.0
      pkg:cocoapods/ShareKit@2.0#Twitter
      pkg:cocoapods/GoogleUtilities@7.5.2#NSData+zlib
|]
  , mkKPT "cargo" (defaultRepository "https://crates.io/")
          [r|
cargo
-----
``cargo`` for Rust:
- The default repository is ``https://crates.io/``
- The ``name`` is the repository name.
- The ``version`` is the package version.
- Examples::

      pkg:cargo/rand@0.7.2
      pkg:cargo/clap@2.33.0
      pkg:cargo/structopt@0.3.11
|]
  , mkKPT "composer" (defaultRepository "https://packagist.org")
          [r|
composer
--------
``composer`` for Composer PHP packages:

- The default repository is ``https://packagist.org``
- The ``namespace`` is the vendor.
- Note: private, local packages may have no name. In this case you cannot
  create a ``purl`` for these.
- Examples::

      pkg:composer/laravel/laravel@5.5.0
|]
  , mkKPT "conan" (defaultRepository "https://center.conan.io" . addValidator (let
      hasChannelQualifier (Purl{purlQualifiers = qs}) = case "channel" `Map.lookup` qs of
        Just "" -> False
        Nothing -> False
        _       -> True
    in \case
          (p@(Purl{purlNamespace = Just _})) -> hasChannelQualifier p
          (p@(Purl{purlNamespace = Nothing})) -> not $ hasChannelQualifier p))
          [r|
conan
-----
``conan`` for Conan C/C++ packages:

- The default repository is ``https://center.conan.io``
- The ``namespace`` is the user if present
- The ``name`` is the package name.
- The ``version`` is the package version.
- The qualifier ``channel`` must be not empty if namespace is present
- Examples::

      pkg:conan/cctz@2.3
      pkg:conan/bincrafters/cctz@2.3?channel=stable
|]
  , mkKPT "conda" (defaultRepository "https://repo.anaconda.com")
          [r|
conda
-----
``conda`` for Conda packages:

- The default repository is ``https://repo.anaconda.com``
- The ``name`` is the package name
- The ``version`` is the package version
- The qualifiers: ``build`` is the build string.
  ``channel`` is the package stored location.
  ``subdir`` is the associated platform.
  ``type`` is the package type.
- Examples::

      pkg:conda/absl-py@0.4.1?build=py36h06a4308_0&channel=main&subdir=linux-64&type=tar.bz2
|]
  , mkKPT "cran" (defaultRepository "https://cran.r-project.org" . versionMandatory)
          [r|
cran
-----
``cran`` for CRAN R packages:

- The default repository is ``https://cran.r-project.org``
- The ``name`` is the package name and is case sensitive, but there cannot be two packages on CRAN with the same name ignoring case.
- The ``version`` is the package version.
- Examples::

      pkg:cran/A3@1.0.0
      pkg:cran/rJava@1.0-4
      pkg:cran/caret@6.0-88
|]
  , mkKPT "deb" (nameCaseInsensitive)
          [r|
deb
---
``deb`` for Debian, Debian derivatives, and Ubuntu packages:

- There is no default package repository: this should be implied either from
  the ``distro`` qualifiers key or using a base url as a ``repository_url``
  qualifiers key
- The ``namespace`` is the "vendor" name such as "debian" or "ubuntu".
  It is not case sensitive and must be lowercased.
- The ``name`` is not case sensitive and must be lowercased.
- The ``version`` is the version of the binary (or source) package.
- ``arch`` is the qualifiers key for a package architecture. The special value
  ``arch=source`` identifies a Debian source package that usually consists of a
  Debian Source control file (.dsc) and corresponding upstream and Debian
  sources. The ``dpkg-query`` command can print the ``name`` and ``version`` of
  the corresponding source package of a binary package::

    dpkg-query -f '${source:Package} ${source:Version}' -W <binary package name>

- Examples::

      pkg:deb/debian/curl@7.50.3-1?arch=i386&distro=jessie
      pkg:deb/debian/dpkg@1.19.0.4?arch=amd64&distro=stretch
      pkg:deb/ubuntu/dpkg@1.19.0.4?arch=amd64
      pkg:deb/debian/attr@1:2.4.47-2?arch=source
      pkg:deb/debian/attr@1:2.4.47-2%2Bb1?arch=amd64
|]
  , mkKPT "docker" (defaultRepository "https://hub.docker.com")
          [r|
docker
------
``docker`` for Docker images

- The default repository is ``https://hub.docker.com``
- The ``namespace`` is the registry/user/organization if present
- The version should be the image id sha256 or a tag. Since tags can be moved,
  a sha256 image id is preferred.
- Examples::

      pkg:docker/cassandra@latest
      pkg:docker/smartentry/debian@dc437cc87d10
      pkg:docker/customer/dockerimage@sha256%3A244fd47e07d10?repository_url=gcr.io
|]
  , mkKPT "gem" (defaultRepository "https://rubygems.org" . addQualifierDefaults [("platform", "ruby")])
          [r|
gem
---
``gem`` for Rubygems:

- The default repository is ``https://rubygems.org``
- The ``platform`` qualifiers key is used to specify an alternative platform
  such as ``java`` for JRuby. The implied default is ``ruby`` for Ruby MRI.
- Examples::

      pkg:gem/ruby-advisory-db-check@0.12.4
      pkg:gem/jruby-launcher@1.1.2?platform=java
|]
  , mkKPT "generic" (id)
          [r|
generic
-------
``generic`` for plain, generic packages that do not fit anywhere else such as
for "upstream-from-distro" packages. In particular this is handy for a plain
version control repository such as a bare git repo.

- There is no default repository. A ``download_url`` and ``checksum`` may be
  provided in `qualifiers` or as separate attributes outside of a ``purl`` for
  proper identification and location.
- When possible another or a new purl ``type`` should be used instead of using
  the ``generic`` type and eventually contributed back to this specification
- as for other ``type``, the ``name`` component is mandatory. In the worst case
  it can be a file or directory name.
- Examples (truncated for brevity)::

      pkg:generic/openssl@1.1.10g
      pkg:generic/openssl@1.1.10g?download_url=https://openssl.org/source/openssl-1.1.0g.tar.gz&checksum=sha256:de4d501267da
      pkg:generic/bitwarderl?vcs_url=git%2Bhttps://git.fsfe.org/dxtr/bitwarderl%40cc55108da32
|]
  , mkKPT "github" (defaultRepository "https://github.com" . nameCaseInsensitive)
          [r|
github
------
``github`` for Github-based packages:

- The default repository is ``https://github.com``
- The ``namespace`` is the user or organization. It is not case sensitive and
  must be lowercased.
- The ``name`` is the repository name. It is not case sensitive and must be
  lowercased.
- The ``version`` is a commit or tag
- Examples::

      pkg:github/package-url/purl-spec@244fd47e07d1004
      pkg:github/package-url/purl-spec@244fd47e07d1004#everybody/loves/dogs
|]
  , mkKPT "golang" (nameCaseInsensitive)
          [r|
golang
------
``golang`` for Go packages

- There is no default package repository: this is implied in the namespace
  using the ``go get`` command conventions
- The ``namespace`` and `name` must be lowercased.
- The ``subpath`` is used to point to a subpath inside a package
- The ``version`` is often empty when a commit is not specified and should be
  the commit in most cases when available.
- Examples::

      pkg:golang/github.com/gorilla/context@234fd47e07d1004f0aed9c
      pkg:golang/google.golang.org/genproto#googleapis/api/annotations
      pkg:golang/github.com/gorilla/context@234fd47e07d1004f0aed9c#api
|]
  , mkKPT "hackage" (defaultRepository "https://hackage.haskell.org")
          [r|
hackage
-------
``hackage`` for Haskell packages

- The default repository is `https://hackage.haskell.org`.
- The `version` is package version.
- The `name` is case sensitive and use kebab-case
- Examples::

      pkg:hackage/a50@0.5
      pkg:hackage/AC-HalfInteger@1.2.1
      pkg:hackage/3d-graphics-examples@0.0.0.2
|]
  , mkKPT "hex" (defaultRepository "https://repo.hex.pm" . nameCaseInsensitive)
          [r|
hex
---
``hex`` for Hex packages

- The default repository is ``https://repo.hex.pm``.
- The ``namespace`` is optional; it may be used to specify the organization for
  private packages on hex.pm. It is not case sensitive and must be lowercased.
- The ``name`` is not case sensitive and must be lowercased.
- Examples::

      pkg:hex/jason@1.1.2
      pkg:hex/acme/foo@2.3.
      pkg:hex/phoenix_html@2.13.3#priv/static/phoenix_html.js
      pkg:hex/bar@1.2.3?repository_url=https://myrepo.example.com
|]
  , mkKPT "maven" (defaultRepository "https://repo.maven.apache.org/maven2")
          [r|
maven
-----
``maven`` for Maven JARs and related artifacts

- The default repository is ``https://repo.maven.apache.org/maven2``
- The group id is the ``namespace`` and the artifact id is the ``name``
- Known qualifiers keys are: ``classifier`` and ``type`` as defined in the
  POM documentation. Note that Maven uses a concept / coordinate called packaging
  which does not map directly 1:1 to a file extension. In this use case, we need
  to construct a link to one of many possible artifacts. Maven itself uses type
  in a dependency declaration when needed to disambiguate between them.
- Examples::

      pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1
      pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1?type=pom
      pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1?classifier=sources
      pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1?type=zip&classifier=dist
      pkg:maven/net.sf.jacob-projec/jacob@1.14.3?classifier=x86&type=dll
      pkg:maven/net.sf.jacob-projec/jacob@1.14.3?classifier=x64&type=dll
|]
  , mkKPT "npm" (defaultRepository "https://registry.npmjs.org" . nameCaseInsensitive)
          [r|
npm
---
``npm`` for Node NPM packages:

- The default repository is ``https://registry.npmjs.org``
- The ``namespace`` is used for the scope of a scoped NPM package.
- Per the package.json spec, new package "must not have uppercase letters in
  the name", therefore the must be lowercased.
- Examples::

      pkg:npm/foobar@12.3.1
      pkg:npm/%40angular/animation@12.3.1
      pkg:npm/mypackage@12.4.5?vcs_url=git://host.com/path/to/repo.git%404345abcd34343
|]
  , mkKPT "nuget" (defaultRepository "https://www.nuget.org") -- TODO: no namespace
          [r|
nuget
-----
``nuget`` for NuGet .NET packages:

- The default repository is ``https://www.nuget.org``
- There is no ``namespace`` per se even if the common convention is to use
  dot-separated package names where the first segment is ``namespace``-like.
- Examples::

      pkg:nuget/EnterpriseLibrary.Common@6.0.1304
|]
  , mkKPT "oci" (nameCaseInsensitive) -- TODO: no namespace
          [r|
oci
------------
``oci`` for all artifacts stored in registries that conform to the
`OCI Distribution Specification <https://github.com/opencontainers/distribution-spec>`_,
including container images built by Docker and others:

- There is no canonical package repository for OCI artifacts. Therefore
  ``oci`` purls must be registry agnostic by default. To specify the repository,
  provide a ``repository_url`` value.
- OCI purls do not contain a ``namespace``, although, ``repository_url`` may
  contain a namespace as part of the physical location of the package.
- The ``name`` is not case sensitive and must be lowercased. The name is the
  last fragment of the repository name. For example if the repository
  name is ``library/debian`` then the ``name`` is ``debian``.
- The ``version`` is the ``sha256:hex_encoded_lowercase_digest`` of the
  artifact and is required to uniquely identify the artifact.
- Optional qualifiers may include:

  - ``arch``: key for a package architecture, when relevant
  - ``repository_url``: A repository URL where the artifact may be found, but not
    intended as the only location. This value is encouraged to identify a
    location the content may be fetched
  - ``tag``: artifact tag that may have been associated with the digest at the time
- Examples::

      pkg:oci/debian@sha256%3A244fd47e07d10?repository_url=docker.io/library/debian&arch=amd64&tag=latest
      pkg:oci/debian@sha256%3A244fd47e07d10?repository_url=ghcr.io/debian&tag=bullseye
      pkg:oci/static@sha256%3A244fd47e07d10?repository_url=gcr.io/distroless/static&tag=latest
      pkg:oci/hello-wasm@sha256%3A244fd47e07d10?tag=v1
|]
  , mkKPT "pub" (defaultRepository "https://pub.dartlang.org" . nameCaseInsensitive) -- TODO: using underscores
          [r|
pub
----
``pub`` for Dart and Flutter packages:

- The default repository is ``https://pub.dartlang.org``
- Pub normalizes all package names to be lowercase and using underscores. The only allowed characters are `[a-z0-9_]`. 
- More information on pub naming and versioning is available in the [pubspec documentation](https://dart.dev/tools/pub/pubspec)
- Examples::

      pkg:pub/characters@1.2.0
      pkg:pub/flutter@0.0.0
|]
  , mkKPT "pypi" (defaultRepository "https://pypi.python.org" . nameCaseInsensitive . addNormalizer (let
      underscoreToDash = map (\c -> if c == '_' then '-' else c)
    in (\p@Purl {purlNamespace = ns, purlName = n} -> p {purlNamespace = fmap underscoreToDash ns, purlName = underscoreToDash n })))
          [r|
pypi
----
``pypi`` for Python packages:

- The default repository is ``https://pypi.python.org``
- PyPi treats ``-`` and ``_`` as the same character and is not case sensitive.
  Therefore a Pypi package ``name`` must be lowercased and underscore ``_``
  replaced with a dash ``-``
- Examples::

      pkg:pypi/django@1.11.1
      pkg:pypi/django-allauth@12.23
|]
  , mkKPT "rpm" (namespaceCaseInsensitive)
          [r|
rpm
---
``rpm`` for RPMs:

- There is no default package repository: this should be implied either from
  the ``distro`` qualifiers key  or using a repository base url as 
  ``repository_url`` qualifiers key
- the ``namespace`` is the vendor such as fedora or opensuse
  It is not case sensitive and must be lowercased.
- the ``name`` is the RPM name and is case sensitive.
- the ``version`` is the combined version and release of an
  RPM
- ``epoch`` (optional for RPMs) is a qualifier as it's not required for
  unique identification, but when the epoch exists we strongly
  encourage using it
- ``arch`` is the qualifiers key for a package architecture
- Examples::

      pkg:rpm/fedora/curl@7.50.3-1.fc25?arch=i386&distro=fedora-25
      pkg:rpm/centerim@4.22.10-1.el6?arch=i686&epoch=1&distro=fedora-25
|]
  , mkKPT "swift" (namespaceMandatory . versionMandatory)
          [r|
swift
-----
``swift`` for Swift packages:

- There is no default package repository: this should be implied from ``namespace``
- The ``namespace`` is source host and user/organization and is required.
- The ``name`` is the repository name.
- The ``version`` is the package version and is required.
- Examples::

      pkg:swift/github.com/Alamofire/Alamofire@5.4.3
      pkg:swift/github.com/RxSwiftCommunity/RxFlow@2.12.4
|]
  ]

knownPurlTypeMap :: Map.Map PurlType KnownPurlType
knownPurlTypeMap = (Map.fromList . map (\ptm -> (getKptPurlType ptm, ptm))) knownPurlTypes

isTypeKnown :: PurlType -> Bool
isTypeKnown = (`Map.member` knownPurlTypeMap)