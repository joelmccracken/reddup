{ mkDerivation, aeson, array, async, base, base16-bytestring
, binary, bytestring, Chart, Chart-diagrams, containers
, cryptohash-sha1, data-default, deepseq, diagrams, diagrams-svg
, directory, extra, fetchgit, filepath, fuzzy, ghc, ghc-boot
, ghc-boot-th, ghc-check, ghc-paths, ghc-typelits-knownnat, gitrev
, haddock-library, hashable, haskell-lsp, haskell-lsp-types
, hie-bios, hslogger, lens, lsp-test, mtl, network-uri
, optparse-applicative, parser-combinators, prettyprinter
, prettyprinter-ansi-terminal, process, QuickCheck
, quickcheck-instances, regex-tdfa, rope-utf16-splay
, safe-exceptions, shake, sorted-list, stdenv, stm, syb, tasty
, tasty-expected-failure, tasty-hunit, tasty-quickcheck
, tasty-rerun, text, time, transformers, unix, unordered-containers
, utf8-string, yaml
}:
mkDerivation {
  pname = "ghcide";
  version = "0.2.0";
  src = fetchgit {
    url = "https://github.com/digital-asset/ghcide";
    sha256 = "1mpq87k6jsda8d5swa67m6dnn62qrg872fx18wprghq8l4vbbcan";
    rev = "8de10e9474898b43c66581f40fe0eea6741a286b";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async base binary bytestring containers data-default
    deepseq directory extra filepath fuzzy ghc ghc-boot ghc-boot-th
    haddock-library hashable haskell-lsp haskell-lsp-types hslogger mtl
    network-uri prettyprinter prettyprinter-ansi-terminal regex-tdfa
    rope-utf16-splay safe-exceptions shake sorted-list stm syb text
    time transformers unix unordered-containers utf8-string
  ];
  executableHaskellDepends = [
    aeson async base base16-bytestring binary bytestring Chart
    Chart-diagrams containers cryptohash-sha1 data-default deepseq
    diagrams diagrams-svg directory extra filepath ghc ghc-check
    ghc-paths gitrev hashable haskell-lsp haskell-lsp-types hie-bios
    hslogger optparse-applicative safe-exceptions shake text time
    unordered-containers yaml
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory extra filepath ghc
    ghc-typelits-knownnat haddock-library haskell-lsp haskell-lsp-types
    lens lsp-test network-uri parser-combinators QuickCheck
    quickcheck-instances rope-utf16-splay shake tasty
    tasty-expected-failure tasty-hunit tasty-quickcheck tasty-rerun
    text
  ];
  benchmarkHaskellDepends = [
    aeson base bytestring containers directory extra filepath lsp-test
    optparse-applicative parser-combinators process safe-exceptions
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/digital-asset/ghcide#readme";
  description = "The core of an IDE";
  license = stdenv.lib.licenses.asl20;
}
