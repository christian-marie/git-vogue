{ mkDerivation, base, bifunctors, Cabal, containers, cpphs, Diff
, directory, extra, filepath, formatting, git, haskell-src-exts
, hlint, hscolour, hspec, optparse-applicative, process, split
, stdenv, strict, stylish-haskell, temporary, text, transformers
, unix
}:
mkDerivation {
  pname = "git-vogue";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base containers directory extra filepath formatting
    optparse-applicative process split text transformers unix
  ];
  executableHaskellDepends = [
    base bifunctors Cabal cpphs Diff directory haskell-src-exts hlint
    hscolour optparse-applicative process strict stylish-haskell text
  ];
  testHaskellDepends = [
    base containers directory filepath hspec process temporary
  ];
  testToolDepends = [ git ];
  homepage = "https://github.com/christian-marie/git-vogue";
  description = "A framework for pre-commit checks";
  license = stdenv.lib.licenses.bsd3;
}
