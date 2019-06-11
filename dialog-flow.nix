{ mkDerivation, aeson, base, hspec, hspec-discover, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "dialog-flow";
  version = "0.1.0.0";
  src = /home/mauricio/projects/haskell/dialog-flow;
  libraryHaskellDepends = [ aeson base text unordered-containers ];
  testHaskellDepends = [ aeson base hspec hspec-discover ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/mauriciofierrom/dialog-flow";
  description = "A Dialog Flow library for Haskell";
  license = stdenv.lib.licenses.bsd3;
}