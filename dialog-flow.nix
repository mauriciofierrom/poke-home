{ mkDerivation, aeson, base, stdenv, text }:
mkDerivation {
  pname = "dialog-flow";
  version = "0.1.0.0";
  src = /home/mauricio/projects/haskell/dialog-flow;
  libraryHaskellDepends = [ aeson base text ];
  homepage = "https://github.com/mauriciofierrom/dialog-flow";
  description = "A Dialog Flow library for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
