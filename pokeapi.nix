{ mkDerivation, aeson, base, hpack, http-client, http-client-tls
, servant, servant-client, stdenv, text, transformers, wai
}:
mkDerivation {
  pname = "pokeapi";
  version = "0.1.0.0";
  src = /home/mauricio/projects/haskell/pokeapi;
  libraryHaskellDepends = [
    aeson base http-client http-client-tls servant servant-client text
    transformers wai
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base http-client http-client-tls servant servant-client text
    transformers wai
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/githubuser/pokeapi#readme";
  license = stdenv.lib.licenses.bsd3;
}
