{ mkDerivation, aeson, base, bytestring, containers, dialog-flow
, http-client, http-client-tls, mtl, pokeapi, servant
, servant-server, stdenv, text, transformers, unordered-containers
, wai, warp, ghcid
}:
mkDerivation {
  pname = "poke-home";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers dialog-flow http-client
    http-client-tls mtl pokeapi servant servant-server text
    transformers unordered-containers wai warp
  ];
  executableHaskellDepends = [
    aeson base bytestring containers http-client http-client-tls mtl
    pokeapi servant servant-server text transformers
    unordered-containers wai warp ghcid
  ];
  testHaskellDepends = [
    aeson base bytestring containers http-client http-client-tls mtl
    pokeapi servant servant-server text transformers
    unordered-containers wai warp
  ];
  homepage = "https://github.com/githubuser/poke-home#readme";
  license = stdenv.lib.licenses.bsd3;
}
