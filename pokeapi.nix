{ mkDerivation, aeson, base, fetchgit, http-client, http-client-tls
, servant, servant-client, stdenv, text, transformers, url, wai
}:
mkDerivation {
  pname = "pokeapi";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/mauriciofierrom/pokeapi.git";
    sha256 = "080y13xni3pgzy8vav7shr45h46swp1qn92yf8ybbdh4zkr26pgc";
    rev = "db4dc8097609bcac7e0e586f939c211b9755af03";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base http-client http-client-tls servant servant-client text
    transformers url wai
  ];
  testHaskellDepends = [
    aeson base http-client http-client-tls servant servant-client text
    transformers url wai
  ];
  homepage = "https://github.com/mauriciofierrom/hpokeapi#readme";
  license = stdenv.lib.licenses.bsd3;
}
