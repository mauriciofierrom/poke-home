{ mkDerivation, aeson, aeson-pretty, base, bytestring, containers
, directory, fetchgit, hspec, hspec-discover, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "dialogflow-fulfillment";
  version = "0.1.1.4";
  src = fetchgit {
    url = "https://github.com/mauriciofierrom/dialogflow-fulfillment";
    sha256 = "0whzji9sxvd70jj5ygadhd5c392l66jl48a8xsl9kl8p0b0crrb6";
    rev = "2840ec45ffe3ae63e7b11158b1d373b39d73796f";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base bytestring containers text unordered-containers
  ];
  testHaskellDepends = [
    aeson aeson-pretty base bytestring containers directory hspec
    hspec-discover unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/mauriciofierrom/dialogflow-fulfillment";
  description = "A Dialogflow Fulfillment library for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
