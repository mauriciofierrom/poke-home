{ mkDerivation, aeson, aeson-pretty, base, bytestring, containers
, directory, fetchgit, hspec, hspec-discover, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "dialogflow-fulfillment";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/mauriciofierrom/dialogflow-fulfillment.git";
    sha256 = "1pb35bp7jk6qf6dky6rg5n4ngs6sbywdxbgygaiivwxa2b205cs0";
    rev = "cc4507f17769638161bc9d08ddf8c70ad5c7161f";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base bytestring containers text unordered-containers
  ];
  testHaskellDepends = [
    aeson aeson-pretty base bytestring containers directory hspec
    hspec-discover
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/mauriciofierrom/dialogflow-fulfillment";
  description = "A Dialogflow Fulfillment library for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
