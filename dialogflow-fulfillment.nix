{ mkDerivation, aeson, aeson-pretty, base, bytestring, containers
, directory, fetchgit, hspec, hspec-discover, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "dialogflow-fulfillment";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/mauriciofierrom/dialogflow-fulfillment.git";
    sha256 = "136n6q0bdvxlb3in89dq0g254bni461wkfznbgkpyp92dqhgyasn";
    rev = "2f46ecc798476fe4ed5dc1cf02eb7629e0b1d66b";
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
