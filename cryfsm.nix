{ mkDerivation, aeson, base, bytestring, containers, cryptol
, data-default, fgl, filepath, graphviz, lens, monad-loops
, monad-supply, monadLib, mtl, optparse-applicative, stdenv, text
, total-map, transformers, universe-base
, universe-reverse-instances, unordered-containers, vector
}:
mkDerivation {
  pname = "cryfsm";
  version = "0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers cryptol data-default fgl filepath
    graphviz lens monad-loops monad-supply monadLib mtl
    optparse-applicative text total-map transformers universe-base
    universe-reverse-instances unordered-containers vector
  ];
  description = "convert cryptol expressions to finite state machines";
  license = stdenv.lib.licenses.bsd3;
}
