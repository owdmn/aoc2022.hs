# minimal nix setup for projecteuler.hs
let
  myNixPkgs = import <nixpkgs> {};
in
myNixPkgs.mkShell {
  nativeBuildInputs = with myNixPkgs; [
    cabal-install
    ghc
    stack
    git
    zlib
  ];
}
