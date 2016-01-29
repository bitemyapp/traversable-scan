with (import <nixpkgs> {});
with haskellPackages;
let inherit (haskell) lib; in
lib.addExtraLibraries (callPackage ./. {}) [ criterion ]
