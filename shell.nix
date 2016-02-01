with (import <nixpkgs> {});
let
  inherit (pkgs.haskell) lib;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      vector = (pkgs.ttuegel or super).vector;
    };
  };
in
with haskellPackages;
lib.addExtraLibraries (callPackage ./. {}) [ criterion ]
