let
  pkgs = import <nixpkgs> {};
  reflexPkgs = import ./reflex-platform { };
  ghc-env = reflexPkgs.ghc.ghcWithPackages (p: with p; [
      # For use with the intero emacs package.
      intero
      reflex-dom
    ]);
  ghcjs-env = reflexPkgs.ghcjs.ghcWithPackages (p: with p; [
      reflex-dom
    ]);
in pkgs.buildEnv {
  name = "ghc-and-ghcjs-env";
  paths = [
      ghc-env
      ghcjs-env
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.stack
      pkgs.nodejs
    ];
}