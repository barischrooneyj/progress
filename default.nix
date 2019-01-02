(import ./reflex-platform {}).project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    common = ./common;
    frontend = ./frontend;
    telescope = ./telescope;
  };

  shells = {
    ghc = ["common" "frontend" "telescope"];
    ghcjs = ["common" "frontend"];
  };
})
