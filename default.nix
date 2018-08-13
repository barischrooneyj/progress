(import ./reflex-platform {}).project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    common = ./common;
    frontend = ./frontend;
    simple-store = ./backend/simple-store;
  };

  shells = {
    ghc = ["common" "frontend" "simple-store"];
    ghcjs = ["common" "frontend"];
  };
})
