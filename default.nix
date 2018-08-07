(import ./reflex-platform {}).project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    frontend = ./progress-frontend;
  };

  shells = {
    ghc = ["frontend"];
    ghcjs = ["frontend"];
  };
})
