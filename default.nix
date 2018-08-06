(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    frontend = ./progress-frontend;
  };

  shells = {
    ghcjs = ["frontend"];
  };
})
