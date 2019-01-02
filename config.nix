{
    packageOverrides = pkgs: with pkgs; {
        # ...other customizations...
        haskellPackages = haskellPackages.override {
            extension = self : super : {
                cabal = pkgs.haskellPackages.cabalNoTest;
            };
        };
    };
}