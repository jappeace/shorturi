import ./pin.nix {
  config = {

    packageOverrides = pkgs: {
        haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
        packageOverrides = hpNew: hpOld: {
            shorturi = hpNew.callPackage ../default.nix {};
            };
        };
    };
  };
}
