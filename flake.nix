{
  description = "A Haskell project template.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    # Example of adding or overriding a package:
    # "example-input" should be added to the outputs,
    # i.e., "output = { self, nixpkgs, flake-utils, example-input }:"
    # exapmle-input = {
    #   url = "github:SheetKey/tigris";
    #   flake = true;
    # };

  };

  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        packageName = "proc-gen-hs";

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

      in {
        packages.${packageName} = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
            # Ex with gi-gtk-declarative:
            # If version is broken then:
            # gi-gtk-declarative = jailbreakUnbreak haskeppPackages.gi-gtk-declarative;
            # or if tests failing: 
            # gi-gtk-declarative = pkgs.haskell.lib.dontCheck haskellPackages.gi-gtk-declarative;

            #example = haskellPackages.callCabal2nix "example" example-input { };
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            haskellPackages.implicit-hie
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      }
    );
}
