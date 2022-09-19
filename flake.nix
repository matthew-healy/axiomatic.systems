{
  description = "the website axiomatic.systems";

  nixConfig.bash-prompt = "\n\\033[1m\\033[38;5;219m[axiomatic.systems]\\033[38;5;45mλ\\033[0m ";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = {};
        overlays = [ (import ./nix/haskell-overlay.nix) ];
        pkgs = import nixpkgs { inherit config overlays system; };
      in rec {
        defaultPackage = packages.website;

        packages = with pkgs.axiomaticSystems.haskellPackages; { inherit generator website; };

        apps.default = flake-utils.lib.mkApp {
          drv = packages.generator;
          exePath = "/bin/site";
        };

        devShell = pkgs.axiomaticSystems.haskellPackages.shellFor {
          packages = p: [ p.generator ];

          buildInputs = with pkgs.axiomaticSystems.haskellPackages; [
            generator
            ghcid
            haskell-language-server
            pkgs.nodePackages.tailwindcss
          ];

          withHoogle = true;
        };
      }
    );
}