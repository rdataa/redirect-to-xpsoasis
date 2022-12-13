{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";
  inputs.deploy-rs = {
    url = "github:serokell/deploy-rs";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, deploy-rs, flake-utils-plus }:
    let
      nixosModules = flake-utils-plus.lib.exportModules (
        nixpkgs.lib.mapAttrsToList (name: value: ./nixosModules/${name}) (builtins.readDir ./nixosModules)
      );
      overlays = [ haskellNix.overlay
        (final: prev: {
          redirect-to-xpsoasis =
            final.haskell-nix.project' {
              src = ./.;
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
              };
              shell.buildInputs = with pkgs; [
                stack
                nixpkgs-fmt
              ];
              shell.additional = hsPkgs: with hsPkgs; [ Cabal ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { system = "x86_64-linux"; inherit overlays; inherit (haskellNix) config; };
      flake = pkgs.redirect-to-xpsoasis.flake {};
      flake-deploy-rs = flake-utils-plus.lib.mkFlake {
        inherit self inputs nixosModules;

        hosts = {
          hetzner.modules = with nixosModules; [
            redirect-to-xpsoasis
            common
            admin
            hardware-hetzner
          ];
        };

        deploy.nodes = {
          my-node = {
            hostname = "127.0.0.1";
            fastConnection = false;
            profiles = {
              my-profile = {
                sshUser = "admin";
                path =
                  inputs.deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.hetzner;
                user = "root";
              };
            };
          };
        };
      };

    in flake-utils.lib.eachSystem [ "x86_64-linux" ] (system: flake // {
        packages = flake.packages // {
          default = flake.packages."redirect-to-xpsoasis:exe:redirect-to-xpsoasis";
        };
        apps = flake.apps // { default = flake.apps."redirect-to-xpsoasis:exe:redirect-to-xpsoasis"; };

        legacyPackages = pkgs;
      })// flake-deploy-rs;
  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };

}
