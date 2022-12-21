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
  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, flake-utils-plus, deploy-rs }:
    let
      nixosModules = flake-utils-plus.lib.exportModules (
        nixpkgs.lib.mapAttrsToList (name: value: ./nixosModules/${name}) (builtins.readDir ./nixosModules)
      );
      # overlays = [ haskellNix.overlay
      #   (final: prev: {
      #     redirect-to-xpsoasis =
      #       final.haskell-nix.project' {
      #         src = ./.;
      #         shell.tools = {
      #           cabal = {};
      #           haskell-language-server = {};
      #         };
      #         shell.buildInputs = with pkgs; [
      #           stack
      #           nixpkgs-fmt
      #           postgresql
      #           nixUnstable
      #           inputs.deploy-rs.defaultPackage.x86_64-linux
      #         ];
      #         shell.additional = hsPkgs: with hsPkgs; [ Cabal ];
      #         # This adds `js-unknown-ghcjs-cabal` to the shell.
      #         # shell.crossPlatforms = p: [p.ghcjs];
      #       };
      #     redirect-to-xpsoasis-wrapper = pkgs.writeShellApplication {
      #       name = "redirect-to-xpsoasis-wrapped";
      #       runtimeInputs = [ self.packages.x86_64-linux.default ];
      #       text = ''
      #        cd /home/xpsoasis/redirect-to-xpsoasis
      #        ${self.packages.x86_64-linux.default}/bin/redirect-to-xpsoasis
      #       '';
      #     };
      #   })
      # ];
      # pkgs = import nixpkgs { system = "x86_64-linux"; inherit overlays; inherit (haskellNix) config; };
      # flake = pkgs.redirect-to-xpsoasis.flake {};
      flake-deploy-rs = flake-utils-plus.lib.mkFlake {
        inherit self inputs nixosModules;

        hosts = {
          hetzner.modules = with nixosModules; [
            # redirect-to-xpsoasis
            common
            admin
            hardware-hetzner
          ];
        };

        # Change these parameters to server parameters.
        deploy.nodes = {
          my-node = {
            hostname = "65.109.136.226";
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

    # in flake-utils.lib.eachSystem [ "x86_64-linux" ] (system: flake // {
    #     packages = flake.packages // {
    #       default = flake.packages."redirect-to-xpsoasis:exe:redirect-to-xpsoasis";
    #       redirect-to-xpsoasis-wrapper = pkgs.redirect-to-xpsoasis-wrapper;
    #     };
    #     apps = flake.apps // { default = flake.apps."redirect-to-xpsoasis:exe:redirect-to-xpsoasis"; };

    #     legacyPackages = pkgs;
    #   })// flake-deploy-rs;
    in flake-deploy-rs;

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
