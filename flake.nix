{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; }
      {
        systems = [
          "x86_64-linux"
          "aarch64-linux"
        ];

        perSystem = { self', lib, system, pkgs, config, ... }: {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;

            overlays = with inputs; [
              rust-overlay.overlays.default
            ];
          };

          packages = rec {
            default = duat;
            duat = pkgs.callPackage (import ./nix/package.nix) { };
          };

          devShells = {
            default =
              let
                duat = pkgs.callPackage (import ./nix/package.nix) { };
                rust-toolchain = pkgs.rust-bin.stable.latest.default.override
                  {
                    extensions = [ "rust-src" "rust-analyzer" ];
                  };
              in
              pkgs.mkShell {
                packages = [ rust-toolchain ];

                buildInputs = duat.buildInputs;
                nativeBuildInputs = duat.nativeBuildInputs;

                LD_LIBRARY_PATH = duat.LD_LIBRARY_PATH;
              };
          };
        };
      };
}
