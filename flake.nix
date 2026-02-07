{
  description = "io-uring Haskell bindings";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      perSystem =
        {
          config,
          self',
          inputs',
          pkgs,
          system,
          ...
        }:
        let
          # Use GHC 9.12 as requested
          hpkgs = pkgs.haskell.packages.ghc912.override {
            overrides = self: super: {
              # Mark zeromq4-haskell as unbroken if marked broken (common for new GHCs)
              zeromq4-haskell = pkgs.haskell.lib.markUnbroken super.zeromq4-haskell;
            };
          };

          # Use callCabal2nix to generate the package derivation
          io-uring = hpkgs.callCabal2nix "io-uring" ./. {
            liburing = pkgs.liburing;
          };

          # Override to ensure system deps are present
          io-uring-dev = pkgs.haskell.lib.overrideCabal io-uring (drv: {
            # We rely on automatic pkg-config handling
            # Just ensure libraries are present
            libraryPkgconfigDepends = (drv.libraryPkgconfigDepends or [ ]) ++ [ pkgs.liburing ];
            testPkgconfigDepends = (drv.testPkgconfigDepends or [ ]) ++ [ pkgs.liburing ];
            extraLibraries = (drv.extraLibraries or [ ]) ++ [ pkgs.liburing ];
          });
        in
        {
          packages.default = io-uring-dev;
          packages.io-uring = io-uring-dev;

          checks = {
            io-uring-test = io-uring-dev;
          };

          devShells.default = hpkgs.shellFor {
            packages = p: [ io-uring-dev ];
            withHoogle = true;

            nativeBuildInputs = [
              pkgs.pkg-config
              pkgs.cabal-install
              hpkgs.haskell-language-server
              hpkgs.ghcid
              hpkgs.hlint
              hpkgs.ormolu
            ];

            buildInputs = [
              pkgs.liburing
              pkgs.zeromq
            ];

            shellHook = ''
              export PKG_CONFIG_PATH="${pkgs.liburing}/lib/pkgconfig:${pkgs.zeromq}/lib/pkgconfig:$PKG_CONFIG_PATH"
              echo "io-uring development environment"
            '';
          };
        };
    };
}
