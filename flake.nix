{
  description = "Trinity Engine - High-performance async I/O engine for Haskell using Linux io_uring";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";

    # sensenet provides Buck2 build infrastructure
    sensenet = {
      url = "github:straylight-software/sensenet/dev";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      imports = [
        # Import sensenet's Buck2 module
        inputs.sensenet.flakeModules.buck2
        inputs.sensenet.flakeModules.std
      ];

      perSystem =
        { pkgs, system, ... }:
        let
          # GHC 9.12 with sensenet's haskell overlay
          inherit (pkgs.haskell.packages) ghc912;
        in
        {
          # ══════════════════════════════════════════════════════════════════════
          # Trinity Engine - Buck2 build
          # ══════════════════════════════════════════════════════════════════════
          # Usage: nix develop .#buck2-default
          #        buck2 build //:trinity-echo
          buck2.projects.default = {
            src = ./.;
            targets = [
              "//:trinity-echo"
              "//:trinity-http"
              "//:trinity-proxy"
            ];
            toolchain = {
              cxx.enable = true;
              haskell = {
                enable = true;
                ghcpackages = ghc912;
                packages = hp: [
                  # Core deps
                  hp.primitive
                  hp.vector
                  hp.bytestring
                  hp.network
                  hp.stm
                  hp.zeromq4-haskell
                  hp.warp
                  hp.wai
                  hp.http-types
                  hp.katip
                  hp.optparse-applicative
                  hp.async
                  hp.random
                  hp.time
                  # Test deps
                  hp.tasty
                  hp.tasty-hunit
                  hp.tasty-quickcheck
                  hp.QuickCheck
                  hp.temporary
                  hp.unix
                ];
              };
            };
            extrapackages = [
              pkgs.liburing
              pkgs.zeromq
              pkgs.pkg-config
            ];
            extrabuckconfigsections = ''

              [trinity]
              liburing_include = ${pkgs.liburing.dev}/include
              liburing_lib = ${pkgs.liburing}/lib
            '';
            devshellpackages = [
              pkgs.dhall
              pkgs.dhall-json
              ghc912.haskell-language-server
            ];
            devshellhook = ''
              export PKG_CONFIG_PATH="${pkgs.liburing}/lib/pkgconfig:${pkgs.zeromq}/lib/pkgconfig:$PKG_CONFIG_PATH"
              export LIBRARY_PATH="${pkgs.liburing}/lib:${pkgs.zeromq}/lib:$LIBRARY_PATH"
              export C_INCLUDE_PATH="${pkgs.liburing.dev}/include:$C_INCLUDE_PATH"

              echo ""
              echo "  Trinity Engine development environment"
              echo "  buck2 build //:trinity-echo"
              echo ""
            '';
          };

          # Legacy cabal-based devshell (keep for compatibility)
          devShells.cabal =
            let
              hpkgs = pkgs.haskell.packages.ghc912.override {
                overrides = self: super: {
                  zeromq4-haskell = pkgs.haskell.lib.markUnbroken super.zeromq4-haskell;
                };
              };
              trinity-engine = hpkgs.callCabal2nix "trinity-engine" ./. {
                liburing = pkgs.liburing;
              };
            in
            hpkgs.shellFor {
              packages = p: [ trinity-engine ];
              withHoogle = true;
              nativeBuildInputs = [
                pkgs.pkg-config
                pkgs.cabal-install
                hpkgs.haskell-language-server
              ];
              buildInputs = [
                pkgs.liburing
                pkgs.zeromq
              ];
              shellHook = ''
                export PKG_CONFIG_PATH="${pkgs.liburing}/lib/pkgconfig:${pkgs.zeromq}/lib/pkgconfig:$PKG_CONFIG_PATH"
                echo "Trinity Engine cabal development environment"
              '';
            };
        };
    };
}
