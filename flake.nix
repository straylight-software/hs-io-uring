{
  description = "io-uring Haskell bindings";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    
    # Pin to nvidia-sdk's nixpkgs for consistency with the rest of the stack
    nvidia-sdk.url = "github:weyl-ai/nvidia-sdk";
    nixpkgs.follows = "nvidia-sdk/nixpkgs";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          # Use GHC 9.10 matching the project requirements
          hpkgs = pkgs.haskell.packages.ghc910;

          # Overlay local package
          haskellPackages = hpkgs.override {
            overrides = hself: hsuper: {
              io-uring = pkgs.haskell.lib.overrideCabal 
                (hself.callCabal2nix "io-uring" ./. { 
                  liburing = pkgs.liburing; 
                  uring = pkgs.liburing;
                })
                (drv: {
                  # Ensure pkg-config dependencies are propagated
                  libraryPkgconfigDepends = (drv.libraryPkgconfigDepends or []) ++ [ pkgs.liburing ];
                  testPkgconfigDepends = (drv.testPkgconfigDepends or []) ++ [ pkgs.liburing ];
                  # Ensure library is linked
                  extraLibraries = (drv.extraLibraries or []) ++ [ pkgs.liburing ];
                });
            };
          };
        in
        {
          packages.default = haskellPackages.io-uring;
          packages.io-uring = haskellPackages.io-uring;
          
          apps.chat-client = {
            type = "app";
            program = "${haskellPackages.io-uring}/bin/chat-client";
          };

          devShells.default = haskellPackages.shellFor {
            packages = p: [ p.io-uring ];
            withHoogle = true;

            nativeBuildInputs = with pkgs; [
              cabal-install
              pkg-config
              haskellPackages.haskell-language-server
              haskellPackages.ghcid
              haskellPackages.ormolu
              haskellPackages.hlint
            ];

            buildInputs = [ pkgs.liburing ];

            shellHook = ''
              export LD_LIBRARY_PATH="${pkgs.liburing}/lib:$LD_LIBRARY_PATH"
              echo "io-uring development environment (GHC 9.10)"
            '';
          };
        };
    };
}
