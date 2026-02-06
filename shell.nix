# shell.nix - Development environment for io-uring Haskell library

{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  name = "io-uring-devel";

  buildInputs = with pkgs; [
    # Haskell toolchain
    ghc
    cabal-install
    hsc2hs

    # System dependencies
    liburing
    pkg-config

    # Useful for testing
    netcat-openbsd # for socket tests
    socat # for complex socket scenarios

    # Development tools
    hlint
    ormolu
    haskell-language-server
  ];

  shellHook = ''
    echo "io-uring Haskell development environment"
    echo ""
    echo "To build:"
    echo "  hsc2hs src/System/IoUring/Internal/FFI.hsc"
    echo "  hsc2hs src/System/IoUring/URing.hsc"
    echo "  cabal build"
    echo ""
    echo "To test:"
    echo "  cabal test"
    echo ""
    echo "Note: Kernel must support io_uring (Linux 5.10+ for socket ops)"
  '';

  # Environment variables
  PKG_CONFIG_PATH = "${pkgs.liburing}/lib/pkgconfig:$PKG_CONFIG_PATH";
}
