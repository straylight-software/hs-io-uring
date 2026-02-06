#!/usr/bin/env bash
set -euo pipefail

echo "Building io-uring Haskell library..."

# Generate FFI bindings from hsc files
if command -v hsc2hs &>/dev/null; then
	echo "Generating bindings with hsc2hs..."
	hsc2hs src/System/IoUring/Internal/FFI.hsc
	hsc2hs src/System/IoUring/URing.hsc
fi

# Build
echo "Building with cabal..."
cabal update
cabal build

echo "Build complete!"
echo ""
echo "To run tests:"
echo "  cabal test"
