# Build Instructions

## Requirements

- Linux kernel 5.10+ (for socket I/O support in io_uring)
- liburing 2.0+ and development headers
- GHC 9.6+
- cabal-install 3.0+
- hsc2hs (for FFI generation)

## Quick Start

### Option 1: Using Nix (Recommended)

```bash
cd /tmp/haskell-io-uring
nix-shell                    # Enters environment with all deps
cabal build                  # Build library
cabal test                   # Run full test suite
```

### Option 2: Manual Build

```bash
# Install dependencies (Ubuntu/Debian)
sudo apt-get install liburing-dev ghc cabal-install pkg-config

# Build
cd /tmp/haskell-io-uring
cabal update

# Generate FFI bindings (if not already generated)
hsc2hs src/System/IoUring/Internal/FFI.hsc
hsc2hs src/System/IoUring/URing.hsc

# Build
cabal build

# Test (requires privileges for io_uring on some kernels)
cabal test
```

## Running Tests

```bash
# All tests
cabal test

# Specific test pattern
cabal test --test-option='--pattern=socket'

# With timing
cabal test --test-option='--timeit'

# Skip io_uring tests (if kernel doesn't support)
cabal build -f io-uring-skip-tests
cabal test -f io-uring-skip-tests
```

## Running Benchmarks

```bash
# Build benchmark executable
cabal build bench

# Run benchmarks
cabal run bench -- high NoCache /path/to/testfile

# Options: low | high
#          Cache | NoCache  
#          filepath
```

## Verification

Check your environment has everything:

```bash
# Check kernel version (need 5.10+)
uname -r

# Check liburing
pkg-config --modversion liburing

# Check GHC
ghc --version

# Test io_uring support
cat << 'EOF' > /tmp/test_uring.c
#include <liburing.h>
int main() {
  struct io_uring ring;
  return io_uring_queue_init(32, &ring, 0);
}
EOF

gcc -o /tmp/test_uring /tmp/test_uring.c $(pkg-config --cflags --libs liburing)
/tmp/test_uring && echo "io_uring is available"
```

## Troubleshooting

**"liburing.h file not found"**
- Install liburing-dev (Ubuntu/Debian) or liburing2-devel (RHEL/Fedora)
- Or use nix-shell which sets up everything

**"Operation not permitted" in tests**
- io_uring requires privileges on some kernels
- Run with sudo or add CAP_IPC_LOCK capability
- Or skip tests with `-f io-uring-skip-tests`

**HSC files not generated**
- hsc2hs should auto-run via build-tool-depends
- If not: `hsc2hs path/to/file.hsc`
