# io-uring

Haskell bindings to Linux io_uring, extending Well-Typed's blockio-uring to support both file and socket I/O operations.

## Features

- **Unified API**: Submit both file (disk) and socket (network) operations in a single batch
- **Type-safety**: GADTs ensure correct usage of operations
- **Performance**: One io_uring per GHC capability for lock-free operation
- **Safety**: Automatic buffer pinning and error handling

## Requirements

- Linux kernel 5.10+ (for socket I/O support)
- liburing 2.0+
- GHC 9.6+

## Quick Start

```bash
nix-shell                    # Enter development environment
cabal build                  # Build library
cabal test                   # Run tests
cabal run bench -- high N    # Run benchmarks
```

## Usage Example

```haskell
import System.IoUring

main :: IO ()
main = withIoUring defaultIoUringParams $ \ctx -> do
  -- Submit mixed batch of file and socket ops
  results <- submitBatch ctx $ \prep -> do
    prep $ ReadOp fd1 buf1 offset1 count1
    prep $ RecvOp sock buf2 flags
    prep $ SendOp sock buf3 flags
  
  -- Process results
  forM_ results $\ case
    Complete bytes -> putStrLn $ "Success: " ++ show bytes
    Errno e       -> putStrLn $ "Error: " ++ show e
```

## Architecture

The library extends Well-Typed's design:

1. **Per-capability rings**: Each GHC capability gets its own io_uring instance for lock-free submission
2. **Typed operations**: GADT `SockOp a` tracks operation type and result type
3. **Batch splitting**: Large batches automatically split to respect ring limits
4. **Completion thread**: One thread per capability handles completions via STM

## Testing

Tests cover:
- Type safety properties (GADT usage)
- Concurrent batch submission
- Socket echo server/client benchmark
- Error handling (EBADF, ECONNRESET)
- Completion ordering invariants

Run with `cabal test --test-options="--timeout=60"`

## License

BSD-3-Clause (same as Well-Typed's blockio-uring)

## Acknowledgments

This library extends Well-Typed's excellent [blockio-uring](https://github.com/well-typed/blockio-uring) library, generalizing it from disk-only to support both file and socket I/O operations.
