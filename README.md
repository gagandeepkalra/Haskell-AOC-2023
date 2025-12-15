# Haskell Advent of Code
This repository contains my solutions for the [Advent of Code](https://adventofcode.com/) programming puzzles, implemented in Haskell.

## Structure

- Solutions are organized by year and day under the `src/AOC/Y_<year>/Day<day>.hs` directory structure.
- Each solution module typically exposes a `solve` function, which prints outputs for both parts of the day's puzzle.
- To explore the source or run solutions, see examples below.

## Building & Running

You will need the [Stack](https://docs.haskellstack.org/en/stable/README/) tool.

**To build the project:**
```sh
stack build
```

# To run a specific year and day (e.g., 2024 Day 10):
```sh
stack run 2024 10
```

**To run a particular day’s solution from GHCI:**
```sh
stack ghci
> :module AOC.Y_2025.Day03
> solve "<your input here>"
```

## Advent of Code Years & Days

- **2023:** Days 1–6 implemented
- **2024:** Days 1–15 implemented
- **2025:** Days 1– implemented

See the `src/AOC` directory for more details.

## Features

- Uses parallel strategies where helpful for performance (see 2025/Day03), ```stack run 2025 03 -- +RTS -N5 -RTS```
- Includes parsing combinators (Parsec), modern idiomatic Haskell and strong warnings enabled.
- Fast part 2 timings using GHC and runtime flags (see `package.yaml`).

## Contributing

Pull requests, suggestions, and comments are welcome! Open an issue or create a PR.

## License

BSD-3-Clause, see `package.yaml` for details.

Enjoy Haskell and happy puzzling!

# Download sources
To download all dependency sources for offline exploration, you can use the following commands with Stack:

```sh
# Lists all installed dependencies except your project
stack ls dependencies | \
  grep -v "Haskell-World" | \
  grep -E "^([^ ]+)\s+([0-9.]+)" | \
  while read -r pkg ver; do
    stack unpack "${pkg}-${ver}" --to ./.stack-work/libraries
  done
```

This will unpack all the Stackage package sources for your project into `.stack-work/libraries`.

# Haddock
To generate local Haddock documentation for your project using Stack, run:

```sh
stack haddock --open
```

This will build your project (if needed) and generate HTML documentation for all modules, then open it in your browser.

You can also find the generated documentation in the `.stack-work` directory, typically at:

```
.stack-work/dist/<arch>/Cabal-*/doc/html/Haskell-World/index.html
```

Replace `<arch>` and `Cabal-*` with your build's architecture and Cabal version.
For more details, see [the Stack documentation on Haddock](https://docs.haskellstack.org/en/stable/GUIDE/#generating-haddock-documentation).
