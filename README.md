# Five

Five is a standalone implementation of the classic five-stage
processor pipeline that can be understood and verified independently of
any particular instruction set. That said, we do provide a RISC-V
microcontroller called
[FiveAlive](https://github.com/blarney-lang/five-alive) that
demonstrates use of the pipeline in an actual processor core.  Five is
written in Haskell using the
[Blarney](https://github.com/blarney-lang/blarney) library. 

Five was developed on the [CAPcelerate
project](https://gow.epsrc.ukri.org/NGBOViewGrant.aspx?GrantRef=EP/V000381/1),
part of the UKRI's Digital Security by Design programme.

## Dependencies

First, download the repo:

```sh
git clone --recursive https://github.com/blarney-lang/five
```

We'll need Verilator, a RISC-V compiler, the z3 solver (version 4.12.2
known working), and GHC (version 9.4.5 known working).

On Ubuntu 22.04, we can do:
```sh
sudo apt install verilator
sudo apt install gcc-riscv64-unknown-elf
sudo apt install libgmp-dev
```

Version 4.12.2 of z3 can be found in this [zip
file](https://github.com/Z3Prover/z3/releases/download/z3-4.12.2/z3-4.12.2-x64-glibc-2.31.zip).
For GHC 9.4.5, [ghcup](https://www.haskell.org/ghcup/) can be used.

If you have trouble meeting any of the dependencies, you can simply
enter a docker shell:

```sh
make shell
```

## Usage

To generate SMT formulae capturing the correctness of the pipeline:

```sh
cabal run
```

The formulae are written to the `SMT/` directory. To run these through
the z3 solver:

```sh
make verify
```

Each property should evaluate to `unsat`, i.e. no counterexamples are
found.
