# Five

Five is a standalone implementation of the classic five-stage
processor pipeline that can be understood and verified independently
of any particular instruction set. Separately, we provide a RISC-V
microcontroller called
[FiveAlive](https://github.com/blarney-lang/five-alive) that
demonstrates use of the pipeline in an actual processor core.  Five is
written in [Blarney](https://github.com/blarney-lang/blarney). 


## Dependencies

First, download the repo:

```sh
git clone --recursive https://github.com/blarney-lang/five
```

We'll need the z3 solver (version 4.12.2 known working) and GHC (version 9.4.5 known working). Version 4.12.2 of z3 can be found in this [zip
file](https://github.com/Z3Prover/z3/releases/download/z3-4.12.2/z3-4.12.2-x64-glibc-2.31.zip).
For GHC 9.4.5, [ghcup](https://www.haskell.org/ghcup/) can be used.

If you have trouble meeting any of the dependencies, you can simply
enter a docker shell:

```sh
make shell
```

## Usage

The pipeline can be verified using both bounded and unbounded model checking on top of the z3 solver.

For bounded checking, SMT formulae can be generated as follows.

```sh
cabal run blarney-five-gen
```

The formulae are written to the `SMT/` directory. Each `.smt` file can be
passed to the z3 solver, and should evaluate to `unsat`, i.e. no
counterexamples are found.

If z3 is in your `PATH` then we recommend using the following command, which
verifies the properties directly and incrementally.

```sh
cabal run blarney-five-verify
```

For unbounded checking, which requires around ten hours to complete, use the
command:

```sh
cabal run blarney-five-verify-unbounded
```

## Acknowledgements

Thanks to Alexandre Joannou and Victor Miquel for there contributions to Blarneyy's SMT backend, which allowed verification of all pipeline properties.
Five was developed on the [CAPcelerate
project](https://gow.epsrc.ukri.org/NGBOViewGrant.aspx?GrantRef=EP/V000381/1),
part of the UKRI's Digital Security by Design programme.
