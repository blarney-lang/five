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

We then have the following dependencies:

  * GHC (version 9.4.5 known working, available [here](https://www.haskell.org/ghcup/))
  * z3 solver (version 4.12.2 known working, available [here](https://github.com/Z3Prover/z3/releases/download/z3-4.12.2/z3-4.12.2-x64-glibc-2.31.zip)),
  * SymbiYosys OSS CAD Suite (version 2024-11-27 known working, available [here](https://github.com/YosysHQ/oss-cad-suite-build/releases/download/2024-11-27/oss-cad-suite-linux-x64-20241127.tgz))

If you have trouble meeting any of the dependencies, you can simply
enter a docker shell:

```sh
make shell
```

## Usage

The pipeline can be verified either via SymbiYosys or the z3 SMT solver.

Verilog and SMT files for verification can be generated as follows.

```sh
make gen
```

The outputs are written to the `gen/` directory and can be checked as follows.

```sh
cd gen
sby Correctness.sby -f       # Check using SymbiYosys
sby ForwardProgress.sby -f   # Check using SymbiYosys
z3 Correctness.smt2          # Check using z3
z3 ForwardProgress.smt2      # Check using z3
```

If z3 is in your `PATH` then the following command can be used.

```sh
make verify
```

For unbounded checking, which requires around ten hours to complete, the following command can be used.

```sh
make verify-unbounded
```

## Acknowledgements

Thanks to Alexandre Joannou and Victor Miquel for their contributions to Blarney's SMT backend, which allowed verification of all pipeline properties.
Five was developed on the [CAPcelerate
project](https://gow.epsrc.ukri.org/NGBOViewGrant.aspx?GrantRef=EP/V000381/1),
part of the UKRI's Digital Security by Design programme.
