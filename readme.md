## Messy - CHC-based solver for Semantics-Guided Synthesis

This repository contains `Messy`, a Semgus problem solver based on the CHC-based reduction described in the paper [Semantics-Guided Synthesis](https://dl.acm.org/doi/abs/10.1145/3434311). This is a new version of the solver intended to interface with the current standard [Semgus format](https://semgus-git.github.io/Semgus-Site/res/semgus-lang.pdf); those looking for the artifact should consult [this](https://dl.acm.org/do/10.1145/3410258/abs/) instead.

For more information on Semgus, please consult [this](semgus.org).

### Dependencies

```
scala, java (>= 1.8), sbt, z3
```

### Installation

Install the dependencies, clone the repo, then run:

```
sbt assembly
```

The file `Messy.jar` file will be created under `target/scala-2.13/`.

If you would prefer to avoid installation, you can download a fat JAR from the releases page, which you can run directly instead. You will still need to install `scala, java, z3` separately.

### Running Messy

To run Messy on its default setting, run: `scala Messy.jar -i <PATH-TO-INPUT-SEMGUS-FILE>`. This will automatically produce an SMT file `out.z3`, upon which Messy will automatically invoke `z3` to try and solve.

#### Flags

Messy currently supports 4 flags:

`--infile, -i`: Input Semgus file to solve.

`--outfile, -o`: Output SMT file. Defaults to `out.z3`.

`--norun, -n`: Do not automatically run `z3` on the produced SMT file. Use this if you want to try out other CHC solvers.

`--timeout, -t`: The timeout setting for running `z3` on the produced SMT file. Defaults to 10 seconds.

### Caveats

- Messy is currently unable to produce synthesized programs for realizable synthesis problems (even if it can prove that the generated CHC file is `sat`). This is because support for recursive datatypes and non-linear CHCs in `z3` is still highly experimental and will often crash when asked for proof witnesses, from which an actual program can be extracted.
- Messy currently does not support the list / array encoding described in the [paper](https://dl.acm.org/doi/abs/10.1145/3434311), and encodes syntactic representations of terms using algebraic datatypes. This functionality will be added in the future.

