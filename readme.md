## Messy - CHC-based solver for Semantics-Guided Synthesis

This repository contains `Messy`, a Semgus problem solver based on the CHC-based reduction described in the paper [Semantics-Guided Synthesis](https://dl.acm.org/doi/abs/10.1145/3434311). This is a new version of the solver intended to interface with the current standard [Semgus format](https://semgus-git.github.io/Semgus-Site/res/semgus-lang.pdf); those looking for the artifact should consult [this](https://dl.acm.org/do/10.1145/3410258/abs/) instead.

For more information on Semgus, please consult [this](https://semgus.org).

### Dependencies

```
scala, java (>= 1.8), z3, Semgus-Parser
```

`Messy` also has dependencies on external libraries such as Scallop and SemgusJava. This should not be a concern if you are running Messy within sbt, or through the provided fat JAR.

### Running Messy

To run Messy, download the fat JAR from the releases page, and run:
```
`scala Messy.jar -i <PATH-TO-INPUT-SEMGUS-FILE>
```
This will produce an SMT file `out.z3`, upon which an external CHC solver (such as Z3) can be run.

The input to Messy should not be a raw SemGuS file, but a SemGuS file parsed to the JSON representation. For this, we recommend the parser hosted at (https://github.com/SemGuS-git/Semgus-Parser).

### Flags

`--infile, -i`: Input Semgus file to solve.

`--outfile, -o`: Output SMT file. Defaults to `out.z3`.

### Installation

If you wish to tweak with Messy directly, you should first install `sbt` and clone the repo. After that, 
run `sbt assembly` to produce the fat JAR on the releases page, or `sbt run` to run Messy directly from within `sbt`.

### Caveats

- Messy is currently unable to produce synthesized programs for realizable synthesis problems (even if it can prove that the generated CHC file is `sat`). This is because support for recursive datatypes and non-linear CHCs in `z3` is still highly experimental and will often crash when asked for proof witnesses, from which an actual program can be extracted.
- Messy currently does not support the list / array encoding described in the [paper](https://dl.acm.org/doi/abs/10.1145/3434311), and encodes syntactic representations of terms using algebraic datatypes. This functionality is expected to be added in the future.

