# SyTeCi: Automating contextual equivalence for higher-order programs with references

This prototype, written in OCaml, generates, from two programs M1,M2 written in a fragment of ML, the Structured-Memory Transition Machine A_S as described in this [paper](https://guilhem.jaber.fr/syteci/draft.pdf).

One can then check that no failed states are reachable to prove that M1,M2 are contextually equivalent.

In the restricted fragment described in the paper cited above, the prototype can also generates the set of constrained Horn clauses corresponding to the non-reachability of failed states of A_S.
These contrained Horn clauses are written in an extension of the SMT-LIB2 formate described [here](https://rise4fun.com/Z3/tutorialcontent/fixedpoints), than can be checked by the SMT-sover [z3](https://github.com/Z3Prover/z3).


## Compilation

This requires `menhir` and `z3` that should be installed using opam.
You can compile the program by either:
- an invocation of `dune build` followed by `dune install` to build then install the binary syteci.
- a simple invocation of `make` that will create the binary syteci in the current directory.

## Basic Use

To use SyTeCi, you first have to put the programs you want to compare in two different files "file1" and "file2".
Then, you have to invoke the following command:
`syteci file1 file2`

You may find examples presented in the paper in the `testsuite/` directory.

So for example, you can use
`syteci -smtm -chc testsuite/wbsc/prog1.ml testsuite/wbsc/prog2.ml`
to test SyTeCi on the "Well-Bracketed State Change" example.
This will generate the Structured-Memory Transition Machine in dot format, and the set of Constrained Horn Clauses for the non-reachability of failed states in SMT2-LIB format.

To indicate the step-indexes to control the unfolding of recursive definitions, you can use
`syteci -smtm -chc testsuite/impure-factorial/prog1.ml testsuite/impure-factorial/prog2.ml -j 2 -k 2`
to test SyTeCi on the "Impure Factorial" example.

Many other options are available, to discover them you can use
`syteci -help`