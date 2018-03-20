# SyTeCi: Towards Automated Proofs of Contextual Equivalence for ML Programs via Symbolic, Temporal and Circular Reasoning

This prototype generates, from two programs M1, M2, an inference graph S that relates the two programs.
It then generates the characteristic temporal formula associated to S, and the transition system of worlds S.
One can then check that no failed states are reachable to prove that M1,M2 are contextually equivalent.

## Compilation

This requires menhir. Then, a simple invocation of `make` should create the binary syteci.

## Basic Use

To use SyTeCi, you first have to put the programs you want to compare in two different files "file1" and "file2".
Then, you have to invoke the following command:
`syteci file1 file2`

So for example, you can use
`./syteci testsuite/wbsc/prog1.ml testsuite/wbsc/prog2.ml`
to test SyTeCi on the "Well-Bracketed State Change" example.
This will generate the Symbolic Transition System in dot format.

Many other options are available, to discover them you can use
`./syteci -help`
