# SyTeCi: Towards Automated Proofs of Contextual Equivalence for ML Programs via Symbolic, Temporal and Circular Reasoning

This prototype generates, from two programs M1, M2, an inference graph S that relates the two programs.
It then generates:
- the characteristic temporal formula associated to S;
- the structured-memory transition machine A_S.
One can then check that no failed states are reachable to prove that M1,M2 are contextually equivalent.
In the restricted fragment described in the paper, it can also generates the set of constrained Horn clauses corresponding to the non-reachability of failed states of A


## Compilation

This requires menhir and z3 that can be installed using opam
You can compile the program by either:
- an invocation of `dune install` followed by `dune install` to build then install the binary syteci.
- a simple invocation of `make` that will create the binary syteci in the current directory.

## Basic Use

To use SyTeCi, you first have to put the programs you want to compare in two different files "file1" and "file2".
Then, you have to invoke the following command:
`syteci file1 file2`

So for example, you can use
`syteci testsuite/wbsc/prog1.ml testsuite/wbsc/prog2.ml`
to test SyTeCi on the "Well-Bracketed State Change" example.
This will generate the Structured-Memory Transition Machine in dot format.

Many other options are available, to discover them you can use
`./syteci -help`
