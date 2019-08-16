SOURCES = debug.ml printer.ml pmap.ml syntax.ml parser.mly lexer.mll type_checker.ml logic.ml smt.ml logic_to_z3.ml symb_red.ml skor.ml unif.ml rewrite.ml tcstruct.ml 	templogic.ml wts.ml wts_closure.ml wts_to_dot.ml chc.ml main.ml
RESULT  = syteci
PACKS = threads str z3

OCAMLYACC = menhir

THREADS = n

all: native-code

-include OCamlMakefile
