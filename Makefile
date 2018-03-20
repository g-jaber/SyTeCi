SOURCES = pmap.ml syntax.ml parser.mly lexer.mll debug.ml type_checker.ml logic.ml logic_to_smt.ml symb_red.ml skor.ml unif.ml rewrite.ml tcstruct.ml templogic.ml wts.ml wts_closure.ml wts_to_dot.ml main.ml
RESULT  = syteci
PACKS = z3

OCAMLYACC = menhir

all: native-code

-include OCamlMakefile
