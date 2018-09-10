SOURCES = debug.ml printer.ml pmap.ml syntax.ml parser.mly lexer.mll type_checker.ml logic.ml smt.ml logic_to_z3.ml symb_red.ml skor.ml unif.ml rewrite.ml tcstruct.ml 	templogic.ml wts.ml wts_closure.ml wts_to_dot.ml pushdown_system.ml chc.ml pred_abstr.ml main.ml
RESULT  = syteci
PACKS = str z3

OCAMLYACC = menhir

all: native-code

-include OCamlMakefile
