SOURCES = parser.mly lexer.mll pmap.ml syntax.ml type_checker.ml logic.ml symb_red.ml skor.ml unif.ml rewrite.ml tcstruct.ml templogic.ml wts.ml wts_closure.ml wts_to_dot.ml main.ml
RESULT  = syteci

OCAMLYACC = menhir

all: native-code

-include OCamlMakefile
