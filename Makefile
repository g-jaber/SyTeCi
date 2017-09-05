SOURCES = parser.mly lexer.mll pmap.ml syntax.ml type_checker.ml unif.ml logic.ml  symb_red.ml skor.ml tcstruct.ml templogic.ml wts.ml wts_closure.ml main.ml
RESULT  = syteci

OCAMLYACC = menhir


-include OCamlMakefile
