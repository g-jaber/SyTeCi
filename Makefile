SOURCES = parser.mly lexer.mll pmap.ml syntax.ml type_checker.ml logic.ml  symb_red.ml skor.ml tcstruct.ml templogic.ml main.ml
RESULT  = syteci

OCAMLYACC = menhir


-include OCamlMakefile
