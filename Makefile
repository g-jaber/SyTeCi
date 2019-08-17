SOURCES = src/debug.ml src/printer.ml src/pmap.ml src/syntax.ml src/parser.mly src/lexer.mll src/type_checker.ml src/logic.ml src/smt.ml src/logic_to_z3.ml src/symb_red.ml src/skor.ml src/unif.ml src/rewrite.ml src/tcstruct.ml src/templogic.ml src/wts.ml src/wts_closure.ml src/wts_to_dot.ml src/chc.ml main.ml
RESULT  = syteci
PACKS = str z3

OCAMLYACC = menhir

OCAMLLDFLAGS = -cclib -lstdc++

THREADS = y

all: native-code

-include OCamlMakefile
