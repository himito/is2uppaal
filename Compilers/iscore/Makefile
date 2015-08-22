OCAMLC=ocamlc
OCAMLBUILD=ocamlbuild -classic-display -use-menhir -menhir "menhir -v --external-tokens Lexer" -use-ocamlfind -package str

FLAGS=-Is src

TARGET=src/iscore2ta.ml

BIN_NAT=$(patsubst %.ml, %.native, $(TARGET))

all: native 

native:
									$(OCAMLBUILD) $(FLAGS) $(BIN_NAT)

clean:
									$(OCAMLBUILD) -clean

cleanall: clean
									find . -iname *~ -exec rm {} \;

realclean: cleanall

.PHONY: all native clean cleanall
