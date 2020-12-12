MODULES= main battle characters authors save episode sp_play mp_play
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

clean:
	ocamlbuild -clean

zip: 
	zip -r avatar.zip *.mli* *.ml* *.json* *.txt* _tags Makefile
