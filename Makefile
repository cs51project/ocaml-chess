all: chess

# These must be in the right order--no forward refs
FILES = board.ml engine.ml server.ml

#chess: $(FILES)
#	ocamlopt -o chess unix.cmxa str.cmxa $(FILES)

chess: $(FILES)
	ocamlc -o chess unix.cma str.cma $(FILES)

check: $(FILES)
	chmod u+x ./check_width
	./check_width $(FILES)

clean: 
	rm -f chess server *.cmi *.cmo
