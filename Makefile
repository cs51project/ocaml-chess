all: chess

# These must be in the right order--no forward refs
FILES = board.ml pieces.ml engine.ml

chess: $(FILES)
	ocamlc -g -o chess unix.cma str.cma $(FILES)

check: $(FILES)
	chmod u+x ./check_width
	./check_width $(FILES)

clean: 
	rm -f chess *.cmi *.cmo
