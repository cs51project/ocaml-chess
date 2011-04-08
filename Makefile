all: chess

# These must be in the right order--no forward refs
FILES = board.ml pieces.ml engine.ml server.ml

chess: $(FILES)
	ocamlc -g -o chess unix.cma str.cma $(FILES)

server: server.ml
	ocamlc -g -o server unix.cma str.cma server.ml

check: $(FILES)
	chmod u+x ./check_width
	./check_width $(FILES)

clean: 
	rm -f chess server *.cmi *.cmo
