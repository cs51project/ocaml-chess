all: chess

# These must be in the right order--no forward refs
FILES = util.ml board.ml pieces.ml engine.ml server.ml

chess: $(FILES)
	ocamlopt -o chess unix.cmxa str.cmxa $(FILES)

server: util.ml server.ml
	ocamlc -g -o server unix.cma str.cma util.ml server.ml

check: $(FILES)
	chmod u+x ./check_width
	./check_width $(FILES)

clean: 
	rm -f chess server *.cmi *.cmo
