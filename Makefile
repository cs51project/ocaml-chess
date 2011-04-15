all: chess

FILES = board.ml engine.ml server.ml
ALL_FILES = $(FILES) index.html css/style.css js/chess.js

chess: $(FILES)
	ocamlopt -o chess unix.cmxa str.cmxa $(FILES)

check: $(FILES)
	chmod u+x ./check_width
	./check_width $(ALL_FILES)

clean: 
	rm -f chess *.cmi *.cmo *.cmx *.o
