include ../Makefile.generic

PCKGS=unix,str,toml
SRC_FILES=type.ml parser.ml lexer.ml render.ml render_html.ml pastek.ml

EXEC=pastek

OBJ=$(SRC_FILES:.ml=.cmo)

.SECONDARY: parser.ml lexer.ml

build: $(EXEC)

$(EXEC): $(OBJ)
	$(CAMLC) -linkpkg -o $@ $^

parser.cmo: type.cmi parser.cmi
lexer.cmo: type.cmi parser.cmi lexer.cmi
render.cmo: type.cmi parser.cmi lexer.cmi render.cmi

clean: generic_clean
	rm -f parser.ml parser.mli lexer.ml $(EXEC)