OCAMLBUILD=ocamlbuild

FLAGS=-use-ocamlfind -yaccflags --explain -use-menhir -lib str
INC=src

TEST_FLAGS=$(FLAGS)
TEST_PKGS=oUnit
TEST_INC=$(INC),tests

COVERAGE_FLAGS=$(TEST_FLAGS)
COVERAGE_TAGS=package\(bisect\),syntax\(camlp4o\),syntax\(bisect_pp\)
COVERAGE_INC=$(TEST_INC)

EXEC=pastek

all: clean test $(EXEC).native

test: clean parser_basic.native
	@./parser_basic.native

$(EXEC).native:
	$(OCAMLBUILD) $(FLAGS) -Is $(INC) $@

parser_basic.native:
	$(OCAMLBUILD) $(TEST_FLAGS) -pkgs $(TEST_PKGS) -Is $(TEST_INC) $@

coverage:
	$(OCAMLBUILD) $(COVERAGE_FLAGS) -pkgs $(TEST_PKGS) -tags $(COVERAGE_TAGS) -Is $(COVERAGE_INC) parser_basic.byte
	BISECT_FILE=_build/coverage ./parser_basic.byte
	cd _build && bisect-report -verbose -html report coverage*.out

clean:
	ocamlbuild -clean
	find . -name "*~" -exec rm {} \;