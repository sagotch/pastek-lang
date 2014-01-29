OCAMLBUILD=ocamlbuild

FLAGS=-use-ocamlfind -yaccflags --explain -use-menhir -lib str
INC=src

TEST_FLAGS=$(FLAGS)
TEST_PKGS=oUnit
TEST_INC=$(INC),tests

COVERAGE_FLAGS=$(TEST_FLAGS)
COVERAGE_TAGS=package\(bisect\),syntax\(camlp4o\),syntax\(bisect_pp\)
COVERAGE_INC=$(TEST_INC)

all: clean test main.native

test: clean basic_tests.native
	@./basic_tests.native

main.native:
	$(OCAMLBUILD) $(FLAGS) -Is $(INC) main.native

basic_tests.native:
	$(OCAMLBUILD) $(TEST_FLAGS) -pkgs $(TEST_PKGS) -Is $(TEST_INC) basic_tests.native

coverage:
	$(OCAMLBUILD) $(COVERAGE_FLAGS) -pkgs $(TEST_PKGS) -tags $(COVERAGE_TAGS) -Is $(COVERAGE_INC) basic_tests.byte
	BISECT_FILE=_build/coverage ./basic_tests.byte
	cd _build && bisect-report -verbose -html report coverage*.out

clean:
	ocamlbuild -clean
	find . -name "*~" -exec rm {} \;