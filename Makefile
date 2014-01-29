OCAMLBUILD=ocamlbuild
FLAGS=-use-ocamlfind -yaccflags --explain -use-menhir -lib str
INC=src

TEST_FLAGS=$(FLAGS)
TEST_PKGS=oUnit
TEST_INC=$(INC),tests

all: clean test main.native

test: clean basic_tests.native
	@./basic_tests.native

main.native:
	$(OCAMLBUILD) $(FLAGS) -Is $(INC) main.native

basic_tests.native:
	$(OCAMLBUILD) $(TEST_FLAGS) -pkgs $(TEST_PKGS) -Is $(TEST_INC) basic_tests.native

clean:
	ocamlbuild -clean
	find . -name "*~" -exec rm {} \;
