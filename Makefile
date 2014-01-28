OCAMLBUILD = ocamlbuild
FLAGS := -use-ocamlfind -yaccflags --explain -use-menhir -lib str
INC := -I src

TEST_FLAGS = $(FLAGS) -pkgs oUnit
TEST_INC = $(INC) -I tests

all: clean test main.native

test: clean basic_tests.native
	@./basic_tests.native

main.native:
	$(OCAMLBUILD) $(FLAGS) $(INC) main.native

basic_tests.native:
	$(OCAMLBUILD) $(TEST_FLAGS) $(TEST_INC) basic_tests.native

clean:
	ocamlbuild -clean
	find . -name "*~" -exec rm {} \;
