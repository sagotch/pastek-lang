OCAMLBUILD=ocamlbuild
FLAGS=-use-ocamlfind -yaccflags --explain -use-menhir -package unix,str,toml
INC=src
EXEC=pastek
TARGET=byte #FIXME: set TARGET=native if ocamlopt is available

# MAIN PROGRAM

all: build

build: $(TARGET)
	mv $(EXEC).$(TARGET) $(EXEC)

native byte:
	$(OCAMLBUILD) $(FLAGS) -Is $(INC) $(EXEC).$@

# CLEANING

clean:
	ocamlbuild -clean
	find . -name "*~" -exec rm {} \;
	find . -name "#*#" -exec rm {} \;

# TEST

TEST_FLAGS=$(FLAGS)
TEST_PKGS=oUnit
TEST_INC=$(INC),tests

parser_basic.native:
	$(OCAMLBUILD) $(TEST_FLAGS) -pkgs $(TEST_PKGS) -Is $(TEST_INC) $@

test: parser_basic.native
	@./parser_basic.native


# CODE COVERAGE

COVERAGE_FLAGS=$(TEST_FLAGS)
COVERAGE_TAGS=package\(bisect\),syntax\(camlp4o\),syntax\(bisect_pp\)
COVERAGE_INC=$(TEST_INC)

coverage:
	$(OCAMLBUILD) $(COVERAGE_FLAGS) -pkgs $(TEST_PKGS) -tags $(COVERAGE_TAGS) -Is $(COVERAGE_INC) parser_basic.byte
	BISECT_FILE=_build/coverage ./parser_basic.byte

report: coverage
	cd _build && bisect-report -verbose -html report coverage*.out
