include ../Makefile.generic

PCKGS=unix,str,toml,oUnit,bisect
COV_FILES=parser.ml lexer.ml
SRC_FILES=type.ml $(COV_FILES)
INC=-I ../src/

TEST_FILES=parser_basic parser_block_suit

%.test: %.cmo
	$(CAMLC) -linkpkg -o $@ $(SRC_FILES:.ml=.cmo) $<

test: $(TEST_FILES:=.test)
	$(foreach file, $^, BISECT_FILE=coverage ./$(file);)

$(COV_FILES:.ml=.cmo):
	make -C ../src CAMLC="$(CAMLC) -syntax camlp4o -ppopt -no_quot" $@

%.cmo:
	make -C ../src $@

parser_basic.cmo parser_advanced.cmo: $(SRC_FILES:.ml=.cmo)

# CODE COVERAGE

report: test
	cd ../src && bisect-report -I ../tests/ -verbose -html ../tests/report ../tests/coverage*.out

clean: generic_clean
	rm -rf report
	rm -f coverage*.out
	rm -f oUnit-anon.cache
	rm -f $(TEST_FILES:=.test)
	rm -f ../src/*.cmp