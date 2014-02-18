build:
	make -C src/ build

test:
	make -C tests/ test

clean:
	make -C src/ clean
	make -C tests/ clean