build:
	make -C src/ build

test:
	make -C tests/ test

report:
	make -C tests/ report

clean:
	make -C src/ clean
	make -C tests/ clean