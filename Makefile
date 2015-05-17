
OPT= -lflags -g -cflag -g -use-menhir -yaccflags --explain,--strict

.PHONY: default test main.byte

default: main.byte

main.byte :
	ocamlbuild ${OPT} src/main.byte

test: main.byte
	ocamlrun main.byte ./examples/config.d

clean:
	rm -rf _build
