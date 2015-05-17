
OPT= -lflags -g -cflag -g -use-menhir -yaccflags --explain,--strict

.PHONY: default

default:
	ocamlbuild ${OPT} src/main.byte

clean:
	rm -rf _build
