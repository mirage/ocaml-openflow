.PHONY: all clean distclean setup build doc install test 
all: build

NAME=openflow
J=4

LWT ?= $(shell if ocamlfind query lwt.ssl >/dev/null 2>&1; then echo --enable-lwt; fi)
MIRAGE ?= $(shell if ocamlfind query mirage-net >/dev/null 2>&1; then echo --enable-mirage; fi)

-include Makefile.config

setup.data: _oasis
	oasis setup 
	ocaml setup.ml -configure $(LWT) --disable-mirage 

clean: setup.data 
	ocaml setup.ml -clean $(OFLAGS)
	rm -f setup.data setup.log setup.ml

distclean: setup.ml setup.data
	ocaml setup.ml -distclean $(OFLAGS)
	rm -f setup.data setup.log setup.ml

setup: setup.data

build: setup.data $(wildcard lib/*.ml)
	oasis setup 
	ocaml setup.ml -configure $(LWT) --disable-mirage 
	ocaml setup.ml -build -j $(J) $(OFLAGS)

doc: setup.data setup.ml
	ocaml setup.ml -doc -j $(J) $(OFLAGS)

install: 
	ocamlfind remove $(NAME)
	ocaml setup.ml -install $(OFLAGS)

test: build
	ocaml setup.ml -test


