.PHONY: all clean distclean setup build doc install test 
all: build

NAME=openflow
J=4

UNIX ?= $(shell if ocamlfind query lwt.ssl >/dev/null 2>&1; then echo --enable-unix; fi)
DIRECT ?= $(shell if [ $(MIRAGE_NET) = "direct" ]; then echo --enable-direct; fi)
XEN ?= $(shell if [ $(MIRAGE_OS) = "xen" ]; then echo --enable-xen; fi)
# MIRAGE = --enable-mirage

-include Makefile.config

setup.ml: _oasis
	oasis setup 

setup.data: setup.ml
	ocaml setup.ml -configure $(UNIX) $(XEN) $(DIRECT)

clean: setup.data 
	ocaml setup.ml -clean $(OFLAGS)
	rm -f setup.data setup.log setup.ml

distclean: setup.ml setup.data
	ocaml setup.ml -distclean $(OFLAGS)
	rm -f setup.data setup.log setup.ml

setup: setup.data

build: setup.data $(wildcard lib/*.ml)
	ocaml setup.ml -build -j $(J) $(OFLAGS) $(DR)

doc: setup.data setup.ml
	ocaml setup.ml -doc -j $(J) $(OFLAGS)

install: 
	ocamlfind remove $(NAME)
	ocaml setup.ml -install $(OFLAGS)

test: build
	ocaml setup.ml -test


