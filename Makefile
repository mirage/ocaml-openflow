.PHONY: all clean distclean setup build doc install test 
all: build

NAME=openflow
J=4

UNIX ?= $(shell if ocamlfind query lwt.ssl >/dev/null 2>&1; then echo --enable-unix; else echo --disable-unix; fi)
DIRECT ?= $(shell if [ $(MIRAGE_NET) = "direct" ]; then echo --enable-direct; else echo --disable-direct; fi)
XEN ?= $(shell if [ $(MIRAGE_OS) = "xen" ]; then echo --enable-xen; else echo --disable-xen; fi)
caml_path ?= $(shell ocamlfind printconf path)

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
	ocaml setup.ml -build -cflags -bin-annot -j $(J) $(OFLAGS) $(DR)
ifeq ($(MIRAGE_OS), xen)
	ld -d -nostdlib -m elf_x86_64 -T $(caml_path)/mirage-xen/mirage-x86_64.lds \
	  $(caml_path)/mirage-xen/x86_64.o _build/switch/xen_switch.nobj.o \
	  $(caml_path)/mirage-xen/libocaml.a   $(caml_path)/mirage-xen/libxen.a \
	  $(caml_path)/mirage-xen/libxencaml.a $(caml_path)/mirage-xen/libdiet.a \
	  $(caml_path)/mirage-xen/libm.a       $(caml_path)/mirage-xen/longjmp.o \
	  -o ofswitch.xen

	ld -d -nostdlib -m elf_x86_64 -T $(caml_path)/mirage-xen/mirage-x86_64.lds \
	  $(caml_path)/mirage-xen/x86_64.o _build/controller/xen_controller.nobj.o \
	  $(caml_path)/mirage-xen/libocaml.a   $(caml_path)/mirage-xen/libxen.a \
	  $(caml_path)/mirage-xen/libxencaml.a $(caml_path)/mirage-xen/libdiet.a \
	  $(caml_path)/mirage-xen/libm.a       $(caml_path)/mirage-xen/longjmp.o \
	  -o ofcontroller.xen

endif 

doc: setup.data setup.ml
	ocaml setup.ml -doc -j $(J) $(OFLAGS)

install: 
	ocamlfind remove $(NAME)
	ocaml setup.ml -install $(OFLAGS)

test: build
	ocaml setup.ml -test


