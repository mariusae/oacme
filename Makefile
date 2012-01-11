SOURCES = bufIO.ml acme.ml 
RESULT = acme
PACKS = o9p ulib
THREADS = yes

all: ncl bcl
install: all libinstall

.PHONY: all install

include OCamlMakefile

