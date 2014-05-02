DIR=$(notdir $(CURDIR))

MLSRC=$(wildcard *.ml)
ASMOBJ=$(MLSRC:.ml=.cmx)
BYTEOBJ=$(MLSRC:.ml=.cmo)

ASM=$(DIR).asm
BYTE=$(DIR).byte

PACKAGES=spoc,sarek
SYNTAX_PACKAGES=spoc_external_kernels,sarek_syntax
SYNTAX_INCLUDES:=-I ` ocamlfind query spoc_external_kernels` \
                 -I `ocamlfind query sarek_syntax`


CAMLC=ocamlfind ocamlc
CAMLOPT=ocamlfind ocamlopt

all: $(ASM) $(BYTE)


%.cmo:%.ml
	$(CAMLC) -thread -package $(PACKAGES) -linkpkg \
	-package $(SYNTAX_PACKAGES) -syntax camlp4o -c $<

%.cmx:%.ml
	$(CAMLOPT) -thread -package $(PACKAGES) -linkpkg \
	-package $(SYNTAX_PACKAGES) -syntax camlp4o -c $<

$(ASM):$(ASMOBJ)
	$(CAMLOPT) -thread -package $(PACKAGES) -linkpkg \
	-package $(SYNTAX_PACKAGES) -syntax camlp4o -o $@ $<

$(BYTE):$(BYTEOBJ)
	$(CAMLC) -thread -package $(PACKAGES) -linkpkg \
	-package $(SYNTAX_PACKAGES) -syntax camlp4o -o $@ $<

clean:
	rm -f *cm* $(ASM) $(BYTE) \#*\# *.out *~

watch:
	make ; while true ; do inotifywait -qe close_write *.ml; \
	clear ; make ; echo OK ; done
