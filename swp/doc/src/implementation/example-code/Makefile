.PHONY: clean example-code

MC_LIB=../../../../src/lib

PML_SOURCES=$(wildcard *.pml)
SML_SOURCES=$(wildcard *.sml)
OUTPUT_TEX=$(SML_SOURCES:sml=tex) $(PML_SOURCES:pml=tex)

%.tex: %.sml
	extract-code $<

%.tex: %.pml
	extract-code $<

example-code: $(OUTPUT_TEX)

clean:
	rm -rf *.tex *~ .cm