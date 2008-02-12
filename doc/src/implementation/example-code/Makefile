.PHONY: clean example-code

SML_SOURCES=$(wildcard *.sml)
OUTPUT_TEX=$(SML_SOURCES:sml=tex)

%.tex: %.sml
	extract-code $<

example-code: $(OUTPUT_TEX)

clean:
	rm -rf *.tex *~ .cm