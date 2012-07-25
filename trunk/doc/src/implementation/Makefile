.PHONY: clean example-code pictures

TEXFILES=runtime-initialization.tex code-generation.tex hlops.tex runtime.tex gc.tex fiber-local-storage.tex inline-bom.tex set-once-memory.tex vproc-protocol.tex
PDFS=$(TEXFILES:.tex=.pdf)

all: $(PDFS)
	

%.pdf : %.tex example-code pictures
	bibtex $* || echo ""
	pdflatex $<
	pdflatex $<

example-code:	
	make -C example-code

pictures:
	make -C pictures

spellcheck:
	ispell -t $(TEXFILES)

clean:
	rm -f *.{pdf,log,aux,ps,dvi,bbl,blg} *~
	make -C example-code clean
	make -C pictures clean
