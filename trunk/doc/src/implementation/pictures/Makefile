EPSTOPDF=epstopdf
DIA=dia

DIAFILES=$(wildcard *.dia)
PDFS=$(DIAFILES:.dia=.pdf)

all: $(PDFS)

%.eps: %.dia
	$(DIA) -t eps-builtin -e $@ $<

%.pdf: %.eps
	$(EPSTOPDF) $<

clean:
	rm -f *~ 