EPS=	eps/big-pict.eps \
	eps/big-pict2.eps \
	eps/cfg-rev.eps \
	eps/cfg.eps \
	eps/compiler-2.eps \
	eps/compiler.eps \
	eps/compiler2.eps \
	eps/hyperblock-formation.eps \
	eps/if-conversion.eps \
	eps/layers.eps \
	eps/mlrisc-ir-archive.eps \
	eps/mlrisc-IR.eps \
	eps/phg.eps \
	eps/region-builder.eps \
	eps/subgraph.eps \
	eps/tail-dupl.eps \
	eps/tail-duplication.eps \
	eps/trace.eps \
	eps/uncol.eps \
	eps/viewer.eps

PNG=	png/compiler-1.png \
	png/compiler-2.png \
	png/hof-1.png \
	png/hof-2.png \
	png/mlrisc-ir-archive.png \
	png/optimization.png \
	png/sharing1.png \
	png/sharing2.png \
	png/sharing3.png \
	png/uncol.png \
	png/uncol1.png \
	png/uncol2.png


all:	mkdir $(EPS) $(PNG)

clean:	
	rm -f $(EPS) $(PNG)

mkdir:
	@if [ ! -d png ]; then mkdir png; fi
	@if [ ! -d eps ]; then mkdir eps; fi

eps/%.eps:	fig/%.fig
	fig2dev -L ps $< $@
jpeg/%.jpg:	fig/%.fig
	fig2dev -L jpeg $< $@
png/%.png:	fig/%.fig
	fig2dev -L png $< $@
