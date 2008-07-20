HC      = ghc
HCFLAGS = -O

nbodygen: NBodyGenSML.hs
	$(HC) $(HCFLAGS) -o $@ $<

nbody: NBody.hs
	$(HC) $(HCFLAGS) -H10M -fglasgow-exts -o $@ $<
