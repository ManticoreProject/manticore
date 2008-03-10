HC      = ghc
HCFLAGS = -O

nbodygen: NBodyGen.hs
	$(HC) $(HCFLAGS) -o $@ $<

nbody: NBody.hs
	$(HC) $(HCFLAGS) -H10M -fglasgow-exts -o $@ $<
