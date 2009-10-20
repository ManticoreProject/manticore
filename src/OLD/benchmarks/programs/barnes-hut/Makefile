HC      = ghc
HCFLAGS = -O

nbodygen_pml: NBodyGen.hs
	$(HC) $(HCFLAGS) -o $@ $<

nbodygen_sml: NBodyGenSML.hs
	$(HC) $(HCFLAGS) -o $@ $<

nbody: NBody.hs
	$(HC) $(HCFLAGS) -H10M -fglasgow-exts -o $@ $<
