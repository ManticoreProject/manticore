/* promote.c
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

/* return true of the given address is within the given heap */
STATIC_INLINE bool inHeap (Addr_t heapBase, Addr_t p)
{
    return (heapBase == (p & ~VP_HEAP_MASK));
}

Value_t PromoteObj (VProc_t *vp, Value_t root)
{
    Addr_t	heapBase = (Addr_t)(vp->heap);
    Value_t	v = *root

  /* NOTE: the following test probably ought to happen before the runtime
   * system gets called.
   */
    if (isPtr(v) && inHeap(heapBase, (Addr_t)v)) {
	Word_t	*nextW = ??;
	Word_t	*nextScan = ??;

      /* promote the root to the global heap */
	root = ForwardObj ((Addr_t)v, &nextW);

      /* promote any reachable values */
/* NOTE: the following loop is common with major collections, so it
 * should be factored out into a shared routine.
 */
	while (nextScan < nextW) {
	    Word_t hdr = *nextScan++;	// get object header
	    if (isRecordHdr(hdr)) {
	      // a record
		Word_t tagBits = GetRecordTag(hdr);
		Word_t *scanP = nextScan;
		while (tagBits != 0) {
		    if (tagBits & 0x1) {
			Word_t p = *scanP;
			if (inHeap(heapBase, (Addr_t)p)) {
			    *p = ForwardObj(p, &nextW);
			}
		    }
		    tagBits >>= 1;
		    scanP++;
		}
		nextScan += GetRecordSizeW(hdr);
	    }
	    else if (isVectorHdr(hdr)) {
	      // an array of pointers
		int len = GetVectorLen(hdr);
		for (i = 0;  i < len;  i++) {
		    Value_t v = (Value_t)*nextScan;
		    if (isPtr(v)) {
			if (inHeap(heapBase, (Addr_t)v)) {
			    *nextScan = ForwardObj((Word_t *)v, &nextW);
			}
			nextScan++;
		    }
		}
	    }
	    else if (isRawHdr(hdr)) {
	      // we can just skip raw objects
		nextScan += GetRawSizeW(hdr);
	    }
	    else {
	      // a promoted object; chase the forward pointer to get the length
		assert (isForwardPtr(hdr));
		Word_t *fwdP = GetForwardPtr(hdr);
		nextScan += GetLength(fwdP[-1]);
	    }
	}

      /* update VProc's info */
	??
    }

    return root;

}
