(* 
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Print Code for the Garbage Collector Scan files
 *)

structure PrintTableDebug = 
struct
    
    (* Headerfiles *)
    fun header (MyoutStrm) = (  
        TextIO.output (MyoutStrm, "#include <stdint.h>\n");
        TextIO.output (MyoutStrm, "#include <stdio.h>\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "#include \"gc-scan.h\"\n");
        TextIO.output (MyoutStrm, "#include \"gc-inline.h\"\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "\n");
        ()
        )
    
    (*Minor GC Debug Functions *)
    fun minorpre (MyoutStrm) = (
        TextIO.output (MyoutStrm, "void possiblePointer (VProc_t *self, Word_t *p,Word_t *scanP,int tag) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "    /* check for possible pointers in non-pointer fields */\n");
        TextIO.output (MyoutStrm, "    Value_t v = *(Value_t *)scanP;\n");
        TextIO.output (MyoutStrm, "    if (isHeapPtr(v)) {\n");
        TextIO.output (MyoutStrm, "        MemChunk_t *cq = AddrToChunk(ValueToAddr(v));\n");
        TextIO.output (MyoutStrm, "        switch (cq->sts) {\n");
        TextIO.output (MyoutStrm, "            case FREE_CHUNK:\n");
        TextIO.output (MyoutStrm, "                 SayDebug(\"[%2d] ** possible free-space pointer %p in mixed object %p+%d\\n\",\n");
        TextIO.output (MyoutStrm, "                            self->id, (void *)v, (void *)p, (int)(scanP-p));\n");
        TextIO.output (MyoutStrm, "                 break;\n");
        TextIO.output (MyoutStrm, "            case TO_SP_CHUNK:\n");    
        TextIO.output (MyoutStrm, "                 SayDebug(\"[%2d] ** possible to-space pointer %p in mixed object %p+%d\\n\",\n");
        TextIO.output (MyoutStrm, "                            self->id, (void *)v, (void *)p, (int)(scanP-p));\n");
        TextIO.output (MyoutStrm, "                 break;\n");
        TextIO.output (MyoutStrm, "            case FROM_SP_CHUNK:\n");
        TextIO.output (MyoutStrm, "                 SayDebug(\"[%2d] ** possible from-space pointer %p in mixed object %p+%d\\n\",\n");
        TextIO.output (MyoutStrm, "                            self->id, (void *)v, (void *)p, (int)(scanP-p));\n");
        TextIO.output (MyoutStrm, "                 break;\n");
        TextIO.output (MyoutStrm, "            case UNMAPPED_CHUNK:\n");
        TextIO.output (MyoutStrm, "                 break;\n");
        TextIO.output (MyoutStrm, "            default:\n");
        TextIO.output (MyoutStrm, "                 if (IS_VPROC_CHUNK(cq->sts)) {\n");
        TextIO.output (MyoutStrm, "                     /* the vproc pointer is pretty common, so filter it out */\n");
        TextIO.output (MyoutStrm, "                     //global or minor; maybe the if clauses are the same??\n"); 
        TextIO.output (MyoutStrm, "                     if (tag == 1) {   \n");           
        TextIO.output (MyoutStrm, "                         if ((Addr_t)v & ~VP_HEAP_MASK != (Addr_t)v)\n");
        TextIO.output (MyoutStrm, "                             SayDebug(\"[%2d] ** possible local pointer %p in mixed object %p+%d\\n\",\n");
        TextIO.output (MyoutStrm, "                                    self->id, (void *)v, (void *)p, (int)(scanP-p));\n");
        TextIO.output (MyoutStrm, "                          }\n");
        TextIO.output (MyoutStrm, "                          else {\n");
        TextIO.output (MyoutStrm, "                              if (ValueToAddr(v) & ~VP_HEAP_MASK != ValueToAddr(v))\n");
        TextIO.output (MyoutStrm, "                                   SayDebug(\"[%2d] ** possible local pointer %p in mixed object %p+%d\\n\",\n");
        TextIO.output (MyoutStrm, "                                              self->id, (void *)v, (void *)p, (int)(scanP-p));\n");
        TextIO.output (MyoutStrm, "                          }\n");
        TextIO.output (MyoutStrm, "                 }\n");
        TextIO.output (MyoutStrm, "                 else {\n");
        TextIO.output (MyoutStrm, "                     SayDebug(\"[%2d] ** strange pointer %p in mixed object %p+%d\\n\",\n");
        TextIO.output (MyoutStrm, "                                self->id, (void *)v, (void *)p, (int)(scanP-p));\n");
        TextIO.output (MyoutStrm, "                 }\n");
        TextIO.output (MyoutStrm, "                 break;\n");
        TextIO.output (MyoutStrm, "         }    \n");
        TextIO.output (MyoutStrm, "     }         \n");
        TextIO.output (MyoutStrm, "}    \n");
        TextIO.output (MyoutStrm, "\n");
        
        TextIO.output (MyoutStrm, "void MinorGCcheckmixedglobal (Word_t *p,Word_t *scanP) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "Value_t v = *(Value_t *)scanP;\n");
        TextIO.output (MyoutStrm, "if (isPtr(v)) {\n");
        TextIO.output (MyoutStrm, "     MemChunk_t *cq = AddrToChunk(ValueToAddr(v));\n");
        TextIO.output (MyoutStrm, "     if (cq->sts != TO_SP_CHUNK) { \n");
        TextIO.output (MyoutStrm, "         if (cq->sts == FROM_SP_CHUNK)\n");
        TextIO.output (MyoutStrm, "              SayDebug(\"** unexpected from-space pointer %p at %p in mixed object\\n\",\n");
        TextIO.output (MyoutStrm, "                       ValueToPtr(v), (void *)p);\n");
        TextIO.output (MyoutStrm, "         else if (IS_VPROC_CHUNK(cq->sts))\n");
        TextIO.output (MyoutStrm, "              SayDebug(\"** unexpected local pointer %p at %p in mixed object\\n\",\n");
        TextIO.output (MyoutStrm, "                       ValueToPtr(v), (void *)p);\n");
        TextIO.output (MyoutStrm, "         else if (cq->sts == FREE_CHUNK)\n");
        TextIO.output (MyoutStrm, "              SayDebug(\"** unexpected free pointer %p at %p in mixed object\\n\",\n");
        TextIO.output (MyoutStrm, "                       ValueToPtr(v), (void *)p); \n");
        TextIO.output (MyoutStrm, "         }\n");
        TextIO.output (MyoutStrm, "      }\n");
        TextIO.output (MyoutStrm, "}\n");
        
        TextIO.output (MyoutStrm, "void minorGCVECTORdebug (VProc_t *self, Word_t *ptr) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "    char buf[32];\n");
        TextIO.output (MyoutStrm, "    int len = GetVectorLen(ptr[-1]);\n");
        TextIO.output (MyoutStrm, "    for (int i = 0;  i < len;  i++, ptr++) {\n");
        TextIO.output (MyoutStrm, "        sprintf(buf, \"local vector[%d/%d]\", i, len);\n");
        TextIO.output (MyoutStrm, "        CheckLocalPtrMinor (self, ptr, buf);\n");
        TextIO.output (MyoutStrm, "    }\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
        
        TextIO.output (MyoutStrm, "void minorGCRAWdebug (VProc_t *self, Word_t *ptr) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "    assert (isRawHdr(ptr[-1]));\n");
        TextIO.output (MyoutStrm, "    int len = GetLength(ptr[-1]);\n");
        TextIO.output (MyoutStrm, "    // look for raw values that might be pointers\n");
        TextIO.output (MyoutStrm, "    for (int i = 0; i < len; i++) {\n");
        TextIO.output (MyoutStrm, "        Value_t v = (Value_t)ptr[i];\n");
        TextIO.output (MyoutStrm, "        if (isPtr(v)) {\n");
        TextIO.output (MyoutStrm, "             if (isHeapPtr(v)) {\n");
        TextIO.output (MyoutStrm, "                 MemChunk_t *cq = AddrToChunk(ValueToAddr(v));\n");
        TextIO.output (MyoutStrm, "                 if (cq->sts != TO_SP_CHUNK) {\n");
        TextIO.output (MyoutStrm, "                     if (cq->sts == FROM_SP_CHUNK)\n");
        TextIO.output (MyoutStrm, "                          SayDebug(\"** suspicious looking from-space pointer %p at %p[%d] in raw object of length %d (in local heap)\\n\",\n");
        TextIO.output (MyoutStrm, "                                    ValueToPtr(v), (void *)ptr, i, len);\n");
        TextIO.output (MyoutStrm, "                     else if (IS_VPROC_CHUNK(cq->sts))\n");
        TextIO.output (MyoutStrm, "                          SayDebug(\"** suspicious looking local pointer %p at %p[%d] in raw object of length %d (in local heap)\\n\",\n");
        TextIO.output (MyoutStrm, "                                    ValueToPtr(v), (void *)ptr, i, len);\n");
        TextIO.output (MyoutStrm, "                     else if (cq->sts == FREE_CHUNK)\n");
        TextIO.output (MyoutStrm, "                          SayDebug(\"** suspicious looking free pointer %p at %p[%d] in raw object of length %d (in local heap)\\n\",\n");
        TextIO.output (MyoutStrm, "                                    ValueToPtr(v), (void *)ptr, i, len);\n");
        TextIO.output (MyoutStrm, "                 }\n");
        TextIO.output (MyoutStrm, "             }\n");
        TextIO.output (MyoutStrm, "         }\n");
        TextIO.output (MyoutStrm, "    }\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
        ()
        )


    
    fun minor (MyoutStrm) = let
        val s = HeaderTableStruct.HeaderTable.print (HeaderTableStruct.header)
        fun printmystring [] = ()
            | printmystring ((a,b)::t) = (let
                    
                fun lp(0,bites,pos) = ()
                | lp(strlen,bites,pos) =(
                    if (String.compare (substring(bites,strlen-1,1),"1") = EQUAL)
                    then (
                        TextIO.output (MyoutStrm,"    CheckLocalPtrMinor (self, scanP, \"local mixed object\");\n");
                        TextIO.output (MyoutStrm,"    scanP++;\n");
                        lp(strlen-1,bites,pos+1)
                        )
                    else (
                        TextIO.output (MyoutStrm,"    possiblePointer(self,ptr,scanP,1);\n");
                        TextIO.output (MyoutStrm,"    scanP++;\n");
                        lp(strlen-1,bites,pos+1)
                        )
                    )
                in
                TextIO.output (MyoutStrm, concat["void minorGC",Int.toString b,"Debug (VProc_t *self, Word_t *ptr) {\n"]);
                TextIO.output (MyoutStrm, "  \n");
                TextIO.output (MyoutStrm, "  Word_t *scanP = ptr;\n");
                TextIO.output (MyoutStrm, "\n");
                
                lp(String.size a,a,0);
                
                TextIO.output (MyoutStrm, "}\n");
                TextIO.output (MyoutStrm, "\n"); 
                
                printmystring t
                end
            )
            
    in
        printmystring s;
        ()
    end
    
    
    fun minorglobalpre (MyoutStrm) = (
        TextIO.output (MyoutStrm, "void checkMixedPointer (VProc_t *self,Word_t *p,Word_t *scanP) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "   Value_t v = *(Value_t *)scanP;\n");
        TextIO.output (MyoutStrm, "   if (isPtr(v)) {\n");
        TextIO.output (MyoutStrm, "     MemChunk_t *cq = AddrToChunk(ValueToAddr(v));\n");
        TextIO.output (MyoutStrm, "     if (cq->sts != TO_SP_CHUNK) {\n");
        TextIO.output (MyoutStrm, "        if (cq->sts == FROM_SP_CHUNK)\n");
        TextIO.output (MyoutStrm, "            SayDebug(\"[%2d] ** unexpected from-space pointer %p at %p in mixed object\\n\",\n");
        TextIO.output (MyoutStrm, "                      self->id, ValueToPtr(v), (void *)p);\n");
        TextIO.output (MyoutStrm, "        else if (IS_VPROC_CHUNK(cq->sts))\n");
        TextIO.output (MyoutStrm, "            SayDebug(\"[%2d] ** unexpected local pointer %p at %p in mixed object\\n\",\n");
        TextIO.output (MyoutStrm, "                    self->id, ValueToPtr(v), (void *)p);\n");
        TextIO.output (MyoutStrm, "        else if (cq->sts == FREE_CHUNK)\n");
        TextIO.output (MyoutStrm, "            SayDebug(\"[%2d] ** unexpected free pointer %p at %p in mixed object\\n\",\n");
        TextIO.output (MyoutStrm, "                     self->id, ValueToPtr(v), (void *)p);\n");
        TextIO.output (MyoutStrm, "     }\n");
        TextIO.output (MyoutStrm, "   }\n");
        TextIO.output (MyoutStrm, "}\n");
        
        TextIO.output (MyoutStrm, "void minorGCRAWdebugGlobal (VProc_t *self, Word_t *ptr) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "    assert (isRawHdr(ptr[-1]));\n");
        TextIO.output (MyoutStrm, "    int len = GetLength(ptr[-1]);\n");
        TextIO.output (MyoutStrm, "    // look for raw values that might be pointers\n");
        TextIO.output (MyoutStrm, "    for (int i = 0; i < len; i++) {\n");
        TextIO.output (MyoutStrm, "        Value_t v = (Value_t)ptr[i];\n");
        TextIO.output (MyoutStrm, "        if (isPtr(v)) {\n");
        TextIO.output (MyoutStrm, "             if (isHeapPtr(v)) {\n");
        TextIO.output (MyoutStrm, "                 MemChunk_t *cq = AddrToChunk(ValueToAddr(v));\n");
        TextIO.output (MyoutStrm, "                 if (cq->sts != TO_SP_CHUNK) {\n");
        TextIO.output (MyoutStrm, "                     if (cq->sts == FROM_SP_CHUNK)\n");
        TextIO.output (MyoutStrm, "                          SayDebug(\"[%2d] ** suspicious looking from-space pointer %p at %p[%d] in raw object of length %d (in local heap)\\n\",\n");
        TextIO.output (MyoutStrm, "                                   self->id, ValueToPtr(v), (void *)ptr, i, len);\n");
        TextIO.output (MyoutStrm, "                     /* the vproc pointer is pretty common, so filter it out */\n"); 
        TextIO.output (MyoutStrm, "                     else if (IS_VPROC_CHUNK(cq->sts))\n");
        TextIO.output (MyoutStrm, "                          SayDebug(\"[%2d] ** suspicious looking local pointer %p at %p[%d] in raw object of length %d (in local heap)\\n\",\n");
        TextIO.output (MyoutStrm, "                                    self->id, ValueToPtr(v), (void *)ptr, i, len);\n");
        TextIO.output (MyoutStrm, "                     else if (cq->sts == FREE_CHUNK)\n");
        TextIO.output (MyoutStrm, "                          SayDebug(\"[%2d] ** suspicious looking free pointer %p at %p[%d] in raw object of length %d (in local heap)\\n\",\n");
        TextIO.output (MyoutStrm, "                                    self->id, ValueToPtr(v), (void *)ptr, i, len);\n");
        TextIO.output (MyoutStrm, "                 }\n");
        TextIO.output (MyoutStrm, "             }\n");
        TextIO.output (MyoutStrm, "         }\n");
        TextIO.output (MyoutStrm, "    }\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
        
        TextIO.output (MyoutStrm, "void minorGCVECTORdebugGlobal (VProc_t *self, Word_t *ptr) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "    int len = GetVectorLen(ptr[-1]);\n");
        TextIO.output (MyoutStrm, "    // an array of pointers\n");
        TextIO.output (MyoutStrm, "    for (int i = 0;  i < len;  i++, ptr++) {\n");
        TextIO.output (MyoutStrm, "          Value_t v = (Value_t)*ptr;\n");
        TextIO.output (MyoutStrm, "          if (isPtr(v)) {\n");
        TextIO.output (MyoutStrm, "              MemChunk_t *cq = AddrToChunk(ValueToAddr(v));\n");
        TextIO.output (MyoutStrm, "              if (cq->sts != TO_SP_CHUNK) {\n");
        TextIO.output (MyoutStrm, "                  if (cq->sts == FROM_SP_CHUNK)\n");
        TextIO.output (MyoutStrm, "                      SayDebug(\"** unexpected from-space pointer %p at %p in vector\\n\",\n");
        TextIO.output (MyoutStrm, "                               ValueToPtr(v), (void *)ptr);\n");
        TextIO.output (MyoutStrm, "                  else if (IS_VPROC_CHUNK(cq->sts))\n");
        TextIO.output (MyoutStrm, "                      SayDebug(\"** unexpected local pointer %p at %p in vector\\n\",\n");
        TextIO.output (MyoutStrm, "                               ValueToPtr(v), (void *)ptr);\n");
        TextIO.output (MyoutStrm, "                  else if (cq->sts == FREE_CHUNK)\n");
        TextIO.output (MyoutStrm, "                      SayDebug(\"** unexpected free pointer %p at %p in vector\\n\",\n");
        TextIO.output (MyoutStrm, "                                ValueToPtr(v), (void *)ptr);\n");
        TextIO.output (MyoutStrm, "              }\n");
        TextIO.output (MyoutStrm, "         }\n");
        TextIO.output (MyoutStrm, "    }\n");
        TextIO.output (MyoutStrm, "}\n");
    
    ()
    )
    
    
    fun minorglobal (MyoutStrm) = let
        val s = HeaderTableStruct.HeaderTable.print (HeaderTableStruct.header)
        fun printmystring [] = ()
            | printmystring ((a,b)::t) = (let
                    
                fun lp(0,bites,pos) = ()
                | lp(strlen,bites,pos) =(
                    if (String.compare (substring(bites,strlen-1,1),"1") = EQUAL)
                    then (
                        TextIO.output (MyoutStrm,"    checkMixedPointer (self,ptr, scanP);\n");
                        TextIO.output (MyoutStrm,"    scanP++;\n");
                        lp(strlen-1,bites,pos+1)
                        )
                    else (
                        TextIO.output (MyoutStrm,"    possiblePointer(self,ptr,scanP,1);\n");
                        TextIO.output (MyoutStrm,"    scanP++;\n");
                        lp(strlen-1,bites,pos+1)
                        )
                    )
                in
                TextIO.output (MyoutStrm, concat["void minorGC",Int.toString b,"DebugGlobal (VProc_t *self, Word_t *ptr) {\n"]);
                TextIO.output (MyoutStrm, "  \n");
                TextIO.output (MyoutStrm, "  Word_t *scanP = ptr;\n");
                TextIO.output (MyoutStrm, "\n");
                
                lp(String.size a,a,0);
                
                TextIO.output (MyoutStrm, "}\n");
                TextIO.output (MyoutStrm, "\n"); 
                
                printmystring t
                end
            )
            
    in
        printmystring s;
        ()
    end
    
    (*Global GC Debug Functions *)
    fun globalpre (MyoutStrm) = (
        TextIO.output (MyoutStrm, "void globalGCRAWdebug (VProc_t *self, Word_t *ptr) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "    assert (isRawHdr(ptr[-1]));\n");
        TextIO.output (MyoutStrm, "    int len = GetLength(ptr[-1]);\n");
        TextIO.output (MyoutStrm, "    // look for raw values that might be pointers\n");
        TextIO.output (MyoutStrm, "    for (int i = 0; i < len; i++) {\n");
        TextIO.output (MyoutStrm, "        Value_t v = (Value_t)ptr[i];\n");
        TextIO.output (MyoutStrm, "        if (isPtr(v)) {\n");
        TextIO.output (MyoutStrm, "             if (isHeapPtr(v)) {\n");
        TextIO.output (MyoutStrm, "                 MemChunk_t *cq = AddrToChunk(ValueToAddr(v));\n");
        TextIO.output (MyoutStrm, "                 if (cq->sts != TO_SP_CHUNK) {\n");
        TextIO.output (MyoutStrm, "                     if (cq->sts == FROM_SP_CHUNK)\n");
        TextIO.output (MyoutStrm, "                          SayDebug(\"[%2d] ** suspicious looking from-space pointer %p at %p[%d] in raw object of length %d (in local heap)\\n\",\n");
        TextIO.output (MyoutStrm, "                                    self->id, ValueToPtr(v), (void *)ptr, i, len);\n");
        TextIO.output (MyoutStrm, "                     else if (IS_VPROC_CHUNK(cq->sts))\n");
        TextIO.output (MyoutStrm, "                          /* the vproc pointer is pretty common, so filter it out */\n");
        TextIO.output (MyoutStrm, "                          if (ValueToAddr(v) & ~VP_HEAP_MASK != ValueToAddr(v))\n");
        TextIO.output (MyoutStrm, "                              SayDebug(\"[%2d] ** suspicious looking local pointer %p at %p[%d] in raw object of length %d (in local heap)\\n\",\n");
        TextIO.output (MyoutStrm, "                                    self->id, ValueToPtr(v), (void *)ptr, i, len);\n");
        TextIO.output (MyoutStrm, "                     else if (cq->sts == FREE_CHUNK)\n");
        TextIO.output (MyoutStrm, "                          SayDebug(\"[%2d] ** suspicious looking free pointer %p at %p[%d] in raw object of length %d (in local heap)\\n\",\n");
        TextIO.output (MyoutStrm, "                                    self->id, ValueToPtr(v), (void *)ptr, i, len);\n");
        TextIO.output (MyoutStrm, "                 }\n");
        TextIO.output (MyoutStrm, "             }\n");
        TextIO.output (MyoutStrm, "         }\n");
        TextIO.output (MyoutStrm, "    }\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
        
        TextIO.output (MyoutStrm, "void globalGCVECTORdebug (VProc_t *self, Word_t *ptr) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "     int len = GetVectorLen(ptr[-1]);\n");
        TextIO.output (MyoutStrm, "     for (int i = 0;  i < len;  i++, ptr++) {\n");
        TextIO.output (MyoutStrm, "          CheckLocalPtrGlobal (self, ptr, \"local vector\");\n");
        TextIO.output (MyoutStrm, "     }\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
        
        ()
    )
    
    fun global (MyoutStrm) = let
        val s = HeaderTableStruct.HeaderTable.print (HeaderTableStruct.header)
        fun printmystring [] = ()
            | printmystring ((a,b)::t) = (let
                    
                fun lp(0,bites,pos) = ()
                | lp(strlen,bites,pos) =(
                    if (String.compare (substring(bites,strlen-1,1),"1") = EQUAL)
                    then (
                        TextIO.output (MyoutStrm,"CheckLocalPtrGlobal (self, scanP, \"local mixed object\");\n");
                        TextIO.output (MyoutStrm,"scanP++;\n");
                        lp(strlen-1,bites,pos+1)
                        )
                    else (
                        TextIO.output (MyoutStrm,"possiblePointer(self,ptr,scanP,0);\n");
                        TextIO.output (MyoutStrm,"scanP++;\n");
                        lp(strlen-1,bites,pos+1)
                        )
                    )
                in
                TextIO.output (MyoutStrm, concat["void globalGC",Int.toString b,"Debug (VProc_t *self, Word_t *ptr) {\n"]);
                TextIO.output (MyoutStrm, "  \n");
                TextIO.output (MyoutStrm, "  Word_t *scanP = ptr;\n");
                TextIO.output (MyoutStrm, "\n");
                
                lp(String.size a,a,0);
                
                TextIO.output (MyoutStrm, "}\n");
                TextIO.output (MyoutStrm, "\n"); 
                
                printmystring t
                end
            )
            
    in
        printmystring s;
        ()
    end
    
    (*Global GC Debug Functions *)
    fun globalglobalpre (MyoutStrm) = (
        TextIO.output (MyoutStrm, "void globalGCRAWdebugGlobal (VProc_t *self, Word_t *ptr) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "    assert (isRawHdr(ptr[-1]));\n");
        TextIO.output (MyoutStrm, "    int len = GetLength(ptr[-1]);\n");
        TextIO.output (MyoutStrm, "    // look for raw values that might be pointers\n");
        TextIO.output (MyoutStrm, "    for (int i = 0; i < len; i++) {\n");
        TextIO.output (MyoutStrm, "        Value_t v = (Value_t)ptr[i];\n");
        TextIO.output (MyoutStrm, "        if (isPtr(v)) {\n");
        TextIO.output (MyoutStrm, "             if (isHeapPtr(v)) {\n");
        TextIO.output (MyoutStrm, "                 MemChunk_t *cq = AddrToChunk(ValueToAddr(v));\n");
        TextIO.output (MyoutStrm, "                 if (cq->sts != TO_SP_CHUNK) {\n");
        TextIO.output (MyoutStrm, "                     if (cq->sts == FROM_SP_CHUNK)\n");
        TextIO.output (MyoutStrm, "                          SayDebug(\"[%2d] ** suspicious looking from-space pointer %p at %p[%d] in raw object of length %d (in local heap)\\n\",\n");
        TextIO.output (MyoutStrm, "                                   self->id, ValueToPtr(v), (void *)ptr, i, len);\n");
        TextIO.output (MyoutStrm, "                     /* the vproc pointer is pretty common, so filter it out */\n"); 
        TextIO.output (MyoutStrm, "                     else if (IS_VPROC_CHUNK(cq->sts))\n");
        TextIO.output (MyoutStrm, "                          if (ValueToAddr(v) & ~VP_HEAP_MASK != ValueToAddr(v))\n"); 
        TextIO.output (MyoutStrm, "                              SayDebug(\"[%2d] ** suspicious looking local pointer %p at %p[%d] in raw object of length %d (in local heap)\\n\",\n");
        TextIO.output (MyoutStrm, "                                    self->id, ValueToPtr(v), (void *)ptr, i, len);\n");
        TextIO.output (MyoutStrm, "                     else if (cq->sts == FREE_CHUNK)\n");
        TextIO.output (MyoutStrm, "                          SayDebug(\"[%2d] ** suspicious looking free pointer %p at %p[%d] in raw object of length %d (in local heap)\\n\",\n");
        TextIO.output (MyoutStrm, "                                    self->id, ValueToPtr(v), (void *)ptr, i, len);\n");
        TextIO.output (MyoutStrm, "                 }\n");
        TextIO.output (MyoutStrm, "             }\n");
        TextIO.output (MyoutStrm, "         }\n");
        TextIO.output (MyoutStrm, "    }\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
        
        TextIO.output (MyoutStrm, "void globalGCVECTORdebugGlobal (VProc_t *self, Word_t *ptr) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "    int len = GetVectorLen(ptr[-1]);\n");
        TextIO.output (MyoutStrm, "    for (int i = 0;  i < len;  i++, ptr++) {\n");
        TextIO.output (MyoutStrm, "        Value_t v = (Value_t)*ptr;\n");
        TextIO.output (MyoutStrm, "        if (isPtr(v)) {\n");
        TextIO.output (MyoutStrm, "           MemChunk_t *cq = AddrToChunk(ValueToAddr(v));\n");
        TextIO.output (MyoutStrm, "           if (cq->sts != TO_SP_CHUNK) {\n");
        TextIO.output (MyoutStrm, "                if (cq->sts == FROM_SP_CHUNK)\n");
        TextIO.output (MyoutStrm, "                SayDebug(\"[%2d] ** unexpected from-space pointer %p at %p in vector\\n\",\n");
        TextIO.output (MyoutStrm, "                        self->id, ValueToPtr(v), (void *)ptr);\n");
        TextIO.output (MyoutStrm, "                else if (IS_VPROC_CHUNK(cq->sts)) {\n");
        TextIO.output (MyoutStrm, "                     if (cq->sts != VPROC_CHUNK(self->id)) {\n");
        TextIO.output (MyoutStrm, "                         SayDebug(\"[%2d] ** unexpected remote pointer %p at %p in vector\\n\",\n");
        TextIO.output (MyoutStrm, "                                   self->id, ValueToPtr(v), (void *)ptr);\n");
        TextIO.output (MyoutStrm, "                      }\n");
        TextIO.output (MyoutStrm, "                      else if (ValueToAddr(v) & ~VP_HEAP_MASK != ValueToAddr(v)) {\n");
        TextIO.output (MyoutStrm, "                           SayDebug(\"[%2d] ** unexpected vproc-structure pointer %p at %p in vector\\n\",\n");
        TextIO.output (MyoutStrm, "                                    self->id, ValueToPtr(v), (void *)ptr);\n");
        TextIO.output (MyoutStrm, "                      }\n");
        TextIO.output (MyoutStrm, "                      else if (! inAddrRange(self->heapBase, self->oldTop - self->heapBase, ValueToAddr(v))) {\n");
        TextIO.output (MyoutStrm, "                           SayDebug(\"[%2d] ** unexpected local pointer %p at %p in vector[%d] is out of bounds\\n\",\n");
        TextIO.output (MyoutStrm, "                                     self->id, ValueToPtr(v), (void *)ptr, i);\n");
        TextIO.output (MyoutStrm, "                      }\n");
        TextIO.output (MyoutStrm, "                      } else {\n");
        TextIO.output (MyoutStrm, "                           SayDebug(\"[%2d] ** unexpected local pointer %p at %p in vector\\n\",\n");
        TextIO.output (MyoutStrm, "                                     self->id, ValueToPtr(v), (void *)ptr);\n");
        TextIO.output (MyoutStrm, "                      }\n");
        TextIO.output (MyoutStrm, "                }\n");
        TextIO.output (MyoutStrm, "                else if (cq->sts == FREE_CHUNK)\n");
        TextIO.output (MyoutStrm, "                    SayDebug(\"[%2d] ** unexpected free pointer %p at %p in vector\\n\",\n");
        TextIO.output (MyoutStrm, "                             self->id, ValueToPtr(v), (void *)ptr);\n");
        TextIO.output (MyoutStrm, "             }\n");
        TextIO.output (MyoutStrm, "          }\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
        
        ()
    )
    
    fun globalglobal (MyoutStrm) = let
        val s = HeaderTableStruct.HeaderTable.print (HeaderTableStruct.header)
        fun printmystring [] = ()
            | printmystring ((a,b)::t) = (let
                    
                fun lp(0,bites,pos) = ()
                | lp(strlen,bites,pos) =(
                    if (String.compare (substring(bites,strlen-1,1),"1") = EQUAL)
                    then (
                        TextIO.output (MyoutStrm,"    checkMixedPointer (self,ptr, scanP);\n");
                        TextIO.output (MyoutStrm,"    scanP++;\n");
                        lp(strlen-1,bites,pos+1)
                        )
                    else (
                        TextIO.output (MyoutStrm,"    possiblePointer(self,ptr,scanP,0);\n");
                        TextIO.output (MyoutStrm,"    scanP++;\n");
                        lp(strlen-1,bites,pos+1)
                        )
                    )
                in
                TextIO.output (MyoutStrm, concat["void globalGC",Int.toString b,"DebugGlobal (VProc_t *self, Word_t *ptr) {\n"]);
                TextIO.output (MyoutStrm, "  \n");
                TextIO.output (MyoutStrm, "  Word_t *scanP = ptr;\n");
                TextIO.output (MyoutStrm, "\n");
                
                lp(String.size a,a,0);
                
                TextIO.output (MyoutStrm, "}\n");
                TextIO.output (MyoutStrm, "\n"); 
                
                printmystring t
                end
            )
            
    in
        printmystring s;
        ()
    end
    
    (* GC_Debug Functions *)
    fun gcdebugpre (MyoutStrm) = (
        TextIO.output (MyoutStrm, "void gc_debug (Word_t * nextScan, Addr_t nurseryBase, Addr_t allocSzB) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "    Value_t *scanP = (Value_t *)nextScan;\n");
        TextIO.output (MyoutStrm, "    Value_t v = *scanP;\n");
        TextIO.output (MyoutStrm, "    if (isPtr(v)) {\n");
        TextIO.output (MyoutStrm, "        if (inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) { // pointer into the nursery\n");
        TextIO.output (MyoutStrm, "           // only young to old pointers allowed in the nursery\n");
        TextIO.output (MyoutStrm, "           assert(ValueToAddr(v) < (Addr_t)nextScan);\n");
        TextIO.output (MyoutStrm, "         } \n");
        TextIO.output (MyoutStrm, "         else { // in the global heap\n");
        TextIO.output (MyoutStrm, "            assert(isGlobalHeapPtr(v));\n");
        TextIO.output (MyoutStrm, "         }\n");
        TextIO.output (MyoutStrm, "     }\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
        
        TextIO.output (MyoutStrm, "void gc_debugVECTOR (Word_t * nextScan, Addr_t nurseryBase, Addr_t allocSzB) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "    int len = GetLength(nextScan[-1]);\n");
        TextIO.output (MyoutStrm, "    for (int i = 0;  i < len;  i++, nextScan++) {\n");
        TextIO.output (MyoutStrm, "        Value_t v = *(Value_t *)nextScan;\n");
        TextIO.output (MyoutStrm, "        if (isPtr(v)) {\n");
        TextIO.output (MyoutStrm, "            if (inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) {  // pointer into the nursery\n");
        TextIO.output (MyoutStrm, "                // only young to old pointers allowed in the nursery\n");
        TextIO.output (MyoutStrm, "                assert(ValueToAddr(v) < (Addr_t)nextScan);\n");
        TextIO.output (MyoutStrm, "            }\n");
        TextIO.output (MyoutStrm, "            else {\n");
        TextIO.output (MyoutStrm, "            // object in an older generation\n");
        TextIO.output (MyoutStrm, "                assert(isGlobalHeapPtr(v));\n");
        TextIO.output (MyoutStrm, "            }\n");
        TextIO.output (MyoutStrm, "         }\n");
        TextIO.output (MyoutStrm, "    }\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
        
        TextIO.output (MyoutStrm, "void gc_debugRAW (Word_t * nextScan, Addr_t nurseryBase, Addr_t allocSzB) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "    // we can just skip raw objects\n");
        TextIO.output (MyoutStrm, "    assert (isRawHdr(nextScan[-1]));\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
          
        ()
        )
        
        fun gcdebug (MyoutStrm) = let
        val s = HeaderTableStruct.HeaderTable.print (HeaderTableStruct.header)
        fun printmystring [] = ()
            | printmystring ((a,b)::t) = (let
                    
                fun lp(0,bites,pos) = ()
                | lp(strlen,bites,pos) =(
                    if (String.compare (substring(bites,strlen-1,1),"1") = EQUAL)
                    then (
                        TextIO.output (MyoutStrm,concat["    gc_debug((nextScan+",Int.toString pos,"), nurseryBase, allocSzB);\n"]);
                        lp(strlen-1,bites,pos+1)
                        )
                    else (
                        lp(strlen-1,bites,pos+1)
                        )
                    )
                in
                TextIO.output (MyoutStrm, concat["void gc_debug",Int.toString b,"mix (Word_t * nextScan, Addr_t nurseryBase, Addr_t allocSzB) {\n"]);
                TextIO.output (MyoutStrm, "\n");
                
                lp(String.size a,a,0);
                
                TextIO.output (MyoutStrm, "}\n");
                TextIO.output (MyoutStrm, "\n"); 
                
                printmystring t
                end
            )
            
    in
        printmystring s;
        ()
    end
    
    
    
    fun createtable (MyoutStrm) = (let
        val s = HeaderTableStruct.HeaderTable.print (HeaderTableStruct.header)
        val length = List.length s
        
        fun printtable (listlength,i) = (
            if (listlength = i)
            then ()
            else (
                TextIO.output (MyoutStrm, concat[",{minorGC",Int.toString i,"Debug,minorGC",Int.toString i,"DebugGlobal,globalGC",Int.toString i,"Debug,globalGC",Int.toString i,"DebugGlobal,gc_debug",Int.toString i,"mix}\n"]);
                printtable(listlength,i+1)
                )
            )
            
        in
        TextIO.output (MyoutStrm, concat["tableentryDebug tableDebug[",Int.toString (length+2),"] = { {minorGCRAWdebug,minorGCRAWdebugGlobal,globalGCRAWdebug,globalGCRAWdebugGlobal,gc_debugRAW},\n"]);
        TextIO.output (MyoutStrm, "{minorGCVECTORdebug,minorGCVECTORdebugGlobal,globalGCVECTORdebug,globalGCVECTORdebugGlobal,gc_debugVECTOR}\n");
        
        printtable (length+2,2);
        
        TextIO.output (MyoutStrm," };\n"); 
        TextIO.output (MyoutStrm,"\n");
        
        ()
        end
        )        
    
    fun print (path) = let
            val Myout = TextIO.openOut path
        in
            header Myout;
            
            minorpre Myout;
            minor Myout;
            
            minorglobalpre Myout;
            minorglobal Myout;
            
            globalpre Myout;
            global Myout;
            
            globalglobalpre Myout;
            globalglobal Myout;
            
            gcdebugpre Myout;
            gcdebug Myout;
            
            createtable Myout;
            
            TextIO.closeOut(Myout)
        end
    
end
