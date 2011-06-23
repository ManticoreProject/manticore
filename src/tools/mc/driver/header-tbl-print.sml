(* 
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Print Code for the Garbage Collector Scan files
 *)

structure PrintTable = 
struct
    
    (* number of predefined table entries, important for the table length!! *)
    val predefined = 3
    
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
    
    (*Minor GC Functions *)
    fun minorpre (MyoutStrm) = (
        TextIO.output (MyoutStrm, "Word_t * minorGCscanRAWpointer (Word_t* ptr, Word_t **nextW, Addr_t allocSzB, Addr_t nurseryBase) {\n");
        TextIO.output (MyoutStrm, "\n" );
        TextIO.output (MyoutStrm, "assert (isRawHdr(ptr[-1]));\n");
		TextIO.output (MyoutStrm, "return (ptr+GetLength(ptr[-1]));\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "  }\n");
        
        TextIO.output (MyoutStrm, "Word_t * minorGCscanVECTORpointer (Word_t* ptr, Word_t **nextW, Addr_t allocSzB, Addr_t nurseryBase) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "Word_t *nextScan = ptr;\n");
        TextIO.output (MyoutStrm, "  int len = GetLength(ptr[-1]);\n" );
        TextIO.output (MyoutStrm, "  for (int i = 0;  i < len;  i++, nextScan++) {\n");
        TextIO.output (MyoutStrm, "   Value_t v = *(Value_t *)nextScan;\n");
        TextIO.output (MyoutStrm, "   if (isPtr(v)) {\n");
        TextIO.output (MyoutStrm, "      if (inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) {\n");
        TextIO.output (MyoutStrm, "          *nextScan = (Word_t)ForwardObjMinor(v, nextW);\n");
        TextIO.output (MyoutStrm, "      }\n");
        TextIO.output (MyoutStrm, "    }\n");
        TextIO.output (MyoutStrm, "   }\n");
		TextIO.output (MyoutStrm, "return (ptr+len);\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
		
		TextIO.output (MyoutStrm, "Word_t * minorGCscanPROXYpointer (Word_t* ptr, Word_t **nextW, Addr_t allocSzB, Addr_t nurseryBase) {\n");
		TextIO.output (MyoutStrm, "  \n");
		TextIO.output (MyoutStrm, "  printf(\"Error This is MinorGC proxyscan, should never be called!\");\n");
		TextIO.output (MyoutStrm, "return (ptr+2);\n");
        TextIO.output (MyoutStrm, "  }\n");
		
		()
        )


    
    fun minor (MyoutStrm) = let
        val s = HeaderTableStruct.HeaderTable.print (HeaderTableStruct.header)
        fun printmystring [] = ()
            | printmystring ((a,b)::t) = (let
                
				val size = String.size a
                fun lp(0,bites,pos) = ()
                | lp(strlen,bites,pos) =(
                    if (String.compare (substring(bites,strlen-1,1),"1") = EQUAL)
                    then (
                        TextIO.output (MyoutStrm,concat["    v = *(scanP+",Int.toString pos,");\n"]);
                        TextIO.output (MyoutStrm,"   if (inAddrRange(nurseryBase, allocSzB, ValueToAddr(v))) {\n");
                        TextIO.output (MyoutStrm,concat["     *(scanP+",Int.toString pos,") = ForwardObjMinor(v, nextW);\n"]);
                        TextIO.output (MyoutStrm,"  }\n");
                        lp(strlen-1,bites,pos+1)
                        )
                    else 
                        lp(strlen-1,bites,pos+1)
                    )
                in
                TextIO.output (MyoutStrm, concat["Word_t * minorGCscan",Int.toString b,"pointer (Word_t* ptr, Word_t **nextW, Addr_t allocSzB, Addr_t nurseryBase) {\n"]);
                TextIO.output (MyoutStrm, "  \n");
                TextIO.output (MyoutStrm, "  Value_t *scanP = (Value_t *)ptr;\n");
                TextIO.output (MyoutStrm, "  Value_t v = *scanP;\n");
                TextIO.output (MyoutStrm, "\n");
                
                lp(size,a,0);
                
				TextIO.output (MyoutStrm, concat["return (ptr+",Int.toString size,");\n"]);
                TextIO.output (MyoutStrm, "}\n");
                TextIO.output (MyoutStrm, "\n"); 
                
                printmystring t
                end
            )
            
    in
        printmystring s;
        ()
    end
    
    
    (*Major GC Functions *)
    fun majorpre (MyoutStrm) = (
        TextIO.output (MyoutStrm, "Word_t * majorGCscanRAWpointer (Word_t* ptr, VProc_t *vp, Addr_t oldSzB, Addr_t heapBase) {\n");
        TextIO.output (MyoutStrm, "\n" );
        TextIO.output (MyoutStrm, "assert (isRawHdr(ptr[-1]));\n");
		TextIO.output (MyoutStrm, "return (ptr+GetLength(ptr[-1]));\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "  }\n");
        
        TextIO.output (MyoutStrm, "Word_t * majorGCscanVECTORpointer (Word_t* ptr, VProc_t *vp, Addr_t oldSzB, Addr_t heapBase) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "Word_t *nextScan = ptr;\n");
        TextIO.output (MyoutStrm, "  int len = GetLength(ptr[-1]);\n" );
        TextIO.output (MyoutStrm, "  for (int i = 0;  i < len;  i++, nextScan++) {\n");
        TextIO.output (MyoutStrm, "   Value_t v = *(Value_t *)nextScan;\n");
        TextIO.output (MyoutStrm, "   if (isPtr(v)) {\n");
        TextIO.output (MyoutStrm, "     if (inAddrRange(heapBase, oldSzB, ValueToAddr(v))) {\n");
        TextIO.output (MyoutStrm, "          *nextScan = (Word_t)ForwardObjMajor(vp, v);\n");
        TextIO.output (MyoutStrm, "      }\n");
        TextIO.output (MyoutStrm, "      else if (inVPHeap(heapBase, (Addr_t)v)) {\n");
        TextIO.output (MyoutStrm, "          // p points to another object in the \"young\" region,\n");
        TextIO.output (MyoutStrm, "          // so adjust it.\n");
        TextIO.output (MyoutStrm, "          *nextScan = (Word_t)((Addr_t)v - oldSzB);\n");
        TextIO.output (MyoutStrm, "       }\n");
        TextIO.output (MyoutStrm, "    }\n");
        TextIO.output (MyoutStrm, "   }\n");
		TextIO.output (MyoutStrm, "return (ptr+len);\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
		
		TextIO.output (MyoutStrm, "Word_t * majorGCscanPROXYpointer (Word_t* ptr, VProc_t *vp, Addr_t oldSzB, Addr_t heapBase) {\n");
		TextIO.output (MyoutStrm, "  printf(\"Error This is MajorGC proxyscan, should never be called!\");\n");
		TextIO.output (MyoutStrm, "return (ptr+2);\n");
        TextIO.output (MyoutStrm, "  }\n");
        ()
        )


    
    fun major (MyoutStrm) = let
        val s = HeaderTableStruct.HeaderTable.print (HeaderTableStruct.header)
        fun printmystring [] = ()
            | printmystring ((a,b)::t) = (let
                    
				val size = String.size a	
                fun lp(0,bites,pos) = ()
                | lp(strlen,bites,pos) =(
                    if (String.compare (substring(bites,strlen-1,1),"1") = EQUAL)
                    then (
                        TextIO.output (MyoutStrm,concat["    v = *(Value_t *)(scanP+",Int.toString pos,");\n"]);
                        TextIO.output (MyoutStrm,"   if (inAddrRange(heapBase, oldSzB, ValueToAddr(v))) {\n");
                        TextIO.output (MyoutStrm,concat["     *(scanP+",Int.toString pos,") = (Word_t)ForwardObjMajor(vp, v);\n"]);
                        TextIO.output (MyoutStrm,"  }\n");
                        TextIO.output (MyoutStrm,"  else if (inVPHeap(heapBase, ValueToAddr(v))) {\n");
                        TextIO.output (MyoutStrm,concat["      *(scanP+",Int.toString pos,") = (Word_t)AddrToValue(ValueToAddr(v) - oldSzB);\n"]);
                        TextIO.output (MyoutStrm,"   }\n");
                        
                        lp(strlen-1,bites,pos+1)
                        )
                    else 
                        lp(strlen-1,bites,pos+1)
                    )
                in
                TextIO.output (MyoutStrm, concat["Word_t * majorGCscan",Int.toString b,"pointer (Word_t* ptr, VProc_t *vp, Addr_t oldSzB, Addr_t heapBase) {\n"]);
                TextIO.output (MyoutStrm, "  \n");
                TextIO.output (MyoutStrm, "  Word_t *scanP = ptr;\n");
                TextIO.output (MyoutStrm, "  Value_t v = *(Value_t *)scanP;\n");
                TextIO.output (MyoutStrm, "\n");
                
                lp(size,a,0);
                
				TextIO.output (MyoutStrm, concat["return (ptr+",Int.toString size,");\n"]);
                TextIO.output (MyoutStrm, "}\n");
                TextIO.output (MyoutStrm, "\n"); 
                
                printmystring t
                end
            )
            
    in
        printmystring s;
        ()
    end
    
    
        
    (*Globaltospace GC Functions *)
    fun globaltospacepre (MyoutStrm) = (
        TextIO.output (MyoutStrm, "Word_t * ScanGlobalToSpaceRAWfunction (Word_t* ptr, VProc_t *vp, Addr_t heapBase)  {\n");
        TextIO.output (MyoutStrm, "\n" );
        TextIO.output (MyoutStrm, "assert (isRawHdr(ptr[-1]));\n");
        TextIO.output (MyoutStrm, "\n");
		TextIO.output (MyoutStrm, "return (ptr+GetLength(ptr[-1]));\n");
        TextIO.output (MyoutStrm, "  }\n");
        
        TextIO.output (MyoutStrm, "Word_t * ScanGlobalToSpaceVECTORfunction (Word_t* ptr, VProc_t *vp, Addr_t heapBase) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "Word_t *nextScan = ptr;\n");
        TextIO.output (MyoutStrm, "  int len = GetLength(ptr[-1]);\n" );
        TextIO.output (MyoutStrm, "  for (int i = 0;  i < len;  i++, nextScan++) {\n");
        TextIO.output (MyoutStrm, "   Value_t v = *(Value_t *)nextScan;\n");
        TextIO.output (MyoutStrm, "     if (isPtr(v) && inVPHeap(heapBase, ValueToAddr(v))) {\n");
        TextIO.output (MyoutStrm, "          *nextScan = (Word_t)ForwardObjMajor(vp, v);\n");
        TextIO.output (MyoutStrm, "      }\n");
        TextIO.output (MyoutStrm, "    }\n");
		TextIO.output (MyoutStrm, "return (ptr+len);\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
		
		TextIO.output (MyoutStrm, "Word_t * ScanGlobalToSpacePROXYfunction (Word_t* ptr, VProc_t *vp, Addr_t heapBase)  {\n");
        TextIO.output (MyoutStrm, "  \n");
		TextIO.output (MyoutStrm, "  Word_t *scanP = ptr;\n");
	    TextIO.output (MyoutStrm, "  Value_t v = *(Value_t *)scanP;\n");
		TextIO.output (MyoutStrm,concat["    v = *(Value_t *)(scanP+",Int.toString 0,");\n"]);
		TextIO.output (MyoutStrm,"   if (inVPHeap(heapBase, ValueToAddr(v))) {\n");
		TextIO.output (MyoutStrm,concat["     *(scanP+",Int.toString 0,") = (Word_t)ForwardObjMajor(vp, v);\n"]);
		TextIO.output (MyoutStrm,"  } \n");
		TextIO.output (MyoutStrm, "  printf(\"Test Proxy ScanGlobalSpace!\");\n");
		TextIO.output (MyoutStrm, "return (ptr+2);\n");
        TextIO.output (MyoutStrm, "  }\n");
        ()
        )


    
    fun globaltospace (MyoutStrm) = let
        val s = HeaderTableStruct.HeaderTable.print (HeaderTableStruct.header)
        fun printmystring [] = ()
            | printmystring ((a,b)::t) = (let
                    
				val size = String.size a	
                fun lp(0,bites,pos) = ()
                | lp(strlen,bites,pos) =(
                    if (String.compare (substring(bites,strlen-1,1),"1") = EQUAL)
                    then (
                        TextIO.output (MyoutStrm,concat["    v = *(Value_t *)(scanP+",Int.toString pos,");\n"]);
                        TextIO.output (MyoutStrm,"   if (inVPHeap(heapBase, ValueToAddr(v))) {\n");
                        TextIO.output (MyoutStrm,concat["     *(scanP+",Int.toString pos,") = (Word_t)ForwardObjMajor(vp, v);\n"]);
                        TextIO.output (MyoutStrm,"  }\n");
                        
                        lp(strlen-1,bites,pos+1)
                        )
                    else 
                        lp(strlen-1,bites,pos+1)
                    )
                in
                TextIO.output (MyoutStrm, concat["Word_t * ScanGlobalToSpace",Int.toString b,"function (Word_t* ptr, VProc_t *vp, Addr_t heapBase) {\n"]);
                TextIO.output (MyoutStrm, "  \n");
                TextIO.output (MyoutStrm, "  Word_t *scanP = ptr;\n");
                TextIO.output (MyoutStrm, "  Value_t v = *(Value_t *)scanP;\n");
                TextIO.output (MyoutStrm, "\n");
                
                lp(size,a,0);
                
				TextIO.output (MyoutStrm, concat["return (ptr+",Int.toString size,");\n"]);
                TextIO.output (MyoutStrm, "}\n");
                TextIO.output (MyoutStrm, "\n"); 
                
                printmystring t
                end
            )
            
    in
        printmystring s;
        ()
    end

    
    (*Global GC Functions *)
    fun globalpre (MyoutStrm) = (
        TextIO.output (MyoutStrm, "Word_t * globalGCscanRAWpointer (Word_t* ptr, VProc_t *vp) {\n");
        TextIO.output (MyoutStrm, "\n" );
        TextIO.output (MyoutStrm, "assert (isRawHdr(ptr[-1]));\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "return (ptr+GetLength(ptr[-1]));\n");
        TextIO.output (MyoutStrm, "}\n");
        
        TextIO.output (MyoutStrm, "Word_t * globalGCscanVECTORpointer (Word_t* ptr, VProc_t *vp) {\n");
        TextIO.output (MyoutStrm, "\n");
        TextIO.output (MyoutStrm, "Word_t *nextScan = ptr;\n");
        TextIO.output (MyoutStrm, "  int len = GetLength(ptr[-1]);\n" );
        TextIO.output (MyoutStrm, "  for (int i = 0;  i < len;  i++, nextScan++) {\n");
        TextIO.output (MyoutStrm, "   Value_t v = *(Value_t *)nextScan;\n");
        TextIO.output (MyoutStrm, "   if (isFromSpacePtr(v)) {\n");
        TextIO.output (MyoutStrm, "          *nextScan = (Word_t)ForwardObjGlobal(vp, v);\n");
        TextIO.output (MyoutStrm, "    }\n");
        TextIO.output (MyoutStrm, "   }\n");
        TextIO.output (MyoutStrm, "return (ptr+len);\n");
        TextIO.output (MyoutStrm, "}\n");
        TextIO.output (MyoutStrm, "\n");
		
        TextIO.output (MyoutStrm, "Word_t * globalGCscanPROXYpointer (Word_t* ptr, VProc_t *vp) {\n");
        TextIO.output (MyoutStrm, "  \n");
        TextIO.output (MyoutStrm, "   Value_t v = (Value_t )ptr[1];\n");
        TextIO.output (MyoutStrm, "  //printf(\"This is GlobalGC proxyscan begin scanptr is %p!, vaue of v is %lu\\n\",(void*)ptr,(unsigned long)v);\n");
        TextIO.output (MyoutStrm, "   if( (unsigned long)v >= vp->maxProxy ) {\n");
        TextIO.output (MyoutStrm, "  //printf(\"Forward the proxyobj 2 %p!\\n\",(void*)ptr[1]);\n");
        TextIO.output (MyoutStrm, "     assert (isFromSpacePtr( v ));\n");
        TextIO.output (MyoutStrm, "     Value_t p = ForwardObjGlobal(vp, v);\n");
        TextIO.output (MyoutStrm, "     ptr[1]=(Word_t)p;\n");
        TextIO.output (MyoutStrm, "   }\n");
        TextIO.output (MyoutStrm, "   return (ptr+2);\n");
        TextIO.output (MyoutStrm, "}\n");

        ()
        )


    
    fun global (MyoutStrm) = let
        val s = HeaderTableStruct.HeaderTable.print (HeaderTableStruct.header)
        fun printmystring [] = ()
            | printmystring ((a,b)::t) = (let
                    
				val size = String.size a	
                fun lp(0,bites,pos) = ()
                | lp(strlen,bites,pos) =(
                    if (String.compare (substring(bites,strlen-1,1),"1") = EQUAL)
                    then (
                        TextIO.output (MyoutStrm,concat["    v = *(Value_t *)(scanP+",Int.toString pos,");\n"]);
                        TextIO.output (MyoutStrm,"   if (isFromSpacePtr(v)) {\n");
                        TextIO.output (MyoutStrm,concat["     *(scanP+",Int.toString pos,") = (Word_t)ForwardObjGlobal(vp, v);\n"]);
                        TextIO.output (MyoutStrm,"  }\n");
                        lp(strlen-1,bites,pos+1)
                        )
                    else 
                        lp(strlen-1,bites,pos+1)
                    )
                in
                TextIO.output (MyoutStrm, concat["Word_t * globalGCscan",Int.toString b,"pointer (Word_t* ptr, VProc_t *vp) {\n"]);
                TextIO.output (MyoutStrm, "  \n");
                TextIO.output (MyoutStrm, "  Word_t *scanP = ptr;\n");
                TextIO.output (MyoutStrm, "  Value_t v = *(Value_t *)scanP;\n");
                TextIO.output (MyoutStrm, "\n");
                
                lp(size,a,0);
                
				TextIO.output (MyoutStrm, concat["return (ptr+",Int.toString size,");\n"]);
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
                TextIO.output (MyoutStrm, concat[",{minorGCscan",Int.toString i,"pointer,majorGCscan",Int.toString i,"pointer,globalGCscan",Int.toString i,"pointer,ScanGlobalToSpace",Int.toString i,"function}\n"]);
                printtable(listlength,i+1)
                )
            )
            
        in
        TextIO.output (MyoutStrm, concat["tableentry table[",Int.toString (length+predefined),"] = { {minorGCscanRAWpointer,majorGCscanRAWpointer,globalGCscanRAWpointer,ScanGlobalToSpaceRAWfunction},\n"]);
        TextIO.output (MyoutStrm, "{minorGCscanVECTORpointer,majorGCscanVECTORpointer,globalGCscanVECTORpointer,ScanGlobalToSpaceVECTORfunction},\n");
		TextIO.output (MyoutStrm, "{minorGCscanPROXYpointer,majorGCscanPROXYpointer,globalGCscanPROXYpointer,ScanGlobalToSpacePROXYfunction}\n");
        
        printtable (length+predefined,predefined);
        
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
            
            majorpre Myout;
            major Myout;
            
            globaltospacepre Myout;
            globaltospace Myout;
            
            globalpre Myout;
            global Myout;
            
            createtable Myout;
            
            TextIO.closeOut(Myout)
        end
    
end
    