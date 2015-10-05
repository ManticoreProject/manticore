(* ordered-rs.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Chronologically ordered read sets for TL2
 *)

 

structure TL2OrderedRS = 
struct
    datatype 'a ritem = NilRead | WithK of 'a * 'a * 'a * 'a * 'a
                      | WithoutK of 'a * 'a | Abort of unit  

    datatype 'a witem = Write of 'a * 'a * 'a | NilWrite

    type 'a witem = 'a FullAbortSTM.witem


    val tref = FullAbortSTM.new 0
    fun getTRef() = tref

    _primcode(

    	typedef read = ![enum(3), any, ritem];

    	typedef read_set = ![int, ritem, ritem, ritem]; (*num conts, head, lastK, tail*)

        define @get-tref = getTRef;

        define @new(abortK : cont(any) / exh:exh) : read_set = 
            let tref : any = @get-tref(UNIT / exh)
            let withK : ritem = WithK(tref, NilRead, abortK, NilWrite, NilRead)
            let rs : read_set = alloc(0, withK, NilRead, withK)
            return(rs);

    	(*Note that these next two defines, rely on the fact that a heap limit check will not get
         *inserted within the body*)
        (*Add a checkpointed read to the read set*)
        define @insert-with-k(tv:any, k:cont(any), ws:witem, readSet : read_set, stamp : ![long,int, int, long] / exh:exh) : read_set = 
            let newItem : ritem = WithK(tv, NilRead, k, ws, #SHORT_PATH(readSet))
            let vp : vproc = host_vproc
            let nurseryBase : long = vpload(NURSERY_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let lastAddr : any = (any) #TAIL(readSet)
            let casted : read = (read)lastAddr
            if I64Gte(lastAddr, nurseryBase)
            then
                if I64Lt(lastAddr, limitPtr)
                then (*last item is still in nursery*)
                    do #R_ENTRY_NEXT(casted) := newItem
                    do #TAIL(readSet) := newItem
                    do #SHORT_PATH(readSet) := newItem
                    do #KCOUNT(readSet) := I32Add(#KCOUNT(readSet), 1)
                    return(readSet)
                else (*not in nursery, add last item to remember set*)
                    let newRS : read_set = alloc(I32Add(#KCOUNT(readSet), 1), #LONG_PATH(readSet), newItem, newItem)
                    let rs : any = vpload(REMEMBER_SET, vp)
                    let newRemSet : [read, int, long, any] = alloc(casted, R_ENTRY_NEXT, #3(stamp), rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    do #R_ENTRY_NEXT(casted) := newItem
                    do FLS.@set-key(READ_SET, newRS / exh)
                    return(newRS)
            else (*not in nursery, add last item to remember set*)
                let newRS : read_set = alloc(I32Add(#KCOUNT(readSet), 1), #LONG_PATH(readSet), newItem, newItem)
                let rs : any = vpload(REMEMBER_SET, vp)
                let newRemSet : [read, int, long, any] = alloc(casted, R_ENTRY_NEXT, #3(stamp), rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                do #R_ENTRY_NEXT(casted) := newItem
                do FLS.@set-key(READ_SET, newRS / exh)
                return(newRS)
        ;

        (*add a non checkpointed read to the read set*)
    	define @insert-without-k(tv:any, readSet : read_set, stamp : ![long,int,int,long] / exh:exh) : read_set =
    		let newItem : ritem = WithoutK(tv, NilRead)
    		let vp : vproc = host_vproc
    		let nurseryBase : long = vpload(NURSERY_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let lastAddr : any = (any) #TAIL(readSet)
            let casted : read = (read) lastAddr
            if I64Gte(lastAddr, nurseryBase)
            then
                if I64Lt(lastAddr, limitPtr)
                then (*last item is still in nursery*)
                    do #R_ENTRY_NEXT(casted) := newItem
                    do #TAIL(readSet) := newItem
                    return(readSet)
                else (*not in nursery, add last item to remember set*)
                    let newRS : read_set = alloc(#KCOUNT(readSet), #LONG_PATH(readSet), #SHORT_PATH(readSet), newItem)
                    let rs : any = vpload(REMEMBER_SET, vp)
                    let newRemSet : [read, int, long, any] = alloc(casted, R_ENTRY_NEXT, #3(stamp), rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    do #R_ENTRY_NEXT(casted) := newItem
                    do FLS.@set-key(READ_SET, newRS / exh)
                    return(newRS)
            else (*not in nursery, add last item to remember set*)
                let newRS : read_set = alloc(#KCOUNT(readSet), #LONG_PATH(readSet), #SHORT_PATH(readSet), newItem)
                let rs : any = vpload(REMEMBER_SET, vp)
                let newRemSet : [read, int, long, any] = alloc(casted, R_ENTRY_NEXT, #3(stamp), rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                do #R_ENTRY_NEXT(casted) := newItem
                do FLS.@set-key(READ_SET, newRS / exh)
                return(newRS)
	    ;

    )



end