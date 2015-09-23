


structure Logging = 
struct

    _primcode(

	extern void postStartTX(void*, long);
	extern void postEvent(void *, int);
	extern void postAbortTX(void*, int, int);
	extern void postRememberObj(void*, void*);
    extern void postWSMatch(void *, int);

#ifdef EVENT_LOGGING

	define inline @log-start-tx() : () = 
            let vp : vproc = host_vproc
            do ccall postStartTX(vp, 0:long)
	    return()
	;

	define inline @log-start-tx-w-msg(msg : long) : () = 
	    let vp : vproc = host_vproc
            do ccall postStartTX(vp, msg)
            return();						

	define inline @log-commit-tx() : () = 
            let vp : vproc = host_vproc
            do ccall postEvent(vp, 8)
	    return()
        ; 

	define inline @log-eager-partial-abort(abortInfo : int) : () = 
            let vp : vproc = host_vproc
            do ccall postAbortTX(vp, abortInfo, 4)
	    return()
        ; 

        define inline @log-eager-full-abort() : () = 
            let vp : vproc = host_vproc
            do ccall postEvent(vp, 5)
	    return()
        ; 

        define inline @log-commit-partial-abort(abortInfo : int) : () = 
            let vp : vproc = host_vproc
            do ccall postAbortTX(vp, abortInfo, 6)
	    return()
        ; 

        define inline @log-commit-full-abort() : () = 
            let vp : vproc = host_vproc
            do ccall postEvent(vp, 7)
	    return()
        ; 

	define inline @log-fast-forward() : () = 
	    let vp : vproc = host_vproc
            do ccall postEvent(vp, 17)
	    return()
        ;

        define inline @log-ff(ffInfo : int) : () =  
            let vp : vproc = host_vproc
            do ccall postAbortTX(vp, ffInfo, 17)
            return();

        define inline @log-remember-obj(objAddr : any) : () = 
            let vp : vproc = host_vproc
            do ccall postRememberObj(vp, objAddr)
	    return()
        ;

	define inline @log-start-validate() : () = 
            let vp : vproc = host_vproc
            do ccall postEvent(vp, 10)
	    return()
        ;

        define inline @log-ts-extension() : () = 
            let vp : vproc = host_vproc
            do ccall postEvent(vp, 19)
            return()
        ;
        
        define inline @log-ws-match(matchType : int) : () = 
            let vp : vproc = host_vproc
            do ccall postWSMatch(vp, matchType)
            return()
        ;
#else
	define inline @log-start-tx() : () = 
            return()
	;

	define inline @log-start-tx-w-msg(msg : long) : () = return();

	define inline @log-commit-tx() : () = 
            return()
        ; 

	define inline @log-eager-partial-abort(abortInfo : int) : () = 
            return()
        ; 

        define inline @log-eager-full-abort() : () = 
            return()
        ; 

        define inline @log-commit-partial-abort(abortInfo : int) : () = 
            return()
        ; 

        define inline @log-commit-full-abort() : () = 
            return()
        ; 

	define inline @log-fast-forward() : () = 
	    return()
        ;

        define inline @log-remember-obj(objAddr : any) : () = 
            return()
        ;
	
	define inline @log-start-validate() : () = 
	    return()
        ;

        define inline @log-ts-extension() : () = 
            return()
        ;
        
        define inline @log-ff(ffInfo : int) : () =  
            return();

        define inline @log-ws-match(matchType : int) : () = return();
#endif

        define @log-commit-tx-wrapper(x : unit / exh : exh) : unit =
            do @log-commit-tx()
            return(UNIT);



    )

	val postCommitTX : unit -> unit = _prim(@log-commit-tx-wrapper)


end

