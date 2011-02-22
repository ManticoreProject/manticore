(* uid.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * UIDs are globally-unique identifiers available to Manticore applications. We avoid 
 * any synchronization overhead by setting the leftmost lg_2(P) bits to the processor
 * id, where P is the number of processors. For example, the first uid to be created
 * by processor 3 would be 3 * 2^(64 - lg_2(P)). Each processor has 64 - lg_2(P) bits
 * for uids.
 *)

structure UID (* :
  sig

    type uid = Word64.word
    val new : unit -> uid

    _prim (
      typedef uid = Word64.word;
      define inline @new (/ exh : exh) : uid;
    )

  end *) = struct

    structure A = LongArray

    _primcode (
      typedef uid = Word64.word;
    )

    type uid = Word64.word

    local

      (* number of bits needed to maintain the vproc id *)
	val nVProcLg = Word64.ceilingLg (Word64.fromInt (VProc.numVProcs ()))
			 
	val cntArr : A.array = A.tabulate (VProc.numVProcs (), 
		    fn i => Word64.lsh (Word64.fromInt i, Word64.sub (64, nVProcLg)))

	_primcode (

	  define @new (cntArr : A.array / exh : exh) : Word64.ml_word =
	      let self : vproc = SchedulerAction.@atomic-begin ()
	      let id : int = VProc.@vproc-id (self)
	      let cnt : Word64.ml_word = A.@sub (cntArr, id / exh)
	      let cntNewU : Word64.ml_word = alloc (I64Add (unwrap (cnt), 1:long))
	      do A.@update (cntArr, id, cntNewU / exh)
	      return (cntNewU)
	    ;

	)

    val new' : A.array -> Word64.word = _prim (@new)

    in

    fun new () = new' cntArr

    _primcode (
      typedef uid = Word64.word;
      define @new-w = new;
      define @new (/ exh : exh) : uid =
	  let uid : Word64.ml_word = @new-w (UNIT / exh)
	  return (#0(uid))
	;
    )

    end

  end
