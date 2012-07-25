(*@FILE tr-prod-ptup.tex *)
(*@BEGIN tr-prod-ptup.tex *)
datatype tree
  = Lf of int
  | Nd of tree * tree

fun trProd (Lf i) = i
  | trProd (Nd (tL, tR)) = 
      (op * ) (|trProd1 tL, trProd1 tR|)
(*@END tr-prod-ptup.tex *)

(*@FILE tr-prod-ptup-xform.tex *)   
(*@BEGIN tr-prod-ptup-xform.tex *)   
fun trProdT (Lf i) = i
  | trProdT (Nd(tL, tR)) = let
      val (l, r) = let
	    val fR = future (fn _ => trProdT tR)
	    in
	      (trProdT tl, touch fR)
		handle e => (cancel fR; raise e)
	    end
      in
        l * r
      end
(*@END tr-prod-ptup-xform.tex *)   
			    
(*@FILE tr-prod-pval.tex *)
(*@BEGIN tr-prod-pval.tex *)
fun trProd (Lf i) = i
  | trProd (Nd (tL, tR)) = let
      pval pL = trProd tL
      pval pR = trProd tR
      in
        if (pL = 0)
	  then 0
	  else (pL * pR)
      end 
(*@END tr-prod-pval.tex *)
	
(*@FILE tr-prod-pval-xform.tex *)
(*@BEGIN tr-prod-pval-xform.tex *)		   
fun trProdT (Lf i) = i
  | trProdT (Nd (tL, tR)) = let
      val fR = future (fn _ => trProdT tR)
      in let
        val pL = trProdT tL
        in
          if (pL = 0) then (cancel fR; 0)
          else (pL * (touch fR))
        end 
          handle e => (cancel fR; raise e) 
      end
(*@END tr-prod-pval-xform.tex *)

(*@FILE thief-loop.tex *)   
(*@BEGIN thief-loop.tex *)   
fun steal () = 
    if computationDone()
      then exit()
      else (
	yield();
	case popOld(pickVictimWorker())
	 of NONE => steal()
	  | SOME slowClone => slowClone())
(*@END thief-loop.tex *)   

fun trProdT (Lf i) = i
  | trProdT (Nd (tL, tR)) = callcc(fn retK => let
      val nDone = ref 1
      val xL = ref NONE and xR = ref NONE
      fun join () = if AtomicFetchAndAdd(nDone, 1) = 2
	    then throw retK(valOf xL * valOf xR)
	    else steal() (* this worker now becomes a thief *)
      fun wait () = if (!xL) = NONE then wait() else ()
      val wR = pushNew(fn _ => (* slow clone *)
		 (xR := SOME(trProdT tR
               (* deliver exceptions in left-to-right order *)
			       handle e => (wait(); raise e)); 
		  join()))
      val vL = trProdT tL handle e => (cancel wR; raise e)
      in
	if popNew() <> NONE    (* true, iff wR was not stolen *)
	  then vL * trProdT tR                  (* fast clone *)
	  else (xL := SOME vL; join())          (* slow clone *)
      end)

(*@FILE tr-prod-ptup-clone.tex *)   
(*@BEGIN tr-prod-ptup-clone.tex *)   
fun trProdT (Lf i) = i
  | trProdT (Nd (tL, tR)) = callcc(fn retK => let
      val resume = ref false
      val xL = ref NONE and xR = ref NONE
      fun join () = if TestAndSet resume
	    then throw retK(valOf (!xL) * valOf (!xR))
	    else steal() (* this worker now becomes a thief *)
      val _ = pushNew (fn _ => (* slow clone *)
		  (xR := SOME(trProdT tR); join()))
      val vL = trProdT tL
      in
	if popNew() <> NONE    (* true, iff tR was not stolen *)
	  then vL * trProdT tR                  (* fast clone *)
	  else (xL := SOME vL; join())          (* slow clone *)
      end)
(*@END tr-prod-ptup-clone.tex *) 
