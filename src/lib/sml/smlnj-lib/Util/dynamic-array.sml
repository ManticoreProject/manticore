(* dynamic-array.sml
 *
 * COPYRIGHT (c) 2009 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Polymorhic arrays of unbounded length
 *)

structure DynamicArray :> DYNAMIC_ARRAY =
  struct

    structure A = Array

    datatype 'a array = BLOCK of ('a A.array ref * 'a * int ref)
 
    exception Subscript = General.Subscript
    exception Size = General.Size

    fun array (sz, dflt) = BLOCK(ref(A.array(sz, dflt)), dflt, ref(~1))

  (* fromList (l, v) creates an array using the list of values l
   * plus the default value v.
   * NOTE: Once MONO_ARRAY includes arrayoflist, this will become trivial.
   *)
    fun fromList (initList, dflt) = let
          val len = length initList
	  val arr = A.array(len, dflt)
	  fun upd ([], _) = ()
	    | upd (x::r, i) = (A.update(arr, i, x); upd(r, i+1))
	  in
	    upd (initList, 0);
	    BLOCK(ref arr, dflt, ref (len-1))
	  end

  (* tabulate (sz,fill,dflt) acts like Array.tabulate, plus 
   * stores default value dflt.  Raises Size if sz < 0.
   *)
    fun tabulate (sz, fillFn, dflt) =
	  BLOCK(ref(A.tabulate(sz, fillFn)), dflt, ref (sz-1))

    fun subArray (BLOCK(arr,dflt,bnd),lo,hi) = let
          val arrval = !arr
          val bnd = !bnd
          fun copy i = A.sub(arrval,i+lo)
          in
            if hi <= bnd
              then BLOCK(ref(A.tabulate(hi-lo,copy)), dflt, ref (hi-lo))
            else if lo <= bnd 
              then BLOCK(ref(A.tabulate(bnd-lo,copy)),dflt,ref(bnd-lo))
            else
              array(0,dflt)
          end

    fun default (BLOCK(_,dflt,_)) = dflt

    fun sub (BLOCK(arr,dflt,_),idx) = (A.sub(!arr,idx)) 
          handle Subscript => if idx < 0 then raise Subscript else dflt

    fun bound (BLOCK(_,_,bnd)) = (!bnd)

    fun expand(arr,oldlen,newlen,dflt) = let
          fun fillfn i = if i < oldlen then A.sub(arr,i) else dflt
          in
            A.tabulate(newlen, fillfn)
          end

    fun update (BLOCK(arr,dflt,bnd),idx,v) = let 
          val len = A.length (!arr)
          in
            if idx >= len 
              then arr := expand(!arr,len, Int.max(len+len,idx+1),dflt) 
              else ();
            A.update(!arr,idx,v);
            if idx > !bnd then bnd := idx else ()
          end

    fun truncate (a as BLOCK(arr,dflt,bndref),sz) = let
          val bnd = !bndref
          val newbnd = sz - 1
          val arr_val = !arr
          val array_sz = A.length arr_val
          fun fillDflt (i,stop) =
                if i = stop then ()
                else (A.update(arr_val,i,dflt);fillDflt(i-1,stop))
          in
            if newbnd < 0 then (bndref := ~1;arr := A.array(0,dflt))
            else if newbnd >= bnd then ()
            else if 3 * sz < array_sz then let
              val BLOCK(arr',_,bnd') = subArray(a,0,newbnd)
              in
                (bndref := !bnd'; arr := !arr')
              end
            else fillDflt(bnd,newbnd)
          end

  (* get the array slice that covers the defined portion of the array *)
    fun slice (BLOCK(arr, _, bnd)) =
	  ArraySlice.slice(!arr, 0, SOME(!bnd))

  (* we implement the iterators by using the array slice operations *)
    fun vector arr = ArraySlice.vector (slice arr)
    fun appi f arr = ArraySlice.appi f (slice arr)
    fun app f arr = ArraySlice.app f (slice arr)
    fun modifyi f arr = ArraySlice.modifyi f (slice arr)
    fun modify f arr = ArraySlice.modify f (slice arr)
    fun foldli f init arr = ArraySlice.foldli f init (slice arr)
    fun foldri f init arr = ArraySlice.foldri f init (slice arr)
    fun foldl f init arr = ArraySlice.foldl f init (slice arr)
    fun foldr f init arr = ArraySlice.foldr f init (slice arr)
    fun findi pred arr = ArraySlice.findi pred (slice arr)
    fun find pred arr = ArraySlice.find pred (slice arr)
    fun exists pred arr = ArraySlice.exists pred (slice arr)
    fun all pred arr = ArraySlice.all pred (slice arr)
    fun collate cmp (arr1, arr2) = ArraySlice.collate cmp (slice arr1, slice arr2)

(* TODO
    val copy : {di:int, dst:'a array, src:'a array} -> unit
    val copyVec : {di:int, dst:'a array, src:'a vector} -> unit
*)

  end (* DynamicArrayFn *)

