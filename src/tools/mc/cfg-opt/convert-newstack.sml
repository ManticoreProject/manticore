(* convert-newstack.sml
 *
 * COPYRIGHT (c) 2020 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure ConvertNewStack : sig

    (* Converts C calls to allocate a new stack into a native call
       that implements the fast-path in assembly.

       If the fast-path's assumptions aren't satisfied, a slower call into
       the original C routine is invoked.

       Thus, this is a trade-off, but this optimization is very likely to
       improve performance.
    *)

    val transform : CFG.module -> CFG.module

  end = struct

    structure C = CFG
    structure CU = CFGUtil
    structure CV = CFG.Var
    structure CL = CFG.Label
    structure VSet = CFG.Var.Set
    structure VMap = CFG.Var.Map

    (* identifies the first element in the list that satisfies the predicate,
       returning the elements before and after it too. *)
    fun findFirst predicate body = let
      fun search (seen, []) = (rev seen, NONE, [])
        | search (seen, x::xs) =
          if predicate x
            then (rev seen, SOME x, xs)
            else search (x::seen, xs)
    in
      search ([], body)
    end

    and findAll pred body = let
      fun lp (befor, SOME exp, rest) =
             (befor, SOME exp) :: lp (findFirst pred rest)
        | lp (befor, NONE, []) = [(befor, NONE)]
    in
      lp (findFirst pred body)
    end

    and asSet lst = VSet.addList (VSet.empty, lst)



    (* the top-level transformation function! *)
    fun transform (C.MODULE{name, externs, mantiExterns, code}) = let
      (* NOTE: this must be kept up-to-date with the assembly-code implementation! *)
      val newStackLab_type = CFGTy.T_StdDirFun {
                      clos = CFGTy.unitTy,
                      args = [CFGTy.T_Any],
                      ret = [CFGTy.T_Any],
                      exh = CFGTy.T_Any
                  }

      val newStackLab = CFG.mkMantiExtern(CFG.Label.new("ASM_NewStack", newStackLab_type))
    in
      C.mkModule (name, externs, newStackLab :: mantiExterns,
                                 map (doFunc newStackLab) code)
    end

    and doFunc newStackLab (C.FUNC{lab, entry, start, body}) = let
      val splitBlock' = splitBlock newStackLab
      val newStart :: startResidual = (splitBlock' (SOME entry)) start
      val newBody = List.concat (map (splitBlock' NONE) body)
    in
      C.mkLocalFunc(lab, entry, newStart, startResidual @ newBody)
    end

    (* In order to replace a ccall with a Manticore call, we need to
       know which variables are live after the ccall.
    *)
    and splitBlock newStackLab convention (C.BLK{lab, args, body, exit}) = (
      (* Assert that the exit is not an AllocCCall.
         this pass should be performed prior to inserting
         heap checks! *)
      case exit
        of C.AllocCCall _ =>
            raise Fail "convert-newstack: pass run at wrong point!"
         | _ => (); let

      val partitioning = findAll isNewStackCall body
      (* TODO: if the partition contains 1 element, then we can return the original block. *)

      val live = asSet (CU.varsOfXfer exit)
      val (blockLive, splitPoints) = foldr computeLiveIn (live, []) partitioning

      (* assertion to ensure liveness is computed correctly *)
      val allBlockParams = if isSome convention
                            then CU.paramsOfConv(valOf convention, args)
                            else args
      val () = if not (VSet.isSubset(blockLive, asSet allBlockParams))
                then raise Fail (concat [
                  "convert-newstack: liveness computation wrong,\n",
                  concat ((map CV.toString) (VSet.listItems blockLive)),
                  "\n\tshould be a subset of\n",
                  concat (map CV.toString allBlockParams)
                  ])
                else ()

    in
      [C.mkBlock(lab, args, body, exit)]
    end)

    (* Given a set of vars that are live OUT of this partition point,
       computes the set of vars that are live IN to this partition point.
       This computes a minimal set of live vars and should be folded
       from right-to-left (reverse order) over the partition points. *)
    and computeLiveIn ((instrs, ccall), (live, splits)) = let
      fun iter ([], live) = (live, (instrs, ccall, live) :: splits)
        | iter (x::xs, live) = let
          (* live = (live \ lhs) + rhs *)
          val lhs = asSet (CU.lhsOfExp x)
          val rhs = asSet (CU.rhsOfExp x)
          val live = VSet.union(VSet.difference(live, lhs), rhs)
        in
          iter (xs, live)
        end

      val allReversed = if isSome ccall
                        then valOf ccall :: rev instrs
                        else rev instrs
    in
      iter (allReversed, live)
    end

    and isNewStackCall exp = (case exp
      of C.E_CCall (_, func, _) => hasName func "NewStack"
       | _ => false
      (* end case *))

    (* very naive and just looks for a direct let binding (not through casts) *)
    and hasName var name = (case CV.kindOf var
      of C.VK_Let (C.E_Label (_, lab)) => CL.nameOf lab = name
       | _ => false
      (* end case *))


  end
