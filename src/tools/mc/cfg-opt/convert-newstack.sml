(* convert-newstack.sml
 *
 * COPYRIGHT (c) 2020 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure ConvertNewStack : sig

    (* Converts C calls to allocate a new stack into a native call
       that implements the fast-path in assembly.
    *)

    val transform : CFG.module -> CFG.module

  end = struct

    structure C = CFG
    structure CU = CFGUtil
    structure CV = CFG.Var
    structure CL = CFG.Label
    structure VSet = CFG.Var.Set
    structure VMap = CFG.Var.Map

    (* NOTE: Enabled by default for linked-frame stacks, for which we saw
       a 1.024x speed-up (2.4%) on cml-pingpong. Virtually no difference on
       cml-spawn.

       Thus, other stack strategies (for which ASM_NewStack is not implemented yet!)
       we're very likely to see no benefit, or a performance regression! This is
       because even _more_ instructions are executed by the fast-path for those
       strategies than for linked stacks. So the handful of instructions saved by
       not switching stacks is probably not going to outweigh my ability to
       write optimal-but-readable assembly code for the fast path, etc!
                                                          - kavon (4/1/2020) *)
    fun isEnabled () = Controls.get BasicControl.linkstack

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
    fun transform (orig as C.MODULE{name, externs, mantiExterns, code}) =
      if not (isEnabled())
        then orig
        else let
      (* NOTE: this must be kept up-to-date with the assembly-code implementation! *)
      val newStackLab_type = CFGTy.T_StdDirFun {
                      clos = CFGTy.unitTy,
                      args = [CFGTy.T_Any],
                      ret = [CFGTy.T_Any],
                      exh = CFGTy.T_Any
                  }

      val newStackLab = CFG.mkMantiExtern(CFG.Label.new("ASM_NewStack", newStackLab_type))
      val module = C.mkModule (name, externs, newStackLab :: mantiExterns,
                                 map (doFunc newStackLab) code)
    in
      (* recompute census information, since it may have been invalidated *)
      Census.census module;
      module
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
    and splitBlock newStackLab convention (oldBlock as C.BLK{lab, args, body, exit}) = (
      (* Assert that the exit is not an AllocCCall.
         this conversion pass should have been performed prior to inserting
         heap checks! *)
      case exit
        of C.AllocCCall _ =>
            raise Fail "convert-newstack: pass run at wrong point!"
         | _ => ();

      (* if the partition contains 1 element, then we can just return the original block. *)
      case findAll isNewStackCall body
        of [_] => [oldBlock]
         | partitioning => let

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

          val emptySubst : CFGUtil.substitution = VMap.empty
          val (_, _, _, otherBlocks) =
              foldl (generateBlock newStackLab exit) (lab, args, emptySubst, []) splitPoints
        in
          rev otherBlocks
        end)



    and generateBlock newStackLab exit ((instrs, ccall, liveOut), (lab, args, env, blks)) =
      (case ccall
        of NONE => (* easy case. this is the last block,
                    so we just pass along the old stuff *)
          (lab, args, env,
            C.mkBlock(lab, args, map (CU.substExp env) instrs, CU.substTransfer env exit) :: blks)

        | SOME ccall => let
          (* generate the call and components of the successor block *)
          val (instrs, transfer, nextLab, nextArgs, nextEnv) =
                replaceNewStackCall (newStackLab, env, instrs, ccall, liveOut)
        in
          (nextLab, nextArgs, nextEnv, C.mkBlock(lab, args, instrs, transfer) :: blks)
        end

    (* end case *))


    and replaceNewStackCall (nsLab, env, instrs, C.E_CCall ([lhsV], _, [_, thdClos]), liveOut) = let

      fun rauw env v = (case VMap.find (env, v)
        of SOME newV => newV
         | NONE => v
       (* end case *))

       fun mkUnit name = let
         val var = CV.new (name, CFGTy.unitTy)
         val binding = CFG.mkConst(var, Literal.Enum 0w0, CFGTy.unitTy)
       in
         (var, binding)
       end

       (* generate a new env for the next block that records the correspondence
            oldLiveOut -> newArg *)
       val nextArgs = map CV.copy liveOut
       val nextEnv = foldl VMap.insert' env (ListPair.zip(liveOut, nextArgs))

       (* name the next block since we can now determine its type *)
       val nextLab = CL.new("afterNewStack", CFGTy.T_Block{args = map CV.typeOf nextArgs})

      (* perform rauw updates *)
      val instrs = map (CU.substExp env) instrs
      val updatedLiveOut = map (rauw env) liveOut

      (* generate bindings and components for the call *)
      val fVar = CV.new (CL.nameOf nsLab, CL.typeOf nsLab)
      val bindF = C.mkLabel (fVar, nsLab)

      val (unitVar, bindUnit) = mkUnit "unit"

      val transfer = C.Call {
          f = fVar,
          clos = unitVar,
          args = [rauw env thdClos, unitVar],  (* last arg is the exn handler *)
          next = C.NK_Resume ([lhsV], (nextLab, updatedLiveOut))
      }

    in
      (instrs @ [bindUnit, bindF], transfer, nextLab, nextArgs, nextEnv)
    end

    (* Given a set of vars that are live OUT of this partition point,
       computes the set of vars that are live IN to this partition point
       and passes that along.
       It also builds a list of split points tagged with the values that
       are needed by the next point (i.e., the live out it was given).

       This computes a minimal set of live vars and should be folded
       from right-to-left (reverse order) over the partition points. *)
    and computeLiveIn ((instrs, ccall), (liveOut, splits)) = let
      fun iter ([], liveIn) = (liveIn, (instrs, ccall, VSet.listItems liveOut) :: splits)
        | iter (x::xs, liveIn) = let
          (* live = (live \ lhs) + rhs *)
          val lhs = asSet (CU.lhsOfExp x)
          val rhs = asSet (CU.rhsOfExp x)
          val liveIn = VSet.union(VSet.difference(liveIn, lhs), rhs)
        in
          iter (xs, liveIn)
        end

      val allReversed = if isSome ccall
                        then valOf ccall :: rev instrs
                        else rev instrs
    in
      iter (allReversed, liveOut)
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
