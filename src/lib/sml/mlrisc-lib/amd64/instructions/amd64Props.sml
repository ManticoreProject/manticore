(* amd64Props.sml
 *
 * This functor encodes semantic information of instructions.
 *  
 *)

signature AMD64INSN_PROPERTIES =
  sig
    include INSN_PROPERTIES
    (* returns the bit width of an instruction's source operand *)
    val szOfInstr : I.instr -> int
    (* returns the bit width of an floating-point instruction's source operand *)
    val szOfFinstr : I.instr -> int
  end 

functor AMD64Props (
    structure Instr : AMD64INSTR
    structure MLTreeHash : MLTREE_HASH (* where T = Instr.T *)
                           where type T.Basis.cond = Instr.T.Basis.cond
                             and type T.Basis.div_rounding_mode = Instr.T.Basis.div_rounding_mode
                             and type T.Basis.ext = Instr.T.Basis.ext
                             and type T.Basis.fcond = Instr.T.Basis.fcond
                             and type T.Basis.rounding_mode = Instr.T.Basis.rounding_mode
                             and type T.Constant.const = Instr.T.Constant.const
                             and type ('s,'r,'f,'c) T.Extension.ccx = ('s,'r,'f,'c) Instr.T.Extension.ccx
                             and type ('s,'r,'f,'c) T.Extension.fx = ('s,'r,'f,'c) Instr.T.Extension.fx
                             and type ('s,'r,'f,'c) T.Extension.rx = ('s,'r,'f,'c) Instr.T.Extension.rx
                             and type ('s,'r,'f,'c) T.Extension.sx = ('s,'r,'f,'c) Instr.T.Extension.sx
                             and type T.I.div_rounding_mode = Instr.T.I.div_rounding_mode
                             and type T.Region.region = Instr.T.Region.region
                             and type T.ccexp = Instr.T.ccexp
                             and type T.fexp = Instr.T.fexp
                             (* and type T.labexp = Instr.T.labexp *)
                             and type T.mlrisc = Instr.T.mlrisc
                             and type T.oper = Instr.T.oper
                             and type T.rep = Instr.T.rep
                             and type T.rexp = Instr.T.rexp
                             and type T.stm = Instr.T.stm
    structure MLTreeEval : MLTREE_EVAL (* where T = Instr.T *)
                           where type T.Basis.cond = Instr.T.Basis.cond
                             and type T.Basis.div_rounding_mode = Instr.T.Basis.div_rounding_mode
                             and type T.Basis.ext = Instr.T.Basis.ext
                             and type T.Basis.fcond = Instr.T.Basis.fcond
                             and type T.Basis.rounding_mode = Instr.T.Basis.rounding_mode
                             and type T.Constant.const = Instr.T.Constant.const
                             and type ('s,'r,'f,'c) T.Extension.ccx = ('s,'r,'f,'c) Instr.T.Extension.ccx
                             and type ('s,'r,'f,'c) T.Extension.fx = ('s,'r,'f,'c) Instr.T.Extension.fx
                             and type ('s,'r,'f,'c) T.Extension.rx = ('s,'r,'f,'c) Instr.T.Extension.rx
                             and type ('s,'r,'f,'c) T.Extension.sx = ('s,'r,'f,'c) Instr.T.Extension.sx
                             and type T.I.div_rounding_mode = Instr.T.I.div_rounding_mode
                             and type T.Region.region = Instr.T.Region.region
                             and type T.ccexp = Instr.T.ccexp
                             and type T.fexp = Instr.T.fexp
                             (* and type T.labexp = Instr.T.labexp *)
                             and type T.mlrisc = Instr.T.mlrisc
                             and type T.oper = Instr.T.oper
                             and type T.rep = Instr.T.rep
                             and type T.rexp = Instr.T.rexp
                             and type T.stm = Instr.T.stm
  ) : AMD64INSN_PROPERTIES =
  struct

    structure I = Instr
    structure C = I.C
    structure T = I.T 
    structure CB = CellsBasis

    exception NegateConditional

    fun error msg = MLRiscErrorMsg.error("AMD64Props",msg)

    datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL 
                  | IK_CALL_WITH_CUTS | IK_PHI | IK_SOURCE | IK_SINK
    datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

    fun instrKind (I.ANNOTATION {i, ...}) = instrKind i
      | instrKind (I.COPY _) = IK_COPY
      | instrKind (I.INSTR instr) = (case instr
	of I.NOP => IK_NOP
	 | ( I.CALL {cutsTo=_::_, ...} ) =>
	   IK_CALL_WITH_CUTS
	 | ( I.CALLQ {cutsTo=_::_, ...} ) =>
	   IK_CALL_WITH_CUTS
	 | ( I.JMP _ ) => IK_JUMP
	 | ( I.JCC _ ) => IK_JUMP
	 | ( I.RET _ ) => IK_JUMP
	 | ( I.INTO) => IK_JUMP
	 | ( I.CALL _ ) => IK_CALL
	 | ( I.CALLQ _ ) => IK_CALL
	 | I.PHI {} => IK_PHI
	 | I.SOURCE {} => IK_SOURCE
	 | I.SINK {} => IK_SINK
	 | _ => IK_INSTR
	(* end case *))
      | instrKind _ = IK_INSTR

    fun moveInstr (I.ANNOTATION {i, ...}) = moveInstr i
      | moveInstr (I.COPY _) = true
      | moveInstr _ = false

    fun moveTmpR (I.ANNOTATION {i, ...}) = moveTmpR i
      | moveTmpR ( I.COPY {k=CB.GP, tmp=SOME (I.Direct (_, r)), ...} ) = 
	SOME r
      | moveTmpR ( I.COPY {k=CB.FP, tmp=SOME (I.FDirect r), ...} ) = 
	SOME r
      | moveTmpR _ = NONE

    fun moveDstSrc (I.ANNOTATION {i, ...}) = moveDstSrc i
      | moveDstSrc (I.COPY {dst, src, ...}) = (dst, src)
      | moveDstSrc _ = error "AMD64Props.moveDstSrc"

    fun nop () = I.nop

    fun jump l = I.jmp (I.ImmedLabel (T.LABEL l), [l])

    fun branchTargets(I.ANNOTATION{i,...}) = branchTargets i
      | branchTargets(I.INSTR i) =  (case i
        of I.JMP(_, []) => [ESCAPES]
	 | I.JMP(_, labs) => List.map LABELLED labs
	 | I.RET _ => [ESCAPES]
	 | I.JCC{opnd=I.ImmedLabel(T.LABEL(lab)), ...} => 
	     [FALLTHROUGH, LABELLED lab]
	 | I.CALL{cutsTo, ...} => FALLTHROUGH :: List.map LABELLED cutsTo
	 | I.CALLQ{cutsTo, ...} => FALLTHROUGH :: List.map LABELLED cutsTo
	 | I.INTO => [ESCAPES]
	 |  _ => error "branchTargets")
      | branchTargets _ = error "branchTargets"

    fun jump label = I.jmp (I.ImmedLabel(T.LABEL label), [label])

    fun setJumpTarget(I.ANNOTATION {a,i}, l) = 
	I.ANNOTATION {a=a, i=setJumpTarget (i, l)}
      | setJumpTarget(I.INSTR (I.JMP (I.ImmedLabel _, _)), lab) = jump lab
      | setJumpTarget _ = error "setJumpTarget"

    fun setBranchTargets{i=I.ANNOTATION{a,i}, t, f} = 
        I.ANNOTATION{a=a, i=setBranchTargets{i=i, t=t, f=f}}
      | setBranchTargets{i=I.INSTR(I.JCC{cond,opnd=I.ImmedLabel _}), t, ...} = 
        I.jcc{cond=cond,opnd=I.ImmedLabel(T.LABEL t)}
      | setBranchTargets _ = error "setBranchTargets"

    val immedRange={lo= ~1073741824, hi=1073741823}
    val toInt32 = Int32.fromLarge o Int.toLarge
    (* immediates are restricted to 32 bits *)
    fun loadImmed {immed, t} =
        I.move {mvOp=I.MOVL, src=I.Immed (toInt32 immed), dst=I.Direct (32, t)}
    fun loadOperand {opn, t} = I.move {mvOp=I.MOVQ, src=opn, dst=I.Direct (64, t)}

    fun hashOpn(I.Immed i) = Word.fromInt(Int32.toInt i)
      | hashOpn(I.Immed64 i) = Word.fromInt(Int64.toInt i)
      | hashOpn(I.ImmedLabel le) = MLTreeHash.hash le + 0w123
      | hashOpn(I.Relative i) = Word.fromInt i + 0w1232
      | hashOpn(I.LabelEA le) = MLTreeHash.hash le + 0w44444
      | hashOpn(I.Direct (_, r))  = CB.hashCell r
      | hashOpn(I.FDirect f) = CB.hashCell f + 0w31245
      | hashOpn(I.Displace {base, disp, ...}) = 
        hashOpn disp + CB.hashCell base
      | hashOpn(I.Indexed {base, index, scale, disp, ...}) =
        CB.hashCell index + Word.fromInt scale + hashOpn disp
    fun eqOpn(I.Immed a,I.Immed b) = a = b
      | eqOpn(I.Immed64 a,I.Immed64 b) = a = b
      | eqOpn(I.ImmedLabel a,I.ImmedLabel b) = MLTreeEval.==(a,b)
      | eqOpn(I.Relative a,I.Relative b) = a = b
      | eqOpn(I.LabelEA a,I.LabelEA b) = MLTreeEval.==(a,b)
      | eqOpn(I.Direct (_,a),I.Direct (_,b)) = CB.sameColor(a,b)
      | eqOpn(I.FDirect a,I.FDirect b) = CB.sameColor(a,b)
      | eqOpn(I.Displace{base=a,disp=b,...},I.Displace{base=c,disp=d,...}) =
        CB.sameColor(a,c) andalso eqOpn(b,d)
      | eqOpn(I.Indexed{base=a,index=b,scale=c,disp=d,...},
          I.Indexed{base=e,index=f,scale=g,disp=h,...}) =
        CB.sameColor(b,f) andalso c = g
         andalso sameCellOption(a,e) andalso eqOpn(d,h)
      | eqOpn _ = false
    and sameCellOption(NONE, NONE) = true
      | sameCellOption(SOME x, SOME y) = CB.sameColor(x,y)
      | sameCellOption _ = false

    fun negateConditional (I.ANNOTATION{i,a}, lab) =
	I.ANNOTATION{i=negateConditional(i,lab), a=a}
      | negateConditional (I.INSTR(I.JCC{cond,
	          opnd=I.ImmedLabel(T.LABEL _)}), lab) = let
	val cond' = (case cond
	       of I.EQ => I.NE
		| I.NE => I.EQ
		| I.LT => I.GE
		| I.LE => I.GT
		| I.GT => I.LE
		| I.GE => I.LT
		| I.B => I.AE
		| I.BE => I.A
		| I.A => I.BE
		| I.AE => I.B
		| I.C => I.NC
		| I.NC => I.C
		| I.P => I.NP
		| I.NP => I.P
		| I.O => I.NO
		| I.NO => I.O
	      (* end case *))
	in
	  I.INSTR(I.JCC{cond=cond', opnd=I.ImmedLabel(T.LABEL lab)})
	end
    | negateConditional _ = error "AMD64Props.negateConditional"

    val raxPair = [C.rdx, C.rax]

    fun defUseR i = let
	fun operandAcc (opnd, acc) = (case opnd
	    of I.Direct (_, r) => r :: acc
	     | I.Displace {base, ...} => base :: acc
	     | I.Indexed {base=NONE, index, ...} => index :: acc
	     | I.Indexed {base=SOME b, index, ...} => b :: index :: acc
	     | _ => acc
	    (* end case *))
	fun operandUse opnd = operandAcc (opnd, [])
	fun operandDef (I.Direct (_, r)) = [r]
	  | operandDef _ = []
	fun cmpTest {lsrc, rsrc} = ([], operandAcc (lsrc, operandUse rsrc))
	fun unary opnd = (operandDef opnd, operandUse opnd)
	fun multDiv {src, multDivOp} = let
	    val uses = operandUse src
	    in
	      case multDivOp
	       of (I.IDIVL1) => 
	           (raxPair, C.rdx::C.rax::uses)
                | (I.DIVL1) => 
	           (raxPair, C.rdx::C.rax::uses)
                | (I.IDIVQ1) => 
	           (raxPair, C.rdx::C.rax::uses)
                | (I.DIVQ1) => 
	           (raxPair, C.rdx::C.rax::uses)
	        | (I.IMULL1) => 
	          (raxPair, C.rax::uses)
	        | (I.MULL1) => 
	          (raxPair, C.rax::uses)
	        | (I.IMULQ1) => 
	          (raxPair, C.rax::uses)
	        | (I.MULQ1) => 
	          (raxPair, C.rax::uses)
	      (* end case *)
	    end
	fun rspOnly () = let val s = [C.stackptrR] in (s, s) end
	fun push opnd = ([C.stackptrR], operandAcc (opnd, [C.stackptrR]))
	fun f i = (case i
	    of ( I.JMP (opnd, _) ) => ([], 	operandUse opnd)
             | ( I.JCC {opnd, ...} ) => ([], 	operandUse opnd)
	     | ( I.CALL {opnd, defs, uses, ...} )=>
 	       (C.getReg defs, operandAcc (opnd, C.getReg uses))
	     | ( I.CALLQ {opnd, defs, uses, ...} )=>
 	       (C.getReg defs, operandAcc (opnd, C.getReg uses))
	     | I.MOVE {src, dst=I.Direct (_, r), ...} => ([r], operandUse src) 
	     | I.MOVE {src, dst, ...} => ([], operandAcc (dst, operandUse src))
	     | ( I.LEAL {r32=r, addr} ) =>
	       ([r], operandUse addr)
	     | ( I.LEAQ {r64=r, addr} ) =>
	       ([r], operandUse addr)
	     | ( I.CMPQ arg )  =>
	       cmpTest arg
	     | ( I.CMPL arg )  =>
	       cmpTest arg
	     | ( I.CMPW arg )  =>
	       cmpTest arg
	     | ( I.CMPB arg )  =>
	       cmpTest arg
	     | ( I.TESTQ arg )  =>
	       cmpTest arg
	     | ( I.TESTL arg )  =>
	       cmpTest arg
	     | ( I.TESTW arg )  =>
	       cmpTest arg
	     | ( I.TESTB arg )  =>
	       cmpTest arg
	     | I.BITOP{lsrc, rsrc, ...} => cmpTest {lsrc=lsrc,rsrc=rsrc}
	     | I.BINARY{binOp=I.XORL, src=I.Direct (_,rs),
	                dst=I.Direct (_,rd),...} =>   
	       if CB.sameColor(rs,rd) then ([rd],[]) else ([rd],[rs,rd])
	     | I.BINARY{binOp=I.XORQ, src=I.Direct (_,rs),
	                dst=I.Direct (_,rd),...} =>   
	       if CB.sameColor(rs,rd) then ([rd],[]) else ([rd],[rs,rd])
	     | I.BINARY {src, dst,...} =>   
	       (operandDef dst, operandAcc (src, operandUse dst))
	     | I.SHIFT {src,dst,count,...} =>   
	       (operandDef dst, 
                operandAcc(count, operandAcc (src, operandUse dst)))
	     | I.XADD {src, dst, ...} =>
	       (operandAcc (src, operandDef dst), operandAcc (src, operandUse dst))
	     | I.CMPXCHG {src, dst, ...} =>
	       (C.rax::operandDef dst, C.rax::operandAcc (src, operandUse dst))
	     | I.XCHG {src, dst, ...} =>
	       (operandDef dst, operandAcc (src, operandUse dst)) 
	     | ( I.ENTER _ ) => ([C.rsp, C.rbp], [C.rsp, C.rbp])
	     | ( I.LEAVE ) => ([C.rsp, C.rbp], [C.rsp, C.rbp])
	     | I.MULTDIV arg => multDiv arg
	     | ( I.MUL3  {src1, dst, ...} ) => 
	       ([dst], operandUse src1)
	     | ( I.MULQ3 {src1, dst, ...} ) => 
	       ([dst], operandUse src1)
	     | ( I.UNARY{opnd, ...} ) => unary opnd
	     | ( I.SET {opnd, ...} ) => unary opnd
	     | ( I.PUSHQ arg ) => push arg
	     | ( I.PUSHL arg ) => push arg
	     | ( I.PUSHW arg ) => push arg
	     | ( I.PUSHB arg ) => push arg
	     | I.POP arg => (C.stackptrR::operandDef arg, [C.stackptrR])
	     | ( I.POPFD )=> rspOnly ()
	     | ( I.POPFD )=> rspOnly ()
	     | I.CDQ => ([C.rdx], [C.rax])
	     | I.FMOVE {dst, src, ...} => ([], operandAcc (dst, operandUse src))
	     | I.FCOM {src, ...} => ([], operandUse src)
	     | I.FBINOP {src, ...} => ([], operandUse src)
	     | I.SAHF		      => ([], [C.rax])
	     | I.LAHF		      => ([C.rax], [])
	     (* This sets the low order byte, 
	      * do potentially it may define *and* use 
	      *)
	     | I.CMOV {src, dst,...} => ([dst], operandAcc(src, [dst]))
	     | _ => ([], [])
	    (* end case *))
	in
	  case i
	   of I.ANNOTATION {i, ...} => defUseR i
	    | I.LIVE {regs, ...} => ([], C.getReg regs)
	    | I.KILL {regs, ...} => (C.getReg regs, [])
	    | I.COPY {k=CB.GP, src, dst, tmp, ...} => (case tmp
	      of SOME (I.Direct (_, t)) => (t :: dst, src)
	       | SOME ea => (dst, operandAcc (ea, src))
	       | NONE => (dst, src)
	      (* end case *))
	    | I.COPY _ => ([], [])
	    | I.INSTR i => f i
	  (* end case *)
	end (* defUseR *)

    fun defUseF i = let
        fun operandAcc (I.FDirect r, acc) = r :: acc
          | operandAcc (_, acc) = acc
        fun operand opnd = operandAcc (opnd, [])
        fun f i = (case i
            of I.FMOVE {dst, src, ...} => (operand dst, operand src)
             | I.FBINOP {dst, src, ...} => ([dst], dst :: operand src)
             | I.FCOM {dst, src, ...} => ([], operandAcc (src, [dst]))
             | ( I.FSQRTS {dst, src} )=> 
               (operand dst, operand src)
             | ( I.FSQRTD {dst, src} )=> 
               (operand dst, operand src)
             | ( I.CALL {defs, uses, ...} ) =>
               (C.getFreg defs, C.getFreg uses)
             | ( I.CALLQ {defs, uses, ...} ) =>
               (C.getFreg defs, C.getFreg uses)
             | _ => ([], [])
            (* end case *))
        in
          case i
	   of I.ANNOTATION {i, ...} => defUseF i
	    | I.LIVE {regs, ...} => ([], C.getFreg regs)
	    | I.KILL {regs, ...} => (C.getFreg regs, [])
	    | I.COPY {k=CB.FP, src, dst, tmp, ...} => (case tmp
	      of SOME (I.FDirect t) => (t :: dst, src)
	       | NONE => (dst, src)
	      (* end case *))
	    | I.COPY _ => ([], [])
	    | I.INSTR i => f i
	  (* end case *)
        end (* defUseF *)

    fun defUse CB.GP = defUseR
      | defUse CB.FP = defUseF
      | defUse _ = error "defUse"

    fun getAnnotations (I.ANNOTATION {i, a}) = let 
	val (i, an) = getAnnotations i 
	in 
	  (i, a::an) 
	end
      | getAnnotations i = (i,[])

    fun annotate (i, a) = I.ANNOTATION {i=i, a=a}

    fun szToInt I.I8 = 8
      | szToInt I.I16 = 16
      | szToInt I.I32 = 32
      | szToInt I.I64 = 64

    fun replicate(I.ANNOTATION{i,a}) = I.ANNOTATION{i=replicate i,a=a}
(*    | replicate(I.COPY{tmp=SOME _, dst, src}) =  
        I.COPY{tmp=SOME(I.Direct(C.newReg())), dst=dst, src=src}
      | replicate(I.FCOPY{tmp=SOME _, dst, src}) = 
        I.FCOPY{tmp=SOME(I.FDirect(C.newFreg())), dst=dst, src=src}  *)
      | replicate i = i

    (* determine the bit width of an instruction *)
    fun szOfInstr instr = (case instr
	of I.JCC _ => 32
	 (* NOTE: CMOV encodes operand length in its operands! *)
	 | I.CMOV {src=I.Direct (sz, _), ...} => sz
	 | I.MOVE {mvOp, ...} => 
	   (case mvOp
	     of ( I.MOVQ ) => 64
              | ( I.MOVSWQ ) => 64
              | ( I.MOVZWQ ) => 64
              | ( I.MOVSBQ ) => 64
              | ( I.MOVZBQ ) => 64
              | ( I.MOVSLQ ) => 64
              | ( I.CVTSD2SIQ ) => 64
              | ( I.CVTSS2SIQ ) => 64
	      | ( I.MOVL ) => 32
              | ( I.MOVSWL ) => 32
              | ( I.MOVZWL ) => 32 
              | ( I.MOVSBL ) => 32
              | ( I.CVTSD2SI ) => 32
              | ( I.CVTSS2SI ) => 32
              | ( I.MOVZBL ) => 32
	      | I.MOVW => 16
	      | I.MOVB => 8
	   (* esac *))
	 | ( I.CALL _ ) => 32
         | ( I.LEAL _ ) => 32
         | ( I.CMPL _ ) => 32
         | ( I.TESTL _ ) => 32
         | ( I.MUL3 _ ) => 32
	 | ( I.CALLQ _ ) => 64 
         | ( I.LEAQ _ ) => 64 
         | ( I.CMPQ _ ) => 64 
         | ( I.TESTQ _ ) => 64 
         | ( I.MULQ3 _ ) => 64 
         | ( I.CMOV _ ) => 64 
	 | ( I.CMPW _ ) => 16
	 | ( I.TESTW _ ) => 16
	 | ( I.CMPB _ ) => 8
	 | ( I.TESTB _ ) => 8
	 | I.SHIFT {shiftOp, ...} => (case shiftOp
	   of ( I.SHLDL ) => 32
            | ( I.SHRDL ) => 32
	   (* esac *))
	 | I.UNARY {unOp, ...} =>
	   (case unOp
	     of ( I.DECQ ) => 64
              | ( I.INCQ ) => 64
              | ( I.NEGQ ) => 64
              | ( I.NOTQ ) => 64
              | ( I.LOCK_DECQ ) => 64
              | ( I.LOCK_INCQ ) => 64
              | ( I.LOCK_NEGQ ) => 64
              | ( I.LOCK_NOTQ ) => 64
	      | ( I.DECL ) => 32
              | ( I.INCL ) => 32
              | ( I.NEGL ) => 32
              | ( I.NOTL ) => 32
	      | ( I.DECW ) => 16
              | ( I.INCW ) => 16
              | ( I.NEGW ) => 16
              | ( I.NOTW ) => 16
	      | ( I.DECB ) => 8
              | ( I.INCB ) => 8
              | ( I.NEGB ) => 8
              | ( I.NOTB ) => 8
	   (* esac *))
	 | I.MULTDIV {multDivOp, ...} => 
	   (case multDivOp
	     of ( I.IMULL1 ) => 32
              | ( I.MULL1 ) => 32
              | ( I.IDIVL1 ) => 32
              | ( I.DIVL1 ) => 32
	      | ( I.IMULQ1 ) => 64 
              | ( I.MULQ1 ) => 64
              | ( I.IDIVQ1 ) => 64
              | ( I.DIVQ1 ) => 64
	   (* esac *))
	 | I.BINARY {binOp, ...} => 
	   (case binOp
	     of ( I.ADDQ ) => 64
              | ( I.SUBQ ) => 64
              | ( I.ANDQ ) => 64
              | ( I.ORQ ) => 64
              | ( I.XORQ ) => 64
              | ( I.SHLQ ) => 64
              | ( I.SARQ ) => 64
              | ( I.SHRQ ) => 64
              | ( I.MULQ ) => 64
              | ( I.IMULQ ) => 64
              | ( I.ADCQ ) => 64
              | ( I.SBBQ ) => 64
	      | ( I.ADDL ) => 32
              | ( I.SUBL ) => 32 
              | ( I.ANDL ) => 32 
              | ( I.ORL ) => 32 
              | ( I.XORL ) => 32 
              | ( I.SHLL ) => 32 
              | ( I.SARL ) => 32 
              | ( I.SHRL ) => 32 
              | ( I.MULL ) => 32 
              | ( I.IMULL ) => 32 
              | ( I.ADCL ) => 32 
              | ( I.SBBL ) => 32 
              | ( I.BTSL ) => 32 
              | ( I.BTCL ) => 32 
              | ( I.BTRL ) => 32 
              | ( I.ROLL ) => 32 
              | ( I.RORL ) => 32 
              | ( I.XCHGL ) => 32
	      | ( I.ADDW ) => 16
              | ( I.SUBW ) => 16
              | ( I.ANDW ) => 16
              | ( I.ORW ) => 16
              | ( I.XORW ) => 16
              | ( I.SHLW ) => 16
              | ( I.SARW ) => 16
              | ( I.SHRW ) => 16
              | ( I.MULW ) => 16
              | ( I.IMULW ) => 16
              | ( I.BTSW ) => 16
              | ( I.BTCW ) => 16
              | ( I.BTRW ) => 16
              | ( I.ROLW ) => 16
              | ( I.RORW ) => 16
              | ( I.XCHGW ) => 16
	      | ( I.ADDB ) => 8
              | ( I.SUBB ) => 8
              | ( I.ANDB ) => 8
              | ( I.ORB ) => 8
              | ( I.XORB ) => 8
              | ( I.SHLB ) => 8
              | ( I.SARB ) => 8
              | ( I.SHRB ) => 8
              | ( I.MULB ) => 8
              | ( I.IMULB ) => 8
              | ( I.XCHGB ) => 8
	      | _ => raise Fail "" (* 64*)
	   (* esac *))
	 | I.XADD {sz, ...} => szToInt sz
	 | I.CMPXCHG {sz, ...} => szToInt sz
	 | I.PAUSE => 64
	 | (I.MFENCE) => 64
	 | (I.SFENCE) => 64
	 | (I.LFENCE) => 64
	 | _ => raise Fail "" (*64*)
      (* esac *))

    fun szOfFinstr instr = (case instr
        of I.FMOVE {fmvOp, ...} => (case fmvOp
           of ( I.MOVSS ) => 32
            | ( I.CVTSS2SD ) => 32
            | ( I.CVTSI2SS ) => 32
            | ( I.CVTSI2SSQ ) => 32
            | ( I.MOVSD ) => 64
            | ( I.CVTSD2SS ) => 64
            | ( I.CVTSI2SD ) => 64
            | ( I.CVTSI2SDQ ) => 64
           (* end case *))
         | I.FCOM {comOp, ...} => (case comOp
           of ( I.COMISS ) => 32
            | ( I.UCOMISS ) => 32
            | ( I.COMISD ) => 64
            | ( I.UCOMISD ) => 64
           (* end case *))
         | I.FBINOP {binOp, ...} => (case binOp
           of ( I.ADDSS ) => 32
            | ( I.SUBSS ) => 32
            | ( I.MULSS ) => 32
            | ( I.DIVSS ) => 32
            | ( I.XORPS ) => 32
            | ( I.ANDPS ) => 32
            | ( I.ORPS ) => 32
            | ( I.ADDSD ) => 64
            | ( I.SUBSD ) => 64
            | ( I.MULSD ) => 64
            | ( I.DIVSD ) => 64
            | ( I.XORPD ) => 64
            | ( I.ANDPD ) => 64
            | ( I.ORPD ) => 64
           (* end case *))
         | I.FSQRTS _ => 32
         | I.FSQRTD _ => 64
        (* end case *))

  end (* AMD64Props *)
