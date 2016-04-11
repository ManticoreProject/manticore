(* llvm-strings.sml
 * 
 * COPYRIGHT (c) 2016 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Infrastructure to translate string literals into LLVM array initializers
 * and to collect all such literals in order to turn them into valid LLVM decls.
 * We use a rather side-effecty way of doing this, in that lookup will modify interally
 * managed state.
 *)

structure LLVMStrings =
  struct
  
  
  local
    structure LV = LLVMVar
    structure LT = LLVMType

    (* We must escape non-printable characters with “\xx” where “xx” is the two digit hex code for that character. 
    
    Here are some strings in C:
    
    char* one = "hello, world!";
	char* two = "M83 - Junk";
    char* fmt = "%s,\n %s ... %llu";
    
    
    and here they are represented in LLVM:
    
    @.str = private unnamed_addr constant [14 x i8] c"hello, world!\00", align 1
    @.str.1 = private unnamed_addr constant [11 x i8] c"M83 - Junk\00", align 1
    @.str.2 = private unnamed_addr constant [17 x i8] c"%s,\0A %s ... %llu\00", align 1
    
    ....
    
    store i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str, i32 0, i32 0), i8** %one
    store i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str.1, i32 0, i32 0), i8** %two
    
    *)
    
    (* map a string to its LLVM version and also include the length of the array needed
       to represent it. *)
    fun cvt (s : string) : (int * string) = let
    
        fun leftpad s = 
            if String.size s = 1
            then "0" ^ s
            else s
        
        fun xltr c = 
            if Char.isPrint c
            then Char.toString c
            else "\\" ^ ((leftpad o Int.fmt) 16 (Char.ord c))
        
        
    in (
        (* add a null terminator *)
        (String.size s) + 1,
        (String.translate xltr s) ^ "\\00"
       ) 
    end
    
    (* stamp/cache stuff *)
    
    val cache = ref HCM.empty
    val ticker = ref 0

    fun stamp() = (ticker := !ticker + 1 ; !ticker)
    
    (* end stamp/cache stuff *)
    
  in
  
  (* If the literal has not already been seen, we will generate a new LLVMVar.
     TODO actually add that cache so we can support export *)
  fun lookup (s : string) : LV.var = let
    val (size, llStr) = cvt s
    
    val ty = LT.mkArray(LT.cnt size, LT.i8)
    val name = ".str." ^ ((Int.toString o stamp) ())
    
    val var = LV.newWithKind(name LV.VK_Global, ty)
  in
    var
  end
  
  (* purge all strings contained. *)
  fun clear () : unit = raise Fail "not implemented"
  
  (* turn all string literals into a list of LLVM string literal declarations  *)
  fun export () : string list = raise Fail "not implemented"
  
    
  end
  
  end (* LLVMStrings *)
