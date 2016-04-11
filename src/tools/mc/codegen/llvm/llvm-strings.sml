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
            
            fun xltr c = 
                if Char.isPrint c
                then Char.toString c
                else "\\" ^ (StringCvt.padLeft #"0" 2 (Int.fmt StringCvt.HEX (Char.ord c)))
            
            
        in (
            (* add a null terminator *)
            (String.size s) + 1,
            (String.translate xltr s) ^ "\\00"
           ) 
        end
        
        (* stamp/cache stuff *)
        structure Map = RedBlackMapFn (struct
                        type ord_key = string
                        val compare = String.compare
                        end)
                        
        (* need type annotation here b/c value restriction *)
        val cache : (LV.var * string) Map.map ref = ref Map.empty

        fun find s = Map.find(!cache, s)
        fun add (s, pair) = cache := (Map.insert(!cache, s, pair))
        (* end stamp/cache stuff *)
    
  in
  
      (* If the literal has not already been seen, we will generate a new LLVMVar. *)
      fun lookup (s : string) : LV.var = (case find s 
          of SOME (llv, _) => llv
           | NONE => let
             val (size, llStr) = cvt s
             
             (* the type of this thing is actually a pointer, but when it comes time to
                write the decl string, we will strip this pointer off. *)
                
             val ty = LT.mkPtr(LT.mkArray(LT.cnt size, LT.i8))
             val name = ".str"
             
             (* we get unique stamps from LLVMVar *)
             val var = LV.newWithKind(name, LV.VK_Global false, ty)
           in
             (add(s, (var, llStr)) ; var)
           end
          (* esac *))
      
      
      
      
      (* purge all strings contained, if you want to do that sort of thing to free memory *)
      fun clear () : unit = cache := Map.empty
      
      (* turn all string literals into a list of LLVM string literal declarations  *)
      fun export () : string list = let            
            fun toDecl (llv, lit) = String.concat [
                LV.toString llv, " = private unnamed_addr constant ",
                (LT.fullNameOf o LT.deref o LV.typeOf) llv, " c\"", lit, "\"\n"
            ]
            
            val mappings = Map.listItems(!cache)
            
          in
            List.map toDecl mappings
          end
      
    
  end
  
  end (* LLVMStrings *)
