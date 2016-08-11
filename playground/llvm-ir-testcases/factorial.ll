target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.11.0"

declare i32 @printf(i8*, ...)
@.str = private unnamed_addr constant [5 x i8] c"%ld\0A\00", align 1

declare i64 addrspace(1)* @get_ref_space1() "gc-leaf-function"="true"

declare i64 @llvm.experimental.gc.result.i64(token)
declare i64 addrspace(1)*
  @llvm.experimental.gc.relocate.p1i64(token %statepoint_token,
                                 i32 %base_offset,
                                 i32 %pointer_offset)
                                 
declare token @llvm.experimental.gc.statepoint.p0f_i64p1i64p1i64f(i64, i32, i64 (i64 addrspace(1)*, i64 addrspace(1)*)*, i32, i32, ...)

define i64 @main() gc "statepoint-example" {
	%val = bitcast i64 4 to i64
    %dummyAllocPtr = call i64 addrspace(1)* @get_ref_space1()
    %dummyN = call i64 addrspace(1)* @get_ref_space1()
        
    %fact_ret_token = call token 
    (i64, i32, i64 (i64 addrspace(1)*, i64 addrspace(1)*)*, i32, i32, ...) 
    @llvm.experimental.gc.statepoint.p0f_i64p1i64p1i64f(i64 2882400001, i32 0, 
        ;; id, patch bytes
         i64 (i64 addrspace(1)*, i64 addrspace(1)*)* @fact, ;; fn
         i32 2, ; num args to fn
         i32 0, ; "flags"
         i64 addrspace(1)* %dummyAllocPtr, ; arg1
         i64 addrspace(1)* %dummyN, ; arg2
         i32 0, i32 0 ; num transition args, num deopt args
         ; from here, we list all live heap pointers that are live after the call
         ; since there are none we don't list any, but we still use a statepoint
         ; call so that a stackmap entry is emitted (need to know how big the frame is
         ; at a minimum while scanning the stack).
         )
         
    %ret = call i64 @llvm.experimental.gc.result.i64(token %fact_ret_token)
	
    ret i64 %ret
}

define i64 @fact(i64 addrspace(1)* %allocPtr, i64 addrspace(1)* %boxedN) gc "statepoint-example" {
entry:
    %loadN = load i64, i64 addrspace(1)* %boxedN
	%check = icmp sle i64 %loadN, 1
    br i1 %check, label %return, label %recurse
	; br i1 %check, label %c1, label %c2

; c1:
;     %val.1 = phi i64 [%loadN, %entry], [%val.2, %c1]
;     %val.2 = mul i64 %val.1, 11
;     %cond = icmp sge i64 %val.2, 123141
;     br i1 %cond, label %done, label %c1
;     
; done:
; 	ret i64 %val.2

return:
    ret i64 1

recurse: 
	%n1 = sub i64 %loadN, 1
    %newSlot = bitcast i64 addrspace(1)* %allocPtr to i64 addrspace(1)*
    store i64 %n1, i64 addrspace(1)* %newSlot
    
    %newAllocPtr = getelementptr i64, i64 addrspace(1)* %allocPtr, i32 1
	
    
    %fact_ret_token = call token 
    (i64, i32, i64 (i64 addrspace(1)*, i64 addrspace(1)*)*, i32, i32, ...) 
    @llvm.experimental.gc.statepoint.p0f_i64p1i64p1i64f(i64 2882400002, i32 0, 
        ;; id, patch bytes
         i64 (i64 addrspace(1)*, i64 addrspace(1)*)* @fact, ;; fn
         i32 2, ; num args to fn
         i32 0, ; "flags"
         i64 addrspace(1)* %newAllocPtr, ; arg1
         i64 addrspace(1)* %newSlot, ; arg2
         i32 0, i32 0,
         ; num transition args, num deopt args
         ; from here, we list all live heap pointers that are live after the call
         ; that may change during the call
         i64 addrspace(1)* %boxedN
         )
         ; the index is the argument number as a whole, so 9 instead of 0
    %currentBoxedN = call i64 addrspace(1)* @llvm.experimental.gc.relocate.p1i64(
        token %fact_ret_token, i32 9, i32 9) 
    
    ;%fibval = call i64 @fact(i64 addrspace(1)* %newAllocPtr, i64 addrspace(1)* %newSlot)
    
    %fibval = call i64 @llvm.experimental.gc.result.i64(token %fact_ret_token)
    
    
    %loadN2 = load i64, i64 addrspace(1)* %currentBoxedN
	%ret = mul i64 %loadN2, %fibval

	ret i64 %ret
}
