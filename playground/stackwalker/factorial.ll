declare void @stackHelper()
declare i32 @printf(i8*, ...)
declare token @llvm.experimental.gc.statepoint.p0f_isVoidf(i64, i32, void ()*, i32, i32, ...)

@.str = private unnamed_addr constant [5 x i8] c"%ld\0A\00", align 1

define i32 @main() {
	%val = bitcast i64 7 to i64
	%counter = bitcast i32 0 to i32
	%ret = call i64 @fact(i64 %val, i32 %counter)
	%formatString = getelementptr inbounds [5 x i8], [5 x i8]* @.str, i32 0, i32 0
	%ignored = call i32 (i8*, ...) @printf(i8* %formatString, i64 %ret)
	ret i32 0
}

define i64 @fact(i64 %n, i32 %counter) gc "statepoint-example" {
entry:
	%check = icmp sle i64 %n, 1
	br i1 %check, label %c1, label %c2

c1:
	ret i64 %n

c2: 
	%n1 = sub i64 %n, 1
	%countercheck = icmp sle i32 %counter, 3 
	br i1 %countercheck, label %c3, label %c4	

c3:
	%newcounter = add i32 1, %counter
	%fibval = call i64 @fact(i64 %n1, i32 %newcounter)
	%ret = mul i64 %n, %fibval
	ret i64 %ret

c4:
	call token(i64, i32, void ()*, i32, i32, ...) @llvm.experimental.gc.statepoint.p0f_isVoidf (i64 12345, i32 0, void()* @stackHelper, i32 0, i32 0, i64 0, i64 0)
	br label %c3
}
