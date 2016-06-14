target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.11.0"

declare i32 @printf(i8*, ...)
@.str = private unnamed_addr constant [5 x i8] c"%ld\0A\00", align 1

define i32 @main() {
	%val = bitcast i64 8 to i64
	%ret = call i64 @fib(i64 %val)
	%formatString = getelementptr inbounds [5 x i8], [5 x i8]* @.str, i32 0, i32 0
	%ignored = call i32 (i8*, ...) @printf(i8* %formatString, i64 %ret)
	ret i32 0
}

define i64 @fib(i64 %n) #0 {
entry:
	%check= icmp sle i64 %n, 2
	br i1 %check, label %c1, label %c2

c1:
	ret i64 1

c2: 
	%n1 = add i64 %n, -1
	%n2 = add i64 %n, -2

	%ret1 = call i64 @fib(i64 %n1)
	%ret2 = call i64 @fib(i64 %n2)

	%ret = add i64 %ret1, %ret2

	ret i64 %ret
}

attributes #0 = {"split-stack"="true"}