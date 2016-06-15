declare i32 @printf(i8*, ...)
@.str = private unnamed_addr constant [5 x i8] c"%ld\0A\00", align 1

define i32 @main() {
	%val = bitcast i64 4 to i64
	%ret = call i64 @fact(i64 %val)
	%formatString = getelementptr inbounds [5 x i8], [5 x i8]* @.str, i32 0, i32 0
	%ignored = call i32 (i8*, ...) @printf(i8* %formatString, i64 %ret)
	ret i32 0
}

define i64 @fact(i64 %n) #0 {
entry:
	%check = icmp sle i64 %n, 1
	br i1 %check, label %c1, label %c2

c1:
	ret i64 %n

c2: 
	%n1 = sub i64 %n, 1
	%fibval = call i64 @fact(i64 %n1)
	%ret = mul i64 %n, %fibval

	ret i64 %ret
}

attributes #0 = {"split-stack"="true"}
