declare void @foo(i32)
declare token @llvm.experimental.gc.statepoint.p0f_isVoidi32f(i64, i32, void (i32)*, i32, i32, ...)


define i8 addrspace(1)* @test1(i8 addrspace(1)* %obj)
       gc "statepoint-example" {
  call token(i64, i32, void (i32)*, i32, i32, ...) @llvm.experimental.gc.statepoint.p0f_isVoidi32f(i64 2882400000, i32 0, void (i32)* @foo, i32 1, i32 0, i32 111, i32 0, i32 0)
  ret i8 addrspace(1)* %obj
}
