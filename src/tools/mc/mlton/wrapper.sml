structure Wrapper =
struct
    structure Compile = Compile()
    structure Main = Main()

    (* Initialize MLton constants with default values*)
    val _ = Main.init ()
                        
    fun makeFileDummy f = fn () =>
        {file = f,
         print = fn s:string => (),
         done = fn () => ()}

    fun compileSML (srcFile : string, asmFile : string) =
        Compile.compileSML {input=[srcFile],
                            outputC=makeFileDummy "foo.c",
                            outputS=makeFileDummy asmFile}
    fun compileMLB (srcFile : string, asmFile : string) =
        Compile.compileMLB {input=srcFile,
                            outputC=makeFileDummy "foo.c",
                            outputS=makeFileDummy asmFile}
end
