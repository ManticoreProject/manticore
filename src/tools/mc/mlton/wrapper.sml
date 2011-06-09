structure Wrapper =
struct
    structure Compile = Compile()
    structure Main = Main()

    (* Initialize MLton constants with default values*)
    val _ = Main.commandLine []
                        
    fun makeFileDummy f = fn () =>
        {file = f,
         print = fn s:string => (),
         done = fn () => ()}

    fun compileSML (srcFile : string, asmFile : string) =
        Compile.compileSML {input=[srcFile],
                            outputC=makeFileDummy "foo.c",
                            outputS=makeFileDummy asmFile}
end
