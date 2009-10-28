fun f () = Print.printLn "test"
val _ = ImplicitThread.runOnWorkGroup (GlobalBFSScheduler.workGroup (), f)
