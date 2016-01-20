


val _ = EventLogging.logVProcStartIdle () 
val _ = EventLogging.logVProcStart 1234 
val _ = EventLogging.logVProcExit 1234 
val _ = EventLogging.logVProcExitMain () 
val _ = EventLogging.logVProcIdle () 
val _ = EventLogging.logVProcSleep ()

val _ = EventLogging.logVProcWakeup () 

val _ = EventLogging.logPreemptVProc 1234 


val _ = EventLogging.logGCSignal 123456 

val _ = EventLogging.logThdSpawn () 

val _ = EventLogging.logThdSpawnOn () 

val _ = EventLogging.logThdStart 123456 

val _ = EventLogging.logThdExit () 
val _ = EventLogging.logMsgSendOffered () 
val _ = EventLogging.logMsgSendResumed 123456 
val _ = EventLogging.logMsgRecv 123456 
val _ = EventLogging.logMsgRecvOffered () 

val _ = EventLogging.logMsgRecvResumed (123456, 123456) 
val _ = EventLogging.logMsgSend 123456 
val _ = EventLogging.logWSInit () 
val _ = EventLogging.logWSTerminate 123456 
val _ = EventLogging.logWSWorkerInit 123456 
val _ = EventLogging.logWSExecute 123456 
val _ = EventLogging.logWSPreempted 123456 
val _ = EventLogging.logWSThiefSend 123456 
val _ = EventLogging.logWSThiefBegin (123456, 123456) 

val _ = EventLogging.logWSThiefEnd (123456, 123456) 

val _ = EventLogging.logWSThiefSuccessful (123456, 123456) 
val _ = EventLogging.logWSThiefUnsuccessful (123456, 123456) 
val _ = EventLogging.logWSSleep 123456 
val _ = EventLogging.logRopeRebalanceBegin 1234 
val _ = EventLogging.logRopeRebalanceEnd 1234 
