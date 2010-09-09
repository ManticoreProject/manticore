val _ = spawnClients(0);
val _ = spawn(rayTracerServer());
val _ = waitForAll(0);
()
