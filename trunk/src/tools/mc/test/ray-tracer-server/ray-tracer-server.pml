val nThreads = readint();

val serverCh = channel();
val clientJoinCh = channel();

(* compute the scene for reqSz and send it back to respCh *)
fun computeScene (reqSz, respCh) = let
    val img = ray(reqSz)
    in
       send(respCh, img);
       ()
    end
;

(* handle scene requests *)
fun rayTracerServer () = let
    val (req, respCh) = recv(serverCh)		      
    in
       spawn(computeScene(req, respCh));
       rayTracerServer()
    end
;

(* have the server render a scene, and then output it to a file *)
fun rayTracerClient (imgName, reqSz) = let
    val respCh = channel()
    val _ = send(serverCh, (reqSz, respCh))
    val scene = recv(respCh)
    in
       sceneToFile(reqSz, imgName^".ppm", scene);
       send(clientJoinCh, ());
       ()
    end
;

(* wait for the clients to finish *) 
fun waitForAll (i) = if (i = nThreads)
    then print "waitForAll completed\n"
    else (
	recv(clientJoinCh);
	waitForAll(i+1))
;

val sizes = 1024:: 512:: 256:: 512::nil;

fun spawnClients (i) = if (i = nThreads)
    then ()
    else (
	spawn(rayTracerClient("s"^itos i, nth(sizes, i)));
	spawnClients(i+1))
;
