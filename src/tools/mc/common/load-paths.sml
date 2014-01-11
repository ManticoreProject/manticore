(* load-paths.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * src/tools/mc/common/load-paths.sml.  Generated from load-paths_sml.in by configure.
 *
 * FIXME: This code should support different locations for the basis; e.g., the
 * source tree or an installation location.
 *)

structure LoadPaths =
  struct

  (* Manticore basis library *)
    val basisDir = "/Users/msl1234/manticore/manticore/branches/mutable-state/src/lib/basis"

    local
      fun join (dir, file) = OS.Path.joinDirFile {dir=dir, file=file}
    in

  (* Manticore basis CPP definition directory *)
    val basisCPPDefDir = join (basisDir, "include")

  (* sequential basis *)
    val sequentialBasisDir = join (basisDir, "sequential")

  (* runtime basis *)
    val runtimeBasisDir = join (basisDir, "runtime")

  (* runtime basis library *)
    val runtimeBasisLib = join (runtimeBasisDir, "runtime.mlb")

  (* scheduling *)
    val schedsDir = join (basisDir, "scheduler")

  (* scheduling library *)
    val schedsLib = join (schedsDir, "scheduler.mlb")

  (* top-level scheduler library *)
    fun topLevelSchedLib sched = join (schedsDir, OS.Path.joinBaseExt{base=sched, ext=SOME "mlb"})

  (* default thread scheduler library *)
    val defImplicitThreadSchedLib = join (schedsDir, "def-implicit-thread-sched.mlb")

  (* implicit threading *)
    val implicitThreadingDir = join (basisDir, "implicit-thread")

  (* implicit-threading library *)
    val implicitThreadingLib = join (implicitThreadingDir, "implicit-thread.mlb")

  (* CML *)
    val cmlDir = join (basisDir, "cml")

  (* CML library *)
    val cmlLib = join (cmlDir, "cml.mlb")

  (* parallel array *)
    val parrayDir = join (basisDir, "parray")

  (* parallel array library *)
    val parrayLib = join (parrayDir, "parray.mlb")

  (* Mutable state directory *)
    val mutStateDir = join(basisDir, "mutable-state")

  (* mutable state library *)
    val mutState = join (mutStateDir, "mut-state.mlb")

    end (* local *)
  end
