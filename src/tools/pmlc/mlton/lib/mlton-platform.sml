(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonPlatform =
   struct
      structure Arch =
         struct
            datatype t =
               Alpha
             | AMD64
             | ARM
             | HPPA
             | IA64
             | m68k
             | MIPS
             | PowerPC
             | PowerPC64
             | S390
             | Sparc
             | X86

            val host: t = X86
            val hostIsBigEndian = false;

	    fun fromString s = (case CharVector.map Char.toLower s
		   of "x86" => SOME X86
		    | _ => NONE
		  (* end case *))

	    fun toString X86 = "X86"
	      | toString _ = "<unknown arch>"

         end

      structure Format =
         struct
            datatype t =
               Archive
             | Executable
             | LibArchive
             | Library

            val host: t = Executable
         end

      structure OS =
         struct
            datatype t =
               AIX
             | Cygwin
             | Darwin
             | FreeBSD
             | Hurd
             | HPUX
             | Linux
             | MinGW
             | NetBSD
             | OpenBSD
             | Solaris

            val host: t = case SMLofNJ.SysInfo.getOSName() of
                  "Darwin" => Darwin
                | "Linux" => Linux
                | _ => raise Fail "strange MLton_Platform_OS_host"

	    fun fromString s = (case CharVector.map Char.toLower s
		   of "darwin" => SOME Darwin
		    | "linux" => SOME Linux
		    | _ => NONE
		  (* end case *))

	    fun toString Darwin = "Darwin"
	      | toString Linux = "Linux"
	      | toString _ = "<unknown os>"

            val forkIsEnabled = true

            val useWindowsProcess = not forkIsEnabled
         end
   end
