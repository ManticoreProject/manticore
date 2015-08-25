(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MONOMORPHISE_STRUCTS = 
   sig
      structure Xml: XML
      structure Sxml: SXML_EXNS
      sharing Xml.Atoms = Sxml.Atoms
      sharing Xml.CoreBOM = Sxml.CoreBOM (* [PML] *)
      sharing Xml.PrimConDef = Sxml.PrimConDef (* [PML] *)
   end

signature MONOMORPHISE = 
   sig
      include MONOMORPHISE_STRUCTS

      val monomorphise: Xml.Program.t -> Sxml.Program.t
   end
