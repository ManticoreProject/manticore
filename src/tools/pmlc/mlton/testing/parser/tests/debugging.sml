fun posToReg (left, right) = let
    val map = SourceMap.getMap()

    fun stringifyPos (file, lineNo, colNo) = let
      val sLineNo = Int.toString lineNo
      val sColNo = Int.toString colNo
    in
      String.concat ["fileName=", file, ", lineNo=", sLineNo, ", colNo=", sColNo]
    end

    val _ = print "pml.grm.sml: called posToReg\n"
    val {fileName = file1, lineNo=lineNo1, colNo=colNo1} = AntlrStreamPos.sourceLoc map left
    val {fileName = file2, lineNo=lineNo2, colNo=colNo2} = AntlrStreamPos.sourceLoc map right
    val file1 = (case file1 of SOME x => x | NONE => "")
    val file2 = (case file2 of SOME x => x | NONE => "")

    val _ = print (String.concat ["left: ", (stringifyPos (file1, lineNo1, colNo1)), "\n"])
    val _ = print (String.concat ["right: ", (stringifyPos (file2, lineNo2, colNo2)), "\n"])

    in
      Region.make {
	  left = SourcePos.make{column=colNo1, file=file1, line=lineNo1},
	  right = SourcePos.make{column=colNo2, file=file2, line=lineNo2}
	}
    end
