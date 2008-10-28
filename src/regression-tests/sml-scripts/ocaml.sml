structure OCaml : COMPILER = struct
  val languageName = "ocaml"
  val ext = "ml"
  fun mkCmd   {infile, outfile} = concat ["ocamlc -o ", outfile, " ", infile]
  fun ballast {infile, outfile} = map (fn s => concat [outfile, ".", s]) ["cmi", "cmo"]
end

