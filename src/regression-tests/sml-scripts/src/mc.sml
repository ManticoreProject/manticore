structure MC : COMPILER = struct
  val languageName = "manticore"
  val ext = "pml"
  fun mkExe infile = "a.out"
  fun mkCmd infile = concat ["mc ", infile]
  fun detritus infile = []
end

