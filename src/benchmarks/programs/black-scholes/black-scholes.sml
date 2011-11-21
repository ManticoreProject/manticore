(* black-scholes.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Black Scholes benchmark.  Ported by Damon Wang.
 *)

structure BlackScholes : sig

    type float = real

    datatype OptionType = Put | Call

    datatype option_t = Option of { 
	spot : float,          (* spot price *)
	strike : float,        (* strike price *)
	interest : float,      (* risk-free interest rate *)
	div_rate : float,      (* dividend rate *)
	volatility : float,    (* volatility *)
	time : float,          (* years to maturity or expiration: 6mos = .5, etc. *)
	opt_type : OptionType,(* Put or Call *)
	div_vals : float,      (* dividend values (not used here) *)
	derivagem : float      (* expected answer from DerivaGem *)
      }

    val dummy_option : option_t

    val readData : string -> option_t list
    val writePrices : string -> float list -> unit
    val poly : float list -> float -> float
    val magic_poly : float -> float 
    val std_normal_cdf : float -> float
    val price : option_t -> float

  end = struct

    type float = real

    datatype OptionType = Put | Call

    datatype option_t = Option of { 
	spot : float,          (* spot price *)
	strike : float,        (* strike price *)
	interest : float,      (* risk-free interest rate *)
	div_rate : float,      (* dividend rate *)
	volatility : float,    (* volatility *)
	time : float,          (* years to maturity or expiration: 6mos = .5, etc. *)
	opt_type : OptionType,(* Put or Call *)
	div_vals : float,      (* dividend values (not used here) *)
	derivagem : float      (* expected answer from DerivaGem *)
      }

    val dummy_option = Option{
	    spot=123.0, strike=234.0, interest=345.0,
	    div_rate=456.0, volatility=567.0, time=678.0,
	    opt_type=Put, div_vals=789.0,
	    derivagem=890.0
	  }

  (* readData : String -> [ Option ] -- don't know how to tell the compiler *)
  fun readData (infname : string) : option_t list = let
    fun real_of_string s = Option.valOf (Real.fromString s)

    val inf = TextIO.openIn infname

    val num_options = 
      (Option.valOf o Option.composePartial(Int.fromString, TextIO.inputLine)) inf

      (* string list -> Option 
      * TODO throw an exception for invalid input *)
    fun option_of_fields [
	    spot, strike, interest, div_rate, volatility,
	    time, opt_type, div_vals, derivagem
	  ] = Option {
	      spot=real_of_string spot,
	      strike=real_of_string strike, 
	      interest=real_of_string interest,
	      div_rate=real_of_string div_rate,
	      volatility=real_of_string volatility, 
	      time=real_of_string time,
	      (* TODO throw exception if neither "P" nor "C" *)
	      opt_type= if opt_type = "P" then Put else Call,  
	      div_vals=real_of_string div_vals,
	      derivagem=real_of_string derivagem
	    } 
      | option_of_fields _ = dummy_option  (* TODO throw an exception here *)

  val options = 
    map (fn line => option_of_fields (String.fields Char.isSpace line))
        (String.tokens (fn c => Char.contains "\n" c) (TextIO.inputAll inf))
    (* TODO does this leave the file open? does that matter? *)
  in
    if num_options = length options
    then options
    else [] (* TODO exception here *)
  end

  fun writePrices outfname prices = let
	val out = TextIO.openOut outfname
	fun wr price = TextIO.output (out, (Real.toString price) ^ "\n")
	in
	  List.app wr prices;
	  TextIO.closeOut out
	end

  (* uses Horner's algorithm to evaluate a polynomial whose coefficients are
   * specified in order of ascending degree: x + 2 is [2, 1] *)
  (* x must be type-hinted to float or smlnj thinks it's an int! *)
  fun poly (c::coeffs) (x : float) = foldl (fn (c, partial) => c + x * partial) c coeffs

  local
    (* probability density function for standard normal distribution: 
     * mean = 0, variance = 1 *)
    val normalization_factor = 1.0 / Math.sqrt(2.0 * Math.pi)
    fun std_normal_pdf x = normalization_factor * Math.exp (~0.5 * x * x)

    (* Approximation of the normal cumulative density function for x > 0 which
     * gives error < 7.5e-8. Algorithm 26.3.7 in Abramowitz and Stegun (1964) *)
    (* magic constants specified in order b5, b4, b3, b2, b1, 0.0 *)
    val magic = [
	    1.330274429, ~1.821255978, 1.781477937, ~0.356563782,
	    0.319381530, 0.0
	  ] 
    val b0 = 0.2316419
  in
    fun magic_poly x = poly magic (1.0 / (1.0 + b0 * x))
    (* cumulative density function for standard normal distribution *)
    fun std_normal_cdf x = if x < 0.0
               then (std_normal_pdf (~x)) * (magic_poly (~x))
               else (*
               TextIO.print ("magic_poly " ^ (Real.toString x) ^ " = " ^
               (Real.toString (magic_poly x)) ^ "\t");
               *)
               1.0 - (std_normal_pdf x) * (magic_poly x)
  end

  fun price (Option{
	spot, strike, interest, volatility, time, opt_type, derivagem, ...
      }) = let
	val denom = volatility * Math.sqrt (time)
	val strike_exp = strike * Math.exp (~interest * time)
	val log_term = Math.ln (spot / strike)
	val time_term = (interest + (volatility * volatility * 0.5)) * time
	val d1 = (log_term + time_term)  / denom
	val d2 = d1 - denom
	val n_d1 = std_normal_cdf d1
	val n_d2 = std_normal_cdf d2
	val debug_print = String.concat [
		"log_term = ", (Real.toString log_term), "\ttime_term =",
		(Real.toString time_term), "\tdenom = ", (Real.toString denom),
		"\nd1 = ", (Real.toString d1), "\td2 = ", (Real.toString d2), 
		"\tn_d1 = ", (Real.toString n_d1), "\tn_d2 = ", (Real.toString n_d2),
		"\n"
	      ]
	val retval = if opt_type = Put
		     then strike_exp * (1.0 - n_d2) - spot * (1.0 - n_d1)
		     else spot * n_d1 - strike_exp * n_d2
	in
	  (* TextIO.print debug_print; *)
	  TextIO.print (if Real.abs (retval - derivagem) > 0.001
		       then "E"
		       else ".");
	  retval
	end

  end

structure Main =
  struct

    fun main (_, args) = let
	  val options = BlackScholes.readData (hd args)
	  fun doit () = map BlackScholes.price options
	  in
	    TextIO.print (hd args);
	    RunSeq.run doit
	  end

  end
