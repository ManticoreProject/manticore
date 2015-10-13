(* pure-rand.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * purely functional implementation of random number generation.  This is 
 * basically a rip off of what is done in System.Random of GHC
 * https://hackage.haskell.org/package/random-1.1/docs/src/System-Random.html#randomR
 *)


structure PureRand =
struct 
	_primcode(

		typedef gen = [long, long];

		define @next(gen : [long, long] / exh:exh) : [[long], [long, long]] = 
			let s1 : long = #0(gen)
			let s2 : long = #1(gen)
			let k : long = I64Div(s1, 53668:long)
			let s1' : long = I64Sub(I64Mul(40014:long, I64Sub(s1, I64Mul(k, 53668:long))), I64Mul(k, 12211:long))
			let s1'' : long = if I64Lt(s1', 0:long) then return(I64Add(s1', 2147483563:long)) else return(s1')
			let k' : long = I64Div(s2, 52774:long)
			let s2' : long = I64Sub(I64Mul(40692:long, I64Sub(s2, I64Mul(k', 52774:long))), I64Mul(k', 3791:long))
			let s2'' : long = if I64Lt(s2', 0) then return(I64Add(s2', 2147483399:long)) else return(s2')
			let newGen : [long, long] = alloc(s1'', s2'')
			let z : long = I64Sub(s1'', s2'')
			let z' : long = if I64Lt(z, 1:long) then return(I64Add(z, 2147483562:long)) else return(z)
			let z' : [long] = alloc(z')
			let res : [[long], [long, long]] = alloc(z', newGen)
			return(res)
		;

		define @mk-gen(x : [long] / exh:exh) : gen = 
			let q : long = I64Div(#0(x), 214783562:long)
			let s1 : long = I64Mod(#0(x), 214783562:long)
			let s2 : long = I64Mod(q, 2147483398:long)
			let gen : [long, long] = alloc(I64Add(s1, 1:long), I64Add(s2, 1:long))
			return(gen);


		(*float M_FRand (float lo, float hi)
{
    return (((float)rand() / ((float)(RAND_MAX)+(float)(1)) ) * (hi-lo)) + lo;
}*)

	)

	type gen = _prim(gen)

	val mkGen : long -> gen = _prim(@mk-gen)

	val next : gen -> long * gen = _prim(@next)

	val randMAX : long = 2147483562

	fun nextFloat (gen, low, high) = 
		let val (n, gen') = next gen
			val nFloat = Float.fromLong n
			val maxFloat = Float.fromLong randMAX
		in (((nFloat / (maxFloat + 1.0)) * (high - low)) + low, gen') end

	fun nextInt(gen, low, high) = 
		let val (n, gen') = next gen
			val x = ((Long.toInt n) mod (high - low)) + low
		in (x, gen') end

end