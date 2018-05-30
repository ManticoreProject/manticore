## Compiler Utilities: Constant Arithmetic

Many compilers perform compile-time arithmetic on constants as an optimization.
While such optimizations may seem trivial to implement, there are a number of
pesky details that must be addressed.  These include supporting varying precisions,
signed vs. unsigned operations, and wrapping vs. trapping on overflow.  This
library provides a collection of modules that support these optimizations and
which support a choice of semantics for arithmetic operations.

### The API

Our basic assumption is that compile-time integer constants are represented as
the **SML** type `IntInf.int` (*i.e.*, arbitrary-precision integers).

Operations are grouped into three signatures:

* `BITWISE_CONST_ARITH` -- these operations implement bitwise operations.  They
  support both signed and unsigned values and interpret negative arguments as
  having a 2's complement representation in bits.  For bitwidth of WID, values
  should be in the range -2^(WID-1)^ to 2^WID^-1 (note the asymmetry).

* `SIGNED_CONST_ARITH` -- these operations implement signed arithmetic.  For
  a bitwidth of WID, values should be in the range -2^(WID-1)^ to 2^(WID-1)^-1.

* `UNSIGNED_CONST_ARITH` -- these operations implement unsigned arithmetic.  For
  a bitwidth of WID, values should be in the range 0 to 2^WID^-1.

The `CONST_ARITH` signature is a union of these three signatures.

### Implementations

The signed and unsigned arithmetic signatures have two implementations:
a *trapping* implementation where the `Overflow` exception is raised when
values are two large to be represented in the specified number of bits, and
a *wrapping* implementation, where results are narrowed to the specified precision.

````sml
structure BitwiseConstArith : BITWISE_CONST_ARITH

structure SignedTrappingArith : SIGNED_CONST_ARITH
structure SignedWrappingArith : SIGNED_CONST_ARITH

structure UnsignedTrappingArith : UNSIGNED_CONST_ARITH
structure UnsignedWrappingArith : UNSIGNED_CONST_ARITH
````

In addition, the `ConstArithGlueFn` functor can be used to glue three modules into an
implementation of the `CONST_ARITH` signature.

````sml
functor ConstArithGlueFn (
    structure B : BITWISE_CONST_ARITH
    structure S : SIGNED_CONST_ARITH
    structure U : UNSIGNED_CONST_ARITH
  ) : CONST_ARITH
````

Lastly, there are functors that wrap the three types of arithmetic modules with
error checking of the arguments.  These are meant to be used when one wants to
include internal consistency checking in one's compiler.

````sml
functor CheckBitwiseArithFn (
    structure A : BITWISE_CONST_ARITH
    val qual : string
    val error : string -> 'a
  ) : BITWISE_CONST_ARITH

functor CheckSignedArithFn (
    structure A : SIGNED_CONST_ARITH
    val qual : string
    val error : string -> 'a
  ) : SIGNED_CONST_ARITH

functor CheckUnsignedArithFn (
    structure A : UNSIGNED_CONST_ARITH
    val qual : string
    val error : string -> 'a
  ) : UNSIGNED_CONST_ARITH
````

The `A` structure argument is the structure being wrapped; the `qual` argument
is typically the name of the structure with a trailing period (*e.g.*, `"BitwiseTrappingArith."`);
and the `error` argument is a function for reporting errors.  The `error` function
is not expected to return.  For example, we can define an implementation of bitwise
arithmetic that checks that its arguments are within range as follows:

```sml
structure BitwiseArith = CheckBitwiseArithFn (
    structure A = BitwiseWrappingArith
    val qual = "BitwiseWrappingArith."
    fun error msg = raise Fail msg)
```

### Examples

In languages like **C** and **C++**, integer arithmetic is non-trapping.  We can define
an instatiation of the `CONST_ARITH` interface for these languages by using the wrapping
implementations:

````sml
structure CArith = ConstArithGlueFn (
    structure B = BitwiseConstArith
    structure S = SignedWrappingArith
    structure U = UnsignedWrappingArith)
````

In Standard ML, however, the semantics of arithmetic is more complicated, since signed operations
are trapping, while unsigned operations (*i.e.*, `word` operations) wrap.

````sml
structure SMLArith = ConstArithGlueFn (
    structure B = BitwiseConstArith
    structure S = SignedTrappingArith
    structure U = UnsignedWrappingArith)
````

### Roadmap

* `README.md` -- this file
* `bitwise-const-arith-sig.sml` -- the `BITWISE_CONST_ARITH` signature
* `bitwise-const-arith.sml` -- the `BitwiseConstArith` structure
* `check-bitwise-arith-fn.sml` -- the `CheckBitwiseArithFn` functor
* `check-signed-arith-fn.sml` -- the `CheckSignedArithFn` functor
* `check-unsigned-arith-fn.sml` -- the `CheckUnsignedArithFn` functor
* `const-arith-glue-fn.sml` -- the `ConstArithGlueFn` functor
* `const-arith-sig.sml` -- the `CONST_ARITH` signature
* `signed-const-arith-sig.sml` -- the `SIGNED_CONST_ARITH` signature
* `signed-trapping-arith.sml` -- the `SignedTrappingArith` structure
* `signed-wrapping-arith.sml` -- the `SignedWrappingArith` structure
* `sources.cm` -- CM file for compiling the code
* `test.cm` -- CM code for testing the code
* `test.sml` -- test cases
* `unsigned-const-arith-sig.sml` -- the `UNSIGNED_CONST_ARITH` signature
* `unsigned-trapping-arith.sml` -- the `UnsignedTrappingArith` structure
* `unsigned-wrapping-arith.sml` -- the `UnsignedWrappingArith` structure
