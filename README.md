# Manticore

[![pipeline status](https://gitlab.com/kavon1/manticore/badges/trunk/pipeline.svg)](https://gitlab.com/kavon1/manticore/commits/trunk)

[Manticore](http://manticore.cs.uchicago.edu) is a high-level parallel programming language aimed at general-purpose applications running on multi-core processors. Manticore supports parallelism at multiple levels: explicit concurrency and coarse-grain parallelism via CML-style constructs and fine-grain parallelism via various light-weight notations, such as parallel tuple expressions and NESL/Nepal-style parallel array comprehensions.

## REQUIREMENTS


Manticore currently only supports the x86-64 (a.k.a. AMD64)
architecture running on either Linux or macOS. It is possible to
build the compiler on other systems, but we have not
ported the runtime system to them yet.

Manticore is implemented in a mix of C and SML code.  You will need a
recent version of SML/NJ (version 110.81+) installed.  Furthermore,
your installation of SML/NJ should include the MLRISC library.
Note that SML/NJ 110.82's version of MLRISC is currently incompatible with our
build system.

If you would like to have the LLVM backend available for use,
follow the instructions below. Otherwise, skip to "Building from Source".

#### Optional Prerequisite: LLVM

You must have a *custom* version of LLVM built prior to configuring and
building Manticore in order to have the LLVM backend available for use.
The following commands will obtain the right LLVM sources and place it in `./llvm/src`

    git submodule init llvm/src
    git submodule update llvm/src

Next, we're going to build LLVM, which has its own set of [prerequisites](http://llvm.org/docs/GettingStarted.html#software) that
any Unix machine setup for C++ development should already have.
To configure LLVM, run the following commands

    cd llvm
    mkdir build
    cd build
    cmake -G "Unix Makefiles" -DLLVM_TARGETS_TO_BUILD="X86" -DCMAKE_BUILD_TYPE=Release ../src

Next, we will build only the parts of LLVM that we need, which will take a few minutes.
Replace `n` below with the number of parallel jobs you would like to use during
the build, such as the number of cores on your system.
To get the build going, run the following

    make llc opt -j n

then, move back to the root directory with

    cd ../..

and continue with configuring and building Manticore below. Note that LLVM will now be
available under `./llvm/build` and you should not need to rebuild it again.


## Building From Source

If building and installing the system from source, you first must
generate the configuration script.  To do so, run the following two commands:

    autoheader -Iconfig
    autoconf -Iconfig

Then proceed with configuration.

### Configuring

Our next step is to run the configure script. If you are using the MLRISC
library *included* with your SML/NJ installation and do *not* plan to use the
LLVM backend, you can simply run

	./configure

and then skip to the build/installation step. Otherwise, you can add some
of the following options to `configure` before moving on to building.

#### Configuring with external MLRISC

If you would like to configure with external MLRISC libraries,
add the `--with-mlrisc` option.

	./configure --with-mlrisc=<path to mlrisc>

#### Configuring with LLVM

If you want to have the LLVM backend available, configure with the local
installation of LLVM after building it (using the instructions above) by
adding the `--with-llvm` option to configure.

    ./configure --with-llvm=./llvm/build


### Building and Installing the Distribution

Next, to build the compiler, we use the following command.

    make build

We can install locally

    make local-install

or globally.

    make install

If you chose to do a local install, you'll find the compiler, `pmlc`,
under the `bin` directory.
Run `pmlc -h` for usage information.

### Testing

Details about running the regression suite with various backends goes here.

### Benchmarks

    git submodule init src/benchmarks
    git submodule update src/benchmarks

Then, see the README file under `src/benchmarks` for more details.

------------------

### Known Issues

Some of the items below are out of date.

#### Larger Issues
- The frontend does not support signatures, functors, record types, and a slew of
corner cases in the language.

- PVal and PTuples cannot be used together. The "fast clone" translation breaks
invariants relied on by the work-stealing scheduler with regards to the valid
intermediate states of the work queues.

- Exception handling is not implemented.

- The inatomic/from-atomic/to-atomic naming convention used in inline BOM is still
a bugfest and should really be replaced by a static annotation that is checked
by the compiler.

- The basis library is a hodgepodge mess. The few structures that exist are
typically dramatically different from the SML basis library due to the subset of
the language implemented, which both makes existing code from another system
hard to reuse and sometimes the interface cannot even be written.

#### Smaller Issues

- The effect analysis defined in bom-opt/remove-atomics.sml should be changed from
being name-based to instead either have a trackable annotation or other better
marker for user-level code that uses mutable state. Additionally, while we
remove ATOMIC operations around PURE functions, we do not handle reducing them
in the case where the code between the parallel spawn and another lock is PURE.

- We cannot handle allocations larger than a single heap page size (minus some
slop). These allocations result in an exception, which is tough to debug because
there is no exception handling.

- The work-stealing scheduler cannot handle more than a stack of 32k tasks, and
crashes quietly when that is exceeded.

- Memoization and mutable state exist only as hand-performed translations to call
basis library functions.

#### Incomplete projects

- The safe-for-space closure conversion was not completed. While its code may be
used for inspiration, we were not able to get a full write-up on its status
before the student graduated.

- In CFG, we now have code that performs rudimentary loop identification and can
also generate a DOT file for visualization of basic blocks. Loop unrolling was
not implemented.

- A branch was created for the BOM implementation of flattening, but it is still
in the design phase.
