# Manticore

[![pipeline status](https://gitlab.com/kavon1/manticore/badges/stacks/pipeline.svg)](https://gitlab.com/kavon1/manticore/commits/stacks)

## PLDI'20 Artifact Evaluation

In order to evaluate the system for PLDI'20 artifact evaluation, you must first
have access to an x86-64 Linux system with [Docker installed](https://docs.docker.com/install/).

The Linux requirement is due to our use of Linux's `perf` for profiling in our
benchmark suite, which will probably *not* work if you are running Docker on any
other OS because `perf` is not Unix compatible.

Before continuing on, you can test if Docker is working on your system with:

```console
$ docker run hello-world
```



### Step 1: Obtain the Docker image

##### Method 1: Download pre-built image

The recommended way to obtain the image is to simply use `registry.gitlab.com/kavon1/manticore:latest` wherever `image-name` appears in the rest of this README.
Docker will automatically download the image the first time you try to run it.

##### Method 2: Build image locally

After performing a recursive clone of Manticore,

```console
$ git clone --recursive --single-branch=stacks https://github.com/ManticoreProject/manticore.git
$ cd manticore
```

You can build the Docker image locally with

```console
$ docker build .
# ... some time later ...
Successfully built SOME_HASH_CODE
```

Then, use `SOME_HASH_CODE` wherever `image-name` appears in the rest of this README.




### Step 2 (Optional): Explore the image

To use the image, use the `-it` flag when running it with Docker to get an
interactive prompt:

```console
$ docker run -it image-name
root@docker:/usr/pmlc#
```

The compiler is available in the image as `pmlc` in PATH:

```console
root@docker:/usr/pmlc# pmlc -version
pmlc [x86_64-linux; 0.0.0 (); built 2020-02-07]
```

You can run the regression suite (~1 hour) to make sure everything's okay with:
```console
root@docker:/usr/pmlc# ./run_ci.sh local
```

The text editors `vim` and `emacs` are provided in the image for your
convenience. Relative to `/usr/pmlc`, the sources are organized as follows:

```
src/tools/mc                          -- Manticore compiler
llvm/src                              -- LLVM (our fork of it)
src/lib/basis                         -- Manticore standard library
src/lib/parallel-rt                   -- runtime system
src/regression-tests/goals            -- regression tests
src/benchmarks/benchmarks/programs    -- benchmark programs
```

Manticore's syntax and semantics are generally that of Standard ML, [but with
extensions for concurrency](http://manticore.cs.uchicago.edu).
The most mundane differences are that Manticore does not offer signatures or
records and large portions of the Standard ML Basis Library are missing.
The non-CPS stack strategies currently only support CML-style concurrency.

Here's an example of how to compile and run a simple Manticore program:

```console
root@docker:~# echo -e "val () = Print.printLn \"hello, world\"" > hello.pml
root@docker:~# pmlc hello.pml
root@docker:~# ls
a.out  hello.ll  hello.pml  hello.s  hello_opt.bc
root@docker:~# ./a.out
hello, world
```

Without additional arguments, `pmlc` will compile a program with the standard runtime system and basis library using the CPS strategy.
Other useful flags are:

```
-o <file>        specify executable-file name
-O<level>        set optimization level when using LLVM (0 to 5)
-sequential      compile a sequential-mode program
-contigstack     use contiguous stacks
-linkstack       use mutable, linked-frame stacks
-segstack        use segmented stacks
-resizestack     use resizing stacks
-noras           emit pop/push jmp instead of call/ret for stacks
```

The most relevant flags and additional context for the above:

1. The `-O1` flag enables some basic clean-up optimizations by LLVM (early-cse,
   simplifycfg, etc) and was used for the evaluation. This flag does not affect
   optimizations performed by `pmlc` prior to emitting LLVM IR.

2. The `-sequential` flag will produce a program that disables the runtime system
scheduler for better sequential-program efficiency and omits parts of the standard
library dealing with concurrency to improve compilation time.
Benchmarks in the paper that are not prefixed with `cml-` were tested using
this flag.

3. The special `-Ccshim=<true/false>` flag controls whether to switch stacks
for FFI calls (i.e., `true` means to switch stacks). Linkstack cannot support
the `false` option.

4. The `-noras` flag replaces `ret` instructions with a `pop; jmp` sequence
to disable the CPU's return-address stack.




### Step 3: Run the benchmark suite

The benchmark suite currently requires Linux's `perf stat`, which itself requires that you run the Docker image with additional permissions (the `--cap-add sys_admin` flag).

#### Ensuring `perf` is working

Before getting started, make sure that `perf stat` is working with the following
expected output:

```console
$ docker run -it --cap-add sys_admin image-name
root@docker:/# perf stat echo

 Performance counter stats for 'echo':
 ...
```

If you see a message stating `No permission to enable task-clock event.`, then
you forgot to add `--cap-add sys_admin` to your `docker run` command (use `CTRL+D` to exit the session and try again).

Otherwise if you see a message stating `WARNING: perf not found for kernel X`, then
that means the Docker image you've obtained was built on a Linux system with
a different kernel version than yours (this is quite likely).
This can be fixed for the currently-running image by running:

```console
root@image-name# apt-get update && apt-get install linux-tools-`uname -r`
```

**Please note** that you'll need to run the above command **every time** you run
the Docker image, because the image is not modified during its use.

#### Running the suite

Once you've confirmed `perf stat echo` works, you can run the benchmark suite and
generate plots from the paper with one command (which will probably take 7-8 hours):

```console
root@docker:/usr/pmlc# ./run_cont_bench.sh
```

Once the command completes, keep the interactive Docker session running and open a new terminal prompt on your system.
In this new window, find the active container ID corresponding to the session that finished running the benchmark suite:

```console
$ docker ps
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES
<container-id>      <image-name>        "/bin/bash"         11 hours ago        Up 11 hours                             reverent_brattain
```

Then use that `container-id` to copy the results directory out of the container
like so (from outside of the Docker session):

```console
$ docker cp container-id:/usr/pmlc/results .
```



### Step 4: Evaluate the results

In this step we detail the claims in the paper supported by the artifact and how
to check them against the results generated in Step 3.

TODO
