# Manticore PLDI'20 Artifact Guide

[![pipeline status](https://gitlab.com/kavon1/manticore/badges/pldi20/pipeline.svg)](https://gitlab.com/kavon1/manticore/commits/pldi20)

## Getting Started

**NOTE**: This Markdown-formatted README is best viewed on GitHub here:

https://github.com/ManticoreProject/manticore/blob/pldi20/README.md


In this section, we provide a quick (30 minute) guide to help you become
familiar with the artifact and ensure that it's working.

In order to use and evaluate the artifact, you must first
have access to an x86-64 Linux system, ideally with Docker installed:

https://docs.docker.com/install/

The Linux requirement is due to our use of Linux's `perf` for profiling in our
benchmark suite, which will probably *not* work if you are running Docker on any
other OS because `perf` is Linux-only.
The particular Linux distribution assumed throughout this README is Ubuntu,
and if you end up using Docker then you'll want your system to be running
Ubuntu so that the kernel version matches up with what the image expects.

Once you believe you have Docker installed, you can test if Docker is working
on your system with:

```console
$ docker run hello-world
```

Throughout this guide, terminal shell prompts that begin with `$` refer to your
system's native shell process, and prompts starting with
`root@container:CURRENT_PATH#` refer to the Docker container's shell process.


### On Docker's Privileges

During our testing of this artifact, we found that Docker adds a *large*
overhead to indirect-jump instructions, unless if the `--privileged` flag is
used.
Note that **no `sudo` is required or granted** to use this flag; it simply
grants privileges equivalent to a process running outside of the container.
The reason for this overhead *without the flag* is likely to be that Docker
intercepts all indirect jumps to check if they're making a system call
(for fine-grained security).

This interception has an impact on our benchmarks, because indirect jumps
are the primary mechanism by which the cps, smlnj, and mlton benchmark
configurations perform ordinary function call-and-return.
Some benchmarks (like `fib`) saw over 2x slow-down in overall running-time
without the `--privileged` flag.
For more information about privileges and Docker, see here:

https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities

Of course, since function-call overhead is precisely one of the focuses of our
evaluation, throughout this guide we assume that you'll be using the flag.
If you or your system administrator disallows the use of `--privileged` when
testing this artifact, then please take into account the overhead we described.

In order to run the benchmark suite for this artifact, at a *minimum* you must
grant the container the `--cap-add sys_admin` Linux capability because we use
`perf`, which is a Linux-specific profiling system that makes calls into the
kernel that are disabled in Docker without that capability.


### Getting Started Step 1: Obtaining the Artifact Docker image

##### Method 1: Pull the pre-built image

The recommended way to obtain the latest version of the Docker image is to run

```console
$ docker pull registry.gitlab.com/kavon1/manticore:pldi20
```

For the remainder of this README, wherever `image-name` appears, you will
use `registry.gitlab.com/kavon1/manticore:pldi20`.
To make sure you have the latest version, run the `docker pull` command above
prior to running the image.


##### Method 2: Load the archived image

Should the Method 1 fail, the artifact package also includes the image
as a tar file, `manticore_docker.tar`.
You can load this image into Docker with:

```console
$ docker load < manticore_docker.tar
Loaded image: registry.gitlab.com/kavon1/manticore:pldi20
```

Then, you should be able to use `registry.gitlab.com/kavon1/manticore:pldi20`
wherever `image-name` appears in the rest of this README.


##### Method 3: Build image locally

You can also build the image from the source code available on GitHub, which
takes about 20 -- 30 minutes on our continuous integration runner.
To do this, perform a recursive clone of Manticore like so,

```console
$ git clone -b pldi20-final --recursive https://github.com/ManticoreProject/manticore.git
```

The `pldi20-final` is the tagged commit on the `pldi20` branch corresponding
to the archived artifact.
As a backup, we've provided a snapshot of `pldi20-final` as the
tarball `manticore.tgz`.

Then build the Docker image locally with

```console
$ cd manticore
$ docker build .
```

Finally, once the Docker build completes you'll see a final message saying
`Successfully built SOME_HASH_CODE`.
You should use `SOME_HASH_CODE` wherever `image-name` appears in the rest of
this README.

**Building Without Docker**
Step-by-step instructions to obtain all dependencies and build the compiler
outside of Docker are the same as those listed in the `Dockerfile`.
Just run each command listed in each `RUN` step, assuming your initial working
directory is the root of the repository where the `Dockerfile` resides.




### Getting Started Step 2: Testing the Docker image

To "kick the tires" of the image to make sure things are working, run
the following command, which launches a container based on the image
with an interactive prompt:

```console
$ docker run -it --privileged image-name
root@container:/usr/pmlc#
```

The text editors `vim` and `emacs` are provided in the image for your
convenience.
The Manticore compiler (which implements the 6 stack strategies) is
available in the image as `pmlc` in PATH:

```console
root@container:/usr/pmlc# pmlc -version
pmlc [x86_64-linux; 0.0.0 (); built 2020-02-28]
```

Try to to compile and run a simple Manticore program:

```console
root@container:/usr/pmlc# cd ~
root@container:~# echo -e "val () = Print.printLn \"hello, manticore\"" > hello.pml
root@container:~# pmlc hello.pml
root@container:~# ls
a.out  hello.ll  hello.pml  hello.s  hello_opt.bc
root@container:~# ./a.out
hello, manticore
```

Then a MLton-compiled SML program:

```console
root@container:~# echo -e "val () = print \"hello, sml\\\n\"" > hello.sml
root@container:~# mlton hello.sml
root@container:~# ./hello
hello, sml
```

Then launch a specific installation of SML/NJ and make sure it says "64-bit"
when launched:

```console
root@container:~# /usr/smlnj64/bin/sml   
Standard ML of New Jersey (64-bit) v110.96 [built: Fri Feb 07 22:57:06 2020]
- 1+1;
val it = 2 : int
(* press CTRL+D to exit the REPL *)
```

Next up is the most important thing to test, `perf`!


#### Ensuring that `perf` is working

The benchmark suite requires Linux's `perf`, which itself requires
that you run the Docker image with additional permissions (described earlier).
Make sure that `perf` is working with the following expected output:

```console
root@container:~# perf stat echo

 Performance counter stats for 'echo':
 ...
```

There are two possible error messages you'll see, which we will cover in the
following sub-sections.

##### Error 1: Perf Not Found

If you see a message starting with `WARNING: perf not found for kernel X`,
then that means the Docker image you've obtained was built on a Linux system
with a different kernel version than yours.
Unfortunately, this is quite likely to happen because the kernel is the one
leaky part of the Docker container.
For reference, this artifact's Docker image was based on Ubuntu 16.04,
and was built on a native machine running Ubuntu 18.04.
If you're running a version of Ubuntu between 16.04 and 18.04, you should
be able to fix this error for the currently-active container by running
the `apt-get` command:

```console
root@container:~# apt-get update && apt-get install -y linux-tools-$(uname -r)
root@container:~# perf stat echo  # should work now!
```

If `apt-get` works for you, wonderful!
Because you would normally need to run that command every time you relaunch the
Docker image, we've added basic detection of this scenario in the
`run_cont_bench.sh` script in case you don't remember.
Feel free to jump to the "Running the regression suite" section.

Otherwise, if the `apt-get` installation of `linux-tools-*` fails,
it's likely to be the case that your native Linux machine's kernel version is newer
than what is available in Ubuntu 16.04's package repository for `linux-tools-*`.
In particular, it seems Ubuntu 16.04 only supports up to 18.04's kernel version.
Here are some workarounds:

**Workaround 1:** If you don't plan to run our benchmarking scripts, i.e.,
`run_cont_bench.sh` and the subsequent scripts it kicks off, then
you don't need `perf` to be working and continue on with using the Docker image.

**Workaround 2:** If you plan to use the benchmark scripts, but *do not care
about the data collected from `perf`*, then you could just add a dummy `perf`
executable to the PATH of the running Docker container that always spits out
the following data (an example taken from the paper's data) to `stderr`:

```
 Performance counter stats for '/home/kavon/manticore/src/benchmarks/benchmarks/programs/seq-fib/mc-seq' (5 runs):

       3608.019854      task-clock (msec)         #    0.984 CPUs utilized            ( +-  0.57% )
               350      context-switches          #    0.097 K/sec                    ( +-  1.21% )
                36      cpu-migrations            #    0.010 K/sec                    ( +-  2.60% )
               330      page-faults               #    0.091 K/sec                    ( +-  0.18% )
    12,640,356,788      cycles                    #    3.503 GHz                      ( +-  0.14% )  (30.81%)
   <not supported>      stalled-cycles-frontend  
   <not supported>      stalled-cycles-backend   
    38,193,194,020      instructions              #    3.02  insns per cycle          ( +-  0.12% )  (38.53%)
     9,575,569,738      branches                  # 2653.968 M/sec                    ( +-  0.10% )  (38.56%)
           652,922      branch-misses             #    0.01% of all branches          ( +-  2.31% )  (38.59%)
     8,234,247,853      L1-dcache-loads           # 2282.207 M/sec                    ( +-  0.05% )  (38.64%)
           307,152      L1-dcache-load-misses     #    0.00% of all L1-dcache hits    ( +-  4.55% )  (38.68%)
            18,110      LLC-loads                 #    0.005 M/sec                    ( +- 10.99% )  (30.90%)
             7,092      LLC-load-misses           #   78.32% of all LL-cache hits     ( +- 10.35% )  (30.97%)
   <not supported>      L1-icache-loads          
           523,516      L1-icache-load-misses     #    0.145 M/sec                    ( +-  8.75% )  (30.91%)
     8,269,966,365      dTLB-loads                # 2292.107 M/sec                    ( +-  0.12% )  (30.86%)
             1,015      dTLB-load-misses          #    0.00% of all dTLB cache hits   ( +- 36.32% )  (30.86%)
            11,362      iTLB-loads                #    0.003 M/sec                    ( +-  7.35% )  (30.82%)
             2,164      iTLB-load-misses          #   19.05% of all iTLB cache hits   ( +- 12.00% )  (30.83%)

       3.666326914 seconds time elapsed                                          ( +-  0.57% )
```

This way, our plotting scripts that try to parse this output will not crash,
and instead produce `perf` data plots that are identical for all strategies (for
you to ignore of course).

**Workaround 3:** Our `Dockerfile` in the source code's root is known to work
if you change the first line to `FROM ubuntu:18.04` and then follow the steps
above in "Method 3" to build the Docker image locally.
This one-line change might work in the future for other Docker image bases
(i.e., `ubuntu:20.04`, etc).

Our best guess as to what might break in the future if you change the image base
is the availability of 32-bit libraries needed to build 32-bit SML/NJ,
which is what is used to build Manticore.
If they're no longer available, it might be possible to build Manticore
with 64-bit SML/NJ, but as of writing we haven't tested that out with
our build system yet.
There might be some lingering heap-image extensions hardcoded to say
`x86-linux` that need to change to `amd64-linux`, but that should be all that's
needed to build with 64-bit SML/NJ.


##### Error 2: No permission to run perf

Otherwise, If you see a message starting with
`No permission to enable task-clock event.`, then you forgot to add
`--privileged` to your `docker run` command (use `CTRL+D` to exit the
session and try again).



#### Running the Regression Suite

**NOTE**: If time permits, we suggest that you also run the compiler's
regression suite (~1 hour) to make sure everything's okay with:

```console
root@container:~# cd /usr/pmlc
root@container:/usr/pmlc# ./run_ci.sh local
```

Once you've reached this point, then you've successfully "kicked the tires"
of this artifact!


------------------------------------------------------------------------------


## Background Information

In this section, we provide basic information about the compiler and source
code contained in the Docker image for reusability purposes.
This information is not needed to reproduce the results of the paper, but it is
useful for future extensions of the system.

Relative to `/usr/pmlc`, which is the root of the GitHub repository,
the sources in the image are organized as follows:

```
src/tools/mc                          -- Manticore compiler
llvm/src                              -- LLVM (our fork of it)
src/lib/basis                         -- Manticore standard library
src/lib/parallel-rt                   -- Manticore runtime system
src/regression-tests/goals            -- Manticore regression tests
src/benchmarks/benchmarks/programs    -- Manticore benchmark programs
```

Manticore's syntax and semantics are generally that of Standard ML, [but with
extensions for concurrency](http://manticore.cs.uchicago.edu).
The most mundane differences are that Manticore does not offer signatures or
records and large portions of the Standard ML Basis Library are missing.
The non-CPS stack strategies currently only support CML-style concurrency.

Without additional arguments, `pmlc` will compile a program with the standard
runtime system and basis library using the CPS strategy.
LLVM is always used to compile programs unless if the `-mlrisc` flag is specified.
Full options can be found by passing `-h` and `-H` to `pmlc`.
The most useful flags are:

```
-o <file>        specify executable-file name
-O<level>        set optimization level when using LLVM (0 to 5)
-sequential      compile a sequential-mode program
-contigstack     use contiguous stacks
-linkstack       use mutable, linked-frame stacks
-segstack        use segmented stacks
-resizestack     use resizing stacks
-hybridstack     use hybrid stacks
-noras           emit pop/push jmp instead of call/ret for stacks
```

The most relevant flags and additional context for the above:

1. The `-O1` flag enables LLVM optimizations like tail-call elimination and
some basic clean-up (instcombine, simplifycfg, etc) and was used for the paper's
evaluation. Note that the `-O<level>` flag does not affect any optimizations
performed by `pmlc` prior to emitting LLVM IR.

2. The `-sequential` flag will produce a program that disables the runtime
system scheduler for better sequential-program efficiency and omits parts of
the standard library dealing with concurrency to improve compilation time.
Benchmarks in the paper that are not prefixed with `cml-` were tested using
this flag.

3. The special `-Ccshim=<true/false>` flag controls whether to switch stacks
for FFI calls (i.e., `true` means to switch stacks). Linkstack cannot support
the `false` option.

4. The `-noras` flag replaces `ret` instructions with a `pop; jmp` sequence
to disable the CPU's return-address stack.


------------------------------------------------------------------------------


## Step-by-Step Evaluation Instructions

In this section we describe how to evaluate the artifact with respect to the
paper.

### Step 1: Run the benchmark suite

**Optional**: If you're connecting to a machine over SSH, consider running
Docker within `screen` to keep the Docker session alive even if the connection
is interrupted:

https://www.linode.com/docs/networking/ssh/using-gnu-screen-to-manage-persistent-terminal-sessions/

##### Step 1a: Run the script.

We have an all-in-one script that runs the benchmark suite and generates plots
from the paper. So, launch Docker and run the script (which will take
7-8 hours to finish) with:

```console
$ docker run -it --privileged image-name
root@container:/usr/pmlc# ./run_cont_bench.sh
```

##### Step 1b: Copy the results out of the Docker container.

Once the script completes, **keep the interactive Docker session running**.
You'll need to find the ID associated with that active container.
Normally the `container-id` is the hash-code between the `@` and `:` in the
shell prompt of the container.
Alternatively, you can find the `container-id` by opening a new terminal prompt
the system and run `docker ps`, looking in the "CONTAINER ID" column.

Once you have the `container-id`, copy the results directory out of the
container and into the real file system like so:

```console
$ docker cp container-id:/usr/pmlc/results ./paper234_results
$ ls paper234_results/
gcstats  normal
```

Once you've copied the results directory to the real file system,
it's safe to end the Docker container with `CTRL+D` at its prompt.

**Optional**: If you're connected to a remote machine and want to view the
data locally, compress the data into an archive with

```console
$ tar czvf paper234_results.tar.gz paper234_results/
```

And then use the `scp` utility to copy the archive from the remote machine to
your local one using your SSH credentials.



### Step 2: Evaluate the results

Now that you have generated the results directory from Step 1, we can evaluate
the claims in the paper that are supported by this artifact.

All files and plots we refer to in this section are paths relative to the
`paper234_results` directory containing the results from Step 1.
The plots are PDF files compatible with any PDF viewer and the other files
should be opened with a plain-text editor.
Some files contain a timestamp in their filename, so in this README we have
`DATE` written in its place.

**ALSO**

1. Please double-check the files you open as you explore the results,
since the file names and paths are all quite similar! :)

2. The benchmark group nicknames used internally within our benchmark
suite and plotting script differs from the paper. The correspondence from
internal name to paper name is: "toy" => "recursive", "tail" => "looping",
"real" => "mixed", "cont" => "continuation".

3. To find the programs in the benchmark suite from Table 1, you are looking
for directories under `src/benchmarks/benchmarks/programs` with names that start
with either `seq-`, `ec-`, `cml-`, or `ffi-` and end with the suffix that are
in Table 1. The exception is that "mcray" is "seq-raytrace", and "barnes-hut"
is "seq-barneshut".


##### Section 5.1.2 -- Recursion Performance

We go step-by-step in order of the claims made in this section
and how to check them against the results gathered in Step 1.


- Figure 3 corresponds with the plot `normal/cross_toy_times.pdf`,
and covers the discussion in the Recursive Benchmarks paragraph.


- Claims regarding garbage collection in the Recursive Benchmarks paragraph
can be checked in the following sub-steps:

  1. Percent of time spent in the garbage collector for `ack` in cps and linked
  strategies etc, is in plot `gcstats/analyze_toy_gc_time_total_pct.pdf`.

  2. The volume of data allocated for **closure-based stacks** (aka "cps") for
  `ack` (24GB) should be checked manually in the file
  `gcstats/seq-ack/seq-ack-cps-mc-seq-DATE.json`.
  The field `minorgc-alloc` corresponds to the number of bytes allocated in
  the nursery. Divide by 2^30 = 1073741824 to get GiB.

  3. The percentage of live data copied from the **nursery to the old space**
  (65.6%) is in the plot `gcstats/analyze_toy_gc_minor_live_pct.pdf`

  4. The percentage of live data copied from the **old space to the global heap**
  (40%) is in `gcstats/analyze_toy_gc_major_live_pct.pdf`.

  5. The volume of data allocated for **linked-frame stacks** for `ack` is
  in the file `gcstats/seq-ack/seq-ack-linkstack-mc-seq-DATE.json`. Look for the
  `minorgc-alloc` value and compare it with cps's value (sub-step 2) to find the
  difference to be 2.7GB.


- The discussion of "high number of segment-overflow events"
for segmented stacks compared to resizing stacks for `ack` and `quicksort`
are supported by examining the `stackcache-access` values (subtracting by 2 for
the fixed number of accesses related to runtime system initialization)
in the following comparisons:
    ```
    For 'ack':
    gcstats/seq-ack/seq-ack-segstack-mc-seq-DATE.json
                    much higher than
    gcstats/seq-ack/seq-ack-resizestack-mc-seq-DATE.json

    For 'quicksort':
    gcstats/seq-quicksort/seq-quicksort-segstack-mc-seq-DATE.json
                    much higher than
    gcstats/seq-quicksort/seq-quicksort-resizestack-mc-seq-DATE.json
    ```


- Figure 5 corresponds with the plot `normal/cross_tail_times.pdf` and supports
the discussion regarding looping benchmarks.


- Figure 6 corresponds with the plot `normal/cross_real_times.pdf`
and supports the discussion regarding mixed benchmarks.



##### Section 5.1.3 -- Continuation Benchmarks

Figure 7 corresponds with the plot `normal/crossCont_cont_times.pdf`.


##### Section 5.2 -- Design Trade-offs

###### Section 5.2.1 Foreign Function Calls

The data for Foreign Function Calls are plotted in a number of files, with
one file per stack strategy.
The claims made about the min-max ranges for
both `ffi-fib` and `ffi-trigfib` are shown in these plots:

```
normal/ffi_contig_ffi_times.pdf
normal/ffi_cps_ffi_times.pdf
normal/ffi_resizestack_ffi_times.pdf
normal/ffi_segstack_ffi_times.pdf
```

Note that `noshim` means "native FFI calls" and `shim` means "switching to
a dedicated stack via a shim to perform the call".

Also, you may notice that the linked-frame stack data is not included.
This is because that strategy does not support this experiment as a consequence
of its design: one cannot make a C call without switching stacks when using
a heap-allocated linked-frame stack that uses the native CPU call/ret
instructions.


###### 5.2.2 Hardware Stack Support

In the plots discussed here, "-noras" appended to a particular stack strategy's
name refers to that stack when it's run with the `ret` instruction replacement
to disable the Return Address Stack mechanism in the CPU. So the speed-ups
are relative to a "noras" baseline.

> We found that for contig, resize, and segmented and hybrid strategies,
> using the native CPU instructions yields a 1.02× – 1.07× speedup for
> the recursive group, [...]

This min-max claim can be checked by inspecting the GMEAN values in the following
plots, which show  the speed-up of using the native stack instructions:

```
normal/ras_contig_toy_times.pdf
normal/ras_resizestack_toy_times.pdf
normal/ras_segstack_toy_times.pdf
normal/ras_hybridstack_toy_times.pdf
```

For the subsequent discussion about motzkin and fib being outliers, use
the same plots above, but look specifically at those two benchmark programs.


> [...] Those strategies see a 1.01× – 1.02× speedup for the mixed benchmarks, [...]

This claim is checked like the previous one by looking at the GMEAN values,
but using these plots instead:

```
normal/ras_contig_real_times.pdf
normal/ras_resizestack_real_times.pdf
normal/ras_segstack_real_times.pdf
normal/ras_hybridstack_real_times.pdf
```

> [...] and a 1.02x speedup overall [...]

This claim is checked in the following way. Gather the four GMEAN values
from the following plots that contain all non-continuation benchmarks:

```
normal/ras_TOTAL_contig_all_times.pdf
normal/ras_TOTAL_resizestack_all_times.pdf
normal/ras_TOTAL_segstack_all_times.pdf
normal/ras_TOTAL_hybridstack_all_times.pdf
```

Then compute the average of those three values and round.
We had (1.03 + 1.03 + 1.01 + 1.01) / 4 = 1.02x overall speedup.


> Linked stacks saw effectively no change in performance even
> though the implementation uses call and return.

This claim can be checked by inspecting the GMEAN values in the following plots:

```
normal/ras_linkstack_toy_times.pdf
normal/ras_linkstack_real_times.pdf
normal/ras_TOTAL_linkstack_all_times.pdf
```

You're looking to see that nearly all programs hover around 1.0 with little
variation.
Particular focus should be placed on linkstack's 'toy' plot, which had the highest
variance among all other stack strategies *except* for linkstack.
On our machine, only `fib` was clearly not at a 1.0 speed-up (at 1.06); for all
other programs the confidence interval overlaps with 1.0 and the GMEAN was 1.01.
Same story for linkstack's real plot: while a few more are above/below 1.0, the
GMEAN was 1.00.


###### 5.2.3 Cache Locality

The data from Table 2 comes from the MEAN values computed in the following
plots:

```
normal/toy_perf_L1-dcache-load-misses.pdf    ==> recursive
normal/tail_perf_L1-dcache-load-misses.pdf   ==> looping
normal/real_perf_L1-dcache-load-misses.pdf   ==> mixed
normal/cont_perf_L1-dcache-load-misses.pdf   ==> continuation
```


### Step 3: Finished

If you've reached this point in the README, you're done evaluating the main
claims of the paper that are supported by the benchmarks!
