# Manticore PLDI'20 Artifact Evaluation

[![pipeline status](https://gitlab.com/kavon1/manticore/badges/stacks/pipeline.svg)](https://gitlab.com/kavon1/manticore/commits/stacks)

## Getting Started Guide

**NOTE**: This README is best viewed on GitHub here: https://github.com/ManticoreProject/manticore/blob/stacks/README.md

In order to evaluate the system for PLDI'20 artifact evaluation, you must first
have access to an x86-64 Linux system with [Docker installed](https://docs.docker.com/install/).

The Linux requirement is due to our use of Linux's `perf` for profiling in our
benchmark suite, which will probably *not* work if you are running Docker on any
other OS because `perf` is not Unix compatible.

Once you believe you have Docker installed, you can test if Docker is working
on your system with:

```console
$ docker run hello-world
```

Throughout this guide, terminal shell prompts that begin with `$` refer to your
system's native shell process, and prompts starting with `root@container:CURRENT_PATH#` refer
to the Docker container's shell process.

### Obtaining the Docker image

##### Method 1: Use the pre-built image

The recommended way to obtain the latest version of the Docker image is to run

```console
$ docker pull registry.gitlab.com/kavon1/manticore:latest
```

For the remainder of this README, wherever `image-name` appears, you will
use `registry.gitlab.com/kavon1/manticore:latest`.
To make sure you have the latest version, run the `docker pull` command above
prior to running the image.

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




### Testing the Docker image

To "kick the tires" of the image to make sure things are working, the
following command launches a container based on the image with an interactive
prompt:

```console
$ docker run -it --cap-add sys_admin image-name
root@container:/usr/pmlc#
```

The text editors `vim` and `emacs` are provided in the image for your
convenience.
The Manticore compiler is available in the image as `pmlc` in PATH:

```console
root@container:/usr/pmlc# pmlc -version
pmlc [x86_64-linux; 0.0.0 (); built 2020-02-07]
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

Then launch a specific installation of SML/NJ and make sure it says "64-bit" when launched:

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
that you run the Docker image with additional permissions (hence the `--cap-add sys_admin` flag).
Make sure that `perf` is working with the following expected output:

```console
root@container:~# perf stat echo

 Performance counter stats for 'echo':
 ...
```

If you see a message starting with `No permission to enable task-clock event.`,
then you forgot to add `--cap-add sys_admin` to your `docker run` command
(use `CTRL+D` to exit the session and try again).

Otherwise if you see a message starting with`WARNING: perf not found for kernel X`,
then that means the Docker image you've obtained was built on a Linux system with
a different kernel version than yours (this is quite likely to happen!).
This can be fixed for the currently-running container by running:

```console
root@container:~# apt-get update && apt-get install -y linux-tools-`uname -r`
```

**Please note** that you'll need to run the above command **every time** you run
the Docker image, because the image is not modified after it's launched, i.e.,
modifications within the container instance initialized with an image are not
preserved.


If time permits, we suggest that you also run the compiler's regression suite
(~1 hour) to make sure everything's okay with:

```console
root@container:~# cd /usr/pmlc
root@container:/usr/pmlc# ./run_ci.sh local
```

Otherwise, if you've reached this point then you've successfully
"kicked the tires" of this artifact!



## Background Information

In this section, we provide basic information about the compiler and source
code contained in the Docker image for reusability purposes.
This information is not needed to reproduce the results of the paper, but it is
useful for future extensions of the system.

Relative to `/usr/pmlc`, the sources in the image are organized as follows:

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

Without additional arguments, `pmlc` will compile a program with the standard
runtime system and basis library using the CPS strategy.
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





## Step-by-Step Evaluation Instructions

In this section we describe how to evaluate the artifact with respect to the paper.

### Step 1: Run the benchmark suite

**Optional**: If you're connecting to a machine over SSH, [consider running Docker
within `screen`](https://www.linode.com/docs/networking/ssh/using-gnu-screen-to-manage-persistent-terminal-sessions/)
to keep the Docker session alive even if the connection is
interrupted.

##### Step 1a: Launch the container and check `perf`.

First, launch the Docker container with the right permissions and make
sure `perf stat echo` works:

```console
$ docker run -it --cap-add sys_admin image-name
root@container:/usr/pmlc# perf stat echo

 Performance counter stats for 'echo':
 ...
```

If you run into problems with `perf`, try the following:

```console
root@container:/usr/pmlc# apt-get update && apt-get install -y linux-tools-`uname -r`
```

If you don't know the `image-name` or have other issues with `perf`,
please see the Getting Started guide.


##### Step 1b: Run the script.

Next, we have an all-in-one script that runs the benchmark suite and generates plots
from the paper (which will take 7-8 hours to finish):

```console
root@container:/usr/pmlc# ./run_cont_bench.sh
```

##### Step 1c: Copy the results out of the Docker container.

Once the script completes, **keep the interactive Docker session running**.
You'll need to find the ID associated with that active container.
Normally the `container-id` is the hash-code between the `@` and `:` in the
shell prompt of the container.

Alternatively, you can find the `container-id` by opening a new terminal prompt
on your system and run `docker ps`, looking in the "CONTAINER ID" column.

Once you have the `container-id`, [copy the results directory](https://docs.docker.com/engine/reference/commandline/cp/)
out of the container and into the real file system like so:

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
we prefer to should be opened with a plain-text editor.
Any files that contain a timestamp in their filename have `DATE` written there
instead.

Also, please double-check the files you open as you explore the results,
since the file names and paths are all quite similar! :)



##### Section 5.1.1 -- Recursion Performance

We go in order of the claims made in this section and how to check them against
the results gathered in Step 1.


- RECHECK_THIS: Figure 3 corresponds with the plot `normal/cross_toy_times.pdf`,
and covers the discussion on lines 747--761. Note that in the camera-ready
version the corrected number of `fib` calls is 331 million per iteration.


- Claims in lines 765--808 can be checked in the following way:

  1. Percent of time spent in the garbage collector for `ack` is in plot
  `gcstats/analyze_toy_gc_time_total_pct.pdf`.

  2. The volume of data allocated for **closure-based stacks** (aka "cps") for
  `ack` should be checked manually in the file `gcstats/seq-ack/seq-ack-cps-mc-seq-DATE.json`.
  The field `minorgc-alloc` corresponds to the number of bytes allocated in
  the nursery. Divide by 2^30 = 1073741824 to get GiB.

  3. The percentage of data promoted from the **nursery to the major heap** is
  in the plot `gcstats/analyze_toy_gc_minor_live_pct.pdf`

  4. The percentage of data promoted from the **major heap to the global heap**
  is in `gcstats/analyze_toy_gc_major_live_pct.pdf`.

  5. The volume of data allocated for **linked-frame stacks** for `ack` is
  in the file `gcstats/seq-ack/seq-ack-linkstack-mc-seq-DATE.json`. Look for the
  `minorgc-alloc` value and compare it with check (2)'s value to find the
  difference.


- Figure 4 corresponds with the plot `normal/toy_perf_L1-dcache-load-misses.pdf`
  and supports the implicit claim on lines 810--812 that closure and linked stacks
  have notably higher data-read miss rates than other stacks on a number of
  programs.


- On lines 813--820, the discussion of "high number of segment-overflow events"
between segmented and resizing stacks for `ack` and `quicksort` are supported by
examining the `stackcache-access` values (subtracting by 2 for the fixed number
of accesses related to runtime system initialization) in the following comparisons:
    ```
    For 'ack':
    gcstats/seq-ack/seq-ack-segstack-mc-seq-DATE.json
                         vs
    gcstats/seq-ack/seq-ack-resizestack-mc-seq-DATE.json

    For 'quicksort':
    gcstats/seq-quicksort/seq-quicksort-segstack-mc-seq-DATE.json
                         vs
    gcstats/seq-quicksort/seq-quicksort-resizestack-mc-seq-DATE.json
    ```


- Figure 5 corresponds with the plot `normal/cross_tail_times.pdf` and supports
the discussion on lines 821--859.


- RECHECK_THIS: Figure 6 corresponds with the plot `normal/cross_real_times.pdf`
and supports the discussion on lines 872--914. Part of that discussion references
data cache miss-rates for the "real" programs in `normal/real_perf_L1-dcache-load-misses.pdf`.



##### Section 5.1.2 -- Escape Continuation Performance

Figure 7 corresponds with the plot `normal/crossCont_cont_times.pdf` and
supports the discussion on lines 917--927. The artifact contains an optimization
we made to the implementation of contig stacks since the paper submission, which
causes its performance on `cml-pingpong` to improve (and thus the speed-ups
relative to it are reduced):

  | strategy | old speed-up | new speed-up |
  |----------|--------------|--------------|
  | resize   | 1.24         | 1.14         |
  | segment  | 1.24         | 1.13         |
  | linked   | 1.30         | 1.11         |
  | cps      | 1.45         | 1.24         |

The discussion regarding Figure 7 will be updated accordingly.



##### Section 5.1.3 -- Design Trade-offs


###### Foreign Function Calls

The data for Foreign Function Calls are plotted in a number of files, with
one file per stack strategy.
The claims made on lines 1026--1029 are about the min-max ranges for
both `ffi-fib` and `ffi-trigfib` and are shown in these plots:

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


###### Non-native Stack on x86-64

> We found that for contig, resize, and segment stacks, using the
> native CPU instructions yields a 1.06× – 1.08× speedup for
> toy programs, [...]

This min-max claim can be checked by inspecting the GMEAN values in the
following plots, which show the speed-up of using the native stack instructions:

```
normal/ras_contig_toy_times.pdf
normal/ras_resizestack_toy_times.pdf
normal/ras_segstack_toy_times.pdf
```

> [...] a 1.01--1.03x speedup for real programs, [...]

This claim is checked like the previous one, but using these plots instead:

```
normal/ras_contig_real_times.pdf
normal/ras_resizestack_real_times.pdf
normal/ras_segstack_real_times.pdf
```

> [...] and a 1.04x speedup overall.

This claim is checked in the following way. Gather the three GEOMEAN values
from the following oversized plots that contain all non-callec benchmarks:

```
normal/ras_TOTAL_contig_all_times.pdf
normal/ras_TOTAL_resizestack_all_times.pdf
normal/ras_TOTAL_segstack_all_times.pdf
```

Then compute the average of those three values and round.
We had (1.04 + 1.04 + 1.03) / 3 = 1.0366667 -> 1.04x overall speedup.


> Linked stacks saw effectively no change in performance even
> though the implementation uses call and return.

This claim can be checked by inspecting the GMEAN values in the following plots:

```
normal/ras_linkstack_toy_times.pdf
normal/ras_linkstack_real_times.pdf
```

You're looking to see that nearly all programs hover around 1.0 with little
variation.
Particular focus should be placed on linkstack's toy plot, which had the highest
variance among all other stack strategies *except* for linkstack.
On our machine, only `fib` was clearly not at a 1.0 speed-up (at 1.07); for all
other programs the confidence interval overlaps with 1.0 and the GMEAN was 1.01.
Same story for linkstack's real plot: while a few more are above/below 1.0, the
GMEAN was 1.00.


### Step 3: Finished

If you've reached this point in the README, you're done!
Thanks for volunteering your time! :)
