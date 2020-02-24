# Manticore

[![pipeline status](https://gitlab.com/kavon1/manticore/badges/stacks/pipeline.svg)](https://gitlab.com/kavon1/manticore/commits/stacks)

[Manticore](http://manticore.cs.uchicago.edu) is a high-level parallel
programming language aimed at general-purpose applications running on multi-core
processors. Manticore supports parallelism at multiple levels: explicit
concurrency and coarse-grain parallelism via CML-style constructs and fine-grain
parallelism via various light-weight notations, such as parallel tuple
expressions and NESL/Nepal-style parallel array comprehensions.

Manticore currently only supports the x86-64 (a.k.a. AMD64)
architecture running on either Linux or Mac OS X. It is possible to
build the compiler on other systems (see below), but we have not
ported the runtime or code generator to them yet.

## PLDI'20 Artifact Evaluation

In order to evaluate the system for PLDI'20 artifact evaluation, you must have
access to a Linux system with [Docker installed](https://docs.docker.com/install/).

The Linux requirement is due to our use of Linux's `perf` for profiling in our
benchmark suite, which will probably *not* work if you are running Docker on any
other OS because it is not Unix compatible.

You can test if Docker is working with:

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


### Step 2 (Optional): Play with the image

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

TODO: describe where source code is and an example of compiling a simple
hello-world program with different stack strategies.

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
In this new window, find the active container ID corresponding to the session that finished running the benchmark suite,

```console
$ docker ps
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES
<container-id>      <image-name>        "/bin/bash"         11 hours ago        Up 11 hours                             reverent_brattain
```

and use that `container-id` to copy the results directory out of the container like so:

```console
$ docker cp container-id:/usr/pmlc/results .
```

### Step 4: Interpreting the results

TODO
