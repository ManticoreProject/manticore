# Manticore

[![pipeline status](https://gitlab.com/kavon1/manticore/badges/stacks/pipeline.svg)](https://gitlab.com/kavon1/manticore/commits/stacks)

[Manticore](http://manticore.cs.uchicago.edu) is a high-level parallel programming language aimed at general-purpose applications running on multi-core processors. Manticore supports parallelism at multiple levels: explicit concurrency and coarse-grain parallelism via CML-style constructs and fine-grain parallelism via various light-weight notations, such as parallel tuple expressions and NESL/Nepal-style parallel array comprehensions.

Manticore currently only supports the x86-64 (a.k.a. AMD64)
architecture running on either Linux or Mac OS X. It is possible to
build the compiler on other systems (see below), but we have not
ported the runtime or code generator to them yet.

## Getting Started (Linux)

### Docker

To build, run

```console
$ docker build .
```

The compiler will be available in the Docker image as `pmlc` in PATH:

```console
$ docker run -it image-name
root@image-name# pmlc -version
pmlc [x86_64-linux; 0.0.0 (); built 2020-02-07]
```

You can run the regression suite (~1 hour) to make sure everything's okay with:
```console
$ docker run -w /usr/pmlc image-name ./run_ci.sh local
```

#### Benchmarking

The benchmark suite currently requires Linux's `perf stat`, which itself requires that you run the Docker image with additional permissions. Before getting started, make sure that `perf stat` is working:

```console
$ docker run -it --cap-add sys_admin image-name
root@image-name# perf stat echo

 Performance counter stats for 'echo':
 ...
```

If instead you see a message stating `WARNING: perf not found for kernel X`, then
that means the Docker image you've obtained was built on a Linux system with
a different kernel version than yours.
This can be fixed for the currently-running image by running:

```console
root@image-name# apt-get update && apt-get install linux-tools-`uname -r`
```

Once you've confirmed `perf stat echo` works, run the following:

```console
root@image-name:/usr/pmlc# \
  cd src/benchmarks/drivers/
root@image-name:/usr/pmlc/src/benchmarks/drivers# \
  mkdir results
root@image-name:/usr/pmlc/src/benchmarks/drivers# \
  PATH=/usr/smlnj64/bin:$PATH ./pldi20.sh /usr/pmlc `pwd`/results 5
```


### Local Install

Check out the `Dockerfile` to find out what packages are needed and how to build the compiler. TODO: explain it here.
