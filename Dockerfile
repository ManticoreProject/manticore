FROM ubuntu:16.04

# grab all dependencies
RUN dpkg --add-architecture i386 \
    && apt-get update \
    && apt-get install --no-install-recommends -y \
       # for SML/NJ download & build
       libc6:i386 gcc-multilib g++ g++-multilib wget \
       \
       # ordinary manticore dependencies
       gcc \
       autoconf \
       automake \
       make \
       libjemalloc-dev \
       python \
       \
       # manticore plotting dependencies
       python3 \
       python3-setuptools \
       python3-dev \
       python3-pip \
       \
       # llvm dependencies
       cmake \
       zlib1g \
       \
       # manticore benchmark suite dependencies
       valgrind \
       ca-certificates \
       git \
       protobuf-compiler \
       linux-tools-`uname -r` \
       \
       # MLton dependencies
       libgmp-dev \
       \
    && rm -rf /var/lib/apt/lists/* \
    && pip3 install --upgrade pip \
    && pip3 install --no-cache-dir \
       seaborn numpy matplotlib pandas click


# download SML/NJ
RUN mkdir -p /usr/smlnj \
    mkdir -p /usr/smlnj64 \
    && cd /usr \
    && wget --no-check-certificate https://smlnj.cs.uchicago.edu/dist/working/110.96/config.tgz \
    && cp config.tgz smlnj \
    && mv config.tgz smlnj64


# build & install SML/NJ 32-bit to user's default path to build manticore.
# it's assumed that the SML/NJ config.tgz is already in the directory.
RUN cd /usr/smlnj \
    && tar xf config.tgz \
    && rm config.tgz \
    && ./config/install.sh -default 32 \
    && cd /usr/bin \
    && ln -s /usr/smlnj/bin/* ./


# SML/NJ 64-bit build only, not added to user's path!
# this is needed for a fair benchmarking comparison.
RUN cd /usr/smlnj64 \
    && tar xf config.tgz \
    && rm config.tgz \
    && ./config/install.sh -default 64


# obtain and install MLton. it needs lib / bin to be in the expected places
RUN cd /usr \
    && wget https://sourceforge.net/projects/mlton/files/mlton/20180207/mlton-20180207-1.amd64-linux.tgz \
    && tar xzf mlton-20180207-1.amd64-linux.tgz \
    && rm mlton-20180207-1.amd64-linux.tgz \
    && mv mlton-20180207-1.amd64-linux mlton \
    && mv mlton/bin/* /usr/bin \
    && mv mlton/lib/* /usr/lib \
    && rm -rf ./mlton


# build/install bloaty:
RUN cd /usr \
    && git clone https://github.com/google/bloaty.git \
    && cd bloaty \
    && cmake . \
    && make bloaty -j4 \
    && cd /usr/bin \
    && ln -s /usr/bloaty/bloaty ./


# copy source code to the image
COPY . /usr/pmlc

# set the working directory
WORKDIR /usr/pmlc

# build LLVM (with asserts on for testing, since compile time is not important)
RUN ./llvm/fresh-build.sh docker \
    # && rm -rf llvm/build llvm/src \  # we leave LLVM sources alone for now
    && cd /usr/bin \
    && ln -s /usr/pmlc/llvm/install/bin/* ./

# build manticore
RUN autoheader -Iconfig \
    && autoconf -Iconfig \
    && ./configure --with-llvm=./llvm/install \
    && make local-install \
    && cd /usr/bin \
    && ln -s /usr/pmlc/bin/* ./


# HACK: the benchmark scripts use git-log to record the revision of the
# benchmark dir, but we can't simply copy the src/benchmarks/.git dir
# because git doesn't seem to support that.
# thus instead replace 'git' with something that
# just spits out some text and uninstall the real git, since we no longer
# need git at this point anyway and can save ~20MB
RUN apt-get autoremove -y git \
    && echo '#!/bin/sh\necho "docker image"\n' > /usr/bin/git \
    && chmod +x /usr/bin/git
