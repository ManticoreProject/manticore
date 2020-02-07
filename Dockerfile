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
     && rm -rf /var/lib/apt/lists/*


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
RUN ./llvm/fresh-build.sh \
    && cd /usr/bin \
    && ln -s /usr/pmlc/llvm/build/bin/* ./

# build manticore
RUN autoheader -Iconfig \
    && autoconf -Iconfig \
    && ./configure --with-llvm=./llvm/build \
    && make local-install \
    && cd /usr/bin \
    && ln -s /usr/pmlc/bin/* ./
