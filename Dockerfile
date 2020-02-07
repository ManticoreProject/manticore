FROM ubuntu:16.04

# grab all dependencies
RUN dpkg --add-architecture i386 \
    && apt-get update \
    && apt-get install --no-install-recommends -y \
       # for 32-bit SML/NJ download & build
       libc6:i386 gcc-multilib g++ g++-multilib \
       \
       # ordinary manticore dependencies
       wget \
       gcc \
       autoconf \
       automake \
       make \
       libjemalloc-dev \
       \
       # llvm dependencies
       cmake \
       zlib1g \
       python2.7 \
     && rm -rf /var/lib/apt/lists/*

# download SML/NJ
RUN mkdir -p /usr/smlnj \
    && cd /usr/smlnj \
    && wget http://smlnj.cs.uchicago.edu/dist/working/110.96/config.tgz \
    && tar xf config.tgz \
    && rm config.tgz

# build/install SML/NJ:
RUN cd /usr/smlnj \
    && ./config/install.sh -default 32 \
    && cd /usr/bin \
    && ln -s /usr/smlnj/bin/* ./

# copy source code to the image
COPY . /usr/pmlc

# set the working directory
WORKDIR /usr/pmlc

# build LLVM
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
