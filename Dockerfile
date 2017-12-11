# contributed by Ryan Newton

FROM ubuntu:16.04

RUN apt-get update -y && apt-get install -y wget gcc autoconf automake make

RUN dpkg --add-architecture i386 && \
    apt-get install -y gcc-multilib libc6-dev-i386 libc6-i386

# Ubuntu version of SML/NJ has problems.  Fetch the main distribution:
RUN mkdir -p /usr/smlnj && cd /usr/smlnj && \
    wget http://smlnj.cs.uchicago.edu/dist/working/110.81/config.tgz && \
    tar xf config.tgz && rm config.tgz

# Build/install SML/NJ:
RUN cd /usr/smlnj && ./config/install.sh && \
    cd /usr/bin && ln -s /usr/smlnj/bin/* ./ && \
    echo

ADD . /usr/pmlc

RUN cd /usr/pmlc && \
    autoheader -Iconfig && \
    autoconf -Iconfig && \
    ./configure && \
    make local-install && \
    cd /usr/bin && ln -s /usr/pmlc/bin/* ./
