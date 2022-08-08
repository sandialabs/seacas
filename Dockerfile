FROM ubuntu:22.04
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
        automake \
        autoconf \
        bison \
        build-essential \
        bzip2 \
        cmake \
        curl \
        flex \
        gfortran \
        git \
        libaec-dev \
        libcurl4-openssl-dev \
        libjpeg-dev \
        libopenmpi-dev \
        libx11-dev \
        libzip-dev \
        m4 \
        openmpi-bin \
        python3.10=3.10.4-3ubuntu0.1 \
        python3-dev=3.10.4-0ubuntu2 \
        python3-mpi4py=3.1.3-1build2 \
        python3-numpy=1:1.21.5-1build2 \
        python3-pip=22.0.2+dfsg-1 \
        wget=1.21.2-2ubuntu1 \
        zlib1g-dev=1:1.2.11.dfsg-2ubuntu9 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.10 1
RUN git clone --depth 1 https://github.com/sandialabs/seacas.git
WORKDIR /seacas
RUN ./install-tpl.sh
RUN mkdir build
WORKDIR /seacas/build
RUN ../cmake-config && \
    make && \
    make install
ENV PATH "${PATH}:/seacas/bin/"
ENV PYTHONPATH "${PYTHONPATH}:/seacas/lib/"
ENV LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:/seacas/lib/"
CMD ["/bin/bash"]
