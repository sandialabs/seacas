FROM ubuntu:24.04@sha256:80dd3c3b9c6cecb9f1667e9290b3bc61b78c2678c02cbdae5f0fea92cc6734ab
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
	python3.10 \
        python3-dev \
        python3-mpi4py \
        python3-numpy \
        python3-pip \
        wget \
        zlib1g-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.12 1
RUN git clone --depth 1 https://github.com/sandialabs/seacas.git
WORKDIR /seacas
RUN ./install-tpl.sh
RUN mkdir build
WORKDIR /seacas/build
RUN ../cmake-exodus && \
    make && \
    make install
ENV PATH "${PATH}:/seacas/bin/"
ENV PYTHONPATH "${PYTHONPATH}:/seacas/lib/"
ENV LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:/seacas/lib/"
CMD ["/bin/bash"]
