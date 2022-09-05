FROM ubuntu:22.04
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
        automake=1:1.16.5-1.3 \
        autoconf=2.71-2 \
        bison=2:3.8.2+dfsg-1build1 \
        build-essential=12.9ubuntu3 \
        bzip2=1.0.8-5build1 \
        cmake=3.22.1-1ubuntu1 \
        curl=7.81.0-1ubuntu1.4 \
        flex=2.6.4-8build2 \
        gfortran=4:11.2.0-1ubuntu1 \
        git=1:2.34.1-1ubuntu1.4 \
        libaec-dev=1.0.6-1 \
        libcurl4-openssl-dev=7.81.0-1ubuntu1.4 \
        libjpeg-dev=8c-2ubuntu10 \
        libopenmpi-dev=4.1.2-2ubuntu1 \
        libx11-dev=2:1.7.5-1 \
        libzip-dev=1.7.3-1ubuntu2 \
        m4=1.4.18-5ubuntu2 \
        openmpi-bin=4.1.2-2ubuntu1 \
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
RUN ../cmake-exodus && \
    make && \
    make install
ENV PATH "${PATH}:/seacas/bin/"
ENV PYTHONPATH "${PYTHONPATH}:/seacas/lib/"
ENV LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:/seacas/lib/"
CMD ["/bin/bash"]
