FROM ubuntu:22.04
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
        libaec-dev zlib1g-dev automake autoconf \
        libcurl4-openssl-dev libjpeg-dev wget \
        curl bzip2 m4 flex bison cmake libzip-dev \
        openmpi-bin libopenmpi-dev \
        build-essential gfortran git libx11-dev \
        python3.10 python3-dev python3-mpi4py python3-pip && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.10 1
RUN git clone --depth 1 https://github.com/sandialabs/seacas.git
RUN pwd
RUN cd seacas/ && \
    ./install-tpl.sh && \
    cd ../
RUN cd seacas/ && \
    mkdir build && \
    cd build && \
    ../cmake-config && \
    make && \
    make install && \
    cd ../../
RUN python -m pip install numpy==1.23.1
ENV PATH "${PATH}:/seacas/bin/"
ENV PYTHONPATH "${PYTHONPATH}:/seacas/lib/"
ENV LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:/seacas/lib/"
CMD ["/bin/bash"]
