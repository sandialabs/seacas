FROM ubuntu:20.04
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y \
        libaec-dev zlib1g-dev automake autoconf \
        libcurl4-openssl-dev libjpeg-dev wget \
        curl bzip2 m4 flex bison cmake libzip-dev \
        openmpi-bin libopenmpi-dev \
        git libx11-dev
RUN git clone --depth 1 https://github.com/sandialabs/seacas.git
ENV NEEDS_ZLIB YES
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
RUN python -m pip install numpy
ENV PYTHONPATH "${PYTHONPATH}:/seacas/lib/"
CMD ["/bin/bash"]
