#! /usr/bin/env bash

# so how do I make this work?
# I want it to keep a build if what you need already exists.
export COMPILER=${COMPILER:-gnu}
SUDO=${SUDO:-}
CGNS=${CGNS:-ON}
MATIO=${MATIO:-ON}
GNU_PARALLEL=${GNU_PARALLEL:-ON}
JOBS=${JOBS:-2}
ACCESS=`pwd`
VERBOSE=${VERBOSE:-1}
pwd

NEEDS_ZLIB=${NEEDS_ZLIB:-NO}

DOWNLOAD=${DOWNLOAD:-YES}
INSTALL=${INSTALL:-YES}

SHARED=${SHARED:-YES}
# Need to think about if we're going to make for a static or a shared environment.
if [ "$MPI" == "ON" ] && [ "$CRAY" == "ON" ]
then
    CC=cc; export CC
    CFLAGS=-static; export CFLAGS
    STATIC=YES
elif [ "$MPI" == "ON" ]
then
    CC=/usr/bin/mpicc; export CC
fi

if [ "$NEEDS_ZLIB" == "YES" ]
then
    cd $ACCESS
    cd TPL
    if [ "$DOWNLOAD" == "YES" ]
    then
        rm -rf zlib-1.2.11.tar.gz
        wget --no-check-certificate https://zlib.net/zlib-1.2.11.tar.gz
    fi

    if [ "$INSTALL" == "YES" ]
    then
        tar -xzf zlib-1.2.11.tar.gz
        cd zlib-1.2.11
        ./configure --prefix=${ACCESS}
        make -j${JOBS} && ${SUDO} make install
    fi
fi

# =================== INSTALL HDF5 ===============
if ([ "$SHARED" == "NO" ] && ! [ -e $ACCESS/lib/libhdf5.a ]) && ! [ -e $ACCESS/lib/libhdf5.so ]
then
    echo Installing HDF5
    hdf_version="1.10.2"

    cd $ACCESS
    cd TPL/hdf5
    if [ "$DOWNLOAD" == "YES" ]
    then
        rm -f hdf5-${hdf_version}.tar.bz2
        wget --no-check-certificate https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-${hdf_version}/src/hdf5-${hdf_version}.tar.bz2
    fi

    if [ "$INSTALL" == "YES" ]
    then
        tar -jxf hdf5-${hdf_version}.tar.bz2
        cd hdf5-${hdf_version}
        NEEDS_ZLIB=${NEEDS_ZLIB} MPI=${MPI} bash ../runconfigure.sh
        if [[ $? != 0 ]]
        then
            echo 1>&2 couldn\'t configure hdf5. exiting.
            exit 1
        fi
        make -j${JOBS} && ${SUDO} make "V=${VERBOSE}" install
        if [[ $? != 0 ]]
        then
            echo 1>&2 couldn\'t make hdf5. exiting.
            exit 1
        fi
    fi
fi
# =================== INSTALL PNETCDF (if mpi) ===============
if [ "$MPI" == "ON" ]
then
    if ([ "$SHARED" == "NO" ] && ! [ -e $ACCESS/lib/libpnetcdf.a ]) && ! [ -e $ACCESS/lib/libpnetcdf.so ]
    then
        echo Installing PNetCDF
        #    pnet_version="1.8.1"
        pnet_version="1.9.0"

        cd $ACCESS
        cd TPL/pnetcdf
        if [ "$DOWNLOAD" == "YES" ]
        then
            rm -f parallel-netcdf-${pnet_version}.tar.gz
            wget http://cucis.ece.northwestern.edu/projects/PnetCDF/Release/parallel-netcdf-${pnet_version}.tar.gz
        fi

        if [ "$INSTALL" == "YES" ]
        then
            tar -xzf parallel-netcdf-${pnet_version}.tar.gz
            cd parallel-netcdf-${pnet_version}
            bash ../runconfigure.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 couldn\'t configure pnetcdf. exiting.
                exit 1
            fi


            if [ "$CRAY" == "ON" ]
            then
                make -j${JOBS} LDFLAGS=-all-static && ${SUDO} make install
            else
                make -j${JOBS} && ${SUDO} make install
            fi
            if [[ $? != 0 ]]
            then
                echo 1>&2 couldn\'t make pnetcdf. exiting.
                exit 1
            fi
        fi
    fi
fi

# =================== INSTALL NETCDF ===============
if ([ "$SHARED" == "NO" ] && ! [ -e $ACCESS/lib/libnetcdf.a ]) && ! [ -e $ACCESS/lib/libnetcdf.so ]
then
    echo Installing NetCDF
    cd $ACCESS
    cd TPL/netcdf
    if [ "$DOWNLOAD" == "YES" ]
    then
        rm -rf netcdf-c
        git clone https://github.com/Unidata/netcdf-c netcdf-c
    fi

    if [ "$INSTALL" == "YES" ]
    then
        cd netcdf-c
        if [ -d build ]
        then
            rm -rf build
        fi
        mkdir build
        cd build
        NEEDS_ZLIB=${NEEDS_ZLIB} MPI=${MPI} bash ../../runcmake.sh
        if [[ $? != 0 ]]
        then
            echo 1>&2 couldn\'t configure cmake for netcdf. exiting.
            exit 1
        fi

        make -j${JOBS} && ${SUDO} make "VERBOSE=${VERBOSE}" install
        if [[ $? != 0 ]]
        then
            echo 1>&2 couldn\'t configure make netcdf. exiting.
            exit 1
        fi
    fi
fi
# =================== INSTALL CGNS ===============
if [ "$CGNS" == "ON" ]
then
    if ([ "$SHARED" == "NO" ] && ! [ -e $ACCESS/lib/libcgns.a ]) && ! [ -e $ACCESS/lib/libcgns.so ]
    then
        echo Installing CGNS
        cd $ACCESS
        cd TPL/cgns
        if [ "$DOWNLOAD" == "YES" ]
        then
            rm -rf CGNS
            git clone https://github.com/cgns/CGNS
        fi

        if [ "$INSTALL" == "YES" ]
        then
            cd CGNS
            if ! [ -d build ]
            then
                mkdir build
            fi
            cd build
            MPI=${MPI} bash ../../runcmake.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 couldn\'t configure CGNS. exiting.
                exit 1
            fi

            make -j${JOBS} && ${SUDO} make install
        fi
    fi
fi

# =================== INSTALL MATIO  ===============
if [ "$MATIO" == "ON" ]
then

    cd $ACCESS
    cd TPL/matio
    if [ "$DOWNLOAD" == "YES" ]
    then
        rm -rf matio
        git clone https://github.com/tbeu/matio.git
    fi

    if [ "$INSTALL" == "YES" ]
    then
        cd matio
        ./autogen.sh
        bash ../runconfigure.sh
        make -j${JOBS} && ${SUDO} make install
    fi
fi

# =================== INSTALL PARALLEL  ===============
if [ "$GNU_PARALLEL" == "ON" ]
then
    if ! [ -e $ACCESS/bin/env_parallel ]
    then
        cd $ACCESS
        cd TPL/parallel
        if [ "$DOWNLOAD" == "YES" ]
        then
            rm -rf parallel-*
            wget --no-check-certificate ftp://ftp.gnu.org/gnu/parallel/parallel-latest.tar.bz2
        fi

        if [ "$INSTALL" == "YES" ]
        then
            tar -jxf parallel-latest.tar.bz2
            cd parallel-*
            bash ../runconfigure.sh
            make -j${JOBS} && ${SUDO} make install
        fi
    fi
fi

# ==================================
cd $ACCESS
ls -l include
ls -l bin
ls -l lib
