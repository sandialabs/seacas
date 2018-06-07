#! /usr/bin/env bash

# Which compiler to use?
export COMPILER=${COMPILER:-gnu}

#By default, download and then install.
DOWNLOAD=${DOWNLOAD:-YES}
INSTALL=${INSTALL:-YES}

# Shared libraries or static libraries?
SHARED=${SHARED:-YES}

# Which TPLS? (HDF5 and NetCDF always, PNetCDF if MPI=ON)
CGNS=${CGNS:-ON}
MATIO=${MATIO:-ON}
GNU_PARALLEL=${GNU_PARALLEL:-ON}
NEEDS_ZLIB=${NEEDS_ZLIB:-NO}

SUDO=${SUDO:-}
JOBS=${JOBS:-2}
VERBOSE=${VERBOSE:-1}

pwd

export ACCESS=`pwd`

# Text color variables
txtred=$(tput setaf 1)    # Red
txtgrn=$(tput setaf 2)    # Green
txtrst=$(tput sgr0)       # Text reset

if [ "$MPI" == "ON" ] && [ "$CRAY" == "ON" ]
then
    CC=cc; export CC
    CFLAGS=-static; export CFLAGS
    SHARED=NO
elif [ "$MPI" == "ON" ]
then
    CC=/usr/bin/mpicc; export CC
fi

if [ "$SHARED" == "YES" ]
then
    OS=`uname -s`
    if [ "$OS" == "Darwin" ] ; then
	LD_EXT="dylib"
    else
	LD_EXT="so"
    fi
else
    LD_EXT="a"
fi

if [ "$NEEDS_ZLIB" == "YES" ]
then
    if ! [ -e $ACCESS/lib/libz.${LD_EXT} ]
    then
	echo "${txtgrn}+++ Installing ZLIB${txtrst}"
					 
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
	    if [[ "$SHARED" == "ON" || "$SHARED" == "YES" ]]
	    then
		USE_SHARED=""
	    else
		USE_SHARED="--static"
	    fi
            ./configure --prefix=${ACCESS}
            if [[ $? != 0 ]]
            then
		echo 1>&2 ${txtred}couldn\'t configure zlib. exiting.${txtrst}
		exit 1
            fi
            make -j${JOBS} && ${SUDO} make install
            if [[ $? != 0 ]]
            then
		echo 1>&2 ${txtred}couldn\'t build zlib. exiting.${txtrst}
		exit 1
            fi
	fi
    else
	echo "${txtgrn}+++ ZLIB already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== INSTALL HDF5 ===============
if ! [ -e $ACCESS/lib/libhdf5.${LD_EXT} ]
then
    echo "${txtgrn}+++ Installing HDF5${txtrst}"
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
        SHARED=${SHARED} NEEDS_ZLIB=${NEEDS_ZLIB} MPI=${MPI} bash ../runconfigure.sh
        if [[ $? != 0 ]]
        then
            echo 1>&2 ${txtred}couldn\'t configure hdf5. exiting.${txtrst}
            exit 1
        fi
        make -j${JOBS} && ${SUDO} make "V=${VERBOSE}" install
        if [[ $? != 0 ]]
        then
            echo 1>&2 ${txtred}couldn\'t build hdf5. exiting.${txtrst}
            exit 1
        fi
    fi
else
    echo "${txtgrn}+++ HDF5 already installed.  Skipping download and installation.${txtrst}"
fi
# =================== INSTALL PNETCDF if parallel build ===============
if [ "$MPI" == "ON" ]
then
    # PnetCDF currently only builds static library...
    if ! [ -e $ACCESS/lib/libpnetcdf.a ]
    then
        echo "${txtgrn}+++ Installing PNetCDF${txtrst}"
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
            SHARED=${SHARED} bash ../runconfigure.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure PnetCDF. exiting.${txtrst}
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
                echo 1>&2 ${txtred}couldn\'t build PnetCDF. exiting.${txtrst}
                exit 1
            fi
        fi
    else
	echo "${txtgrn}+++ PNetCDF already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== INSTALL NETCDF ===============
if ! [ -e $ACCESS/lib/libnetcdf.${LD_EXT} ]
then
    echo "${txtgrn}+++ Installing NetCDF${txtrst}"
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
        SHARED=${SHARED} NEEDS_ZLIB=${NEEDS_ZLIB} MPI=${MPI} bash ../../runcmake.sh
        if [[ $? != 0 ]]
        then
            echo 1>&2 ${txtred}couldn\'t configure cmake for NetCDF. exiting.${txtrst}
            exit 1
        fi

        make -j${JOBS} && ${SUDO} make "VERBOSE=${VERBOSE}" install
        if [[ $? != 0 ]]
        then
            echo 1>&2 ${txtred}couldn\'t build NetCDF. exiting.${txtrst}
            exit 1
        fi
    fi
else
    echo "${txtgrn}+++ NetCDF already installed.  Skipping download and installation.${txtrst}"
fi
# =================== INSTALL CGNS ===============
if [ "$CGNS" == "ON" ]
then
    if ! [ -e $ACCESS/lib/libcgns.${LD_EXT} ]
    then
        echo "${txtgrn}+++ Installing CGNS${txtrst}"
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
            SHARED=${SHARED} NEEDS_ZLIB=${NEEDS_ZLIB} MPI=${MPI} bash ../../runcmake.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure CGNS. exiting.${txtrst}
                exit 1
            fi

            make -j${JOBS} && ${SUDO} make install
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t build CGNS. exiting.${txtrst}
                exit 1
            fi
        fi
    else
	echo "${txtgrn}+++ CGNS already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== INSTALL MATIO  ===============
if [ "$MATIO" == "ON" ]
then
    if ! [ -e $ACCESS/lib/libmatio.${LD_EXT} ]
    then
	echo "${txtgrn}+++ Installing MatIO${txtrst}"
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
            SHARED=${SHARED} bash ../runconfigure.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure MatIO. exiting.${txtrst}
                exit 1
            fi
            make -j${JOBS} && ${SUDO} make install
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t build MatIO. exiting.${txtrst}
                exit 1
            fi
	fi
    else
	echo "${txtgrn}+++ MatIO already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== INSTALL PARALLEL  ===============
if [ "$GNU_PARALLEL" == "ON" ]
then
    if ! [ -e $ACCESS/bin/env_parallel ]
    then
	echo "${txtgrn}+++ Installing Parallel${txtrst}"
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
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure parallel. exiting.${txtrst}
                exit 1
            fi
            make -j${JOBS} && ${SUDO} make install
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t build parallel. exiting.${txtrst}
                exit 1
            fi
        fi
    else
	echo "${txtgrn}+++ Parallel already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# ==================================
cd $ACCESS
#ls -l include
#ls -l bin
#ls -l lib
