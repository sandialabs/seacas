#! /usr/bin/env bash

# Text color variables
if [[ $TERM != *"xterm"* ]]; then
    export TERM=dumb
fi
txtred=$(tput setaf 1)    # Red
txtgrn=$(tput setaf 2)    # Green
txtylw=$(tput setaf 3)    # Yellow
#txtblu=$(tput setaf 4)    # Blue
#txtpur=$(tput setaf 5)    # Purple
txtcyn=$(tput setaf 6)    # Cyan
#txtwht=$(tput setaf 7)    # White
txtrst=$(tput sgr0)       # Text reset

# Which compiler to use?
export COMPILER=${COMPILER:-gnu}

function check_exec()
{
    local var=$1
    command -v ${var} >/dev/null 2>&1 || { echo >&2 "${txtred}---${var} is required, but is not currently in path.  Aborting TPL Install.${txtrst}"; exit 1; }
}

function check_valid_yes_no()
{
    local var=$1
    if ! [ "${!var}" == "YES" ] && ! [ "${!var}" == "NO" ]; then
    echo "${txtred}Invalid value for $var (${!var}) -- Must be YES or NO${txtrst}"
    exit 1
fi
}

function check_valid_on_off()
{
    local var=$1
    if ! [ "${!var}" == "ON" ] && ! [ "${!var}" == "OFF" ]; then
    echo "${txtred}Invalid value for $var (${!var}) -- Must be ON or OFF${txtrst}"
    exit 1
fi
}

#By default, download and then install.
DOWNLOAD=${DOWNLOAD:-YES}
check_valid_yes_no DOWNLOAD

BUILD=${BUILD:-YES}
check_valid_yes_no BUILD

# Force downloading and installation even if the TPL already exists in lib/include
FORCE=${FORCE:-NO}
check_valid_yes_no FORCE

DEBUG=${DEBUG:-NO}
check_valid_yes_no DEBUG

# Shared libraries or static libraries?
SHARED=${SHARED:-YES}
check_valid_yes_no SHARED

# Enable Burst-Buffer support in PnetCDF?
BB=${BB:-NO}
check_valid_yes_no BB

CRAY=${CRAY:-OFF}
check_valid_on_off CRAY

# Which TPLS? (HDF5 and NetCDF always, PnetCDF if MPI=ON)
CGNS=${CGNS:-ON}
check_valid_on_off CGNS

MATIO=${MATIO:-ON}
check_valid_on_off MATIO

METIS=${METIS:-OFF}
check_valid_on_off METIS

PARMETIS=${PARMETIS:-OFF}
check_valid_on_off PARMETIS

GNU_PARALLEL=${GNU_PARALLEL:-ON}
check_valid_on_off GNU_PARALLEL

NEEDS_ZLIB=${NEEDS_ZLIB:-NO}
check_valid_yes_no NEEDS_ZLIB

NEEDS_SZIP=${NEEDS_SZIP:-NO}
check_valid_yes_no NEEDS_SZIP

KOKKOS=${KOKKOS:-OFF}
check_valid_on_off KOKKOS

H5VERSION=${H5VERSION:-V110}
ADIOS2=${ADIOS2:-OFF}
check_valid_on_off ADIOS2

GTEST=${GTEST:-OFF}

MPI=${MPI:-OFF}
check_valid_on_off MPI

SUDO=${SUDO:-}
JOBS=${JOBS:-2}
VERBOSE=${VERBOSE:-1}
USE_PROXY=${USE_PROXY:-NO}
check_valid_yes_no USE_PROXY

if [ "${USE_PROXY}" == "YES" ]
then
    export http_proxy="http://wwwproxy.sandia.gov:80"
    export https_proxy="https://wwwproxy.sandia.gov:80"
fi

pwd
export ACCESS=$(pwd)
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

if [ "$MPI" == "ON" ] && [ "$CRAY" == "ON" ]
then
    CC=cc; export CC
    CFLAGS=-static; export CFLAGS
    SHARED=NO
elif [ "$MPI" == "ON" ]
then
    CC=mpicc; export CC
fi

OS=$(uname -s)
if [ "$SHARED" == "YES" ]
then
    if [ "$OS" == "Darwin" ] ; then
	LD_EXT="dylib"
    else
	LD_EXT="so"
    fi
else
    LD_EXT="a"
fi

if [ $# -gt 0 ]; then
    if [ "$1" == "--help" ]; then
	echo "${txtcyn}Environment Variables used in the script and their default values:"
	echo ""
	echo "   ACCESS       = ${txtgrn}${ACCESS}${txtcyn} (Automatically set to current directory)"
	echo "   OS           = ${txtgrn}${OS}${txtcyn} (Automatically set)"
	echo "   COMPILER     = ${COMPILER}  (gnu clang intel ibm)"
	echo "   MPI          = ${MPI} (Parallel Build?)"
	echo ""
	echo "   FORCE        = ${FORCE}"
	echo "   DOWNLOAD     = ${DOWNLOAD}"
	echo "   BUILD        = ${BUILD}"
	echo "   SHARED       = ${SHARED}"
	echo "   DEBUG        = ${DEBUG}"
	echo "   USE_PROXY    = ${USE_PROXY}"
	echo ""
	echo "   H5VERSION    = ${H5VERSION}"
	echo "   CGNS         = ${CGNS}"
	echo "   MATIO        = ${MATIO}"
	echo "   GNU_PARALLEL = ${GNU_PARALLEL}"
	echo "   NEEDS_ZLIB   = ${NEEDS_ZLIB}"
	echo "   NEEDS_SZIP   = ${NEEDS_SZIP}"
	echo "   KOKKOS       = ${KOKKOS}"
	echo "   BB           = ${BB}"
	echo "   ADIOS2       = ${ADIOS2}"
	echo "   GTEST        = ${GTEST}"
	echo ""
	echo "   SUDO         = ${SUDO}"
	echo "   JOBS         = ${JOBS}"
	echo "   VERBOSE      = ${VERBOSE}"
	echo "${txtrst}"
	exit 0
    fi
fi

# Check that cmake, git, wget exist at the beginning instead of erroring out later on...
check_exec cmake
check_exec git
check_exec wget

if [ "$NEEDS_SZIP" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libsz.${LD_EXT} ]
    then
	echo "${txtgrn}+++ SZIP${txtrst}"
        szip_version="2.1.1"

	cd $ACCESS
	cd TPL
	if [ "$DOWNLOAD" == "YES" ]
	then
	    echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf szip-${szip_version}
            rm -rf szip-${szip_version}.tar.gz
            wget --no-check-certificate https://support.hdfgroup.org/ftp/lib-external/szip/2.1.1/src/szip-${szip_version}.tar.gz
            tar -xzf szip-${szip_version}.tar.gz
            rm -rf szip-${szip_version}.tar.gz
	fi

	if [ "$BUILD" == "YES" ]
	then
	    echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd szip-${szip_version}
            ./configure --prefix=${INSTALL_PATH}
            if [[ $? != 0 ]]
            then
		echo 1>&2 ${txtred}couldn\'t configure szip. exiting.${txtrst}
		exit 1
            fi
            make -j${JOBS} && ${SUDO} make install
            if [[ $? != 0 ]]
            then
		echo 1>&2 ${txtred}couldn\'t build szip. exiting.${txtrst}
		exit 1
            fi
	fi
    else
	echo "${txtylw}+++ SZIP already installed.  Skipping download and installation.${txtrst}"
    fi
fi


if [ "$NEEDS_ZLIB" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libz.${LD_EXT} ]
    then
	echo "${txtgrn}+++ ZLIB${txtrst}"
        zlib_version="1.2.11"

	cd $ACCESS
	cd TPL
	if [ "$DOWNLOAD" == "YES" ]
	then
	    echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf zlib-${zlib_version}
            rm -rf zlib-${zlib_version}.tar.gz
            wget --no-check-certificate https://zlib.net/zlib-${zlib_version}.tar.gz
            tar -xzf zlib-${zlib_version}.tar.gz
            rm -rf zlib-${zlib_version}.tar.gz
	fi

	if [ "$BUILD" == "YES" ]
	then
	    echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd zlib-${zlib_version}
            ./configure --prefix=${INSTALL_PATH}
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
	echo "${txtylw}+++ ZLIB already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== BUILD HDF5 ===============
if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libhdf5.${LD_EXT} ]
then
    echo "${txtgrn}+++ HDF5${txtrst}"
    if [ "${H5VERSION}" == "V18" ]; then
	hdf_version="1.8.21"
    elif [ "${H5VERSION}" == "V110" ]; then
	hdf_version="1.10.6"
    elif [ "${H5VERSION}" == "V112" ]; then
	hdf_version="1.12.0"
    else
	echo 1>&2 ${txtred}Invalid HDF5 version specified: ${H5VERSION}.  Must be one of V18, V110, V112. exiting.${txtrst}
	exit 1
    fi

    cd $ACCESS
    cd TPL/hdf5
    if [ "$DOWNLOAD" == "YES" ]
    then
	echo "${txtgrn}+++ Downloading...${txtrst}"
        rm -rf hdf5-${hdf_version}
        rm -f hdf5-${hdf_version}.tar.bz2
	if [ "${H5VERSION}" == "V18" ]
	then
	    wget --no-check-certificate https://support.hdfgroup.org/ftp/HDF5/current18/src/hdf5-${hdf_version}.tar.bz2
	else
	    wget --no-check-certificate https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-${hdf_version}/src/hdf5-${hdf_version}.tar.bz2
	fi
        tar -jxf hdf5-${hdf_version}.tar.bz2
        rm -f hdf5-${hdf_version}.tar.bz2
    fi

    if [ "$BUILD" == "YES" ]
    then
	echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
        cd hdf5-${hdf_version}
        CRAY=${CRAY} H5VERSION=${H5VERSION} DEBUG=${DEBUG} SHARED=${SHARED} NEEDS_ZLIB=${NEEDS_ZLIB} NEEDS_SZIP=${NEEDS_SZIP} MPI=${MPI} bash ../runconfigure.sh
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
    echo "${txtylw}+++ HDF5 already installed.  Skipping download and installation.${txtrst}"
fi
# =================== INSTALL PnetCDF if parallel build ===============
if [ "$MPI" == "ON" ]
then
    # PnetCDF currently only builds static library...
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libpnetcdf.a ]
    then
        echo "${txtgrn}+++ PnetCDF${txtrst}"
        pnet_version="1.12.1"
	pnet_base="pnetcdf"
        cd $ACCESS
        cd TPL/pnetcdf
        if [ "$DOWNLOAD" == "YES" ]
        then
	    echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf ${pnet_base}-${pnet_version}
            rm -f ${pnet_base}-${pnet_version}.tar.gz
            wget --no-check-certificate https://parallel-netcdf.github.io/Release/${pnet_base}-${pnet_version}.tar.gz
            tar -xzf ${pnet_base}-${pnet_version}.tar.gz
            rm -f ${pnet_base}-${pnet_version}.tar.gz
        fi

        if [ "$BUILD" == "YES" ]
        then
	    echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd ${pnet_base}-${pnet_version}
            CRAY=${CRAY} BB=${BB} DEBUG=${DEBUG} SHARED=${SHARED} bash ../runconfigure.sh
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
	echo "${txtylw}+++ PnetCDF already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== INSTALL NETCDF ===============
if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libnetcdf.${LD_EXT} ]
then
    echo "${txtgrn}+++ NetCDF${txtrst}"
    cd $ACCESS
    cd TPL/netcdf
    if [ "$DOWNLOAD" == "YES" ]
    then
	echo "${txtgrn}+++ Downloading...${txtrst}"
        rm -rf netcdf-c
        git clone https://github.com/Unidata/netcdf-c netcdf-c
    fi

    if [ "$BUILD" == "YES" ]
    then
	echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
        cd netcdf-c
	git checkout v4.7.4
        if [ -d build ]
        then
            rm -rf build
        fi
        mkdir build
        cd build
        CRAY=${CRAY} SHARED=${SHARED} DEBUG=${DEBUG} NEEDS_ZLIB=${NEEDS_ZLIB} MPI=${MPI} bash -x ../../runcmake.sh
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
    echo "${txtylw}+++ NetCDF already installed.  Skipping download and installation.${txtrst}"
fi
# =================== INSTALL CGNS ===============
if [ "$CGNS" == "ON" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libcgns.${LD_EXT} ]
    then
        echo "${txtgrn}+++ CGNS${txtrst}"
        cd $ACCESS
        cd TPL/cgns
        if [ "$DOWNLOAD" == "YES" ]
        then
	    echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf CGNS
            git clone https://github.com/cgns/CGNS
	fi

        if [ "$BUILD" == "YES" ]
        then
	    echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd CGNS
            if ! [ -d build ]
            then
                mkdir build
            fi
            cd build
            CRAY=${CRAY} SHARED=${SHARED} DEBUG=${DEBUG} NEEDS_ZLIB=${NEEDS_ZLIB} MPI=${MPI} bash ../../runcmake.sh
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
	echo "${txtylw}+++ CGNS already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== INSTALL METIS  ===============
if [ "$METIS" == "ON" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libmetis.a ]
    then
	echo "${txtgrn}+++ Metis${txtrst}"
	cd $ACCESS
	cd TPL/metis
	if [ "$DOWNLOAD" == "YES" ]
	then
	    echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf metis-5.1.0
            rm -f metis-5.1.0.tar.gz
            wget --no-check-certificate https://github.com/scivision/METIS/raw/master/metis-5.1.0.tar.gz
	    tar zxvf metis-5.1.0.tar.gz
	fi

	if [ "$BUILD" == "YES" ]
	then
	    echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd metis-5.1.0
	    sed 's/TYPEWIDTH 32/TYPEWIDTH 64/' include/metis.h > tmp
	    mv tmp include/metis.h
            CRAY=${CRAY} SHARED=${SHARED} DEBUG=${DEBUG} bash ../runconfigure.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure Metis. exiting.${txtrst}
                exit 1
            fi
            make -j${JOBS} && ${SUDO} make install
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t build Metis. exiting.${txtrst}
                exit 1
            fi
	fi
    else
	echo "${txtylw}+++ Metis already installed.  Skipping download and installation.${txtrst}"
    fi
fi


# =================== INSTALL PARMETIS  ===============
if [ "$PARMETIS" == "ON" ] && [ "$MPI" == "ON" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libparmetis.a ]
    then
	echo "${txtgrn}+++ ParMETIS${txtrst}"
	cd $ACCESS
	cd TPL/parmetis
	if [ "$DOWNLOAD" == "YES" ]
	then
	    echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf parmetis
	    git clone https://github.com/gsjaardema/parmetis
	fi

	if [ "$BUILD" == "YES" ]
	then
	    echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd parmetis
            CRAY=${CRAY} MPI=${MPI} SHARED=${SHARED} DEBUG=${DEBUG} bash ../runconfigure.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure ParMETIS. exiting.${txtrst}
                exit 1
            fi
            make -j${JOBS} && ${SUDO} make install
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t build ParMETIS. exiting.${txtrst}
                exit 1
            fi
	fi
    else
	echo "${txtylw}+++ ParMETIS already installed.  Skipping download and installation.${txtrst}"
    fi
fi


# =================== INSTALL MATIO  ===============
if [ "$MATIO" == "ON" ]
then
    # Check that aclocal, automake, autoconf exist...
    check_exec aclocal
    check_exec automake
    check_exec autoconf

    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libmatio.${LD_EXT} ]
    then
	echo "${txtgrn}+++ MatIO${txtrst}"
	cd $ACCESS
	cd TPL/matio
	if [ "$DOWNLOAD" == "YES" ]
	then
	    echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf matio
            git clone https://github.com/tbeu/matio.git
	fi

	if [ "$BUILD" == "YES" ]
	then
	    echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd matio
            ./autogen.sh
            CRAY=${CRAY} SHARED=${SHARED} DEBUG=${DEBUG} bash ../runconfigure.sh
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
	echo "${txtylw}+++ MatIO already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== INSTALL KOKKOS  ===============
if [ "$KOKKOS" == "ON" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libkokkos.${LD_EXT} ]
    then
	kokkos_version="2.8.00"
	echo "${txtgrn}+++ KOKKOS${txtrst}"
	cd $ACCESS
	cd TPL/kokkos
	if [ "$DOWNLOAD" == "YES" ]
	then
	    echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf kokkos
	    wget --no-check-certificate https://github.com/kokkos/kokkos/archive/${kokkos_version}.tar.gz
            tar -zxf ${kokkos_version}.tar.gz
            rm -f ${kokkos_version}.tar.gz
	    ln -s kokkos-${kokkos_version} kokkos
	fi

	if [ "$BUILD" == "YES" ]
	then
	    echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd kokkos-${kokkos_version}
            if [ -d build ]
            then
                rm -rf build
            fi
            mkdir build
            cd build
            CUDA=${CUDA} SHARED=${SHARED} DEBUG=${DEBUG} MPI=${MPI} bash ../../runcmake.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure cmake for KOKKOS. exiting.${txtrst}
                exit 1
            fi

            make -j${JOBS} && ${SUDO} make "VERBOSE=${VERBOSE}" install
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t build KOKKOS. exiting.${txtrst}
                exit 1
            fi
	fi
    else
	echo "${txtylw}+++ KOKKOS already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== INSTALL ADIOS2  ===============
if [ "$ADIOS2" == "ON" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libadios2.${LD_EXT} ]
    then
        echo "${txtgrn}+++ ADIOS2${txtrst}"
        cd $ACCESS
        cd TPL/adios2
        if [ "$DOWNLOAD" == "YES" ]
        then
	    echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf ADIOS2
            git clone https://github.com/ornladios/ADIOS2.git
        fi

        if [ "$BUILD" == "YES" ]
        then
	    echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd ADIOS2
	    git checkout v2.5.0
            if [ -d build ]
            then
                rm -rf build
            fi
            mkdir build
            cd build
            SHARED=${SHARED} MPI=${MPI} DEBUG=${DEBUG} bash -x ../../runcmake.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure cmake for ADIOS2. exiting.${txtrst}
                exit 1
            fi

            make -j${JOBS} && ${SUDO} make "VERBOSE=${VERBOSE}" install
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t build ADIOS2. exiting.${txtrst}
                exit 1
            fi
        fi
    else
        echo "${txtylw}+++ ADIOS2 already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== INSTALL gtest  ===============
if [ "$GTEST" == "ON" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libgtest.${LD_EXT} ]
    then
        echo "${txtgrn}+++ gtest${txtrst}"
        cd $ACCESS
        cd TPL/gtest
        if [ "$DOWNLOAD" == "YES" ]
        then
	    echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf gtest
            git clone https://github.com/google/googletest.git
        fi

        if [ "$BUILD" == "YES" ]
        then
	    echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd googletest
	    git checkout release-1.8.1
            if [ -d build ]
            then
                rm -rf build
            fi
            mkdir build
            cd build
            SHARED=${SHARED} DEBUG=${DEBUG} bash -x ../../runcmake.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure cmake for gtest. exiting.${txtrst}
                exit 1
            fi

            make -j${JOBS} && ${SUDO} make "VERBOSE=${VERBOSE}" install
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t build gtest. exiting.${txtrst}
                exit 1
            fi
        fi
    else
        echo "${txtylw}+++ gtest already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== INSTALL PARALLEL  ===============
if [ "$GNU_PARALLEL" == "ON" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/bin/env_parallel ]
    then
	echo "${txtgrn}+++ GNU Parallel${txtrst}"
        cd $ACCESS
        cd TPL/parallel
        if [ "$DOWNLOAD" == "YES" ]
        then
	    echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf parallel-*
            wget --no-check-certificate ftp://ftp.gnu.org/gnu/parallel/parallel-latest.tar.bz2
            tar -jxf parallel-latest.tar.bz2
            rm -rf parallel-latest.tar.bz2
        fi

        if [ "$BUILD" == "YES" ]
        then
	    echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
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
	echo "${txtylw}+++ Parallel already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# ==================================
cd $ACCESS
#ls -l include
#ls -l bin
#ls -l lib
