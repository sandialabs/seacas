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
export ACCESS=$(pwd)
. ${ACCESS}/TPL/compiler.sh

function check_exec()
{
    local var=$1
    command -v ${var} >/dev/null 2>&1 || { echo >&2 "${txtred}---${var} is required, but is not currently in path.  Aborting TPL Install.${txtrst}"; exit 1; }
}

function check_valid()
{
    if [ "${!1}" == "YES" ] || [ "${!1}" == "ON" ]; then
        echo "YES"
        return 1
    fi
    if [ "${!1}" == "NO" ] || [ "${!1}" == "OFF" ]; then
        echo "NO"
        return 1
    fi
    echo "${txtred}Invalid value for $1 (${!1}) -- Must be ON, YES, NO, or OFF${txtrst}"
    exit 1
}

#By default, download and then install.
DOWNLOAD=${DOWNLOAD:-YES}
DOWNLOAD=$(check_valid DOWNLOAD)

BUILD=${BUILD:-YES}
BUILD=$(check_valid BUILD)

# Force downloading and installation even if the TPL already exists in lib/include
FORCE=${FORCE:-NO}
FORCE=$(check_valid FORCE)

DEBUG=${DEBUG:-NO}
DEBUG=$(check_valid DEBUG)

# Shared libraries or static libraries?
# For CRAY, must explicitly specify SHARED=YES, or it will default to NO
# All other platforms default to SHARED=YES
if [ "$CRAY" == "YES" ]
then
    SHARED="${SHARED:-NO}"
else
    SHARED="${SHARED:-YES}"
fi
SHARED=$(check_valid SHARED)

# Enable Burst-Buffer support in PnetCDF?
BB=${BB:-NO}
BB=$(check_valid BB)

CRAY=${CRAY:-NO}
CRAY=$(check_valid CRAY)

# Which TPLS? (HDF5 and NetCDF always, PnetCDF if MPI=ON)
CGNS=${CGNS:-YES}
CGNS=$(check_valid CGNS)

MATIO=${MATIO:-YES}
MATIO=$(check_valid MATIO)

PARMETIS=${PARMETIS:-NO}
PARMETIS=$(check_valid PARMETIS)

METIS=${METIS:-NO}
METIS=$(check_valid METIS)

if [ "$PARMETIS" == "YES" ]
then
    METIS="YES"
fi

FMT=${FMT:-YES}
FMT=$(check_valid FMT)

GNU_PARALLEL=${GNU_PARALLEL:-YES}
GNU_PARALLEL=$(check_valid GNU_PARALLEL)

USE_ZLIB_NG=${USE_ZLIB_NG:-NO}
USE_ZLIB_NG=$(check_valid USE_ZLIB_NG)

if [ "${USE_ZLIB_NG}" == "YES" ]
then
    export NEEDS_ZLIB="YES"
fi

NEEDS_ZLIB=${NEEDS_ZLIB:-NO}
NEEDS_ZLIB=$(check_valid NEEDS_ZLIB)

NEEDS_SZIP=${NEEDS_SZIP:-NO}
NEEDS_SZIP=$(check_valid NEEDS_SZIP)

USE_AEC=${USE_AEC:-NO}
USE_AEC=$(check_valid USE_AEC)

KOKKOS=${KOKKOS:-NO}
KOKKOS=$(check_valid KOKKOS)

H5VERSION=${H5VERSION:-V114}

FAODEL=${FAODEL:-NO}
FAODEL=$(check_valid FAODEL)

ADIOS2=${ADIOS2:-NO}
ADIOS2=$(check_valid ADIOS2)

CATALYST2=${CATALYST2:-NO}
CATALYST2=$(check_valid CATALYST2)

GTEST=${GTEST:-NO}
GTEST=$(check_valid GTEST)

MPI=${MPI:-NO}
MPI=$(check_valid MPI)

SUDO=${SUDO:-}
JOBS=${JOBS:-2}
VERBOSE=${VERBOSE:-1}

USE_PROXY=${USE_PROXY:-NO}
USE_PROXY=$(check_valid USE_PROXY)

if [ "${USE_PROXY}" == "YES" ]
then
    export http_proxy="http://proxy.sandia.gov:80"
    export https_proxy="https://proxy.sandia.gov:80"
fi

pwd
INSTALL_PATH=${INSTALL_PATH:-${ACCESS}}

if [ "$MPI" == "YES" ] && [ "$CRAY" == "YES" ]
then
    CC=cc; export CC
    CFLAGS=-static; export CFLAGS
    CXX=CC; export CXX
    CXXFLAGS=-static; export CXXFLAGS
elif [ "$MPI" == "YES" ]
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
        echo "   INSTALL_PATH = ${txtgrn}${INSTALL_PATH}${txtcyn}"
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
        echo "   METIS        = ${METIS}"
        echo "   PARMETIS     = ${PARMETIS}"
        echo "   GNU_PARALLEL = ${GNU_PARALLEL}"
        echo "   FMT          = ${FMT}"
        echo "   NEEDS_ZLIB   = ${NEEDS_ZLIB}"
        echo "   USE_ZLIB_NG  = ${USE_ZLIB_NG}"
        echo "   NEEDS_SZIP   = ${NEEDS_SZIP}"
        echo "   USE_AEC      = ${USE_AEC}"
        echo "   KOKKOS       = ${KOKKOS}"
        echo "   BB           = ${BB}"
        echo "   FAODEL       = ${FAODEL}"
        echo "   ADIOS2       = ${ADIOS2}"
        echo "   CATALYST2    = ${CATALYST2}"
        echo "   GTEST        = ${GTEST}"
        echo ""
        echo "   SUDO         = ${SUDO} (empty unless need superuser permission via 'sudo')"
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
if [ "$USE_AEC" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libsz.${LD_EXT} ]
    then
        echo "${txtgrn}+++ SZIP (via libaec library)${txtrst}"
        szip_version="1.0.4"

        cd $ACCESS || exit
        cd TPL/szip || exit
        if [ "$DOWNLOAD" == "YES" ]
        then
            echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf libaec-${szip_version}
            rm -rf v-${szip_version}.tar.gz
            wget --no-check-certificate https://github.com/MathisRosenhauer/libaec/archive/v${szip_version}.tar.gz
            tar -xzf v${szip_version}.tar.gz
            rm -rf v${szip_version}.tar.gz
        fi

        if [ "$BUILD" == "YES" ]
        then
            echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd libaec-${szip_version} || exit
            mkdir build
            cd build || exit

            CRAY=${CRAY} SHARED=${SHARED} DEBUG=${DEBUG} MPI=${MPI} bash -x ../../runcmake.sh

            if [[ $? != 0 ]]
            then
                echo 1>&2 "${txtred}couldn\'t configure libaec(szip). exiting.${txtrst}"
                exit 1
            fi
            make -j${JOBS} && ${SUDO} make install
            if [[ $? != 0 ]]
            then
                echo 1>&2 "${txtred}couldn\'t build libaec(szip). exiting.${txtrst}"
                exit 1
            fi
        fi
    else
        echo "${txtylw}+++ SZIP already installed.  Skipping download and installation.${txtrst}"
    fi
else
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libsz.${LD_EXT} ]
    then
        echo "${txtgrn}+++ SZIP${txtrst}"
        szip_version="2.1.1"

        cd $ACCESS || exit
        cd TPL/szip || exit
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
            cd szip-${szip_version} || exit
            # mkdir build
            # cd build || exit

            ./configure --prefix=${INSTALL_PATH}
            # CRAY=${CRAY} SHARED=${SHARED} DEBUG=${DEBUG} MPI=${MPI} bash -x ../../runcmake.sh

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
fi

if [ "$NEEDS_ZLIB" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libz.${LD_EXT} ]
    then
        if [ "$USE_ZLIB_NG" == "YES" ]
        then
            echo "${txtgrn}+++ ZLIB-NG${txtrst}"
            zlib_ng_version="develop"

            cd $ACCESS || exit
            cd TPL || exit
            if [ "$DOWNLOAD" == "YES" ]
            then
                echo "${txtgrn}+++ Downloading...${txtrst}"
                rm -rf zlib-ng
                git clone https://github.com/zlib-ng/zlib-ng
            fi

            if [ "$BUILD" == "YES" ]
            then
                echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
                cd zlib-ng || exit
                git checkout ${zlib_ng_version}
                rm -rf build
                mkdir build
                cmake -Bbuild -DCMAKE_INSTALL_PREFIX=${INSTALL_PATH} -DZLIB_COMPAT=YES .
                if [[ $? != 0 ]]
                then
                    echo 1>&2 ${txtred}couldn\'t configure zlib-ng. exiting.${txtrst}
                    exit 1
                fi
                cmake --build build --config Release
                if [[ $? != 0 ]]
                then
                    echo 1>&2 ${txtred}couldn\'t build zlib-ng. exiting.${txtrst}
                    exit 1
                fi
                cmake --install build --config Release
            fi
        else
            echo "${txtgrn}+++ ZLIB${txtrst}"
            zlib_version="1.2.13"

            cd $ACCESS || exit
            cd TPL || exit
            if [ "$DOWNLOAD" == "YES" ]
            then
                echo "${txtgrn}+++ Downloading...${txtrst}"
                rm -rf zlib-${zlib_version}
                rm -rf zlib-${zlib_version}.tar.gz
                wget --no-check-certificate https://zlib.net/fossils/zlib-${zlib_version}.tar.gz
                tar -xzf zlib-${zlib_version}.tar.gz
                rm -rf zlib-${zlib_version}.tar.gz
            fi

            if [ "$BUILD" == "YES" ]
            then
                echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
                cd zlib-${zlib_version} || exit
                if [ "$SHARED" == "NO" ]
                then
                    USE_STATIC="--static"
                fi
                ./configure --prefix=${INSTALL_PATH} ${USE_STATIC}
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
        hdf_base="1.8"
    elif [ "${H5VERSION}" == "V110" ]; then
        hdf_version="1.10.9"
        hdf_base="1.10"
    elif [ "${H5VERSION}" == "V112" ]; then
        hdf_version="1.12.2"
        hdf_base="1.12"
    elif [ "${H5VERSION}" == "V113" ]; then
        hdf_version="1.13.1"
        hdf_base="1.13"
    elif [ "${H5VERSION}" == "V114" ]; then
        hdf_version="1.14.0"
        hdf_base="1.14"
    elif [ "${H5VERSION}" == "develop" ]; then
        hdf_version="develop"
    else
        echo 1>&2 ${txtred}Invalid HDF5 version specified: ${H5VERSION}.  Must be one of V18, V110, V112. exiting.${txtrst}
        exit 1
    fi

    cd $ACCESS || exit
    cd TPL/hdf5 || exit
    if [ "$DOWNLOAD" == "YES" ]
    then
        echo "${txtgrn}+++ Downloading...${txtrst}"
        rm -rf hdf5-${hdf_version}
        rm -f hdf5-${hdf_version}.tar.bz2
        if [ "${H5VERSION}" == "develop" ]; then
            git clone https://github.com/HDFGroup/hdf5.git hdf5-develop
        else
            wget --no-check-certificate https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-${hdf_base}/hdf5-${hdf_version}/src/hdf5-${hdf_version}.tar.bz2
        fi
        if [ "${H5VERSION}" != "develop" ]
        then
            tar -jxf hdf5-${hdf_version}.tar.bz2
            rm -f hdf5-${hdf_version}.tar.bz2
        fi
    fi

    if [ "$BUILD" == "YES" ]
    then
        echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
        cd hdf5-${hdf_version} || exit
        rm -rf build
        mkdir build
        cd build || exit
        CRAY=${CRAY} H5VERSION=${H5VERSION} DEBUG=${DEBUG} SHARED=${SHARED} NEEDS_ZLIB=${NEEDS_ZLIB} NEEDS_SZIP=${NEEDS_SZIP} MPI=${MPI} bash -x ../../runcmake.sh
        #CRAY=${CRAY} H5VERSION=${H5VERSION} DEBUG=${DEBUG} SHARED=${SHARED} NEEDS_ZLIB=${NEEDS_ZLIB} NEEDS_SZIP=${NEEDS_SZIP} MPI=${MPI} bash ../runconfigure.sh
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
    # Create default plugin directory...
    mkdir  ${INSTALL_PATH}/lib/hdf5
    mkdir  ${INSTALL_PATH}/lib/hdf5/lib
    mkdir  ${INSTALL_PATH}/lib/hdf5/lib/plugin
else
    echo "${txtylw}+++ HDF5 already installed.  Skipping download and installation.${txtrst}"
fi
# =================== INSTALL PnetCDF if parallel build ===============
if [ "$MPI" == "YES" ]
then
    # PnetCDF currently only builds static library...
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libpnetcdf.a ]
    then
        echo "${txtgrn}+++ PnetCDF${txtrst}"
        pnet_version="1.12.2"
        pnet_base="pnetcdf"
        cd $ACCESS || exit
        cd TPL/pnetcdf || exit
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
            cd ${pnet_base}-${pnet_version} || exit
            CRAY=${CRAY} BB=${BB} DEBUG=${DEBUG} SHARED=${SHARED} bash ../runconfigure.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure PnetCDF. exiting.${txtrst}
                exit 1
            fi


            if [ "$CRAY" == "YES" ]
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
    cd $ACCESS || exit
    cd TPL/netcdf || exit
    if [ "$DOWNLOAD" == "YES" ]
    then
        echo "${txtgrn}+++ Downloading...${txtrst}"
        rm -rf netcdf-c
        git clone https://github.com/Unidata/netcdf-c netcdf-c
    fi

   net_version="v4.9.0"
#   net_version="v4.8.1"
#   net_version="master"

    if [ "$BUILD" == "YES" ]
    then
        echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
        cd netcdf-c || exit
        if [ "$net_version" != "master" ]
           then
               git checkout $net_version
        fi
        rm -rf build
        mkdir build
        cd build || exit
        export HDF5_PLUGIN_PATH=${INSTALL_PATH}/lib/hdf5/lib/plugin
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
if [ "$CGNS" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libcgns.${LD_EXT} ]
    then
        echo "${txtgrn}+++ CGNS${txtrst}"
        cd $ACCESS || exit
        cd TPL/cgns || exit
        if [ "$DOWNLOAD" == "YES" ]
        then
            echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf CGNS
            git clone https://github.com/cgns/CGNS
        fi

        if [ "$BUILD" == "YES" ]
        then
            echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd CGNS || exit
            git checkout v4.3.0
            rm -rf build
            mkdir build
            cd build || exit
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
if [ "$METIS" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libmetis.a ]
    then
        echo "${txtgrn}+++ Metis${txtrst}"
        cd $ACCESS || exit
        cd TPL/metis || exit
        if [ "$DOWNLOAD" == "YES" ]
        then
            echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf metis-5.1.0
            rm -f metis-5.1.0.tar.gz
            wget --no-check-certificate https://github.com/scivision/METIS/archive/v5.1.0.1.tar.gz
            tar zxvf v5.1.0.1.tar.gz
        fi

        if [ "$BUILD" == "YES" ]
        then
            echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd METIS-5.1.0.1 || exit
            sed 's/TYPEWIDTH 32/TYPEWIDTH 64/' src/include/metis.h > tmp
            mv tmp src/include/metis.h
            INSTALL_PATH=${INSTALL_PATH} CRAY=${CRAY} SHARED=${SHARED} DEBUG=${DEBUG} bash ../runconfigure.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure Metis. exiting.${txtrst}
                exit 1
            fi
            cd build || exit; make -j${JOBS} && ${SUDO} make install
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
if [ "$PARMETIS" == "YES" ] && [ "$MPI" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libparmetis.a ]
    then
        echo "${txtgrn}+++ ParMETIS${txtrst}"
        cd $ACCESS || exit
        cd TPL/parmetis || exit
        if [ "$DOWNLOAD" == "YES" ]
        then
            echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf parmetis
            git clone https://github.com/gsjaardema/parmetis
        fi

        if [ "$BUILD" == "YES" ]
        then
            echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd parmetis || exit
            INSTALL_PATH=${INSTALL_PATH} CRAY=${CRAY} MPI=${MPI} SHARED=${SHARED} DEBUG=${DEBUG} bash ../runconfigure.sh
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
if [ "$MATIO" == "YES" ]
then
    # Check that aclocal, automake, autoconf exist...
    check_exec aclocal
    check_exec automake
    check_exec autoconf

    matio_version="1.5.23"
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libmatio.${LD_EXT} ]
    then
        echo "${txtgrn}+++ MatIO${txtrst}"
        cd $ACCESS || exit
        cd TPL/matio || exit
        if [ "$DOWNLOAD" == "YES" ]
        then
            echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf matio
            git clone https://github.com/tbeu/matio.git
        fi

        if [ "$BUILD" == "YES" ]
        then
            echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd matio || exit
            git checkout ${matio_version}
            rm -rf build
            mkdir build
            cd build || exit
            CRAY=${CRAY} SHARED=${SHARED} DEBUG=${DEBUG} NEEDS_ZLIB=${NEEDS_ZLIB} bash -x ../../runcmake.sh
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

# =================== INSTALL FMT ===============
if [ "$FMT" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/include/fmt/core.h ]
    then
        echo "${txtgrn}+++ FMT${txtrst}"
        cd $ACCESS || exit
        cd TPL/fmt || exit
        fmt_version="9.1.0"

        if [ "$DOWNLOAD" == "YES" ]
        then
            echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf fmt
            git clone https://github.com/fmtlib/fmt
        fi

        if [ "$BUILD" == "YES" ]
        then
            echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd fmt || exit
            git checkout ${fmt_version}
            rm -rf build
            mkdir build
            cd build || exit
            cmake -DCMAKE_CXX_COMPILER:FILEPATH=${CXX} -DCMAKE_INSTALL_PREFIX:PATH=${INSTALL_PATH} -DCMAKE_INSTALL_LIBDIR:PATH=lib -DFMT_TEST:BOOL=OFF -DBUILD_SHARED_LIBS=${SHARED} ..
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure FMT. exiting.${txtrst}
                exit 1
            fi

            make -j${JOBS} && ${SUDO} make install
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t build FMT. exiting.${txtrst}
                exit 1
            fi
        fi
    else
        echo "${txtylw}+++ FMT already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== INSTALL KOKKOS  ===============
if [ "$KOKKOS" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libkokkoscore.${LD_EXT} ]
    then
        kokkos_version="3.6.00"
        echo "${txtgrn}+++ KOKKOS${txtrst}"
        cd $ACCESS || exit
        cd TPL/kokkos || exit
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
            cd kokkos-${kokkos_version} || exit
            rm -rf build
            mkdir build
            cd build || exit
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
if [ "$ADIOS2" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libadios2.${LD_EXT} ]
    then
        echo "${txtgrn}+++ ADIOS2${txtrst}"
        cd $ACCESS || exit
        cd TPL/adios2 || exit
        if [ "$DOWNLOAD" == "YES" ]
        then
            echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf ADIOS2
            git clone https://github.com/ornladios/ADIOS2.git
        fi

        if [ "$BUILD" == "YES" ]
        then
            echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd ADIOS2 || exit
            git checkout v2.5.0
            rm -rf build
            mkdir build
            cd build || exit
            CRAY=${CRAY} SHARED=${SHARED} MPI=${MPI} DEBUG=${DEBUG} bash -x ../../runcmake.sh
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

# =================== INSTALL CATALYST2  ===============
if [ "$CATALYST2" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libcatalyst.${LD_EXT} ]
    then
        echo "${txtgrn}+++ Catalyst2${txtrst}"
        cd $ACCESS || exit
        cd TPL/catalyst2 || exit
        if [ "$DOWNLOAD" == "YES" ]
        then
            echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf catalyst
            git clone https://gitlab.kitware.com/paraview/catalyst.git
        fi

        if [ "$BUILD" == "YES" ]
        then
            echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd catalyst || exit
                  git checkout master #todo: a specific version
            rm -rf build
            mkdir build
            cd build || exit
            CRAY=${CRAY} SHARED=${SHARED} MPI=${MPI} DEBUG=${DEBUG} bash -x ../../runcmake.sh
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t configure cmake for Catalyst2. exiting.${txtrst}
                exit 1
            fi

            make -j${JOBS} && ${SUDO} make "VERBOSE=${VERBOSE}" install
            if [[ $? != 0 ]]
            then
                echo 1>&2 ${txtred}couldn\'t build Catalyst2. exiting.${txtrst}
                exit 1
            fi
        fi
    else
        echo "${txtylw}+++ Catalyst2 already installed.  Skipping download and installation.${txtrst}"
    fi
fi

# =================== INSTALL gtest  ===============
if [ "$GTEST" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libgtest.${LD_EXT} ]
    then
        echo "${txtgrn}+++ gtest${txtrst}"
        cd $ACCESS || exit
        cd TPL/gtest || exit
        if [ "$DOWNLOAD" == "YES" ]
        then
            echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf googletest
            git clone https://github.com/google/googletest.git
        fi

        if [ "$BUILD" == "YES" ]
        then
            echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd googletest || exit
            git checkout release-1.11.0
            rm -rf build
            mkdir build
            cd build || exit
            CRAY=${CRAY} SHARED=${SHARED} DEBUG=${DEBUG} bash -x ../../runcmake.sh
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
if [ "$GNU_PARALLEL" == "YES" ]
then
    if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/bin/env_parallel ]
    then
        echo "${txtgrn}+++ GNU Parallel${txtrst}"
        cd $ACCESS || exit
        cd TPL/parallel || exit
        if [ "$DOWNLOAD" == "YES" ]
        then
            echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf parallel-*
            wget --no-check-certificate https://ftp.gnu.org/gnu/parallel/parallel-latest.tar.bz2
            tar -jxf parallel-latest.tar.bz2
            rm -rf parallel-latest.tar.bz2
        fi

        if [ "$BUILD" == "YES" ]
        then
            echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd parallel-* || exit
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

# =================== INSTALL FAODEL ===============
if [ "$FAODEL" == "YES" ]
then
  if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/lib/libkelpie.a ]
  then
    faodel_base="faodel"
    echo "${txtgrn}+++ Faodel${txtrst}"
    cd $ACCESS || exit
    cd TPL/faodel || exit
    if [ "$DOWNLOAD" == "YES" ]
    then
      echo "${txtgrn}+++ Downloading...${txtrst}"
      rm -rf faodel*
      git clone git@github.com:faodel/faodel.git
    fi

    if [ "$BUILD" == "YES" ]
    then
      echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
      mkdir ${faodel_base}/build
      cd ${faodel_base}/build || exit
      echo "------------- ${faodel_base}"
      echo "------------- $(pwd)"
      MPI=${MPI} bash ../../runcmake.sh
      if [[ $? != 0 ]]
      then
        echo 1>&2 ${txtred}couldn\'t configure faodel. exiting.${txtrst}
        exit 1
      fi
      make -j${JOBS} && ${SUDO} make install
      if [[ $? != 0 ]]
      then
        echo 1>&2 ${txtred}couldn\'t build faodel. exiting.${txtrst}
        exit 1
      fi
    fi
  else
    echo "${txtylw}+++ Faodel already installed.  Skipping download and installation.${txtrst}"
  fi
fi

# =================== INSTALL CEREAL ===============
if [ "$FAODEL" == "YES" ]
then
  # Currently, the FAODEL backend requires cereal, so if Faodel is enabled, we'll install cereal, too.
  if [ "$FORCE" == "YES" ] || ! [ -e $INSTALL_PATH/include/cereal/archives/portable_binary.hpp ]
  then
    echo "${txtgrn}+++ Cereal${txtrst}"
    cd $ACCESS || exit
    CEREAL_DIR="TPL/cereal"
    if [ ! -d "${CEREAL_DIR}" ]; then
      mkdir ${CEREAL_DIR}
    fi
    cd ${CEREAL} || exit
    if [ "$DOWNLOAD" == "YES" ]
    then
      echo "${txtgrn}+++ Downloading...${txtrst}"
      rm -rf cereal*
      wget --no-check-certificate https://github.com/USCiLab/cereal/archive/v1.3.0.tar.gz
      tar xzf v1.3.0.tar.gz
      rm -f v1.3.0.tar.gz
      cp -R cereal-1.3.0/include/cereal $INSTALL_PATH/include/
    fi

    if [ "$BUILD" == "YES" ]
    then
      echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
    fi
  else
    echo "${txtylw}+++ Cereal already installed.  Skipping download and installation.${txtrst}"
  fi
fi

# ==================================
cd $ACCESS || exit
#ls -l include
#ls -l bin
#ls -l lib
