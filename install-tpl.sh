#! /usr/bin/env bash

# Text color variables
if [[ $TERM != *"xterm"* ]]; then
    export TERM=dumb
fi
txtred=$(tput setaf 1)    # Red
txtgrn=$(tput setaf 2)    # Green
txtylw=$(tput setaf 3)    # Yellow
txtblu=$(tput setaf 4)    # Blue
txtpur=$(tput setaf 5)    # Purple
txtcyn=$(tput setaf 6)    # Cyan
txtwht=$(tput setaf 7)    # White
txtrst=$(tput sgr0)       # Text reset

# Which compiler to use?
export COMPILER=${COMPILER:-gnu}

function check_valid_yes_no()
{
    local var=$1
    if ! [ "${!var}" == "YES" ] && ! [ "${!var}" == "NO" ]; then
    echo "${txtred}Invalid value for $var (${!var}) -- Must be YES or NO${txtrst}"
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

# Shared libraries or static libraries?
SHARED=${SHARED:-YES}
check_valid_yes_no SHARED

# Enable Burst-Buffer support in PnetCDF?
BB=${BB:-NO}
check_valid_yes_no BB

CRAY=${CRAY:-NO}

# Which TPLS? (HDF5 and NetCDF always, PnetCDF if MPI=ON)
CGNS=${CGNS:-ON}
MATIO=${MATIO:-ON}
GNU_PARALLEL=${GNU_PARALLEL:-ON}
NEEDS_ZLIB=${NEEDS_ZLIB:-NO}
H5VERSION=${H5VERSION:-V110}

SUDO=${SUDO:-}
JOBS=${JOBS:-2}
VERBOSE=${VERBOSE:-1}

if [ "${USE_PROXY}" == "YES" ]
then
    export http_proxy="http://wwwproxy.sandia.gov:80"
    export https_proxy="https://wwwproxy.sandia.gov:80"
fi

pwd
export ACCESS=`pwd`
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

OS=`uname -s`
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
	echo "   USE_PROXY    = ${USE_PROXY}"
	echo ""
	echo "   H5VERSION    = ${H5VERSION}"
	echo "   CGNS         = ${CGNS}"
	echo "   MATIO        = ${MATIO}"
	echo "   GNU_PARALLEL = ${GNU_PARALLEL}"
	echo "   NEEDS_ZLIB   = ${NEEDS_ZLIB}"
	echo "   BB           = ${BB}"
	echo ""
	echo "   SUDO         = ${SUDO}"
	echo "   JOBS         = ${JOBS}"
	echo "   VERBOSE      = ${VERBOSE}"
	echo "${txtrst}"
	exit 0
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
	    if [[ "$SHARED" == "ON" || "$SHARED" == "YES" ]]
	    then
		USE_SHARED=""
	    else
		USE_SHARED="--static"
	    fi
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
    if [ "${H5VERSION}" == "V18" ]
    then
	hdf_version="1.8.20"
    else
	hdf_version="1.10.4"
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
        CRAY=${CRAY} H5VERSION=${H5VERSION} SHARED=${SHARED} NEEDS_ZLIB=${NEEDS_ZLIB} MPI=${MPI} bash ../runconfigure.sh
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
        pnet_version="1.11.0"
	pnet_base="pnetcdf"
	#      pnet_version="1.10.0"
	#      pnet_base="parallel-netcdf"
        cd $ACCESS
        cd TPL/pnetcdf
        if [ "$DOWNLOAD" == "YES" ]
        then
	    echo "${txtgrn}+++ Downloading...${txtrst}"
            rm -rf ${pnet_base}-${pnet_version}
            rm -f ${pnet_base}-${pnet_version}.tar.gz
            wget https://parallel-netcdf.github.io/Release/${pnet_base}-${pnet_version}.tar.gz
            tar -xzf ${pnet_base}-${pnet_version}.tar.gz
            rm -f ${pnet_base}-${pnet_version}.tar.gz
        fi

        if [ "$BUILD" == "YES" ]
        then
	    echo "${txtgrn}+++ Configuring, Building, and Installing...${txtrst}"
            cd ${pnet_base}-${pnet_version}
            CRAY=${CRAY} BB=${BB} SHARED=${SHARED} bash ../runconfigure.sh
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
        if [ -d build ]
        then
            rm -rf build
        fi
        mkdir build
        cd build
        CRAY=${CRAY} SHARED=${SHARED} NEEDS_ZLIB=${NEEDS_ZLIB} MPI=${MPI} bash -x ../../runcmake.sh
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
	    if [ "$MPI" == "ON" ]
	    then
		cd CGNS
		git am ../CGNS-parallel.patch
		if [[ $? != 0 ]]
		then
		    echo 1>&2 ${txtred}couldn\'t patch CGNS for SNL parallel changes. exiting.${txtrst}
		    exit 1
		fi
		cd ..
	    fi
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
            CRAY=${CRAY} SHARED=${SHARED} NEEDS_ZLIB=${NEEDS_ZLIB} MPI=${MPI} bash ../../runcmake.sh
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

# =================== INSTALL MATIO  ===============
if [ "$MATIO" == "ON" ]
then
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
            CRAY=${CRAY} SHARED=${SHARED} bash ../runconfigure.sh
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
