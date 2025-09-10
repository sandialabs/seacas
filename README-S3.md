
# Introduction

This document describes the requirements to build and use the IOSS-S3
DatabaseIO component.

In addition to the usual TPLs required for building SEACAS, the
IOSS-S3 component requires the Amazon Simple Storage Service (S3) SDK
for C++ (AWSSDK) and the Cereal serialization library.  Below are
instructions to configure and build AWSSDK and Cereal followed by
instructions to configure and build SEACAS with the IOSS-S3 component
enabled.

## Setup the Environment

First identify a place to build the AWSSDK and Cereal TPLs and SEACAS.

```
SEACAS_S3_ROOT=$HOME/seacas

SEACAS_TPL_SRC_PATH=$SEACAS_S3_ROOT/tpls/src
SEACAS_TPL_INSTALL_PATH=$SEACAS_S3_ROOT/tpls/install
mkdir -p $SEACAS_TPL_SRC_PATH
mkdir -p $SEACAS_TPL_INSTALL_PATH

SEACAS_SRC_PATH=$SEACAS_S3_ROOT/src
SEACAS_INSTALL_PATH=$SEACAS_S3_ROOT/install
mkdir -p $SEACAS_SRC_PATH
mkdir -p $SEACAS_INSTALL_PATH
```

## Installing the AWSSDK TPL

Complete build instructions for the AWSSDK TPL can be found here: 
https://docs.aws.amazon.com/sdk-for-cpp/v1/developer-guide/setup-linux.html

The IOSS-S3 component has been tested against AWSSDK tag 1.11.77.
Here is an example of building and installing the tested version.

```
cd $SEACAS_TPL_SRC_PATH
git clone --recurse-submodules https://github.com/aws/aws-sdk-cpp
cd aws-sdk-cpp
git branch branch/tag-1.11.77 1.11.77
git checkout branch/tag-1.11.77
git submodule update --recursive
mkdir build
cd build

cmake -DAUTORUN_UNIT_TESTS:BOOL=OFF -DCMAKE_INSTALL_PREFIX=$SEACAS_TPL_INSTALL_PATH ..
make install
```

## Installing the Cereal TPL

The IOSS-S3 component has been tested against Cereal v1.3.2.
Here is an example of building and installing the tested version.

```
cd $SEACAS_TPL_SRC_PATH
curl -o cereal-1.3.2.tar.gz https://codeload.github.com/USCiLab/cereal/tar.gz/refs/tags/v1.3.2
tar xf cereal-1.3.2.tar.gz
cd cereal-1.3.2
mkdir build
cd build

cmake -DBUILD_SANDBOX=OFF -DBUILD_TESTS=OFF -DSKIP_PERFORMANCE_COMPARISON=true -DCMAKE_INSTALL_PREFIX=$SEACAS_TPL_INSTALL_PATH ..
make install
```

## Building SEACAS TPLs unrelated to the IOSS-S3 component

SEACAS has many optional TPLs that may be required for your build.

For any SEACAS build, you will need libfmt.  The latest release can be
found here:

https://github.com/fmtlib/fmt/releases

It should be built using shared libraries, so it should be built like
this:

```
cd $SEACAS_TPL_SRC_PATH
curl -o fmt-11.2.0.tar.gz https://codeload.github.com/fmtlib/fmt/tar.gz/refs/tags/11.2.0
tar xf fmt-11.2.0.tar.gz
cd fmt-11.2.0
mkdir build
cd build
cmake -DBUILD_SHARED_LIBS=TRUE -DCMAKE_INSTALL_PREFIX=$SEACAS_TPL_INSTALL_PATH ..
make install
```

In order to test the IOSS-S3 component, you may want to build the
IOSS-Exodus component.  IOSS-Exodus requires the NetCDF TPL.  It is
important that the correct NetCDF installation is found so it must be
spcified explicitly by setting the NETCDF_ROOT variable.

There are other optional TPLs depending on the enabled features.  The
`cmake-s3` script referenced below expects to find these TPLs in the
directory specified by TPL_INSTALL.

## Building SEACAS with S3

The `cmake-s3` script will discover the available TPLs and execute
cmake to configure SEACAS with the IOSS-S3 and IOSS-Exodus components
enabled.

`cmake-s3` uses the full path to the nc-config utility to ensure that
it finds the correct NetCDF on your system.  In the example below, we
assume that NetCDF and all other TPLs not installed for S3 are in
`/usr`, so both NETCDF_ROOT and TPL_INSTALL are set to `/usr`.

```
cd $SEACAS_SRC_PATH
git clone https://github.com/sandialabs/seacas.git
cd seacas
mkdir build
cd build

NETCDF_ROOT=/usr \
TPL_INSTALL=/usr \
CEREAL_ROOT=$SEACAS_TPL_INSTALL_PATH \
FMT_PATH=$SEACAS_TPL_INSTALL_PATH \
AWSSDK_DIR=$SEACAS_TPL_INSTALL_PATH \
INSTALL_PATH=$SEACAS_INSTALL_PATH \
bash ../cmake-s3
make install
```

## Passing Parameters to the IOSS-S3 component

The IOSS-S3 component has several parameters that control its
behavior.  They are descibed in the table below.

| Environment Variable | Values          | Default | Description |
|----------------------|:---------------:|:-------:|-------------|
| IOSS_S3_ENDPOINT             | URI of the S3 object store           | None    | This parameter is required and does not have a default. |
| IOSS_S3_PROFILE              | name of the S3 credential profile    | default |  |
| IOSS_S3_CA_FILE              | path to a certificate authority file | None    | Only required with your S3 object store requires it. |
| IOSS_S3_USE_CA_FILE          | enable the CA file                   | False   |  |
| IOSS_S3_USE_TRANSFER_MANAGER | enable the transfer manager          | False   | Enable to transfer large data. |
| IOSS_S3_ENABLE_AWS_TRACING   | enable debug in AWSSDK               | False   | Caution: the output is very verbose. |
| IOSS_S3_DISABLE_EC2_LOOKUP   | disable an EC2 lookup at startup     | False   | Useful for on-prem object stores. |

## Example: copy an Exodus file to S3 and back

A simple way to verify your IOSS-S3 installation is to copy an Exodus
file into your S3 object store, copy it back out to disk and then
compare them.  The io_shell tool can be used to copy data in and out
of S3.  In this example, the data is Exodus data so the exodiff tool
will be used to compare the original file to the copy.

```
export IOSS_S3_ENDPOINT="https://mys3server"
export IOSS_S3_PROFILE=my-s3-profile
$SEACAS_INSTALL_PATH/bin/io_shell -in_type exodus -out_type s3 my_exodus_file.exo my_exodus_file.exo
$SEACAS_INSTALL_PATH/bin/io_shell -in_type s3 -out_type exodus my_exodus_file.exo my_exodus_file_copy.exo
$SEACAS_INSTALL_PATH/bin/exodiff my_exodus_file.exo my_exodus_file_copy.exo
```
