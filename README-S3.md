# Introduction

This document describes the requirements to build and use the IOSS-S3
DatabaseIO component.

## Installing the AWS SDK for C++ and Cereal TPLs

In addition to the usual TPLs required for building SEACAS, the
IOSS-S3 component requires the Amazon Web Services (AWS) Software
Development Kit (SDK) for C++ and the Cereal serialization
library.  These TPLs can be installed using the `install-tpl.sh`
script as described in [README.md](README.md).  There is also a
description of manual installation in
[TPL-Manual-Install.md](TPL-Manual-Install.md).

## Building SEACAS with S3

The `cmake-config` script is able to build the IOSS-S3 component using
the TPLs installed by the `install-tpl.sh` script.  When it finds both
the AWS SDK for C++ and Cereal, it will enable the IOSS-S3 component.
A full description of `cmake-config` can be found in
[README.md](README.md).

## Passing Parameters to the IOSS-S3 component

The IOSS-S3 component has several parameters that control its
behavior.  Each parameter can be specified either by the user setting
an environment variable or by the application by setting an IOSS
Property.  If both are specified, the environment variable takes
precedence over the Property.

The parameters are descibed in the table below.

| Property or Environment Variable | Values          | Default | Description |
|----------------------------------|:---------------:|:-------:|-------------|
| IOSS_S3_ENDPOINT                 | URI of the S3 object store           | None    | This parameter is required and does not have a default. |
| IOSS_S3_PROFILE                  | name of the S3 credential profile    | default |  |
| IOSS_S3_CA_FILE                  | path to a certificate authority file | None    | Only required with your S3 object store requires it. |
| IOSS_S3_USE_CA_FILE              | enable the CA file                   | False   |  |
| IOSS_S3_USE_TRANSFER_MANAGER     | enable the transfer manager          | False   | Enable to transfer large data. |
| IOSS_S3_ENABLE_AWS_TRACING       | enable debug in AWSSDK               | False   | Caution: the output is very verbose. |
| IOSS_S3_DISABLE_EC2_LOOKUP       | disable an EC2 lookup at startup     | False   | Useful for on-prem object stores. |


## Example: copy an Exodus file to S3 and back

A simple way to verify your IOSS-S3 installation is to copy an Exodus
file into your S3 object store, copy it back out to disk and then
compare them.  The io_shell tool can be used to copy data in and out
of S3.  In this example, the data is Exodus data so the exodiff tool
will be used to compare the original file to the copy.

```
export IOSS_S3_ENDPOINT="https://mys3server"
export IOSS_S3_PROFILE=my-s3-profile
io_shell -in_type exodus -out_type s3 my_exodus_file.exo my_exodus_file.exo
io_shell -in_type s3 -out_type exodus my_exodus_file.exo my_exodus_file_copy.exo
exodiff my_exodus_file.exo my_exodus_file_copy.exo
```
