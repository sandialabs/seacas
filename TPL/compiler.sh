#! /usr/bin/env bash

MPI="${MPI:-NO}"
if [ "$MPI" == "YES" ] && [ "$CRAY" = "YES" ]
then
  export CC=cc
  export CXX=CC
elif [ "$MPI" == "YES" ]
then
  export CC=mpicc
  export CXX=mpicxx
else
  COMPILER="${COMPILER:-gnu}"
  if [ "$COMPILER" == "gnu" ]
  then
      export CC=gcc
      export CXX=g++
  fi
  if [ "$COMPILER" == "clang" ]
  then
      export CC=clang
      export CXX=clang++
  fi
  if [ "$COMPILER" == "gnumacport" ]
  then
      VER=${VER:-5}
      export CXX=g++-mp-${VER}
      export CC=gcc-mp-${VER}
  fi
  if [ "$COMPILER" == "intel" ]
  then
      export CC=icc
      export CXX=icc
  fi
  if [ "$COMPILER" == "analyzer" ]
  then
      export CXX=/opt/local/libexec/llvm-9.0/libexec/c++-analyzer
      export CC=/opt/local/libexec/llvm-9.0/libexec/ccc-analyzer
  fi
  if [ "$COMPILER" == "ibm" ]
  then
      export CC=xlc
      export CXX=xlc
  fi
fi
