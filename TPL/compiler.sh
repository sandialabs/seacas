#! /usr/bin/env bash

COMPILER="${COMPILER:-gnu}"
MPI="${MPI:-NO}"
if [ "$COMPILER" == "mpi" ]
then
    MPI="YES"
fi

if [ "$MPI" == "YES" ] && [ "$CRAY" = "YES" ]
then
  export CC=cc
  export CXX=CC
elif [ "$MPI" == "YES" ]
then
  export CC=mpicc
  export CXX=mpicxx
else
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
  if [ "$COMPILER" == "gnubrew" ]
  then
      VER=${VER:-10}
      CXX=g++-${VER}
      CC=gcc-${VER}
  fi
  if [ "$COMPILER" == "gnumacport" ]
  then
      VER=${VER:-5}
      export CXX=g++-mp-${VER}
      export CC=gcc-mp-${VER}
  fi
  if [ "$COMPILER" == "clangmacport" ]
  then
      VER=${VER:-7}
      export CXX=clang++-mp-${VER}.0
      export CC=clang-mp-${VER}.0
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
