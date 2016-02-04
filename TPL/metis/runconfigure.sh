#! /bin/sh

if [ "X$ACCESS" == "X" ] ; then
  echo "ERROR: Please set the ACCESS environment variable before executing this script."
  exit
fi

make config cc=gcc prefix=${ACCESS}
