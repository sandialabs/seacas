#!/bin/sh

if [ $# -lt 1 ]; then echo "no arguments" 1>&2; exit 1; fi

dirs=`echo $PATH|sed 's/:/ /g'`

for prog in "$@"; do
	found=n
	for dir in $dirs; do
		if [ -f $dir/$prog ]; then
			echo $dir/$prog
			found=y
			break
		fi
	done
	if [ "$found" = "n" ]; then echo no $prog in $dirs 1>&2; fi
done
