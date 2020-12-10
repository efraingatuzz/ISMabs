#!/bin/bash

# For Linux
rm lib*
sed -i "s,local_dir = '.*',local_dir = \'`pwd`\'," ismabsion.f90
echo "initpackage ismabsion lmodel_ismabs_ion.dat "`pwd`"\nquit\ny" | xspec

rm *~ *.o
rm *FunctionMap.* lpack_* 
rm -f *.mod Makefile
