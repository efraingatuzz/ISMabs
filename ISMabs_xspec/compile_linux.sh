#!/bin/bash
#File to compile the ISMabs model

# For Linux
rm lib*
sed -i "s,local_dir = '.*',local_dir = \'`pwd`\'," ismabs.f90
echo "initpackage ismabs lmodel_ismabs.dat "`pwd`"\nquit\ny" | xspec


rm *~ *.o
rm *FunctionMap.* lpack_* 
rm -f *.mod Makefile
