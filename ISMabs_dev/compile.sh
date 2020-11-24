#!/bin/bash

# For Linux
rm lib*
sed -i "s,local_dir = '.*',local_dir = \'`pwd`\'," ismabs_dev.f90  
echo "initpackage ismabsdev lmodel_ismabs_dev.dat "`pwd`"\nquit\ny" | xspec

# For Mac OSX
#rm *.dylib
#sed -i.ori "s,local_dir = '.*',local_dir = \'`pwd`\'," ismabs_dev.f90  
#echo "initpackage ismabsdev lmodel_ismabs_dev.dat "`pwd`"\nquit\ny" | xspec

rm *~ *.o
rm *FunctionMap.* lpack_* 
rm -f *.mod Makefile
