#!/bin/bash

# For Linux
rm lib*
fullpath=$(pwd)

part1=${fullpath:0:80}
part2=${fullpath:80} 

# Set local_dir1 and local_dir2 properly
sed -i "s,local_dir1 = '.*',local_dir1 = '$part1'," ismabsne.f90
sed -i "s,local_dir2 = '.*',local_dir2 = '$part2'," ismabsne.f90
    
echo "initpackage ismabsne lmodel_ismabsne.dat "`pwd`"\nquit\ny" | xspec

         
            
            
# For Mac OSX
#rm *.dylib
#fullpath=$(pwd)
#part1=${fullpath:0:80}
#part2=${fullpath:80} 
## Set local_dir1 and local_dir2 properly
#sed -i.ori "s,local_dir1 = '.*',local_dir1 = '$part1'," ismabsne.f90
#sed -i.ori "s,local_dir2 = '.*',local_dir2 = '$part2'," ismabsne.f90
#echo "initpackage ismabsne lmodel_ismabsne.dat "`pwd`"\nquit\ny" | xspec

rm *~ *.o
rm *FunctionMap.* lpack_* 
rm -f *.mod Makefile
