#!/bin/bash

# For Linux
rm lib*
fullpath=$(pwd)

part1=${fullpath:0:80}
part2=${fullpath:80} 

# Set local_dir1 and local_dir2 properly
sed -i "s,local_dir1 = '.*',local_dir1 = '$part1'," ismabscalcium.f90
sed -i "s,local_dir2 = '.*',local_dir2 = '$part2'," ismabscalcium.f90
    
echo "initpackage ismabscalcium lmodel_ismabs_calcium.dat "`pwd`"\nquit\ny" | xspec

         
            
            
# For Mac OSX
#rm *.dylib
#fullpath=$(pwd)
#part1=${fullpath:0:80}
#part2=${fullpath:80} 
## Set local_dir1 and local_dir2 properly
#sed -i.ori "s,local_dir1 = '.*',local_dir1 = '$part1'," ismabscalcium.f90
#sed -i.ori "s,local_dir2 = '.*',local_dir2 = '$part2'," ismabscalcium.f90
#echo "initpackage ismabscalcium lmodel_ismabs_calcium.dat "`pwd`"\nquit\ny" | xspec

rm *~ *.o
rm *FunctionMap.* lpack_* 
rm -f *.mod Makefile
