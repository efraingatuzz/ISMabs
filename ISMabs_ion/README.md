# ISMabs_ion: 

ISMabsion model, based on ISMabs (Gatuzz et al. 2015). This model computes column densities for neutral species assuming the XSPEC solar abundance. Column densities for ions and metallic iron are parameters of the model.

The contents of the ISMAabs_ion folder include:
- atomic_data/AtomicData.fits  -- atomic database binary fits file. This must reside in the directory atomic_data inside the folder where the model is located.
- ismabs_ion.f90 -- source code for ISMabsion
- lmodel_ismabs_ion.dat -- local model definition file needed by xspec.
- compile.sh -- installation script written on bash.
- README.md -- this file

INSTALLATION

You can use the compile.sh file to install the model by doing (depending on your OS)

sh compile_linux.sh/sh compile_mac.sh

In the  model folder or you can setting up and using this model is as described in the xspec manual:

0) You need to have the heasoft package installed on your machine, but it must be built from source. Local models cannot be installed from a binary installation.
1) untar this directory somewhere in your user area
2) setup your headas environment (eg. 'setenv HEADAS /path/to/architecture',and 'source \$HEADAS/headas-init.csh')
3) start up xspec, and in response to the prompt type 

'initpackage ismabsion lmodel_ismabs_ion.dat <path-to-current-directory>',

where <path-to-current-directory> is the full path of the current directory. After the build is complete type 

'lmod ismabsion <path-to-current-directory>'

In subsequent  sessions you don't neet to do the initpackage step again, just the lmod.

ATOMIC DATA 

With the default set up - that is, if you have run compile.sh, the model will look for the cross-section data file in atomic_data/AtomicData.fits, relative to the directory in which the module is located.

The XSPEC xset command can be used to set the ISMABSIONROOT variable; if this is set then it is used instead of the path to the module. So after

xset ISMABSIONROOT /data/ismabs_dev/

then the model will use the file /data/ismabs/atomic_data/AtomicData.fits (the ISMABSIONROOT refers to the directory containing the atomic_data/ directory). Note that ISMABSIONROOT over-rides any changes made by running compile.sh when building the model.

The location of the file can be found by setting the XSPEC chatter level to 20 or higher (e.g. "chatter 20") before evaluating the model.

PARAMETERS

Inside of xspec, the model can be invoked by typing 'mo ismabsion*pow' or variations on that. The input parameters included the hydrogen column density, column densities for singly/doubly ionized species and redshift.

CONTACT

Please contact me with any reports or questions.

egatuzz@mpe.mpg.de

ISMabsion Release Notes
 
New in version 1.1 (Nov 2020): 
 - The atomic data is now stored in a .fits table with variable length (i.e. the file is smaller). The data is interpolated once when the model is called


