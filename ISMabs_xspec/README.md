ISMabs_xspec: ISMabs model version 1.2 (the same version included as part of the standard XSPEC software). The contents of the ISMAabs_xspec include:

- atomic_data/AtomicData.fits  -- atomic database binary fits file. This must reside in the directory atomic_data inside the folder where the model is located.  
- ismabs.f90 -- source code for ISMabs
- lmodel_ismabs.dat -- local model definition file needed by xspec.  
- compile.sh -- installation script written on bash.
- README.md -- this file

INSTALLATION

You can use the compile.sh file to install the model by doing

sh compile.sh

In the  model folder or you can setting up and using this model is as described in the xspec manual:

0) You need to have the heasoft package installed on your machine, but it must be built from source. Local models cannot be installed from a binary installation.
1) untar this directory somewhere in your user area
2) setup your headas environment (eg. 'setenv HEADAS /path/to/architecture',and 'source \$HEADAS/headas-init.csh')
3) start up xspec, and in response to the prompt type 

'initpackage ismabs lmodel.dat <path-to-current-directory>',

where <path-to-current-directory> is the full path of the current directory. After the build is complete type 

'lmod ismabs <path-to-current-directory>'

In subsequent  sessions you don't neet to do the initpackage step again, just the lmod.

ATOMIC DATA 

With the default set up - that is, if you have run compile.sh, the model will look for the cross-section data file in atomic_data/AtomicData.fits, relative to the directory in which the module is located.

The XSPEC xset command can be used to set the ISMABSROOT variable; if this is set then it is used instead of the path to the module. So after

xset ISMABSROOT /data/ismabs/

then the model will use the file /data/ismabs/atomic_data/AtomicData.fits (the ISMABSROOT refers to the directory containing the atomic_data/ directory). Note that ISMABSROOT over-rides any changes made by running compile.sh when building the model.

The location of the file can be found by setting the XSPEC chatter level to 20 or higher - e.g.

chatter 20

before evaluating the model.

PARAMETERS

Inside of xspec, the model can be invoked by typing 'mo ismabs*pow' or variations on that.The input parameters included the elemental column densities and redshift.

NOTE that the HeI column density is not a free parameter due to the constraining imposed to the model:

HeI = 0.1 * H

See Gatuzz et al. (2015) for details.

ISMabs Release Notes

New in version 1.2 (May 2015):
- The parameter names do not have mathematical operators.
- The subroutine names in the fortran code have been changed.

New in version 1.1 (Dec 2014): 
 - the startup now does not use the ifl parameter.
 - the model now prints a message to STDOUT if it is unable to
 read from the AtomicData.fits file.
 - the ISMABSROOT xset variable has been added to allow the
 location of the AtomicData.fits file to be changed from within
 XSPEC.
