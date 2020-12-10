# ISMabs_dev: 

ISMabs model develop version. The developing version includes turbulence broadening. The velocity, in units of km/s, is a parameter of the model. Please note that the turbulence is applied to the complete optical depth (i.e. the same velocity is applied to all ions)

The contents of the ISMAabs_dev folder include:
- atomic_data/AtomicData.fits  -- atomic database binary fits file. This must reside in the directory atomic_data inside the folder where the model is located.  
- ismabs_dev.f90 -- source code for ISMabsdev
- lmodel_ismabs_dev.dat -- local model definition file needed by xspec.  
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

'initpackage ismabsdev lmodel_ismabs_dev.dat <path-to-current-directory>',

where <path-to-current-directory> is the full path of the current directory. After the build is complete type 

'lmod ismabsdev <path-to-current-directory>'

In subsequent  sessions you don't neet to do the initpackage step again, just the lmod. The model is called "ismabsdev" inside XSPEC

ATOMIC DATA 

With the default set up - that is, if you have run compile.sh, the model will look for the cross-section data file in atomic_data/AtomicData.fits, relative to the directory in which the module is located.

The XSPEC xset command can be used to set the ISMABSDEVROOT variable; if this is set then it is used instead of the path to the module. So after

xset ISMABSDEVROOT /data/ismabs_dev/

then the model will use the file /data/ismabs/atomic_data/AtomicData.fits (the ISMABSDEVROOT refers to the directory containing the atomic_data/ directory). Note that ISMABSDEVROOT over-rides any changes made by running compile.sh when building the model.

The location of the file can be found by setting the XSPEC chatter level to 20 or higher (e.g. "chatter 20") before evaluating the model.

PARAMETERS

Inside of xspec, the model can be invoked by typing 'mo ismabsdev*pow' or variations on that.The input parameters included the elemental column densities and redshift. NOTE that the HeI column density is not a free parameter due to the constraining imposed to the model:

HeI = 0.1 * H

See Gatuzz et al. (2015) for details.

CONTACT

Please contact me with any reports or questions.

egatuzz@mpe.mpg.de

ISMabsdev Release Notes

New in version 1.6 (November 2020)
- Now the model uses the "fftw3" package included in heasoft.
- Si I photoabsorption cross-section from Gatuzz et al (2020) have been added
- The atomic data is now stored in a .fits table with variable length (i.e. the file is smaller). The data is interpolated once when the model is called
- Ni and Zn has been added
- CO cross-section has been removed

New in version 1.5 (July 20018)
- Carbon photabsorption cross-section from Gatuzz et al. (2018) have been added (including the benchmarking described in the paper)

New in version 1.4 (January 2016)
-Turbulence broadening is applied to the optical depth instead of each cross-section to improve the velocity of the code
 
New in version 1.3 (December 2016):
- The model includes broadening. The external library "fftw3" must be installed

New in version 1.2 (May 2015):
- The parameter names do not have mathematical operators.
- The subroutine names in the fortran code have been changed.

New in version 1.1 (Dec 2014): 
 - the startup now does not use the ifl parameter.
 - the model now prints a message to STDOUT if it is unable to  read from the AtomicData.fits file.
 - the ISMABSROOT xset variable has been added to allow the  location of the AtomicData.fits file to be changed from within  XSPEC.

