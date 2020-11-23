# ISMabs

ISMabs is an X-ray photoabsorption model for the interstellar medium that takes into account both neutral and ionized species from H, He, N, O, Ne, Mg, Si, S, Ar, Ca, Fe, Ni and Zn. Particularly we include in our model the following cross sections (See Gatuzz et al. (2014b) for details.)

- Neutral states of Si, S, Ar and Ca from Verner et al. (1995)
- Singly and doubly ionized states of Si, S, Ar and Ca from Witthoeft et al. (2009) and Witthoeft et al. (2011)
- Neutral, singly and doubly ionized states of N from Garcia et al. (2009)
- Neutral states of O from Gorczyca et al. (2013) 
- Singly and doubly ionized states of O from Garcia et al. (2005), including corrections applied by Gatuzz et al. (2013)
- Neutral state of Ne from Gorczyca et al. (2000)
- Singly and doubly ionized states of Ne from Gorczyca et al. (2005)
- For the Fe-L edge region we use the measurement of metallic iron by Kortright & Kim (2000)
- Neutral, singly and doubly ionized states of Mg from Hasoglu et al. (2014).

Compared with previous photoabsorption models, which solely rely on neutral species, the inclusion of ions leads to improvements of the spectral fits. Fit parameters comprise the column densities of abundant contributors that allow direct estimates of ionization states. 

OBTAINING ISMabs:

As of this writing, the ISMabs package is not included as part of the standard xspec software, either within heasoft or as part of standalone xspec.  Instead, it must be downloaded and installed separately from the sites:

http://heasarc.gsfc.nasa.gov/xanadu/xspec/models//ismabs.html

The contents of the tarfile include:

atomic_data/AtomicData.fits  -- atomic database binary fits file. This must reside in the directory atomic_data inside the folder where the model is located.  

ismabs.f90 -- source code for ISMabs

lmodel_ismabs.dat -- local model definition file needed by xspec.  

compile.sh -- installation script written on bash.

README.txt -- this file

INSTALLATION

You can use the compile.sh file to install the model by doing

sh compile.sh

In the  model folder or you can setting up and using this model is as described in the xspec manual:


0) You need to have the heasoft package installed on your machine, but it must be built from source. Local models cannot be installed from a binary installation.

1) untar this directory somewhere in your user area

2) setup your headas environment (eg. 'setenv HEADAS /path/to/architecture', and 'source \$HEADAS/headas-init.csh')


3) start up xspec, and in response to the prompt type 

'initpackage ismabs lmodel.dat <path-to-current-directory>',

where <path-to-current-directory> is the full path of the current directory After the build is complete type 

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

Inside of xspec, the model can be invoked by typing 'mo ismabs*pow' or variations on that. The input parameters included the elemental column densities and redshift.

NOTE that the HeI column density is not a free parameter due to the constraining imposed to the model:

HeI = 0.1 * H

See Gatuzz et al. (2014b) for details.

CONTACT

This package is still being tested. Please contact me with any reports or questions.

egatuzz@mpe.mpg.de

New in version 1.2 (May 2015):
- The parameter names do not have mathematical operators.
- The subroutine names in the fortran code have been changed.

New in version 1.1 (Dec 2014): 
 - the startup now does not use the ifl parameter.
 - the model now prints a message to STDOUT if it is unable to  read from the AtomicData.fits file.
 - the ISMABSROOT xset variable has been added to allow the location of the AtomicData.fits file to be changed from within XSPEC.
