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

Different flavours of the model can be downloaded from the Github repository at https://github.com/efraingatuzz/ISMabs. Currently versions include:

- ISMabs_xspec: The ISMabs package (version 1.2) that appears in Gatuzz et al. (2015). It is included as part of the standard xspec software (from version 12.10). 
- ISMabs_solar: A model with neutral metallic column densities set as relative to the hydrogen column densities (i.e. similar to TBabs) while column densities for single and double ionized species are included as parameters of the model.
- ISMabs_dev: latest develop version of the model. It includes broadening of the lines. See README.md file for more details

CONTACT

Please contact me with any reports or questions.

egatuzz@mpe.mpg.de
