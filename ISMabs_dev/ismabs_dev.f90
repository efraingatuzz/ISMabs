! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! ISMABS
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! XSPEC local model for ISM absorption
! Version 1.6 November 2020
!
! Additions to version 1.6
! - Now the code includes the "fftw3" package included in heasoft.
! - Carbon photabsorption cross-section from Gatuzz et al. (2018) have been added (including the benchmarking described in the paper)
! - Si I photoabsorption cross-section from Gatuzz et al (2020) have been added
!
! Additions to version 1.5
! -Turbulence broadening is applied to the optical depth instead of each cross-section 
!  to improve the velocity of the code
!
!
! Additions to version 1.4
! - Turbulence broadenig has been added 
!  It requires the installation of the package 'fftw3.f'. 
! The 'fftw3.f' package need to be installed by using the options
! ./configure CFLAGS='-fPIC'
! make
! make install
!
! After the package is installed, xspackage.tmpl file at  heasoft-[ver]/Xspec/src/tools/initpackage
! need to be modified by adding the flag 
! 
! -L/usr/local/lib -lfftw3
!
!  to the HD_SHLIB_LIBS setting (Please note that this is different 
!  than the process described in the appendix C of the XSPEC's manual 
! This last step is required because "fftw3" is an static library
!
! Additions to version 1.3
! - the CO cross-section has been added (Experimental measurement from Barrus et al. 1979)
!
! Additions to version 1.2
! - The parameter names do not have mathematical operators.
! - The subroutine names in the fortran code have been changed.
! - The lmodel.dat file has been rename to lmodel_ismabs.dat
!
! Additions to version 1.1
! - the model now prints a message to STDOUT if it is unable to
! read from the AtomicData.fits file
! - the ISMABSROOT xset variable has been added to allow the
! location of the AtomicData.fits file to be changed from within
! X-Spec
! - the startup does not depend on the ifl parameter
!
! 
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
subroutine ismabsdev(ear, ne, param, ifl, photar)
!
! The main routine to call all subroutines
!
implicit none


integer,parameter :: num_param = 33, out_unit=20, nion=31
integer,parameter :: nemod=650000 !Number of elements for each ion cross section.
integer :: ne, ifl, i
double precision :: nH, rshift, emod(1:nemod), coemod(0:nemod), bxs(0:nion,nemod), cion(nion), bener(1:nemod)
double precision :: optical_depth(0:nemod)
double precision :: optical_depth_convolved(0:nemod)
double precision :: N_He_0, N_He_1, N_C_0, N_C_1, N_C_2
double precision :: N_N_0, N_N_1, N_N_2, N_O_0, N_O_1, N_O_2
double precision :: N_Ne_0, N_Ne_1, N_Ne_2, N_Mg_0, N_Mg_1, N_Mg_2
double precision :: N_Si_0, N_Si_1, N_Si_2, N_S_0, N_S_1, N_S_2
double precision :: N_Ar_0, N_Ar_1, N_Ar_2, N_Ca_0
double precision :: N_Ca_1, N_Ca_2, N_Fe_0, N_CO 
double precision :: vturb,zfac,temp1
real :: ear(0:ne), param(num_param),photar(ne)
logical :: startup=.true.
character (len=40) version
!Arrays for fourier transform
 

version='dev'
 if(startup)then
  print *, ' '
  print *, 'ISMabs: ISM absorption model develop version'
  print *, 'Gatuzz et al. (2015)'
  print *, 'Note: Default column densities are given'
  print *, 'according to Grevesse, N. & Sauval (1998)'     
  print *, 'assuming N_H = 1.E21 cm^-2'  
  print *, ' '
  call read_cross_sections_ismabs_dev(nemod,bxs,ifl)  !To read the atomic data (photoabsorption cross-sections)
  call create_energy_grid_ismabs_dev(1.d1,1.d4,bener,nemod) !Absorption coefficient calculation grid  = cross section grid
  startup=.false.  
 endif


! To set model parameters
nH = param(1)
N_He_0 = 0.1*nH
N_He_1 = param(2)
N_C_0 = param(3)
N_C_1 = param(4)
N_C_2 = param(5)
N_N_0 = param(6)
N_N_1 = param(7)
N_N_2 = param(8)
N_O_0 = param(9)
N_O_1 = param(10)
N_O_2 = param(11)
N_Ne_0 = param(12)
N_Ne_1 = param(13)
N_Ne_2 = param(14)
N_Mg_0 = param(15)
N_Mg_1 = param(16)
N_Mg_2 = param(17)
N_Si_0 = param(18)
N_Si_1 = param(19)
N_Si_2 = param(20)
N_S_0 = param(21)
N_S_1 = param(22)
N_S_2 = param(23)
N_Ar_0 = param(24)
N_Ar_1 = param(25)
N_Ar_2 = param(26)
N_Ca_0 = param(27)
N_Ca_1 = param(28)
N_Ca_2 = param(29)
N_Fe_0 = param(30)
N_CO = param(31)
vturb=param(32)
rshift = param(33)
zfac = 1/(1.d0+dble(rshift))

!To compute the absorption coefficient for the energy grid
call absorption_ismabs_dev(nH, N_He_0, N_He_1, N_C_0, N_C_1, N_C_2, &
N_N_0, N_N_1, N_N_2, N_O_0, N_O_1, N_O_2, &
N_Ne_0, N_Ne_1, N_Ne_2, N_Mg_0, N_Mg_1, N_Mg_2, &
N_Si_0, N_Si_1, N_Si_2, N_S_0, N_S_1, N_S_2, &
N_Ar_0, N_Ar_1, N_Ar_2, N_Ca_0, &
N_Ca_1, N_Ca_2, N_Fe_0, N_CO,   &
zfac, emod, nemod, optical_depth,bxs,cion,ifl,bener)

!Compute absorption with original cross-sections (vturb=0)
if (vturb.eq.0)then

temp1=0.0
optical_depth(0)=dexp(-temp1)
do i=1,nemod
coemod(i)=dexp(-optical_depth(i))
enddo

!To perform convolution (vturb>0)
else if(vturb.gt.0)then

call optical_depth_convolved_ismabs_dev( nemod,optical_depth,bener,vturb,optical_depth_convolved)


temp1=0.0
optical_depth_convolved(0)=dexp(-temp1)
do i=1,nemod
coemod(i)=dexp(-optical_depth_convolved(i))
enddo
 

endif

 
!To convert to the energy grid of the spectra analyzed
call map_to_grid_ismabs_dev(dble(ear),ne,emod,nemod,photar,coemod,ifl)
return
end subroutine ismabsdev
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine read_cross_sections_ismabs_dev(bnene,xs,ifl)
!
! This routine reads all cross sections and puts them on a given grid
!
! It uses the X-Spec local variable/dictionary value ISMABSROOT
! to locate the data file. If this is not set then it uses
! the setting of the local_dir parameter below (which should be
! changed to match the location of the data file). The path -
! however it is given - should point to the location that contains
! the atomic_data/ directory - i.e. it loads
! <path>/atomic_data/AtomicData.fits
!
implicit none
integer,parameter :: nion=31, out_unit=20
integer :: bnene, ifl, i, j, status
integer :: nemax(0:nion)
double precision :: ener(0:nion,bnene), xs(0:nion,bnene)
character (*), parameter :: fileloc = '/atomic_data/AtomicData.fits'
character (*), parameter :: ismreadchat = 'ismabs: reading from '
character (len=255 + 29) :: filename2 ! len(fileloc)
character (len=240) :: local_dir = '/media/efrain/DATA/softwares/modelosXSPEC/ismabs/ismabs_turb/ismabs.dev' 
character (len=255) :: ismabs_root = ''
character (len=len(ismreadchat)+len(filename2)) :: chatmsg = ''
integer inunit,readwrite,blocksize
integer :: hdutype,colnum
integer :: felem=1, nulld=0
logical :: anynull
character (len=255) :: fgmstr
external :: fgmstr
  
!Number of elements for each ion cross section.
do i=0,nion
nemax(i)=650000
enddo
! Where do we look for the data?

ismabs_root = trim(fgmstr('ISMABSROOT'))
if (ismabs_root .EQ. '') then
ismabs_root = local_dir
endif


! parameters to specify the opening process
status=0
readwrite=0
blocksize=1
filename2=trim(ismabs_root) // fileloc

chatmsg=ismreadchat // filename2
call xwrite(chatmsg, 20)
! Get an unused Logical Unit Number to use to open the FITS file.
call ftgiou(inunit,status)
! Open the FITS file
call ftopen(inunit,filename2,readwrite,blocksize,status)
! Move to the extension 2 (the binary table)
call ftmahd(inunit,2,hdutype,status)
!Read one energy grid (column 1)
do i=0, 1
do j=1,nemax(i)
colnum=1
call ftgcvd(inunit,colnum,j,felem,1,nulld,ener(i,j),anynull,status)
enddo
enddo
!Assign the energy grid to all ion energy grids
do i=1, nion
do j=1,nemax(i)
ener(i,j)= ener(0,j)
enddo
enddo
!Read cross sections
colnum=2
do i=0, nion
do j=1,nemax(i)
call ftgcvd(inunit,colnum,j,felem,1,nulld,xs(i,j),anynull,status)

enddo
colnum=colnum+1
enddo
! Report on errors (done before closing the file in case the error
! comes from closing the file). Unfortunately the X-Spec API does not
! provide a way to signal an error to the calling code, so a screen
! message is used, using the same method used to report the model
! the first time it is used. An alternative would be to use xwrite()
! with a low chatter level.
!
! This message could be displayed only once, but it is probaly worth
! repeating each time it is used.
if (status .ne. 0) then
write (*,*) 'ERROR: unable to read cross sections from ', filename2
write (*,*) 'ERROR: unable to read cross sections from ', filename2
endif
! Close the file and free the unit number
call ftclos(inunit, status)
call ftfiou(-1, status)
end subroutine read_cross_sections_ismabs_dev
! ======================================= !
subroutine absorption_ismabs_dev(col22, N_He_0, N_He_1, N_C_0, N_C_1, N_C_2, &
N_N_0, N_N_1, N_N_2, N_O_0, N_O_1, N_O_2, &
N_Ne_0, N_Ne_1, N_Ne_2, N_Mg_0, N_Mg_1, N_Mg_2, &
N_Si_0, N_Si_1, N_Si_2, N_S_0, N_S_1, N_S_2, &
N_Ar_0, N_Ar_1, N_Ar_2, N_Ca_0, &
N_Ca_1, N_Ca_2, N_Fe_0, N_CO,   &
zfac, e1, bnene, coeff, bxs2,cion,ifl,bener)
!
! This is routine that calculates the optical depth given the column densities
! Finally returns the absorption coefficient exp(-tau)
!
implicit none
integer,parameter :: nion=31, out_unit=20
integer :: bnene, ifl
integer :: i, j
double precision :: col22, col, tmp, cion(nion)
double precision :: bener(0:bnene), bxs2(0:nion,bnene), e1(0:bnene)
double precision :: tau, coeff(0:bnene)
double precision :: N_He_0, N_He_1, N_C_0, N_C_1, N_C_2
double precision :: N_N_0, N_N_1, N_N_2, N_O_0, N_O_1, N_O_2
double precision :: N_Ne_0, N_Ne_1, N_Ne_2, N_Mg_0, N_Mg_1, N_Mg_2
double precision :: N_Si_0, N_Si_1, N_Si_2, N_S_0, N_S_1, N_S_2
double precision :: N_Ar_0, N_Ar_1, N_Ar_2, N_Ca_0
double precision :: N_Ca_1, N_Ca_2, N_Fe_0, N_CO 
double precision :: zfac
real hphoto, gphoto
external hphoto, gphoto


!Column densities
 cion(1)=N_He_0
 cion(2)=N_He_1
 cion(3)=N_C_0
 cion(4)=N_C_1
 cion(5)=N_C_2
 cion(6)=N_N_0
 cion(7)=N_N_1
 cion(8)=N_N_2
 cion(9)=N_O_0
 cion(10)=N_O_1
 cion(11)=N_O_2
 cion(12)=N_Ne_0
 cion(13)=N_Ne_1
 cion(14)=N_Ne_2
 cion(15)=N_Mg_0
 cion(16)=N_Mg_1
 cion(17)=N_Mg_2
 cion(18)=N_Si_0
 cion(19)=N_Si_1
 cion(20)=N_Si_2
 cion(21)=N_S_0
 cion(22)=N_S_1
 cion(23)=N_S_2
 cion(24)=N_Ar_0
 cion(25)=N_Ar_1
 cion(26)=N_Ar_2
 cion(27)=N_Ca_0
 cion(28)=N_Ca_1
 cion(29)=N_Ca_2
 cion(30)=N_Fe_0
 cion(31)=N_CO
 
!Calculates the optical depth and the absorption coefficient exp(-tau)
col=col22*1.d22
e1(0)=(bener(0)*zfac)/1.d3

coeff(0)=0.0

do i=1,bnene
e1(i)=(bener(i)*zfac)/1.d3
! In case you want to read xspec hydrogen column density
! bxs2(0,i)=hphoto(real(e1(i-1)),real(e1(i)))
! tau for hydrogen column density
tmp=col*bxs2(0,i)
! Calculates the optical depth and the absorption coefficient exp(-tau)
tmp= tmp+(bxs2(1,i)*0.1*col) ! He I column density = 0.1 Nh
do j=2,nion

tmp=tmp+(cion(j)*bxs2(j,i)*1.d16)
enddo
tau=tmp
coeff(i)=tau 
enddo
 

end subroutine absorption_ismabs_dev
! ======================================= !
subroutine map_to_grid_ismabs_dev(new_en,nne,old_en, one, nflux, old_flu,ifl)
! This routine map to a given grid
implicit none
integer :: i, j, k, one, nne, bmin, bmax,ifl
double precision :: new_en(0:nne)
double precision :: old_en(0:one), old_flu(one)
double precision :: stemp,etemp, s, etemp2
real :: nflux(nne)
integer,parameter :: out_unit=20
do i=1,nne
nflux(i)=real(0.d0)
call dbinsrch_ismabs_dev(new_en(i-1),bmin,old_en,one+1)
call dbinsrch_ismabs_dev(new_en(i),bmax,old_en,one+1)
bmin = bmin-1
bmax = bmax-1
! Linear interpolation
if (bmin.eq.bmax) then
if(new_en(i).le.old_en(1))then
s=real(old_flu(1))
else if(new_en(i).gt.old_en(one))then
s=real(old_flu(one))
else
do j=2,one
if(new_en(i).gt.old_en(j-1).and.new_en(i).le.old_en(j))then
etemp2=(new_en(i)+new_en(i-1))/2
s=old_flu(j-1)+(old_flu(j)-old_flu(j-1))*(etemp2-old_en(j-1))/(old_en(j)-old_en(j-1))
endif
enddo
endif
! Average (integral)
else
stemp=0.d0
etemp=0.d0
do k=bmin,bmax
stemp=stemp+(old_flu(k))*(old_en(k)-old_en(k-1))
etemp=etemp+(old_en(k)-old_en(k-1))
enddo
s=real(stemp/etemp)
endif
nflux(i)=real(s)
enddo
end subroutine map_to_grid_ismabs_dev
! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
subroutine dbinsrch_ismabs_dev(e,k,ener,n)
!
! search for energy e in array ener(1:n) and return bin k for
! which ener(k).le.e.lt.ener(k+1)
! adapted from J. Wilms's tbvabs_new.f routine
!
implicit none
double precision :: e
integer :: n,k,klo,khi
double precision :: ener(n)
klo=1
khi=n
1 if (khi-klo.gt.1) then
k=(khi+klo)/2
if(ener(k).gt.e)then
khi=k
else
klo=k
endif
goto 1
endif
if (klo.eq.0) then
print *,'Energy out of bounds. Should not happen'
stop
endif
k=klo
end subroutine dbinsrch_ismabs_dev
! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
subroutine create_energy_grid_ismabs_dev(emin,emax,en,nen)
implicit none
integer :: i, nen
double precision :: en(nen)
double precision :: emin,emax
!
do i=1,nen
en(i)=10**(((log10(emax)-log10(emin))*real(i)/nen)+log10(emin))
enddo
!
end subroutine create_energy_grid_ismabs_dev


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

subroutine optical_depth_convolved_ismabs_dev( nemax, bxs2,energy,vturb,cross_section_convolved)

implicit none

 
integer,parameter :: nion=31, out_unit=20
integer :: i,nemax,aa
double precision :: bxs2(0:nemax)
double precision ::  vturb

!Constants for fftw3
integer, parameter :: fftw_r2hc=0, fftw_hc2r=1, fftw_dht=2, fftw_redft00=3, fftw_redft01=4
integer, parameter :: fftw_redft10=5, fftw_redft11=6, fftw_rodft00=7, fftw_rodft01=8, fftw_rodft10=9
integer, parameter :: fftw_rodft11=10, fftw_forward=-1, fftw_backward=+1, fftw_measure=0, fftw_destroy_input=1
integer, parameter :: fftw_unaligned=2, fftw_conserve_memory=4, fftw_exhaustive=8, fftw_preserve_input=16
integer, parameter :: fftw_patient=32, fftw_estimate=64, fftw_wisdom_only=2097152, fftw_estimate_patient=128
integer, parameter :: fftw_believe_pcost=256, fftw_no_dft_r2hc=512, fftw_no_nonthreaded=1024, fftw_no_buffering=2048
integer, parameter :: fftw_no_indirect_op=4096, fftw_allow_large_generic=8192, fftw_no_rank_splits=16384
integer, parameter :: fftw_no_vrecurse=65536, fftw_no_simd=131072, fftw_no_slow=262144  
integer, parameter :: fftw_no_fixed_radix_large_n=524288, fftw_allow_pruning=1048576, fftw_no_vrank_splits=32768


!For fourier transform and convolution f*g
integer :: plan
double complex,allocatable :: f_o(:),g_o(:),f_1(:),g_1(:),h_o(:),h_1(:)
double precision :: gauss_norm,delta_v,A,cc,x,B, tmp3,y_o 
double precision,parameter ::   PI=3.141592654, c=299792.458 
double precision :: energy(0:nemax) , gauss(-nemax:nemax),cross_section(nemax),cross_section_convolved(0:nemax)

!Preparing arrays for fourier transform only once (I will use complex numbers with z=a+0*i) 
logical :: startup=.true.
 if(startup)then
   allocate(f_o(nemax))
   allocate(g_o(nemax))
   allocate(f_1(nemax))
   allocate(g_1(nemax))
   allocate(h_o(nemax))
   allocate(h_1(nemax))

  startup=.false.  
 endif


!Gaussian to do the convolution
 A=energy(1)
 y_o=energy(nemax/2)
 B=10**(((log10(energy(nemax)))-(log10(energy(1))))/nemax) 
 delta_v=(vturb/c)*y_o
 gauss_norm=1/(delta_v*sqrt(2*PI))
  
!Gaussian a*exp(-((x-b)**2)/(2*cc*cc))
 do i=1,nemax
 cc=1/(2*delta_v*delta_v)
 
 if(energy(i).lt.energy(nemax/2))then
 x=((energy(nemax/2)-energy(i)))
 
 else if (energy(i).ge.energy(nemax/2))then
 x=((energy(i)-energy(nemax/2)))
 endif

 gauss(i)=gauss_norm*exp(-((x)**2)*cc)
 enddo
 

!Fourier transform of cross_section 

   do i=1,nemax
    cross_section(i) =bxs2(i)
   enddo


   do i=1,nemax
    f_o(i) = dcmplx(cross_section(i),0.0)
   enddo

   call dfftw_plan_dft_1d(plan,nemax,f_o,f_1,FFTW_FORWARD,FFTW_ESTIMATE)
   call dfftw_execute_dft(plan,f_o,f_1)
   call dfftw_destroy_plan(plan)

!Fourier transform of gaussian
  tmp3=0.0

   do i=1,nemax
    tmp3 =tmp3+ gauss(i)
   enddo


   do i=1,nemax
    g_o(i) = dcmplx(gauss(i)/tmp3,0.0)
   enddo

   call dfftw_plan_dft_1d(plan,nemax,g_o,g_1,FFTW_FORWARD,FFTW_ESTIMATE)
   call dfftw_execute_dft(plan,g_o,g_1)
   call dfftw_destroy_plan(plan)  

!Multiplication
      do i=1,nemax
!         in(i)=f(i)*conjg(g(i))
         h_o(i)=f_1(i)*(conjg(g_1(i)))
 
      enddo   

! Inverse Fourier transform
   call dfftw_plan_dft_1d(plan,nemax,h_o,h_1,FFTW_BACKWARD,FFTW_ESTIMATE)
   call dfftw_execute_dft(plan,h_o,h_1)
   call dfftw_destroy_plan(plan)
 

   aa=nemax/2   
   do i=1,nemax/2
    cross_section_convolved(i) = real(h_1(aa)/((real(nemax))))

   aa=aa+1

   enddo


   aa=1
   do i=nemax/2,nemax
    cross_section_convolved(i) = real(h_1(aa)/((real(nemax))))
   aa=aa+1
   enddo

 call dfftw_cleanup()



end subroutine optical_depth_convolved_ismabs_dev
