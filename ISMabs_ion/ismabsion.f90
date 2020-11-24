! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! ISMABS_ION
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! XSPEC local model for ISM absorption
! Version 1.0 August 2020
!  
! This model computes column densities for neutral elements
! assuming the solar abundances loaded in XSPEC. Column densities
! for ionized elements (and for metallic iron) are included
! as parameters of the model.
!
! Atomic data is the same than ISMabs develop version (v1.6)
!
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
subroutine ismabsion(ear, ne, param, ifl, photar)
!
! The main routine to call all subroutines
!
implicit none
integer,parameter :: num_param = 22, out_unit=20, nion=30
integer,parameter :: nemod=650000 !Number of elements for each ion cross section.
integer :: ne, ifl 
double precision :: nH, rshift, emod(1:nemod), coemod(nemod), bxs(0:nion,nemod), cion(nion), bener(1:nemod)
double precision :: N_He_0, N_He_1, N_C_1, N_C_2
double precision :: N_N_1, N_N_2, N_O_1, N_O_2
double precision :: N_Ne_1, N_Ne_2, N_Mg_1, N_Mg_2
double precision :: N_Si_1, N_Si_2, N_S_1, N_S_2
double precision :: N_Ar_1, N_Ar_2
double precision :: N_Ca_1, N_Ca_2, N_Fe_0 
double precision :: zfac
real :: ear(0:ne), param(num_param),photar(ne)

logical :: startup=.true.
character (len=40) version
version='1.0'
 if(startup)then
  print *, ' '
  print *, 'ISMabs_ion: ISM absorption model '
  print *, 'Based on ISMabs (Gatuzz et al. 2015)'
  print *, 'Column densities for neutral species are'
  print *, 'computed assuming XSPEC solar abundances'    
  print *, 'Column densities for ions and metallic Fe'
  print *, 'are parameters of the model' 
  print *, ' '
  call read_cross_sections_ismabs_ion( nemod,bxs,ifl)
  call create_energy_grid_ismabs_ion(1.d1,1.d4,bener,nemod) !Absorption coefficient calculation grid  = cross section grid
  startup=.false.  
 endif
! Model parameters
nH = param(1)
N_He_0 = 0.1*nH
N_He_1 = param(2)
N_C_1 = param(3)
N_C_2 = param(4)
N_N_1 = param(5)
N_N_2 = param(6)
N_O_1 = param(7)
N_O_2 = param(8)
N_Ne_1 = param(9)
N_Ne_2 = param(10)
N_Mg_1 = param(11)
N_Mg_2 = param(12)
N_Si_1 = param(13)
N_Si_2 = param(14)
N_S_1 = param(15)
N_S_2 = param(16)
N_Ar_1 = param(17)
N_Ar_2 = param(18)
N_Ca_1 = param(19)
N_Ca_2 = param(20)
N_Fe_0 = param(21)
rshift = param(22)
zfac = 1/(1.d0+dble(rshift))



call absorption_ismabs_ion(nH,  N_He_1, N_C_1, N_C_2, &
 N_N_1, N_N_2,  N_O_1, N_O_2, &
 N_Ne_1, N_Ne_2,  N_Mg_1, N_Mg_2, &
 N_Si_1, N_Si_2,  N_S_1, N_S_2, &
 N_Ar_1, N_Ar_2,  &
N_Ca_1, N_Ca_2, N_Fe_0,  &
zfac, emod, nemod, coemod,bxs,cion,ifl,bener)

call map_to_grid_ismabs_ion(dble(ear),ne,emod,nemod,photar,coemod,ifl)
return
end subroutine ismabsion
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine read_cross_sections_ismabs_ion(bnene,xs,ifl)
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
integer,parameter :: nion=30, out_unit=20
integer :: bnene, ifl, i, j, status
integer :: nemax(0:nion)
double precision :: ener(0:nion,bnene), xs(0:nion,bnene)
character (*), parameter :: fileloc = '/atomic_data/AtomicData.fits'
character (*), parameter :: ismreadchat = 'ismabs: reading from '
character (len=255 + 29) :: filename2 ! len(fileloc)
character (len=240) :: local_dir = '/media/efrain/DATA/softwares/modelosXSPEC/ismabs/ismabs_no_turb/ismabs_ions.v0.1'
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
end subroutine read_cross_sections_ismabs_ion
! ======================================= !
subroutine absorption_ismabs_ion(col22,  N_He_1, N_C_1, N_C_2, &
 N_N_1, N_N_2,  N_O_1, N_O_2, &
 N_Ne_1, N_Ne_2,  N_Mg_1, N_Mg_2, &
 N_Si_1, N_Si_2,  N_S_1, N_S_2, &
 N_Ar_1, N_Ar_2,  &
N_Ca_1, N_Ca_2, N_Fe_0,    &
zfac, e1, bnene, coeff, bxs2,cion,ifl,bener)
!
! This is routine that calculates the optical depth given the column densities
! Finally returns the absorption coefficient exp(-tau)
!
implicit none
integer,parameter :: nion=30, out_unit=20
integer :: bnene, ifl
integer :: i, j
double precision :: col22, col, tmp, cion(nion)
double precision :: bener(0:bnene), bxs2(0:nion,bnene), e1(0:bnene)
double precision :: tau, coeff(0:bnene)
double precision :: N_He_0, N_He_1, N_C_1, N_C_2
double precision :: N_N_1, N_N_2, N_O_1, N_O_2
double precision :: N_Ne_1, N_Ne_2, N_Mg_1, N_Mg_2
double precision :: N_Si_1, N_Si_2, N_S_1, N_S_2
double precision :: N_Ar_1, N_Ar_2
double precision :: N_Ca_1, N_Ca_2, N_Fe_0 
double precision :: zfac
real hphoto, gphoto,fgabnz
external hphoto, gphoto,fgabnz


! Calculates the optical depth and the absorption coefficient exp(-tau)
col=col22*1.d22
e1(0)=(bener(0)*zfac)/1.d3
coeff(0)=0.0

 
!Column densities
 cion(1)=N_He_0
 cion(2)=N_He_1
 cion(3)=fgabnz(6)*col/1.d16
 cion(4)=N_C_1
 cion(5)=N_C_2
 cion(6)=fgabnz(7)*col/1.d16
 cion(7)=N_N_1
 cion(8)=N_N_2
 cion(9)=fgabnz(8)*col/1.d16
 cion(10)=N_O_1
 cion(11)=N_O_2
 cion(12)=fgabnz(10)*col/1.d16
 cion(13)=N_Ne_1
 cion(14)=N_Ne_2
 cion(15)=fgabnz(12)*col/1.d16
 cion(16)=N_Mg_1
 cion(17)=N_Mg_2
 cion(18)=fgabnz(14)*col/1.d16
 cion(19)=N_Si_1
 cion(20)=N_Si_2
 cion(21)=fgabnz(16)*col/1.d16
 cion(22)=N_S_1
 cion(23)=N_S_2
 cion(24)=fgabnz(18)*col/1.d16
 cion(25)=N_Ar_1
 cion(26)=N_Ar_2
 cion(27)=fgabnz(20)*col/1.d16
 cion(28)=N_Ca_1
 cion(29)=N_Ca_2
! cion(30)=fgabnz(26)*col/1.d16
 cion(30)=N_Fe_0
 
! print *,cion(3),col,fgabnz(6)*col,fgabnz(6)*col/1.d16

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
coeff(i)=dexp(-tau)
enddo
  



end subroutine absorption_ismabs_ion
! ======================================= !
subroutine map_to_grid_ismabs_ion(new_en,nne,old_en, one, nflux, old_flu,ifl)
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
call dbinsrch_ismabs(new_en(i-1),bmin,old_en,one+1)
call dbinsrch_ismabs(new_en(i),bmax,old_en,one+1)
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
end subroutine map_to_grid_ismabs_ion
! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
subroutine dbinsrch_ismabs_ion(e,k,ener,n)
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
end subroutine dbinsrch_ismabs_ion
! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
subroutine create_energy_grid_ismabs_ion(emin,emax,en,nen)
implicit none
integer :: i, nen
double precision :: en(nen)
double precision :: emin,emax
!
do i=1,nen
en(i)=10**(((log10(emax)-log10(emin))*real(i)/nen)+log10(emin))
enddo
!
end subroutine create_energy_grid_ismabs_ion
