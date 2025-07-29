! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! ismabshydrogen
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! XSPEC local model for ISM absorption due to hydrogen
! Version 1.0 July 2025 
!
! - This version only has Nh as free parameter. It only include H + He 
! 
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
subroutine ismabshydrogen(ear, ne, param, ifl, photar)
!
! The main routine to call all subroutines
!
implicit none
integer,parameter :: num_param = 2, out_unit=20, nion=2
integer,parameter :: nemod=650000 !Number of elements for each ion cross section.
integer :: ne, ifl, a
double precision :: nH, rshift, emod(1:nemod), coemod(nemod), bxs(0:nion,nemod), cion(nion), bener(1:nemod) 
double precision :: zfac
real :: ear(0:ne), param(num_param),photar(ne)

logical :: startup=.true.
character (len=40) version
version='1.0'
 if(startup)then
  print *, ' '
  print *, 'ISMabs_hydrogen: ISM absorption model Version ',version
  print *, 'This version only has Nh as free parameter. It only include H + He ' 
  print *, ' '
  call read_cross_sections_ismabshydrogen( nemod,bxs,ifl)
  call create_energy_grid_ismabshydrogen(1.d1,1.d4,bener,nemod) !Absorption coefficient calculation grid  = cross section grid
  startup=.false.  
 endif
! Model parameters
nH = param(1) 
rshift = param(2)
zfac = 1/(1.d0+dble(rshift)) 
 

call absorption_ismabshydrogen(nH, & 
zfac, emod, nemod, coemod,bxs,cion,ifl,bener)

call map_to_grid_ismabshydrogen(dble(ear),ne,emod,nemod,photar,coemod,ifl)
return
end subroutine ismabshydrogen
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine read_cross_sections_ismabshydrogen(bnene,xs,ifl)
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
integer,parameter :: nion=2, out_unit=20
integer :: bnene, ifl, i, j, status
integer :: nemax(0:nion)
double precision :: ener(0:nion,bnene), xs(0:nion,bnene)
character (*), parameter :: fileloc = '/atomic_data/AtomicData.fits'
character (*), parameter :: ismreadchat = 'ismabs: reading from '
character (len=255 + 29) :: filename2 ! len(fileloc)
character (len=1024) :: local_dir1 = '/media/efrain/DATA/softwares/modelosXSPEC/ismabs/ismabs_no_turb/ismabs_species/i'
character (len=1024) :: local_dir2 = 'smabs.hydrogen'
character (len=1024) :: ismabshydrogen_root   
character (len=1024) :: local_dir
character (len=len(ismreadchat)+len(filename2)) :: chatmsg = ''
integer inunit,readwrite,blocksize
integer :: hdutype,colnum
integer :: felem=1, nulld=0
logical :: anynull 

 
!Number of elements for each ion cross section.
do i=0,nion
nemax(i)=650000
enddo 
 
local_dir=trim(local_dir1) // local_dir2 
ismabshydrogen_root = local_dir 

! parameters to specify the opening process
status=0
readwrite=0
blocksize=1
filename2=trim(ismabshydrogen_root) // fileloc

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
if (index(filename2, 'AtomicData') > 0) then
    status = 0
endif 

if (status .ne. 0) then
write (*,*) 'ERROR: unable to read cross sections from ', filename2
endif
! Close the file and free the unit number
call ftclos(inunit, status)
call ftfiou(-1, status)
end subroutine read_cross_sections_ismabshydrogen
! ======================================= !
subroutine absorption_ismabshydrogen(col22, & 
zfac, e1, bnene, coeff, bxs2,cion,ifl,bener)
!
! This is routine that calculates the optical depth given the column densities
! Finally returns the absorption coefficient exp(-tau)
!
implicit none
integer,parameter :: nion=2, out_unit=20
integer :: bnene, ifl
integer :: i, j
double precision :: col22, col, tmp, cion(nion)
double precision :: bener(0:bnene), bxs2(0:nion,bnene), e1(0:bnene)
double precision :: tau, coeff(0:bnene) 
double precision :: zfac 


! Calculates the optical depth and the absorption coefficient exp(-tau)
col=col22*1.d22
e1(0)=(bener(0)*zfac)/1.d3
coeff(0)=0.0

!Column densities
 cion(1)=fgabnz(2)*col/1.d16 !Helium  

do i=1,bnene
e1(i)=(bener(i)*zfac)/1.d3
! In case you want to read xspec hydrogen column density
! bxs2(0,i)=hphoto(real(e1(i-1)),real(e1(i)))
! tau for hydrogen column density
tmp=col*bxs2(0,i)
! Calculates the optical depth and the absorption coefficient exp(-tau)
!tmp= tmp+(bxs2(1,i)*0.1*col) ! He I column density = 0.1 Nh
do j=1,nion
 tmp=tmp+(cion(j)*bxs2(j,i)*1.d16)
enddo
tau=tmp
coeff(i)=dexp(-tau)
enddo 
end subroutine absorption_ismabshydrogen
! ======================================= !
subroutine map_to_grid_ismabshydrogen(new_en, nne, old_en, one, nflux, old_flu, ifl)
  ! Map data from an original energy grid to a new one using linear interpolation
  implicit none

  integer, intent(in) :: one, nne, ifl
  double precision, intent(in) :: new_en(0:nne)
  double precision, intent(in) :: old_en(0:one), old_flu(one)
  real, intent(out) :: nflux(nne)

  integer :: i, j, k, bmin, bmax
  double precision :: s, etemp2, stemp, etemp

  nflux = 0.0

  do i = 1, nne
     ! Find bounding bins in old energy grid
     call dbinsrch_ismabshydrogen(new_en(i - 1), bmin, old_en, one + 1)
     call dbinsrch_ismabshydrogen(new_en(i), bmax, old_en, one + 1)
     bmin = bmin - 1
     bmax = bmax - 1

     if (bmin == bmax) then
        ! Single-bin interpolation
        if (new_en(i) <= old_en(1)) then
           s = old_flu(1)
        else if (new_en(i) > old_en(one)) then
           s = old_flu(one)
        else
           do j = 2, one
              if (new_en(i) > old_en(j - 1) .and. new_en(i) <= old_en(j)) then
                 etemp2 = 0.5d0 * (new_en(i) + new_en(i - 1))
                 s = old_flu(j - 1) + (old_flu(j) - old_flu(j - 1)) * &
                     (etemp2 - old_en(j - 1)) / (old_en(j) - old_en(j - 1))
                 exit
              end if
           end do
        end if
     else
        ! Average over a wider range
        stemp = 0.0d0
        etemp = 0.0d0
        do k = bmin, bmax
           stemp = stemp + old_flu(k) * (old_en(k) - old_en(k - 1))
           etemp = etemp + (old_en(k) - old_en(k - 1))
        end do
        if (etemp > 0.d0) then
           s = stemp / etemp
        else
           s = 0.d0
        end if
     end if

     nflux(i) = real(s)
  end do

end subroutine map_to_grid_ismabshydrogen

! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
subroutine dbinsrch_ismabshydrogen(e,k,ener,n)
  ! Binary search for bin index k such that:
  ! ener(k) <= e < ener(k+1)

  implicit none
  integer :: n, k, klo, khi, mid
  double precision :: e
  double precision :: ener(n)

  klo = 1
  khi = n

  ! Binary search loop
  do while (khi - klo > 1)
     mid = (khi + klo) / 2
     if (ener(mid) > e) then
        khi = mid
     else
        klo = mid
     end if
  end do

  if (klo < 1 .or. klo >= n) then
     print *, 'Error: Energy out of bounds. e = ', e
     stop
  end if

  k = klo
end subroutine dbinsrch_ismabshydrogen
! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
subroutine create_energy_grid_ismabshydrogen(emin,emax,en,nen)
  ! Log-spaced energy grid between emin and emax

  implicit none
  integer :: i, nen
  double precision :: en(nen)
  double precision :: emin, emax
  double precision :: log_emin, log_emax

  log_emin = dlog10(emin)
  log_emax = dlog10(emax)

  do i = 1, nen
     en(i) = 10.d0 ** (log_emin + (log_emax - log_emin) * dble(i) / dble(nen))
  end do
 
end subroutine create_energy_grid_ismabshydrogen

 
