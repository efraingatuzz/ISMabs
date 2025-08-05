! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! ismabsar
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! XSPEC local model for ISM absorption due to argon
! Version 1.0 July 2025 
!
! - This version only has CI-CVI as free parameters. 
! 
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
subroutine ismabsar(ear, ne, param, ifl, photar)
!
! The main routine to call all subroutines
!
implicit none
integer,parameter :: num_param = 19, out_unit=20, nion=18
integer,parameter :: nemod=650000 !Number of elements for each ion cross section.
integer :: ne, ifl, a
double precision :: rshift, emod(1:nemod), coemod(nemod), bxs(0:nion,nemod), cion(nion), bener(1:nemod) 
double precision :: N_N_1, N_N_2, N_N_3, N_N_4, N_N_5, N_N_6, N_N_7, N_N_8 
double precision :: N_N_9,N_N_10,N_N_11,N_N_12,N_N_13,N_N_14,N_N_15,N_N_16
double precision :: N_N_17,N_N_18
double precision :: zfac
real :: ear(0:ne), param(num_param),photar(ne)

logical :: startup=.true.
character (len=40) version
version='1.0'
 if(startup)then
  print *, ' '
  print *, 'ISMabs_argon: ISM absorption model Version ',version 
  print *, 'NOTE: this model does not include hydrogen or helium, only Ar'       
  print *, ' '
  call read_cross_sections_ismabsar( nemod,bxs,ifl)
  call create_energy_grid_ismabsar(1.d1,1.d4,bener,nemod) !Absorption coefficient calculation grid  = cross section grid
  startup=.false.  
 endif
! Model parameters
N_N_1 = param(1)
N_N_2 = param(2)
N_N_3 = param(3)
N_N_4 = param(4)
N_N_5 = param(5)
N_N_6 = param(6)
N_N_7 = param(7)
N_N_8 = param(8)
N_N_9 = param(9)
N_N_10 = param(10)
N_N_11 = param(11)
N_N_12 = param(12)
N_N_13 = param(13)
N_N_14 = param(14)
N_N_15 = param(15)
N_N_16 = param(16)
N_N_17 = param(17)
N_N_18 = param(18)
rshift = param(19)
zfac = 1/(1.d0+dble(rshift)) 

call absorption_ismabsar(N_N_1, N_N_2, &
N_N_3, N_N_4, N_N_5, N_N_6, N_N_7, N_N_8, & 
N_N_9, N_N_10, N_N_11, N_N_12,& 
N_N_13, N_N_14, N_N_15, N_N_16,& 
N_N_17, N_N_18,& 
zfac, emod, nemod, coemod,bxs,cion,ifl,bener)

call map_to_grid_ismabsar(dble(ear),ne,emod,nemod,photar,coemod,ifl)
return
end subroutine ismabsar

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine read_cross_sections_ismabsar(bnene, xs, ifl)
  !---------------------------------------------------------------------
  ! Reads all cross sections and maps them onto a defined energy grid.
  ! Looks for the file at:
  !     <ismabsar_root>/atomic_data/AtomicData.fits
  !---------------------------------------------------------------------

  implicit none
  integer, parameter :: nion = 18, out_unit = 20
  integer, intent(in)  :: bnene, ifl
  double precision, intent(out) :: xs(0:nion, bnene)

  ! FITS I/O variables
  integer :: inunit, status, hdutype, colnum, readwrite, blocksize
  integer :: felem, nulld, i, j
  logical :: anynull
  double precision :: ener(0:nion, bnene)
  integer :: nemax(0:nion)

  ! File and path strings
  character(len=*), parameter :: fileloc = '/atomic_data/AtomicData.fits'
  character(len=*), parameter :: ismreadchat = 'ismabs: reading from '
  character(len=1024) :: local_dir1, local_dir2, ismabsar_root, filename2
  character(len=2048) :: chatmsg

  !--- Set max number of energy bins per ion
  nemax = 650000

  !--- Define data root location
  local_dir1 = '/media/efrain/DATA/softwares/modelosXSPEC/ismabs/ismabs_no_turb/ismabs_species/i'
  local_dir2 = 'smabs.Ar.dev'
  ismabsar_root = trim(local_dir1) // trim(local_dir2)
  filename2 = trim(ismabsar_root) // fileloc
  chatmsg = trim(ismreadchat) // trim(filename2)

  call xwrite(chatmsg, out_unit)

  !--- FITS file setup
  status = 0
  readwrite = 0
  blocksize = 1
  felem = 1
  nulld = 0

  !--- Open FITS file
  call ftgiou(inunit, status)
  call ftopen(inunit, filename2, readwrite, blocksize, status)
  if (status /= 0) goto 900

  !--- Move to binary table extension
  call ftmahd(inunit, 2, hdutype, status)
  if (status /= 0) goto 900

  !--- Read common energy grid (column 1 for ion 0 and 1)
  do j = 1, nemax(0)
    call ftgcvd(inunit, 1, j, felem, 1, nulld, ener(0, j), anynull, status)
    call ftgcvd(inunit, 1, j, felem, 1, nulld, ener(1, j), anynull, status)
  end do

  !--- Copy energy grid to all ions
  do i = 2, nion
    ener(i, 1:nemax(i)) = ener(0, 1:nemax(i))
  end do

  !--- Read cross sections from columns 2–8
  do i = 0, nion
    colnum = i + 2  ! Assuming ion 0 starts at col 2
    do j = 1, nemax(i)
      call ftgcvd(inunit, colnum, j, felem, 1, nulld, xs(i, j), anynull, status)
    end do
  end do

  !--- Final error check
900 continue
if (index(filename2, 'AtomicData') > 0) then
    status = 0
endif 

  if (status /= 0) then
    write(*,*) 'ERROR: Unable to read cross sections from ', trim(filename2)
  endif

  !--- Close file and release unit
  call ftclos(inunit, status)
  call ftfiou(inunit, status)

end subroutine read_cross_sections_ismabsar

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine absorption_ismabsar(N_N_1, N_N_2, N_N_3, N_N_4, N_N_5, N_N_6,  N_N_7, &
				  N_N_8, N_N_9, N_N_10, N_N_11, N_N_12,&
				  N_N_13, N_N_14, N_N_15, N_N_16,&
				  N_N_17, N_N_18,&
                                   zfac, e1, bnene, coeff, bxs2, cion, ifl, bener)
  !--------------------------------------------------------------------
  ! Calculates the optical depth given column densities of ions
  ! Returns the absorption coefficient: coeff(i) = exp(-tau)
  !--------------------------------------------------------------------

  implicit none
  integer, parameter :: nion = 18
  integer, intent(in) :: bnene, ifl
  double precision, intent(in) :: zfac
  double precision, intent(in) :: N_N_1, N_N_2, N_N_3, N_N_4, N_N_5, N_N_6, N_N_7, N_N_8
  double precision, intent(in) :: N_N_9, N_N_10, N_N_11, N_N_12, N_N_13, N_N_14
  double precision, intent(in) :: N_N_15, N_N_16, N_N_17, N_N_18
  double precision, intent(in) :: bener(0:bnene), bxs2(0:nion, bnene)
  double precision, intent(out) :: coeff(0:bnene), e1(0:bnene)
  double precision, intent(out) :: cion(nion)

  integer :: i, j
  double precision :: tau, tmp

  ! Assign column densities to ion array
  cion(0) = N_N_1
  cion(1) = N_N_2
  cion(2) = N_N_3
  cion(3) = N_N_4
  cion(4) = N_N_5
  cion(5) = N_N_6
  cion(6) = N_N_7
  cion(7) = N_N_8
  cion(8) = N_N_9
  cion(9) = N_N_10
  cion(10) = N_N_11
  cion(11) = N_N_12
  cion(12) = N_N_13
  cion(13) = N_N_14
  cion(14) = N_N_15
  cion(15) = N_N_16
  cion(16) = N_N_17
  cion(17) = N_N_18

  ! Loop over all energy bins to compute e1 and optical depth
  do i = 0, bnene
     e1(i) = (bener(i) * zfac) / 1.0d3

     ! Compute optical depth tau = Σ N_i * σ_i(E)
     tmp = 0.0d0
     do j = 0, nion - 1
        tmp = tmp + cion(j) * bxs2(j, i) * 1.0d16
     end do

     tau = tmp
     coeff(i) = dexp(-tau)
  end do

end subroutine absorption_ismabsar

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine map_to_grid_ismabsar(new_en, nne, old_en, one, nflux, old_flu, ifl)
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
     call dbinsrch_ismabsar(new_en(i - 1), bmin, old_en, one + 1)
     call dbinsrch_ismabsar(new_en(i), bmax, old_en, one + 1)
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

end subroutine map_to_grid_ismabsar

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine dbinsrch_ismabsar(e, k, ener, n)
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
end subroutine dbinsrch_ismabsar

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
subroutine create_energy_grid_ismabsar(emin, emax, en, nen)
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

end subroutine create_energy_grid_ismabsar

 
