!===========================!
!   VolumeLibrary : r4d2h   !
!===========================!
  !_______________________________________________________________________

subroutine R4D2H (VOLEQ,HTTOT,DBHOB,DRC,FCLASS,VOL,ERRFLAG)
  !_______________________________________________________________________

  !..    This routine wil calculate cubic volumes using the equations
  !..    developed by David Chojnacky (Research Paper INT-339).

  use DEBUG_MOD

  implicit none

  !REV  Created ???
  !REV  Revised TDH 01/12/2010
  !REV  Fixed bug, missing '.' in species 066 equation
  !REV  Added 402DVEW065 and 403DVEW065 YW 2018/09/04

  ! Variables
  character(len=10) :: VOLEQ
  real    :: HTTOT,DBHOB,DRC,VOL(15),D2H
  integer :: ERRFLAG,MSTEM,FCLASS,I

  if (DEBUG%MODEL) then
     write  (LUDBG, 10) ' -->Enter R4D2H'
     10 format (A)
  end if

  !     look for drc, if not use dbhob
  if (DBHOB .le.0.0.and. DRC.le.0.0) then
     ERRFLAG = 3
     go to 1000
  end if
  if ( HTTOT .le.0.0) then
     ERRFLAG = 4
     go to 1000
  end if

  do 100, I = 1,15
  VOL(I) = 0
  100 continue

  if (DRC .gt.0) then
     D2H = DRC*DRC*HTTOT
  else
     D2H = DBHOB*DBHOB*HTTOT
  end if

  if (DEBUG%MODEL) then
     write  (LUDBG, 110)' DRC     HTTOT     DBHOB     D2H'
     110 format (A)
     write  (LUDBG, 115)DRC, HTTOT, DBHOB, D2H
     115 format (4F8.1)
  end if

  if (FCLASS .eq.1) then
     MSTEM = 1
  else
     MSTEM = 0
  end if
  !     find species, do the calculation
  !     Western Juniper
  if (VOLEQ(8:10).eq.'064') then
     VOL(1) = (-0.22048 + 0.125468*D2H**(1./3.)+0.100092*MSTEM)**3.
     VOL(4) = VOL(1)
     !     Rocky Mountain Juniper
  else if (VOLEQ(8:10).eq.'066') then
     VOL(1) = (0.02434+0.119106*D2H**(1./3.))**3.
     VOL(4) = VOL(1)

     if (DEBUG%MODEL) then
        write  (LUDBG, 175)' D2H ', D2H,' VOL(1)',VOL(1)
        175 format (A,F8.1,A,F8.1)
     end if

     !     Utah Juniper
  else if (VOLEQ(8:10).eq.'065') then
     if (VOLEQ(2:3) .eq. '01') then
        !         --W. Colorado, E. Utah, Wyoming
        VOL(1) = (-0.08728+0.135420*D2H**(1./3.)-0.019587*MSTEM)**3.
     else if (VOLEQ(2:3) .eq. '02') then
        !         --Ely BLM District Nevada
        VOL(1) = (-.03655+.135689*D2H**(1./3.)-0.018476*MSTEM)**3.
     else if (VOLEQ(2:3) .eq. '03') then
        !         --Winnemucca and Susanville BLM Districts in Nevada
        VOL(1) = (.04829+.114358*D2H**(1./3.)-0.045779*MSTEM)**3.
     else
        !         --S. Idaho, Parts of Nevada, W. Utah
        VOL(1) = (-0.13386+0.133726*D2H**(1./3.)+0.036329*MSTEM)**3.
     end if
     !Set to 0.1 for tree DIA < 3 (20240426)
     if (DBHOB.lt.3.and.DRC.lt.3) VOL(1) = 0.1
     VOL(4) = VOL(1)
     !     Single Leaf Pinyon Pine
  else if (VOLEQ(8:10).eq.'133') then
     VOL(1) = (-0.14240 + 0.148190*D2H**(1./3.)-0.016712*MSTEM)**3.
     !Set to 0.1 for tree DIA < 3 (20240426)
     if (DBHOB.lt.3.and.DRC.lt.3) VOL(1) = 0.1
     VOL(4) = VOL(1)
     !     Pinyon Pine
  else if (VOLEQ(8:10).eq.'106') then
     VOL(1) = (-0.20296 + 0.150283*D2H**(1./3.)+0.054178*MSTEM)**3.
     VOL(4) = VOL(1)
     !     Mountain Mahogany
  else if (VOLEQ(8:10).eq.'475') then
     VOL(1) = (-0.13363 + 0.128222*D2H**(1./3.)+0.080208*MSTEM)**3.
     VOL(4) = VOL(1)
     !     Other Hardwoods
  else if (VOLEQ(8:10).eq.'998') then
     VOL(1) = (-0.13822 + 0.121850*D2H**(1./3.))**3.
     VOL(4) = VOL(1)
  end if

  1000 return
end
