!============================!
!   VolumeLibrary : r6vol2   !
!============================!
  !== last modified  03-29-2004
subroutine R6VOL2(VOLEQ,DBHOB,HT,VOL,ERRFLAG)
  !  CREATED:   06-05-91

  !  PURPOSE:   THIS ROUTINE WILL CALCULATE R6 BDFT VOLUMES ACCORDING TO
  !                THE MOST RECENTLY ADDED VOLUME EQUATIONS.
  !  DECLARE VARIABLES

  ! Variables
  character(len=10) :: VOLEQ
  integer :: EQU,ERRFLAG

  real    :: DBHOB,HT,VOL(15),COEFF(6,5)

  data ((COEFF(I,J), J = 1,5), I = 1,6) /&
       &0.0000000,  1.9510920, -.1313649,  -.3926337,  .0115273,&
       &-38.6517248, 14.7619230, -.2773275, -1.3283856,  .0142368,&
       &-50.9378600, 21.4280400, -.4476645, -2.1493860,  .0203710,&
       &0.0000000,  3.9434930, -.3229676,  -.5601420,  .0144415,&
       &-50.9602199, 21.9511399, -.5506570, -2.3294660,  .0247944,&
       &8.7000000,  0.0000000, 0.0000000, -0.7600000, 0.0200000/

  ERRFLAG = 0
  if (VOLEQ(8:10).eq.'205' .and. VOLEQ(1:3).eq.'601') then
     EQU = 1
  else if (VOLEQ(8:10).eq.'263' .and. VOLEQ(1:3).eq.'601') then
     EQU = 2
  else if (VOLEQ(8:10).eq.'015' .and. VOLEQ(1:3).eq.'601') then
     EQU = 3
  else if (VOLEQ(8:10).eq.'204' .and. VOLEQ(1:3).eq.'602') then
     EQU = 4
  else if (VOLEQ(8:10).eq.'015' .and. VOLEQ(1:3).eq.'602') then
     EQU = 5
  else if (VOLEQ(8:10).eq.'108' .and. VOLEQ(1:3).eq.'602') then
     EQU = 6
  else if (VOLEQ(8:10).eq.'122') then
     EQU = 7
  else
     ERRFLAG = 0
     return
  end if

  if (EQU.eq.7) then
     VOL(2) = -2.9815 + (-0.2013*DBHOB*DBHOB) + (0.000141*DBHOB**3 *&
          &HT) + (0.0084*DBHOB*DBHOB*HT)
  else
     VOL(2) = COEFF(EQU,1) + COEFF(EQU,2)*DBHOB + COEFF(EQU,3)*HT +&
          &COEFF(EQU,4)*DBHOB*DBHOB + COEFF(EQU,5)*DBHOB*DBHOB*HT
  end if

  VOL(3) = VOL(2)

  return
end
