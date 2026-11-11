!=================================!
!   VolumeLibrary : bia_johnson   !
!=================================!
subroutine VolEq_Johnson(dbh, ht, FCLASS, VOL)

  implicit none

  ! Variables
  integer :: ispc,FCLASS
  real    :: dbh,ht,formclass,LogLength,MinCUFTDBH,MinCUFTTop,strVolEq,CalcCUFTVolume,VOL(15)

  real    :: UPPER,BUTT,C,C3,B,A

  real    :: FC16
  real    :: DIB16,MerchHt

  !     SPECIFICATION STATEMENTS
  real    :: DIB,BD79
  double precision :: BD80
  real    :: D,RHT,SEG

  !     CALCULATE CU FT VOL WITH JOHNSON'S FORM CLASS FORMULA

  !     Note: there used to be a long set of code to calculate Form
  !     Class.  This needs to be calculated BEFORE coming to this
  !     subroutine
  MinCUFTDBH = 1.0
  if (FCLASS.lt.10) then
     formclass = 0.0
  else
     formclass = real(FCLASS)/100.0
  end if
  FC16 = formclass

  CalcCUFTVolume = 0
  MerchHt = 0
  DIB16 = 0
  FC16 = 0
  if (formclass.eq.0) formclass = 0.72

  DIB16 = dbh * formclass
  if (DIB16.gt.dbh) DIB16 = dbh

  CalcCUFTVolume = 0.0
  SEG = 0.0
  DIB = DIB16
  BD79 = 0
  BD80 = 0
  RHT = 0

  A = 0
  B = 0
  C3 = 0
  C = 0
  D = 0
  BUTT = 0.0
  UPPER = 0.0
  CalcCUFTVolume = 0.0
  if (dbh.lt.MinCUFTDBH) then
     CalcCUFTVolume = 0
     return
  else if (dbh.lt.6) then
     BUTT = real(((dbh - 4.0) * 0.5) * (-4.68329&
          &+ (18.7668 * formclass)&
          &- (12.18 * (formclass**2))))
  else
     BUTT = real(8.436 - 2.608 * dbh + 0.070242 * (dbh**2)&
          &+ 3.1278 * DIB16 - 12.18 * (formclass**2))
  end if
  if (DIB16.lt.4.0.or. ht.le.17) then
     CalcCUFTVolume = BUTT
  else
     A = real((ht - 17.0) * (DIB16**2) * (0.022716))
     B = real((DIB16 - 4.0) / (DIB16 - 1.96))
     C3 = real(((DIB16 - 1.96)**2) / (DIB16**2))
     C = real(LOG(C3))
     D = real((0.51 * (DIB16 - 4.0)) / (DIB16))
     UPPER = real((A) * (B - 1.401656 - (1.040816 * C)&
          &+ D))
     CalcCUFTVolume = BUTT + UPPER
  end if
  VOL(4) = CalcCUFTVolume
end
