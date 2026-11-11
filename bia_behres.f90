!================================!
!   VolumeLibrary : bia_behres   !
!================================!
subroutine BIA_Behres_Hyperobla(dbh, ht, FCLASS,&
     &MinBDFTTop, VOL)
  !     The source code is from BIA with little modification to work with NVEL
  !     2019/04/04 YW Assigned equation number I16BEHW000 for the equation
  !     It only work for 16 feet log.
  implicit none
  ! Variables
  real    :: dbh,ht,formclass,LogLength,MinBDFTTop,CalcBDFTVolume

  real    :: MerchHt,VOL(15)

  integer :: ISEG,numsegs,FCLASS,ILOGLEN
  real    :: BD1,BD2,DIB16,FC16
  real    :: D,DIB,BD79,RHT,BHT,SEG,XL,CDIB
  real    :: tht
  double precision :: BD80

  !     --- SPECIFICATION STATEMENTS
  if (FCLASS.lt.10) then
     formclass = 0.0
  else
     formclass = real(FCLASS)/100.0
  end if
  LogLength = 16.0
  tht = ht
  FC16 = formclass

  !     Note: there used to be a long set of code to calculate
  !     Form Class.  This needs to be calculated BEFORE coming
  !     to this subroutine

  CalcBDFTVolume = 0.0
  MerchHt = 0.0
  DIB16 = 0.0
  FC16 = 0.0
  if (formclass.eq.0.0) formclass = 0.7

  DIB16 = dbh * formclass
  if (DIB16.gt.dbh) DIB16 = dbh

  CalcBDFTVolume = 0.0
  SEG = 0.0

  DIB = DIB16
  BD79 = 0.0
  BD80 = 0.0
  RHT = 0.0

  if (DIB.lt.MinBDFTTop) then
     return
  end if

  !     --- CALCULATE VOLUME OF BUTT LOG
  if (DIB.lt.8.0) then
     BD79 = real((-0.083714 + 0.018569 * DIB + 0.059009&
          &* (DIB**2) - 0.003894 * (DIB**3)) * LogLength)
     CalcBDFTVolume = real(BD79)
  else
     BD80 = (-0.26875 - 0.12375 * DIB + 0.049375 *&
          &(DIB**2)) * LogLength
     CalcBDFTVolume = real(BD80)
  end if

  MerchHt = 17.3

  !     --- CALCULATE BHT (HEIGHT FROM TOP OF 1ST LOG TO TOP)
  BHT = real(tht - 17.3)
  SEG = 0
  numsegs = NINT(BHT / 16.3)
  do ISEG = 1, numsegs
     SEG = real(SEG + 16.3)
     BD80 = 0
     BD79 = 0
     RHT = BHT - SEG
     if (RHT.gt.0.0) then
        XL = RHT / BHT
        D = real(XL / (0.49 * XL + 0.51))

        !             ' --- CALCULATE DIB OF CURRENT LOG
        CDIB = DIB * D
        if (CDIB.ge.MinBDFTTop) then
           if (CDIB.le.8) then
              BD79 = real((-0.083714 + 0.018569 * CDIB&
                   &+ 0.059009 * (CDIB**2) - 0.003894&
                   &* (CDIB**3)) * LogLength)
              CalcBDFTVolume = CalcBDFTVolume + BD79
              BD1 = BD79
           else
              BD80 = (-0.26875 - 0.12375 * CDIB +&
                   &0.049375 * (CDIB**2)) * LogLength
              CalcBDFTVolume = real(CalcBDFTVolume&
                   &+ BD80)
              BD2 = real(BD80)
           end if
           MerchHt = real(MerchHt + (ISEG * 16.3))
        else
           !                  Segment top diameter less than minimum
           !                  add length back on to the log and exit loop
           !                                 Print #6, "FULLSEG "; Iseg;
           !                                 Print #6, Space(15) & CDIB &
           !                                 Print #6, "Segment Skipped"
           RHT = real(RHT + 16.3)
           exit
        end if
     end if
     10 continue
  end do

  !     --- CYCLE UP LAST SEGMENT IN 1' JUMPS
  SEG = 1.01875
  LogLength = 0
  BD79 = 0
  BD80 = 0

  numsegs = NINT(RHT / SEG)
  do ISEG = 1, numsegs
     RHT = RHT - SEG
     LogLength = LogLength + 1.0
     XL = RHT / BHT
     D = real(XL / (0.49 * XL + 0.51))
     CDIB = DIB * D
     if (CDIB.lt.MinBDFTTop) then
        if (CDIB.lt.8.0) then
           BD79 = real((-0.083714 + 0.018569 * CDIB&
                &+ 0.059009 * (CDIB**2) - 0.003894&
                &* (CDIB**3)) * LogLength)
           CalcBDFTVolume = CalcBDFTVolume + BD79
        else if (CDIB.ge.8.0) then
           BD80 = (-0.26875 - 0.12375 * CDIB + 0.049375&
                &* (CDIB**2)) * LogLength
           CalcBDFTVolume = real(CalcBDFTVolume + BD80)
        end if
        MerchHt = MerchHt + (ISEG * SEG)
        exit
     end if
     20 continue
  end do
  VOL(2) = CalcBDFTVolume
end
