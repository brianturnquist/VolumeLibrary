!============================!
!   VolumeLibrary : r8prep   !
!============================!
  !_______________________________________________________________________

subroutine r8Prep(volEq,dbhOb,topDib,topHt,ht1Prd,ht2Prd,htTot,&
     &spp,geog,COEFFS,forst,maxLen,&
     &minLen,merchL,mTopP,mTopS,stump,trim,minBfD,&
     &prod,iProd,sawDib,plpDib,short,shrtHt,errFlg,&
     &upsHt1,COEFFSO)
  !_______________________________________________________________________

  !  Parse out the info in the volume equation (volEq).  Check to see if
  !  the specified measurements are reasonable.  Get coefficients and
  !  merchantability rules for the specified species and product.
  !  2023/06/09 YW Set DIB17 = 0.1 when DIB17 is <= 0 to avoid divided by 0 error

  use DEBUG_MOD
  use CLKCOEF_MOD

  implicit none
  ! Common blocks
  include 'r8clkcoef.inc' !'R8CLKCOEF.INC'
  include 'r8dib.inc' !'R8dib.inc'
  include 'r8cfo.inc'

  ! Variables
  integer :: errFlg,spp,sppGrp,geog,iProd,k,sppIdx
  real    :: dbhOb,topDib,topHt,sawHt,maxLen,minLen
  real    :: dib17,upprHt,lowrHt,merchL,stump,upsHt1
  real    :: mTopP,mTopS,trim,minBfD,ht1Prd,ht2Prd,htTot
  TYPE(CLKCOEF):: COEFFS
  TYPE(CLKCOEF):: COEFFSO
  real    :: plpDib,sawDib,shrtHt,dbhIb
  character(len=10) :: volEq
  character(len=2) :: forst
  character(len=2) :: prod
  character(len=2) :: tmpStr
  logical :: short
  integer :: TOTSPP = 49
  integer :: GEOA,GSPEC,DONEFLAG,LASTFLAG,FIRST,LAST,HALF,CHECK,PTR
  integer :: SPGRP,REGN,SPEC
  real    :: FIXDI4,FIXDI79,BTR,DBTBH,MINLENT
  character(len=1) :: COR
  integer :: OPT,EVOD
  real    :: FCLSS,THT,FCDIB,FCMIN,DBH,HT2

  !  DW 08/22 initializations of variables otherwise uninitialized prior to MRULES call
  OPT = 0
  EVOD = 0
  MINLENT = 0
  BTR = 0
  DBTBH = 0

  if (volEq(10:10).lt.'0' .or. volEq(10:10).gt.'9') then
     tmpStr = volEq(8:9)
     volEq(8:10) = '0'//tmpStr
  end if
  read(prod,'(i2)',err = 210) iProd
  read(VOLEQ(8:10),'(I3)',err = 211)SPEC
  read(VOLEQ(2:2),'(I1)',err = 211)GEOA
  read(VOLEQ(1:1),'(I1)',err = 211)REGN
  spp = SPEC
  if (GEOA.lt.1.or. GEOA.gt.9.or. GEOA.eq.8) then
     ERRFLG = 1
     go to 999
  end if

  goto 220
  210 errFlg = 1
  return
  211 errFlg = 6
  return
  220 continue

  !-----Check to determine if the heights and diameters are reasonable.
  if (dbhOb.le.0.0) then
     errFlg = 3
  else if (ht1Prd.lt.0.0) then
     errFlg = 7
  else if (ht2Prd.lt.0.0) then
     errFlg = 8
  else if (htTot.lt.0.0) then
     errFlg = 10
  else if (ht1Prd.le.0.01.and. ht2Prd.le.0.01&
       &.and. htTot.le.0.01.and. upsHt1.le.0.01) then
     errFlg = 10
  else if (htTot.gt.0.0.and. ht1Prd.gt.0.0&
       &.and. htTot.lt.ht1Prd) then
     errFlg = 7
  else if (htTot.gt.0.0.and. ht2Prd.gt.0.0&
       &.and. htTot.lt.ht2Prd) then
     errFlg = 8
  else if (ht2Prd.gt.0.0.and. ht1Prd.gt.0.0&
       &.and. ht2Prd.lt.ht1Prd) then
     errFlg = 7
  else if (prod.ne.'01'.and.ht1Prd.gt.0.0) then
     !        errFlg=7
     !       R8 uses ht1Prd pulpwood broken HT
     ht2Prd = ht1Prd
  else if (upsHt1.gt.0.and. ht1Prd.gt.0&
       &.and. upsHt1.lt.ht1Prd) then
     errFlg = 10
  end if
  if (errFlg.ne.0) return

  if (SPEC.eq.123.or. SPEC.eq.197) then
     SPEC = 100
  else if (SPEC.eq.268) then
     SPEC = 261
  else if (SPEC.eq.313.or. SPEC.eq.314.or. SPEC.eq.317.or.&
       &SPEC.eq.650.or. SPEC.eq.651.or.&
       &SPEC.eq.691.or. SPEC.eq.711.or. SPEC.eq.742.or.&
       &SPEC.eq.762.or. SPEC.eq.920.or. SPEC.eq.930.or.&
       &SPEC.eq.545.or. SPEC.eq.546) then
     SPEC = 300
  else if (SPEC.eq.521.or. SPEC.eq.550.or. SPEC.eq.580.or.&
       &SPEC.eq.601.or. SPEC.eq.602.or. SPEC.eq.318) then
     SPEC = 500
  else if (SPEC.eq.804.or. SPEC.eq.817.or. SPEC.eq.820.or.&
       &SPEC.eq.823.or. SPEC.eq.825.or.&
       &SPEC.eq.826.or. SPEC.eq.830.or. SPEC.eq.834) then
     SPEC = 800
  end if

  GSPEC = GEOA*1000 + SPEC

  !-----Get species group to assign coefficients if there are none
  !     specific to the particular species.
  sppGrp = 0
  sppIdx = 0
  k = 1
  do 240, while(k.le.totSpp)
  if (spec.eq.DIBMEN(k,1)) then
     sppIdx = k
     k = totSpp
  else if (spec.lt.DIBMEN(k,1)) then
     k = totSpp
  end if
  k = k+1
  240 continue
  if (sppIdx.eq.0) then
     errflg = 1
     return
  else
     FIXDI4 = DIBMEN(sppIdx,2)
     FIXDI79 = DIBMEN(sppIdx,3)
  end if

  !     BINARY SEARCH FOR CORRECT COEFFICIENTS
  DONEFLAG = 0
  LASTFLAG = 0
  FIRST = 1
  LAST = 182
  do 5, while (DONEFLAG.eq.0)
  if (FIRST.eq.LAST) LASTFLAG = 1
  !DETERMINE WHERE TO CHECK
  HALF = ((LAST-FIRST+1)/2) + FIRST

  CHECK = R8CF(HALF,1)*1000+R8CF(HALF,2)
  !FOUND THE COEFFECIENTS
  if (GSPEC.eq.CHECK) then
     PTR = HALF
     DONEFLAG = 1
     !MOVE DOWN THE LIST
  else if (GSPEC.gt.CHECK) then
     FIRST = HALF
     !MOVE UP THE LIST
  else if (GSPEC.lt.CHECK) then
     LAST = HALF - 1
  end if
  !DID NOT FIND A MATCH
  if (LASTFLAG.eq.1.and. DONEFLAG.eq.0) then
     !END ROUTINE, NO MATCH
     if (GEOA.eq.9) then
        !                SPECPR = 0
        !                EQNPR = 0
        !                GEOAPR = 0
        ERRFLG = 6
        go to 999
        !SET GEOAPR TO 9 AND RETRY THE SEARCH
     else
        GEOA = 9
        GSPEC = GEOA*1000 + SPEC
        !FIRST = 1
        !geocode 9 start at 135
        FIRST = 135
        LAST = 182
        LASTFLAG = 0
     end if
  end if
  5 continue

  !     END BINARY SEARCH

  SPGRP = R8CF(PTR,3) + .5
  if (SPGRP.ne.100.and. SPGRP.ne.300.and. SPGRP.ne.500) then
     ERRFLG = 6
     go to 999
  end if
  dbhIb = R8CF(PTR,4)+R8CF(PTR,5)*dbhOb

  !-----Get coefficients for inside-bark calculations.  All are for
  !     total height, except a17 and b17, which correspond to the top DIB.
  COEFFS%R = 0.0
  COEFFS%C = 0.0
  COEFFS%E = 0.0
  COEFFS%P = 0.0
  COEFFS%B = 0.0
  COEFFS%A = 0.0
  COEFFS%A4 = 0.0
  COEFFS%B4 = 0.0
  COEFFS%A17 = 0.0
  COEFFS%B17 = 0.0
  COEFFS%DBHIB = 0.0
  COEFFS%DIB17 = 0.0
  COEFFS%TOTHT = 0.0

  COEFFS%R = TOTAL(sppIdx,2)
  COEFFS%C = TOTAL(sppIdx,3)
  COEFFS%E = TOTAL(sppIdx,4)
  COEFFS%P = TOTAL(sppIdx,5)
  COEFFS%B = TOTAL(sppIdx,6)
  COEFFS%A = TOTAL(sppIdx,7)
  COEFFS%A4 = R8CF(PTR,4)
  COEFFS%b4 = R8CF(PTR,5)
  COEFFS%FIXDI = FIXDI4
  COEFFS%SPGRP = SPGRP

  !-----Get coefficients for outside-bark calculations. All are for
  !     total height
  COEFFSO%R = 0.0
  COEFFSO%C = 0.0
  COEFFSO%E = 0.0
  COEFFSO%P = 0.0
  COEFFSO%B = 0.0
  COEFFSO%A = 0.0
  COEFFSO%A4 = 0.0
  COEFFSO%B4 = 0.0
  COEFFSO%A17 = 0.0
  COEFFSO%B17 = 0.0
  COEFFSO%DBHIB = 0.0
  COEFFSO%DIB17 = 0.0
  COEFFSO%TOTHT = 0.0

  COEFFSO%R = OTOTAL(sppIdx,2)
  COEFFSO%C = OTOTAL(sppIdx,3)
  COEFFSO%E = OTOTAL(sppIdx,4)
  COEFFSO%P = OTOTAL(sppIdx,5)
  COEFFSO%B = OTOTAL(sppIdx,6)
  COEFFSO%A = OTOTAL(sppIdx,7)

  666 if (htTot.gt.0.0) then
     COEFFS%A17 = R8CF(PTR,14)
     COEFFS%B17 = R8CF(PTR,15)
     COEFFS%AFI = R8CF(PTR,6)
     COEFFS%BFI = R8CF(PTR,7)
     COEFFSO%A17 = R8CFO(PTR,4)
     COEFFSO%B17 = R8CFO(PTR,5)
  else if (upsHt1.gt.0.0) then
     if (PROD.eq.'01') then
        !       upsHT1 is the height to 7/9 top
        if (SPEC.lt.300) then
           COEFFS%A17 = R8CF(PTR,10)
           COEFFS%B17 = R8CF(PTR,11)
        else
           COEFFS%A17 = R8CF(PTR,12)
           COEFFS%B17 = R8CF(PTR,13)
        end if
        COEFFSO%A17 = R8CFO(PTR,8)
        COEFFSO%B17 = R8CFO(PTR,9)
     else
        !       upsHT1 is the height to 4" top
        COEFFS%A17 = R8CF(PTR,8)
        COEFFS%B17 = R8CF(PTR,9)
        COEFFSO%A17 = R8CFO(PTR,6)
        COEFFSO%B17 = R8CFO(PTR,7)
     end if
  else if (ht2Prd.gt.0) then
     COEFFS%A17 = R8CF(PTR,8)
     COEFFS%B17 = R8CF(PTR,9)
     COEFFSO%A17 = R8CFO(PTR,6)
     COEFFSO%B17 = R8CFO(PTR,7)
  else if (ht1Prd.gt.0) then
     if (SPEC.lt.300) then
        COEFFS%A17 = R8CF(PTR,10)
        COEFFS%B17 = R8CF(PTR,11)
     else
        COEFFS%A17 = R8CF(PTR,12)
        COEFFS%B17 = R8CF(PTR,13)
     end if
     COEFFSO%A17 = R8CFO(PTR,8)
     COEFFSO%B17 = R8CFO(PTR,9)
  else
     errFlg = 1
     return
  end if

  if (dbhIb.lt.FIXDI4) dbhib = FIXDI4
  COEFFS%DBHIB = dbhIb
  !      COEFFS%DIB17=dbhOb*(COEFFS%A17+COEFFS%B17*(17.3/topHt)**2)
  !      IF(COEFFS%DIB17.LT.0.0) COEFFS%DIB17=0.0

  !-----Get merchantability rules
  call MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,MINLEN,&
       &MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,&
       &PROD)
  !     R8 use UPSHT1 for HT2PRD in the old Clark equation
  !     we need to put UPSHT1 to HT2PRD for the new Clark eq
  if (prod.ne.'01'.and.UPSHT1.gt.0.and.&
       &HT2PRD.le.0.and.MTOPS.eq.4.0) then
     HT2PRD = UPSHT1
  end if

  !    R8 Clark uses diameter outside bark for top diameter.
  !    This includes MTOPP, MTOPS, sawDib, plpDib, topDib
  if (SPEC.lt.300) then
     if (mTopP.ne.0) then
        sawDib = mTopP
     else
        sawDib = 7
     end if
  else
     if (mTopP.ne.0) then
        sawDib = mTopP
     else
        sawDib = 9
     end if
  end if
  if (mTopS.ne.0) then
     plpDib = mTopS
  else
     plpDib = 4
  end if

  !-----Get top height and top DIB Oct 2022 Increase filter to 17.4' from 17.3 DW
  short = .false.
  if (htTot.gt.0.0) then
     topDib = 0.0
     if (htTot.ge.17.4) then
        topHt = htTot
     else
        short = .true.
        topHt = 17.4
        shrtHt = htTot
     end if

     COEFFS%DIB17 = dbhOb*(COEFFS%A17+COEFFS%B17*(17.3/topHt)**2)
     if (COEFFS%DIB17.lt.0.0) COEFFS%DIB17 = 0.1

     FCLSS = COEFFS%DIB17
     DBH = dbhOb
     THT = htTot
     if (SPEC.ne.221.and. SPEC.ne.222.and. SPEC.ne.544) then
        if (SPGRP.eq.100) then
           if (THT.lt.32.5)                   FCMIN = 56
           if (THT.ge.32.5.and. THT.lt.37.5) FCMIN = 64
           if (THT.ge.37.5.and. THT.lt.42.5) FCMIN = 66
           if (THT.ge.42.5)                   FCMIN = 67
        else if (SPGRP.eq.300) then
           if (THT.lt.32.5)                   FCMIN = 57
           if (THT.ge.32.5.and. THT.lt.37.5) FCMIN = 60
           if (THT.ge.37.5.and. THT.lt.42.5) FCMIN = 64
           if (THT.ge.42.5)                   FCMIN = 67
        else
           if (THT.lt.32.5)                   FCMIN = 58
           if (THT.ge.32.5.and. THT.lt.37.5) FCMIN = 65
           if (THT.ge.37.5.and. THT.lt.42.5) FCMIN = 67
           if (THT.ge.42.5)                   FCMIN = 69
        end if
        FCDIB = DBH * FCMIN * .01
        if (THT.lt.47.5.and. FCLSS.lt.FCDIB) FCLSS = FCDIB
        COEFFS%DIB17 = FCLSS
        COEFFSO%DIB17 = (FCLSS - COEFFS%AFI)/COEFFS%BFI
     end if
  else if (upsHt1.gt.0.0) then
     ! 02/24/2021 not necessary to check Prod. HT to 4 is already set to HT2PRD
     !        IF(PROD.EQ.'01')THEN
     goto 888
     !        ELSEIF(PROD.EQ.'08')THEN
     !        !Old equation may enter TotalHt into UPSHT1 field
     !          htTot = upsHt1
     !          GOTO 666
     !        ELSE
     !          GOTO 777
     !        ENDIF
  else if (ht2Prd.gt.0) then
     777 topDib = 4
     !topDib=FIXDI4
     if (upsHT1.gt.0.0) then
        if (upsHt1.ge.17.4) then
           topHt = upsHt1
        else
           short = .true.
           topHt = 17.4
           shrtHt = upsHt1
        end if
     else
        if (ht2Prd.ge.17.4) then
           topHt = ht2Prd
        else
           short = .true.
           topHt = 17.4
           shrtHt = ht2Prd
        end if
     end if
     COEFFS%DIB17 = dbhOb*(COEFFS%A17+COEFFS%B17*(17.3/topHt)**2)
     if (COEFFS%DIB17.lt.0.0) COEFFS%DIB17 = 0.1

     DBH = dbhOb
     HT2 = topHt
     FCLSS = COEFFS%DIB17
     if (FCLSS.lt.FIXDI4) FCLSS = FIXDI4

     if (SPEC.ne.221.and. SPEC.ne.222.and. SPEC.ne.544) then
        if (SPGRP.eq.100) then
           if (DBH.lt.5.5) then
              if (HT2.lt.22.5)                   FCMIN = 70
              if (HT2.ge.22.5.and. HT2.lt.27.5) FCMIN = 75
              if (HT2.ge.27.5)                   FCMIN = 80
           else if (DBH.lt.6.5) then
              if (HT2.lt.22.5)                   FCMIN = 66
              if (HT2.ge.22.5.and. HT2.lt.27.5) FCMIN = 70
              if (HT2.ge.27.5)                   FCMIN = 74
           else if (DBH.lt.7.5) then
              if (HT2.lt.22.5)                   FCMIN = 64
              if (HT2.ge.22.5.and. HT2.lt.27.5) FCMIN = 67
              if (HT2.ge.27.5)                   FCMIN = 72
           else if (DBH.lt.8.5) then
              if (HT2.lt.22.5)                   FCMIN = 60
              if (HT2.ge.22.5.and. HT2.lt.27.5) FCMIN = 67
              if (HT2.ge.27.5)                   FCMIN = 69
           else
              if (HT2.lt.22.5)                   FCMIN = 59
              if (HT2.ge.22.5.and. HT2.lt.27.5) FCMIN = 67
              if (HT2.ge.27.5)                   FCMIN = 69
           end if
        else if (SPGRP.eq.300) then
           if (DBH.lt.5.5) then
              if (HT2.lt.22.5)                   FCMIN = 74
              if (HT2.ge.22.5.and. HT2.lt.27.5) FCMIN = 76
              if (HT2.ge.27.5)                   FCMIN = 76
           else if (DBH.lt.6.5) then
              if (HT2.lt.22.5)                   FCMIN = 65
              if (HT2.ge.22.5.and. HT2.lt.27.5) FCMIN = 69
              if (HT2.ge.27.5)                   FCMIN = 74
           else if (DBH.lt.7.5) then
              if (HT2.lt.22.5)                   FCMIN = 61
              if (HT2.ge.22.5.and. HT2.lt.27.5) FCMIN = 65
              if (HT2.ge.27.5)                   FCMIN = 68
           else
              if (HT2.lt.22.5)                   FCMIN = 60
              if (HT2.ge.22.5.and. HT2.lt.27.5) FCMIN = 62
              if (HT2.ge.27.5)                   FCMIN = 68
           end if
        else
           if (DBH.lt.5.5) then
              if (HT2.lt.22.5)                   FCMIN = 71
              if (HT2.ge.22.5.and. HT2.lt.27.5) FCMIN = 72
              if (HT2.ge.27.5)                   FCMIN = 76
           else if (DBH.lt.6.5) then
              if (HT2.lt.22.5)                   FCMIN = 68
              if (HT2.ge.22.5.and. HT2.lt.27.5) FCMIN = 71
              if (HT2.ge.27.5)                   FCMIN = 74
           else
              if (HT2.lt.22.5)                   FCMIN = 63
              if (HT2.ge.22.5.and. HT2.lt.27.5) FCMIN = 67
              if (HT2.ge.27.5)                   FCMIN = 70
           end if
        end if
        FCDIB = DBH * FCMIN * .01
        if (HT2.lt.32.5.and. FCLSS.lt.FCDIB) FCLSS = FCDIB
     end if
     COEFFS%DIB17 = FCLSS

  else
     888 if (spec.lt.300) then
        topDib = 7
     else
        topDib = 9
     end if
     !topDib=FIXDI79
     !       if sawtimber topHt is provided, no recalc topHt. 11/15/2011 (yw)
     if (upsHt1.gt.0) then
        if (upsHt1.ge.17.4) then
           topHt = upsHt1
        else
           short = .true.
           topHt = 17.4
           shrtHt = ht1Prd
        end if
        if (ht1prd.le.0.and.MTOPP.eq.topDib) ht1Prd = upsHt1
     else
        if (ht1Prd.ge.17.4) then
           topHt = ht1Prd
        else
           short = .true.
           topHt = 17.4
           shrtHt = ht1Prd
        end if
     end if
     COEFFS%DIB17 = dbhOb*(COEFFS%A17+COEFFS%B17*(17.3/topHt)**2)
     FCLSS = COEFFS%DIB17
     if (FCLSS.lt.FIXDI79) FCLSS = FIXDI79
     COEFFS%DIB17 = FCLSS
     if (COEFFS%DIB17.lt.0.0) COEFFS%DIB17 = 0.1

  end if
  if (dbhOb.le.topDib) errFlg = 11

  COEFFSO%DBHIB = dbhOb
  if (htTot.le.0.0) then
     COEFFSO%DIB17 = dbhOb*(COEFFSO%A17+COEFFSO%B17*(17.3/topHt)**2)
  end if
  if (COEFFSO%DIB17.lt.COEFFS%DIB17) COEFFSO%DIB17 = COEFFS%DIB17
  if (COEFFSO%DIB17.lt.topDib) then
     !COEFFSO%DIB17=topDib
     if (topHt.gt.17.2) then
        COEFFSO%DIB17 = topDib+(dbhOb-topDib)*(topHt-17.3)/(topHt-4.5)
     end if
  end if
  999 return
  end subroutine R8PREP
