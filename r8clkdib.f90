!==============================!
!   VolumeLibrary : r8clkdib   !
!==============================!
  ! YW 2016/03/08 Modofied the DIB calculation for TOPHT less than 17.3 to avoid Nan vaue error.
subroutine R8PREPCOEF(VOLEQ, COEFFS, ERRFLAG)
  use CLKCOEF_MOD

  implicit none
  TYPE(CLKCOEF):: COEFFS
  ! Variables
  character(len=10) :: VOLEQ
  integer :: ERRFLAG

  !      *** LOCAL VARIABLES ***
  integer :: SPEC,GEOA,EQN,SPECPR,GEOAPR,EQNPR,SPGRP
  integer :: PTR,FIRST,LAST,HALF,GSPEC,CHECK,DONEFLAG,LASTFLAG
  integer :: DIBCNT,TOTCNT,FOURCNT,NINECNT
  ! Common blocks
  include 'r8clkcoef.inc' !'R8CLKCOEF.INC'
  include 'r8dib.inc' !'R8DIB.INC'

  read(VOLEQ(8:10),'(I3)')SPEC
  read(VOLEQ(2:2),'(I1)')GEOA

  if (GEOA.lt.1.or. GEOA.gt.9.or. GEOA.eq.8) then
     ERRFLAG = 1
     go to 999
  end if

  read(VOLEQ(3:3),'(I1)')EQN
  if (EQN.ne.0.and. EQN .ne.4.and. EQN .ne.7.and. EQN .ne.9&
       &.and. EQN .ne.8) then
     ERRFLAG = 1
     go to 999
  end if

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

  if (SPEC.ne.SPECPR .or. EQN.ne.EQNPR .or. GEOA.ne.GEOAPR) then
     SPECPR = SPEC
     EQNPR = EQN
     GEOAPR = GEOA
     GSPEC = GEOA*1000 + SPEC

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
           SPECPR = 0
           EQNPR = 0
           GEOAPR = 0
           ERRFLAG = 6
           go to 999
           !SET GEOAPR TO 9 AND RETRY THE SEARCH
        else
           GEOA = 9
           GSPEC = GEOA*1000 + SPEC
           FIRST = 1
           LAST = 182
           LASTFLAG = 0
        end if
     end if
     5 continue

     !     END BINARY SEARCH

     SPGRP = R8CF(PTR,3) + .5
     if (SPGRP.ne.100.and. SPGRP.ne.300.and. SPGRP.ne.500) then
        ERRFLAG = 6
        go to 999
     end if

     COEFFS%A4 = R8CF(PTR,4)
     COEFFS%B4 = R8CF(PTR,5)

     DONEFLAG = 0
     LASTFLAG = 0
     FIRST = 1
     LAST = 49
     do 10, while (DONEFLAG.eq.0)
     if (FIRST.eq.LAST) LASTFLAG = 1
     !DETERMINE WHERE TO CHECK
     HALF = ((LAST-FIRST+1)/2) + FIRST

     !FOUND THE COEFFECIENTS
     if ((INT(DIBMEN(HALF,1)+.5)).eq.SPEC) then
        DIBCNT = HALF
        DONEFLAG = 1
        !MOVE DOWN THE LIST
     else if ((INT(DIBMEN(HALF,1)+.5)).gt.SPEC) then
        LAST = HALF -1
        !MOVE UP THE LIST
     else if ((INT(DIBMEN(HALF,1)+.5)).lt.SPEC) then
        FIRST = HALF
     end if
     !DID NOT FIND A MATCH
     if (LASTFLAG.eq.1.and. DONEFLAG.eq.0) then
        ERRFLAG = 6
        go to 999
     end if
     10 continue
     !     END BINARY SEARCH

     if (EQN.eq.0.or.EQN.eq.4.or. EQN.eq.8) then
        COEFFS%FIXDI = DIBMEN(DIBCNT,2)
        !         FIXDI = DIBMEN(DIBCNT,2)
     else
        COEFFS%FIXDI = DIBMEN(DIBCNT,3)
        !         FIXDI = DIBMEN(DIBCNT,3)
     end if

     if (EQN.eq.0.or. EQN.eq.8) then
        DONEFLAG = 0
        LASTFLAG = 0
        FIRST = 1
        LAST = 49
        do 20, while (DONEFLAG.eq.0)
        if (FIRST.eq.LAST) LASTFLAG = 1
        !DETERMINE WHERE TO CHECK
        HALF = ((LAST-FIRST+1)/2) + FIRST

        !FOUND THE COEFFECIENTS
        if ((INT(TOTAL(HALF,1)+.5)) .eq. SPEC) then
           TOTCNT = HALF
           DONEFLAG = 1
           !MOVE DOWN THE LIST
        else if ((INT(TOTAL(HALF,1)+.5)).gt.SPEC) then
           LAST = HALF -1
           !MOVE UP THE LIST
        else if ((INT(TOTAL(HALF,1)+.5)).lt.SPEC) then
           FIRST = HALF
        end if
        !DID NOT FIND A MATCH
        if (LASTFLAG.eq.1.and. DONEFLAG.eq.0) then
           ERRFLAG = 6
           go to 999
        end if
        20 continue

        COEFFS%R = TOTAL (TOTCNT,2)
        COEFFS%C = TOTAL (TOTCNT,3)
        COEFFS%E = TOTAL (TOTCNT,4)
        COEFFS%P = TOTAL (TOTCNT,5)
        COEFFS%B = TOTAL (TOTCNT,6)
        COEFFS%A = TOTAL (TOTCNT,7)
        COEFFS%A17 = R8CF(PTR,14)
        COEFFS%B17 = R8CF(PTR,15)

     else if (EQN.eq.4) then
        DONEFLAG = 0
        LASTFLAG = 0
        FIRST = 1
        LAST = 49
        do 40, while (DONEFLAG.eq.0)
        if (FIRST.eq.LAST) LASTFLAG = 1
        !DETERMINE WHERE TO CHECK
        HALF = ((LAST-FIRST+1)/2) + FIRST

        !FOUND THE COEFFECIENTS
        if ((INT(FOUR(HALF,1)+.5)) .eq. SPEC) then
           FOURCNT = HALF
           DONEFLAG = 1
           !MOVE DOWN THE LIST
        else if ((INT(FOUR(HALF,1)+.5)).gt.SPEC) then
           LAST = HALF -1
           !MOVE UP THE LIST
        else if ((INT(FOUR(HALF,1)+.5)).lt.SPEC) then
           FIRST = HALF
        end if
        !DID NOT FIND A MATCH
        if (LASTFLAG.eq.1.and. DONEFLAG.eq.0) then
           ERRFLAG = 6
           go to 999
        end if
        40 continue

        COEFFS%R = FOUR (FOURCNT,2)
        COEFFS%C = FOUR (FOURCNT,3)
        COEFFS%E = FOUR (FOURCNT,4)
        COEFFS%P = FOUR (FOURCNT,5)
        COEFFS%Q = FOUR (FOURCNT,6)
        COEFFS%A17 = R8CF(PTR,8)
        COEFFS%B17 = R8CF(PTR,9)

     else if (EQN.eq.7) then

        I = 1
        60 if (INT(SEVEN(I,1)+.5).lt.SPEC .and. I.le.15) then
           I = I + 1
           go to 60
        else if (INT(SEVEN(I,1)+.5).gt.SPEC .or. I.gt.15) then
           ERRFLAG = 6
           go to 999
        end if

        COEFFS%R = SEVEN (I,2)
        COEFFS%C = SEVEN (I,3)
        COEFFS%E = SEVEN (I,4)
        COEFFS%P = SEVEN (I,5)
        COEFFS%Q = SEVEN (I,6)
        COEFFS%A17 = R8CF(PTR,10)
        COEFFS%B17 = R8CF(PTR,11)

     else if (EQN.eq.9) then

        DONEFLAG = 0
        LASTFLAG = 0
        FIRST = 1
        LAST = 34
        do 80, while (DONEFLAG.eq.0)
        if (FIRST.eq.LAST) LASTFLAG = 1
        !DETERMINE WHERE TO CHECK
        HALF = ((LAST-FIRST+1)/2) + FIRST

        !FOUND THE COEFFECIENTS
        if ((INT(NINE(HALF,1)+.5)) .eq. SPEC) then
           NINECNT = HALF
           DONEFLAG = 1
           !MOVE DOWN THE LIST
        else if ((INT(NINE(HALF,1)+.5)).gt.SPEC) then
           LAST = HALF -1
           !MOVE UP THE LIST
        else if ((INT(NINE(HALF,1)+.5)).lt.SPEC) then
           FIRST = HALF
        end if
        !DID NOT FIND A MATCH
        if (LASTFLAG.eq.1.and. DONEFLAG.eq.0) then
           ERRFLAG = 6
           go to 999
        end if
        80 continue
        COEFFS%R = NINE(NINECNT,2)
        COEFFS%C = NINE(NINECNT,3)
        COEFFS%E = NINE(NINECNT,4)
        COEFFS%P = NINE(NINECNT,5)
        COEFFS%Q = NINE(NINECNT,6)
        COEFFS%A17 = R8CF(PTR,12)
        COEFFS%B17 = R8CF(PTR,13)

     end if

  end if
  999 continue

  return
end

subroutine R8CLKDIB(VOLEQ, FORST, DBHOB, HTTOT, UPSHT1,HTUP,DIB,&
     &ERRFLAG)
  use CLKCOEF_MOD

  implicit none
  TYPE(CLKCOEF):: COEFFS
  ! Variables
  character(len=10) :: VOLEQ
  character(len=10) :: VOLEQ0
  character(len=2) :: FORST
  character(len=1) :: CTYPE
  integer :: ERRFLAG,EQN,IS,IB,IT,IM
  real    :: DBHOB,HTTOT,HTUP,DIB,DIB17,DBHIB,TOPHT,HIGHHT,UPSHT1
  real    :: R,C,E,P,B,A,Q,A17,B17,DX,AD,BD,VOLTMP(15)
  integer :: SI,BA
  real    :: HTONE,HTTWO,MTOPP
  real    :: DXcalc,FAC,topDib,totHt

  !      INCLUDE 'R8CLKCOEF.INC'
  !      INCLUDE 'R8DIB.INC'

  !  DW 08/22 Add initialization of COEFFS variables to prevent underflow is left otherwise uninitialized
  COEFFS%DBHIB = 0.0
  COEFFS%DIB17 = 0.0
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
  COEFFS%TOTHT = 0.0
  COEFFS%Q = 0.0
  COEFFS%FIXDI = 0.0

  read(VOLEQ(3:3),'(I1)')EQN
  ! GET COEF FOR THE EQUATION
  call R8PREPCOEF(VOLEQ, COEFFS, ERRFLAG)

  R = COEFFS%R
  C = COEFFS%C
  E = COEFFS%E
  P = COEFFS%P
  B = COEFFS%B
  A = COEFFS%A
  Q = COEFFS%Q
  AD = COEFFS%A4
  BD = COEFFS%B4
  B17 = COEFFS%B17
  A17 = COEFFS%A17
  DX = COEFFS%FIXDI

  DBHIB = AD+BD*DBHOB

  !--   Set height indicator variables
  IS = 0
  IB = 0
  IT = 0
  IM = 0

  if (HTUP.lt.4.5) IS = 1
  if (HTUP.ge.4.5.and.HTUP.le.17.3) IB = 1
  if (HTUP.gt.17.3) IT = 1
  if (HTUP.lt.(17.3+A*(HTTOT-17.3))) IM = 1

  HIGHHT = HTUP
  !-----Get DIB at specified height---------------------------------------
  !--     Total height provided (eqn 1)

  if (EQN.eq.0.or. EQN.eq.8) then
     TOPHT = HTTOT
     DIB17 = DBHOB*(A17+B17*(17.3/TOPHT)**2)

     dib = (is*(dbhib**2*(1+(c+e/dbhib**3)*((1-highht/topht)**r&
          &-(1-4.5/topht)**r)/(1-(1-4.5/topht)**r)))&
          &+ib*(dbhib**2-(dbhib**2-dib17**2)*((1-4.5/topht)**p&
          &-(1-highht/topht)**p)/((1-4.5/topht)**p-(1-17.3/topht)**p))&
          &+it*(dib17**2*(b*(((highht-17.3)/(topht-17.3))-1)**2&
          &+im*((1-b)/a**2)*(a-(highht-17.3)/(topht-17.3))**2)))**0.5

  else
     !--    HEIGHT TO 4, 7 OR 9 TOP DOB
     if (UPSHT1.lt.0.1) then
        !           SI = 0
        !           BA = 0
        !           CALL R8_MHTS(FORST,VOLEQ,DBHOB,HTTOT,SI,BA,UPSHT1,ERRFLAG)
        ! CALL R8VOL2 TO GET THE UPSHT1. THIS WILL BE SAME AS FVS
        HTONE = 0
        HTTWO = 0
        CTYPE = 'F'
        call R8VOL2(VOLEQ,VOLTMP,DBHOB,HTONE,HTTWO,MTOPP,HTTOT,CTYPE,&
             &ERRFLAG)
        UPSHT1 = HTTWO
     end if

     TOPHT = UPSHT1
     DIB17 = DBHOB*(A17+B17*(17.3/TOPHT)**2)
     !         dib=(is*(dbhib**2*(1+(c+e/dbhib**3)*((1-highht/topht)**r
     !     &       -(1-4.5/topht)**r)/(1-(1-4.5/topht)**r)))
     !     &     +ib*(dbhib**2-(dbhib**2-dib17**2)*((1-4.5/topht)**p
     !     &       -(1-highht/topht)**p)/((1-4.5/topht)**p-(1-17.3/topht)**p))
     !     &     +IT*(DIB17**2-(DIB17**2-DX**2)*(1-((TOPHT-HTUP)/
     !     &      (TOPHT-17.3))**Q)))**0.5
     ! The codes below replaces the above comment out codes for NaN problem
     ! when TOPHT < 17.3 (YW 2016/03/08)
     if (IS.gt.0) then
        dib = (is*(dbhib**2*(1+(c+e/dbhib**3)*((1-highht/topht)**r&
             &-(1-4.5/topht)**r)/(1-(1-4.5/topht)**r))))**0.5
     else if (IB.gt.0) then
        if (TOPHT.gt.17.3) then
           dib = (ib*(dbhib**2-(dbhib**2-dib17**2)*((1-4.5/topht)**p&
                &-(1-highht/topht)**p)/((1-4.5/topht)**p&
                &-(1-17.3/topht)**p)))**0.5
        else
           if (highht.le.topht) then
              dib = (dbhib**2-(highht-4.5)/(topht-4.5)&
                   &*(dbhib**2-DX**2))**0.5
           else if (HTTOT.gt.0) then
              dib = (DX**2-(highht-topht)/(httot-topht)*DX**2)**0.5
           end if
        end if
     else if (IT.gt.0) then
        if (TOPHT.gt.17.3) then
           if (HTUP.le.TOPHT) then
              dib = (IT*(DIB17**2-(DIB17**2-DX**2)*(1-((TOPHT-HTUP)/&
                   &(TOPHT-17.3))**Q)))**0.5
           else
              !   For the part above UPSHT1, need to use total height equation
              !   If HTTOT is not provided, using R9 logic to calculate the total height.
              !   The dib is calculated with total equation
              VOLEQ0 = VOLEQ
              VOLEQ0(3:3) = '0'
              call R8PREPCOEF(VOLEQ0, COEFFS, ERRFLAG)
              COEFFS%DBHIB = DBHIB
              COEFFS%DIB17 = DIB17
              if (HTTOT.le.0.0) then
                 topDib = DX
                 a = COEFFS%A
                 b = COEFFS%B
                 call r9totHt(totHt,htTot,dbhIb,dib17,topHt,topDib,a,b,&
                      &errFlag)
                 HTTOT = totHt
              end if
              COEFFS%TOTHT = HTTOT
              call R9DIB(DXcalc, UPSHT1, COEFFS)
              FAC = DX/DXcalc
              if (HTUP.lt.HTTOT) then
                 call R9DIB(dib, HTUP, COEFFS)
              else
                 dib = 0.0
              end if
              dib = dib*FAC
           end if
        else
           dib = (DX**2-(highht-topht)/(httot-topht)*DX**2)**0.5
        end if
     end if
  end if
  return
end
