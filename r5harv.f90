!============================!
!   VolumeLibrary : r5harv   !
!============================!
  !== last modified  10-30-2018
  ! 01/18/2013 Added calculation for stump VOL(14) and tip VOL(15)
  ! 04/13/2017 moved stump and tip vol calc to voinit
  ! 08/07/2018 Added species code 64 to the Juniper species for use by FIA equation
  ! 10/30/2018 YW CHANGED THE COFB FOR ENGELMAN OAK TO BE SAME AS CA WHITE OAK TO MATCH FIA EQUQTION
subroutine  R5HARV (VOLEQ,DBHOB,HTTOT,MTOPP,VOL,BFPFLG,CUPFLG,&
     &ERRFLAG)
  ! FROM THE PNW RESEARCH NOTE PNW-414
  ! EQUATIONS FOR TOTAL, WOOD, AND SAW-LOG VOLUME FOR FIFTEEN
  ! CALIFORNIA HARDWOODS - Norman H. Pillsbury and Michael L. Kirkley

  !    INPUT - INTEGER  EQUNUM   THREE DIGIT SPECIES CODE
  !            REAL     DBHOB      DIAMETER BREAST HEIGHT
  !            REAL     HTTOT       HEIGHT
  !            INTEGER  TOP      IF 4 USES WVOL EQUATIONS
  !                              IF 8 USES SVOL EQUATIONS
  !                              IF 0 USE TO TIP
  !   OUTPUT   REAL  VOL(15)
  !            INTEGER ERROR     IF ERROR OCCURS VOLUMES SET TO 0.0
  !                              -1 SPEC  ERROR
  ! Variables
  character(len=10) :: VOLEQ
  real*8  :: COFA(15,4),COFB (15,4),COFC(15,4)
  real*8  :: IV,COEFSEQB(3),COEFSEQC(3)
  real    :: DBHOB,VOL(15),TARIF,CV4,CV6,CV8,CVT
  real    :: CUFTGROS,BDFTGROS,INTLGROS,BALOG,DLOG,TLOG
  real    :: DE,HE,BA,HTTOT,HLOG,TERM1
  real    :: B4,RS616,RS616L,RI6,XINT6,RI8
  real    :: XINT8,CVTS,TOPTW,TOPC,TOPB
  real    :: MTOPP
  real    :: F,V,D,RS816,SV616,SV816,R16,R18
  integer :: SPEC,BFPFLG,CUPFLG,ERRFLAG,I

  data IV/10.0/

  ! REDALDER              71   DONE IN REDALDER.F77 ROUTINE
  data (COFA(1,I),I = 1,4)/0.0,0.0,0.0,0.0/
  data (COFB(1,I),I = 1,4)/0.0,0.0,0.0,0.0/
  data (COFC(1,I),I = 1,4)/0.0,0.0,0.0,0.0/
  ! BIGLEAF MAPLE         76, 95
  data (COFA(2,I),I = 1,4) /0.0034214162,2.35347,0.69586,0.0/
  data (COFB(2,I),I = 1,4) /0.0004236332,2.10316,1.08584,0.40017/
  data (COFC(2,I),I = 1,4) /0.0101786350,2.22462,0.57561,0.0/
  ! CALIFORNIA BLACK OAK  81
  data (COFA(3,I),I = 1,4) /0.0036795695,2.12635,0.83339,0.0/
  data (COFB(3,I),I = 1,4) /0.0012478663,2.68099,0.42441,0.28385/
  data (COFC(3,I),I = 1,4) /0.0070538108,1.97437,0.85034,0.0/
  ! BLUE OAK              88
  data (COFA(4,I),I = 1,4) /0.0042324071,2.53987,0.50591,0.0/
  data (COFB(4,I),I = 1,4) /0.0036912408,1.79732,0.838884,0.15958/
  data (COFC(4,I),I = 1,4) /0.0125103008,2.33089,0.46100,0.0/
  ! CANYON LIVE OAK       84
  data (COFA(5,I),I = 1,4) /0.0031670596,2.32519,0.74348,0.0/
  data (COFB(5,I),I = 1,4) /0.0006540144,2.24437,0.81358,0.43381/
  data (COFC(5,I),I = 1,4) /0.0097438611,2.20527,0.61190,0.0/
  ! GIANT CHINKAPIN       93
  data (COFA(6,I),I = 1,4) /0.0055212937,2.07202,0.77467,0.0/
  data (COFB(6,I),I = 1,4) /0.0018985111,2.38285,0.77105,0.0/
  data (COFC(6,I),I = 1,4) /0.0120372263,2.02232,0.68638,0.0/
  ! COAST LIVE OAK        82   / also 96 & 98
  data (COFA(7,I),I = 1,4) /0.0024574847,2.53284,0.60764,0.0/
  data (COFB(7,I),I = 1,4) /0.0006540144,2.24437,0.81358,0.43381/
  data (COFC(7,I),I = 1,4) /0.0065261029,2.31958,0.62528,0.0/
  ! INTERIOR LIVE OAK     85
  data (COFA(8,I),I = 1,4) /0.0041192264,2.14915,0.77843,0.0/
  data (COFB(8,I),I = 1,4) /0.0006540144,2.24437,0.81358,0.43381/
  data (COFC(8,I),I = 1,4) /0.0136818837,2.02989,0.63257,0.0/
  ! CALIFORNIA LAUREL     91
  data (COFA(9,I),I = 1,4) /0.0016380753,2.05910,1.05293,0.0/
  data (COFB(9,I),I = 1,4) /0.0007741517,2.23009,1.037,  0.0/
  data (COFC(9,I),I = 1,4) /0.0057821322,1.94553,0.88389,0.0/
  ! PACIFIC MADRONE       94
  data (COFA(10,I),I = 1,4) /0.0025616425,1.99295,1.01532,0.0/
  data (COFB(10,I),I = 1,4) /0.000618153, 1.72635,1.26462,0.37867/
  data (COFC(10,I),I = 1,4) /0.0067322665,1.96628,0.83458,0.0/
  ! OREGON WHITEOAK       86
  data (COFA(11,I),I = 1,4) /0.0024277027,2.25575,0.87108,0.0/
  data (COFB(11,I),I = 1,4) /0.0008281647,2.10651,0.91215,0.32652/
  data (COFC(11,I),I = 1,4) /0.0072695058,2.14321,0.74220,0.0/
  ! TANOAK                87     / also 72 73 75
  data (COFA(12,I),I = 1,4) /0.000577497, 2.19576,1.14078,0.0/
  data (COFB(12,I),I = 1,4) /0.0002526443,2.30949,1.21069,0.0/
  data (COFC(12,I),I = 1,4) /0.0058870024,1.94165,0.86562,0.0/
  ! CALIFORNIA WHITE OAK  83
  data (COFA(13,I),I = 1,4) /0.0009684363,2.39565,0.98878,0.0/
  data (COFB(13,I),I = 1,4) /0.0001880044,1.87346,1.62443,0.0/
  data (COFC(13,I),I = 1,4) /0.0042870077,2.33631,0.74872,0.0/
  ! NOT TO BE USED
  data (COFA(14,I),I = 1,4)/0.0,0.0,0.0,0.0/
  data (COFB(14,I),I = 1,4)/0.0,0.0,0.0,0.0/
  data (COFC(14,I),I = 1,4)/0.0,0.0,0.0,0.0/
  ! ENGLEMANN OAK         79
  data (COFA(15,I),I = 1,4)/.0053866353,2.61268,.31103,0.0/
  !      DATA (COFB(15,I),I=1,4)/0.0,0.0,0.0,0.0/
  ! CHANGED THE ABOVE 0 FOR COFB TO USE WHITE OAK TO MATCH FIA EQUATION (YW 2018/10/30)
  data (COFB(15,I),I = 1,4) /0.0001880044,1.87346,1.62443,0.0/
  !      DATA (COFB(15,I),I=1,4)/0.0006540144,2.24437,0.81358,0.43381/
  data (COFC(15,I),I = 1,4)/.0191453191,2.40248,.28060,0.0/
  ! GIANT SEQUOIA
  data (COEFSEQB(I),I = 1,3) /0.001682608,1.755956,1.490641/
  data (COEFSEQC(I),I = 1,3) /0.002438339,1.694874,1.098957/

  ! CHECK FOR VALID SPECIES AND IF NOT SET VOLUME TO -1 AND RETURN
  do 10,I = 1,15
  VOL(I) = 0
  10 continue

  ERRFLAG = 0
  if (dbhob .lt.1.0) then
     errflag = 3
     goto 999
  end if

  if (BFPFLG.eq.1.or. CUPFLG.eq.1) then
     if (VOLEQ(8:10).eq.'060'.or.VOLEQ(8:10).eq.'064') then
        SPEC = 0
     else if (VOLEQ(8:10).eq.'351') then
        SPEC = 1
     else if (VOLEQ(8:10).eq.'312') then
        SPEC = 2
     else if (VOLEQ(8:10).eq.'818') then
        SPEC = 3
     else if (VOLEQ(8:10).eq.'807') then
        SPEC = 4
     else if (VOLEQ(8:10).eq.'805') then
        SPEC = 5
     else if (VOLEQ(8:10).eq.'431') then
        SPEC = 6
     else if (VOLEQ(8:10).eq.'801') then
        SPEC = 7
     else if (VOLEQ(8:10).eq.'839') then
        SPEC = 8
     else if (VOLEQ(8:10).eq.'981') then
        SPEC = 9
     else if (VOLEQ(8:10).eq.'361') then
        SPEC = 10
     else if (VOLEQ(8:10).eq.'815') then
        SPEC = 11
     else if (VOLEQ(8:10).eq.'631') then
        SPEC = 12
     else if (VOLEQ(8:10).eq.'821') then
        SPEC = 13
     else if (VOLEQ(8:10).eq.'212') then
        SPEC = 14
     else if (VOLEQ(8:10).eq.'811') then
        SPEC = 15
     else
        SPEC = -1
        errflag = 1
        return
     end if
  end if

  ! END CHECK

  ! JUNIPER SPECIES

  if (SPEC .eq.0) then
     D = DBHOB
     TOPC = MTOPP
     if (D.lt.5.or. HTTOT.lt.10) then
        CVTS = 0.00272708 * D * D * HTTOT
        V = 0
     else
        F = 0.307 + 0.00086*HTTOT - 0.0037*D*HTTOT / (HTTOT - 4.5)
        BA = 0.005454154 * D * D
        CVTS = BA * F * HTTOT * (HTTOT / (HTTOT - 4.5))**2
        !  IF TOP NOT EQUAL TO 0 THEN DEFAULT TO 4 INCH TOP
        if (TOPC.gt.0) then
           V = (CVTS + 3.48)/(1.18052+0.32736 * EXP(-0.1 * D))-2.948
        else
           V = CVTS
        end if
     end if
     VOL(4) = ANINT(V*10 + 0.5)/10.0
     vol(1) = cvts
     ! REDALDER SPECIES

  else if (SPEC .eq.1) then
     DE = DBHOB
     HE = HTTOT
     BA = DE * DE * 0.005454154

     TERM1 = (1.033 * (1.0 + 1.382937 * EXP(-4.015292 *&
          &(DE / 10.0)))) * (BA + 0.087266) - 0.174533
     DLOG = LOG10(DE)
     HLOG = LOG10(HE)

     CVTS = 10.0**(-2.672775 + 1.920617 * DLOG + 1.074024 * HLOG)

     TARIF = (CVTS * 0.912733) / TERM1

     if (TARIF.le.0.0) then
        TARIF = 0.01
     end if

     !   CUBIC FOOT VOLUMES FOR TIP, 4", 6", 8"  TOPS **

     TOPC = MTOPP
     CV4 = TARIF * (BA - 0.087266) / 0.912733
     CV8 = CV4 * (0.983 - 0.983 * 0.65**(DE - 8.6))

     CVT = TARIF * (0.9679 - 0.1051 * 0.5523**(DE - 1.5)) *&
          &TERM1 / 0.912733
     CV6 = CV4 * (0.993 - 0.993 * 0.62**(DE - 6.0))

     if (TOPC.ge.3.and. topc.lt.5) then
        VOL(4) = CV4
     else if (TOPC.ge.5.and. topc.lt.7) then
        VOL(4) = CV6
     else if (TOPC.ge.7.and. topc.le.9) then
        VOL(4) = CV8
     else if (TOPC.lt.3) then
        VOL(4) = cvt
     end if

     vol(1) = cvt

     if (VOL(4).le.0.0) then
        VOL(4) = 0.0
     end if

     !             SCRIBNER BOARD FOOT VOLUMES       **

     if (DBHOB .ge.7.0) then

        TOPB = MTOPP
        B4 = TARIF / 0.912733
        BALOG = LOG10(B4)
        RS616L = 0.174439 + 0.117594 * DLOG * BALOG - 8.210585 /&
             &DE**2 + 0.236693 * BALOG - 0.00001345 * B4**2 -&
             &0.00001937 * DE**2

        RS616 = 10.0**RS616L
        SV616 = RS616 * CV6
        RS816 = 0.99 - 0.58 * (0.484**(DE - 9.5))
        SV816 = RS816 * SV616

        if (TOPB.ge.5.and. topb.lt.7) then
           VOL(2) = SV616
        else if (TOPB.ge.7.and. topb.le.9) then
           VOL(2) = SV816
        else
           VOL(2) = 0.0
        end if

        !                 INTERNATIONAL 1/4"            **

        TLOG = LOG10(DE * TARIF)
        R16 = -2.904157 + 3.466328 * TLOG - 0.02765985 * DE -&
             &0.00008025 * TARIF**2 + 11.29598 / DE**2
        XINT6 = R16 * CV6
        R18 = 0.99 - 0.55 * (0.485**(DE - 9.5))
        XINT8 = R18 * XINT6

        if (TOPB.ge.5.and. topb.lt.7) then
           VOL(10) = XINT6
        else if (TOPB.ge.7.and. topb.le.9) then
           VOL(10) = XINT8
        else
           VOL(10) = 0.0
        end if

        !                  TOPWOOD LOGIC                **

        TOPTW = MTOPP
        if (TOPTW.le.0.0) then
           VOL(7) = 0.0
        else
           if (TOPTW.eq.6) then
              VOL(7) = CV4 - CV6
           else if (TOPTW.eq.8) then
              VOL(7) = CV4 - CV8
           else if (TOPTW.eq.4) then
              VOL(7) = 0.0
           else
              VOL(7) = 0.0
           end if
        end if
     end if ! DBHOB >= 7.0

     !    END RED ALDER ROUTINE         ***

     !   GIANT SEQUOIA (pillsbury et al (1991) **

  else if (SPEC.eq.14) then

     VOL(2) = COEFSEQB(1)*(DBHOB**COEFSEQB(2))*HTTOT ** COEFSEQB(3)
     VOL(4) = COEFSEQC(1)*(DBHOB**COEFSEQC(2))*HTTOT ** COEFSEQC(3)
     vol(1) = vol(4)

     !--  MISC HARDWOOD SPECIES                   ****

  else
     TOPC = MTOPP
     D = DBHOB
     BA = D**2 * 0.005454154
     CV4 = COFA(SPEC,1) * DBHOB**COFA(SPEC,2)*HTTOT**COFA(SPEC,3) *&
          &IV**COFA(SPEC,4)
     CV8 = COFB(SPEC,1) * DBHOB**COFB(SPEC,2)*HTTOT**COFB(SPEC,3) *&
          &IV**COFB(SPEC,4)
     if (CV4.gt.0.and.CV8.gt.0) then
        CV6 = CV4-((CV4-CV8)*.4)
     else
        CV6 = 0
     end if
     CVT = COFC(SPEC,1) * DBHOB**COFC(SPEC,2)*HTTOT**COFC(SPEC,3) *&
          &IV**COFC(SPEC,4)
     if (TOPC.ge.3.and. topc.lt.5) then
        CUFTGROS = CV4
     else if (TOPC.ge.5.and. topc.lt.7) then
        CUFTGROS = CV6
     else if (TOPC.ge.7.and. topc.le.9) then
        CUFTGROS = CV8
     else if (TOPC.lt.3) then
        CUFTGROS = CVT
     else
        CUFTGROS = 0.0
     end if
     VOL(4) = CUFTGROS
     VOL(7) = CV4-CUFTGROS
     vol(1) = cvt
     ! END CUFT CALCULATIONS

     ! DOCUMENTATION FOR CONVERTING CUBIC TO SCRIBNER AND INTL 1/4
     !   IS CONTAINED IN THE SUBROUTINE REDALDER.F77

     TOPB = MTOPP

     if (DBHOB.ge.5.0) then

        ! FOR 5" <= DBHOB < 11", FIND SCRIBNER AND INTERNATIONAL VOLUME BASED
        ! ON BOARD FOOT/CUBIC FOOT RATIO (CUBIC * 4 FOR SCRIBNER,
        ! CUBIC * 5 FOR I-1/4" RULE)
        if (DBHOB.lt.11.0) then
           if (TOPB.ge.5.and. topb.lt.7) then
              BDFTGROS = CV6 * 4
              INTLGROS = CV6 * 5
           else if (TOPB.ge.7.and. topb.le.9) then
              BDFTGROS = CV8 * 4
              INTLGROS = CV8 * 5
           else
              BDFTGROS = 0.0
              INTLGROS = 0.0
           end if
           VOL(2) = BDFTGROS

        else
           ! START SCRIBNER CALCULATIONS
           TARIF = (CV8*.912733)/((.983-.983*.65**(D-8.6))*(BA-.087266))

           if (TARIF.le.0.0) TARIF = 0.01

           B4 = TARIF/.912733
           DLOG = LOG10(D)
           BALOG = LOG10(B4)
           RS616L = 0.174439 + 0.117594 * DLOG * BALOG - 8.210585 /&
                &D**2 + 0.236693 * BALOG - 0.00001345 * B4**2 -&
                &0.00001937 * D**2
           RS616 = 10.0**RS616L
           SV616 = RS616 * CV6
           RS816 = 0.99 - 0.58 * (.484**(D - 9.5))
           SV816 = RS816 * SV616
           if (TOPB.ge.5.and. topb.lt.7) then
              BDFTGROS = SV616
           else if (TOPB.ge.7.and. topb.le.9) then
              BDFTGROS = SV816
           else
              BDFTGROS = 0.0
           end if
           VOL(2) = BDFTGROS
           ! END SCRIBNER

           ! NOW GET INTERNATIONAL
           TLOG = LOG10(D*TARIF)
           RI6 = -2.904154 + 3.466328 * TLOG - 0.02765985 * D -&
                &0.00008025 * TARIF**2 + 11.29598 / D**2
           XINT6 = RI6 * CV6
           RI8 = 0.99 - 0.55 * (0.485**(D - 9.5))
           XINT8 = RI8 * XINT6

           if (TOPB.ge.5.and. topb.lt.7) then
              INTLGROS = XINT6
           else if (TOPB.ge.7.and. topb.le.9) then
              INTLGROS = XINT8
           else
              INTLGROS = 0.0
           end if

           VOL(10) = INTLGROS
        end if ! END INTL CALC
     end if ! END DBH CHECK
  end if ! END MISC HARDWOODS
  ! CHECK MERCHANTABLITY OPTIONS
  !        check for top diameter greater then dbh; no merch volume
  if (MTOPP .gt. DBHOB) VOL(2) = 0.0
  if (MTOPP .gt. DBHOB) VOL(10) = 0.0
  if (MTOPP .gt. DBHOB) VOL(4) = 0.0

  !       calculate stump and tip volume
  !        VOL(14)=CVTS-CVT
  !        IF(VOL(14).LT.0.01) VOL(14)=0.0
  !        VOL(15)=CVT-CUFTGROS
  !        IF(VOL(15).LT.0.01) VOL(15)=0.0

  999 return
end
