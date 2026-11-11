!==============================!
!   VolumeLibrary : pnwtarif   !
!==============================!
  !  12/17/2012
  !  The tarif volume equation used by FIA PNW
  !  Created by YW 12/17/2012
  !  2018/08/08 YW Added species code 116, 119 and 020 for FIA equation
subroutine PNWTARIF(VOLEQ,DBHOB,HTTOT,MTOPP,VOL,BFPFLG,CUPFLG,&
     &ERRFLAG)

  ! Variables
  character(len=10) :: VOLEQ
  real    :: DBHOB,HTTOT,MTOPP,VOL(15)
  integer :: BFPFLG,CUPFLG,ERRFLAG,SPN,SCRBLOGL,FCLASS
  character(len=2) :: REGION
  character(len=3) :: SPEC
  real    :: DBH,HT,CVTS,CV4,CVT,COFA,COFB,COFC
  real    :: BA,BA_TMP,TMP_DBH,CF4,CF4_TMP,TERM,TARIF,CV4_TMP,TARIF_TMP
  real    :: Z,F,Z25,THT,DRC
  real    :: RC6,CV6,CUBUS,B4,RS616L,RS616,RS632,SV616,SV632,R16,XINT6

  do 10,I = 1,15
  VOL(I) = 0
  10 continue

  ERRFLAG = 0
  if (dbhob .lt.1.0) then
     errflag = 3
     goto 999
  end if

  DBH = DBHOB
  HT = HTTOT
  BA = DBH**2*0.005454154
  SPEC = VOLEQ(8:10)
  read(SPEC,'(i3)') SPN
  read(VOLEQ(2:3), '(I2)') SCRBLOGL
  !     Move initialization of variables prior to any regional filtering
  CV4 = 0.0
  CVT = 0.0
  CVTS = 0.0
  TARIF = 0.0

  if (VOLEQ(1:1).eq.'6') then
     !       REGION 6 EQUATION
     if (SPEC.eq.'202') then
        if (SCRBLOGL.eq.32) then
           !         COASTAL DOUGLAS-FIR  (FIA EQ#1)
           CVTS = 10**(- 3.21809+ 0.04948*LOG10(DBH)*LOG10(HT)&
                &-0.15664*LOG10(DBH)**2+ 2.02132*LOG10(DBH)&
                &+ 1.63408*LOG10(HT)- 0.16185*LOG10(HT)**2)
        else if (SCRBLOGL.eq.16) then
           !         INTERIOR DOUGLAS-FIR (FIA EQ#2)
           CVTS = EXP(-6.110493+1.81306*LOG(DBH)+ 1.083884*LOG(HT))
        end if
     else if (SPEC.eq.'122') then
        !         PONDEROSA PINE (FIA EQ#4)
        CVTS = EXP(-8.521558+1.977243*LOG(DBH)-0.105288*LOG(HT)**2&
             &+ 136.0489/HT**2+1.99546 *LOG(HT))
     else if (SPEC.eq.'263') then
        !         WESTERN HELMLOCK (FIA EQ#6)
        CVTS = 10**(-2.72170+ 2.00857*log10(DBH)+1.08620*log10(HT)&
             &-.00568*DBH)
     else if (SPEC.eq.'242') then
        if (SCRBLOGL.eq.16) then
           !         INTERIOR REDCEDAR  (FIA EQ#8)
           COFA = -2.464614
           COFB = 1.701993
           COFC = 1.067038
           call BROWNE_CVTS(DBH,HT,COFA,COFB,COFC,CVTS)
        else if (SCRBLOGL.eq.32) then
           !         COSTAL MATURE REDCEDAR  (FIA EQ#9)
           COFA = -2.379642
           COFB = 1.682300
           COFC = 1.039712
           call BROWNE_CVTS(DBH,HT,COFA,COFB,COFC,CVTS)
        end if
     else if (SPEC.eq.'019') then
        !         INTERIOR TRUE FIR  (FIA EQ#10)
        COFA = -2.502332
        COFB = 1.864963
        COFC = 1.004903
        call BROWNE_CVTS(DBH,HT,COFA,COFB,COFC,CVTS)
     else if (SPEC.eq.'011') then
        !         COSTAL MATURE TRUE FIR  (FIA EQ#11)
        COFA = -2.575642
        COFB = 1.806775
        COFC = 1.094665
        call BROWNE_CVTS(DBH,HT,COFA,COFB,COFC,CVTS)
     else if (SPEC.eq.'094') then
        !         INTERIOR SPRUCE (FIA EQ#12)
        COFA = -2.539944
        COFB = 1.841226
        COFC = 1.034051
        call BROWNE_CVTS(DBH,HT,COFA,COFB,COFC,CVTS)
     else if (SPEC.eq.'098') then
        !         COSTAL MATURE SITKA SPRUCE
        COFA = -2.700574
        COFB = 1.754171
        COFC = 1.164531
        call BROWNE_CVTS(DBH,HT,COFA,COFB,COFC,CVTS)
     else if (SPEC.eq.'108') then
        !         LODGEPOLE PINE (FIA EQ#15)
        COFA = -2.615591
        COFB = 1.847504
        COFC = 1.085772
        call BROWNE_CVTS(DBH,HT,COFA,COFB,COFC,CVTS)
     else if (SPEC.eq.'264') then
        !         MOUNTAIN HEMLOCK  (FIA EQ#17)
        CVTS = 0.001106485*DBH**1.8140497*HT**1.2744923
     else if (SPEC.eq.'073') then
        !         WESTERN LARCH  (FIA EQ#22)
        COFA = -2.624325
        COFB = 1.847123
        COFC = 1.044007
        call BROWNE_CVTS(DBH,HT,COFA,COFB,COFC,CVTS)
     else if (SPEC.eq.'211') then
        !         REDWOOD (FIA EQ# 24)
        CVTS = EXP(-6.2597+1.9967*LOG(DBH)+0.9642*LOG(HT))
     else if (SPEC.eq.'351') then
        !         REDALDER FIA EQ#25
        if (HT.lt.18.0) then
           THT = 18.0
        else
           THT = HT
        end if
        Z = (THT-0.5-DBH/24)/(THT-4.5)
        Z25 = Z**2.5
        F = 0.3651*Z25-7.9032*Z25*DBH/1000.0+3.295*Z25 * THT/1000.0&
             &- 1.9856*Z25*THT*DBH/100000.0-2.9668*Z25*THT**2/1000000.0&
             &+ 1.5092*Z25*THT**.5/1000.0+4.9395*Z**4*DBH/1000.0&
             &- 2.05937*Z**4*THT/1000.0+1.5042*Z**33*THT*DBH/1000000.0&
             &- 1.1433*Z**33*THT**0.5/10000.0+1.809*Z**41*THT**2/10000000.0
        CVT = 0.00545415*DBH**2*(THT-4.5)*F
        CVTS = CVT/(0.9679-0.105*0.5525**(DBH-1.5))
        call GET_TARIF_FROM_CVT(DBH,CVT,TARIF)
        BA = 0.005454154*DBH*DBH
        CV4 = TARIF*(BA-0.087266)/0.912733
     else if (SPEC.eq.'746') then
        !        ASPEN FIA EQ#28
        COFA = -2.635360
        COFB = 1.946034
        COFC = 1.024793
        call BROWNE_CVTS(DBH,HT,COFA,COFB,COFC,CVTS)
     else if (SPEC.eq.'747') then
        !        COTTONWOOD FIA EQ#27
        COFA = -2.945047
        COFB = 1.803973
        COFC = 1.238853
        call BROWNE_CVTS(DBH,HT,COFA,COFB,COFC,CVTS)
     else if (SPEC.eq.'998') then
        !        OTHER HARDWOOD FIA EQ#31 USED FOR EUCALYPTUS
        CVTS = .0016144*DBH**2*HT
     else
        ERRFLAG = 1
        do I = 1,15
           VOL(I) = 0.0
           5 continue
        end do
        return
     end if
     if (TARIF.eq.0.0) call GET_TARIF_FROM_CVTS(DBH,CVTS,TARIF)
     if (CV4.eq.0.0) CV4 = TARIF*(BA-0.087266)/0.912733
     if (CVT.eq.0.0) CVT = CVTS*(0.9679-0.105*0.5525**(DBH-1.5))
     VOL(1) = CVT
     VOL(4) = CV4
     VOL(14) = CVTS - CVT
     call GET_MERCH_VOL(SPN,DBH,MTOPP,SCRBLOGL,&
          &CV4,CVT,TARIF,VOL,ERRFLAG)
  else if (VOLEQ(1:1).eq.'5') then
     !     REGION 5 (CA) SPECIES
     if (SPN.lt.300.and. SPN.ne.60) then
        !       REGION 5 SOFTWOOD SPECIES
        if (DBH.lt.6.0) then
           TMP_DBH = 6.0
        else
           TMP_DBH = DBH
        end if
        !         CALCULATE BASAL AREA PER TREE USING DBH AND DBH_TEMP
        BA_TMP = TMP_DBH**2*0.005454154
        !         CALCULATE A CUBIC FORM FACTOR (CF4) USING TMP_DBH and DBH
        !         CF4  EQUATIONS VARY BY VOLUME EQUATION
        if (SPEC.eq.'202') then
           !           FIA EQ#3
           CF4 = 0.248569+0.0253524*(HT/DBH)-0.0000560175*(HT**2/DBH)
           call CHECK_CF(CF4)
           CF4_TMP = 0.248569+0.0253524*(HT/TMP_DBH)&
                &-0.0000560175*(HT**2/TMP_DBH)
           call CHECK_CF(CF4_TMP)
        else if (SPEC.eq.'122'.or.SPEC.eq.'116') then
           !           FIA EQ#5
           CF4 = 0.402060 - 0.899914 * (1.0/DBH)
           call CHECK_CF(CF4)
           CF4_TMP = 0.402060 - 0.899914 * (1.0/TMP_DBH)
           call CHECK_CF(CF4_TMP)
        else if (SPEC.eq.'108') then
           !           FIA EQ#16
           CF4 = 0.422709 - 0.0000612236 * (HT**2/DBH)
           call CHECK_CF(CF4)
           CF4_TMP = 0.422709 - 0.0000612236 * (HT**2/TMP_DBH)
           call CHECK_CF(CF4_TMP)
        else if (SPEC.eq.'021'.or.SPEC.eq.'020') then
           !           FIA EQ#18
           CF4 = 0.231237 + 0.028176 * (HT/DBH)
           call CHECK_CF(CF4)
           CF4_TMP = 0.231237 + 0.028176 * (HT/TMP_DBH)
           call CHECK_CF(CF4_TMP)
        else if (SPEC.eq.'081') then
           !           FIA EQ#19
           CF4 = 0.225786 + 4.44236 * (1.0/HT)
           if (CF4.lt.0.27) CF4 = 0.27
           CF4_TMP = 0.225786 + 4.44236 * (1.0/HT)
           if (CF4_TMP.lt.0.27) CF4_TMP = 0.27
        else if (SPEC.eq.'117'.or.SPEC.eq.'119') then
           !           FIA EQ#20
           CF4 = 0.358550 - 0.488134 * (1.0/DBH)
           call CHECK_CF(CF4)
           CF4_TMP = 0.358550 - 0.488134 * (1.0/TMP_DBH)
           call CHECK_CF(CF4_TMP)
        else if (SPEC.eq.'015') then
           !           FIA EQ# 23
           CF4 = 0.299039+1.91272*(1.0/HT)+0.0000367217 * (HT**2/DBH)
           call CHECK_CF(CF4)
           CF4_TMP = 0.299039+1.91272*(1.0/HT)+&
                &0.0000367217*(HT**2/TMP_DBH)
           call CHECK_CF(CF4_TMP)
        else
           ERRFLAG = 1
           do I = 1,15
              VOL(I) = 0.0
              15 continue
           end do
           return
        end if

        !         For ease of use and to improve readability of equations, calculate the following term and use it
        !         in the equations that follow. Note that actual DBH and BA are used for all trees.
        !         Do not use TMP_DBH or BA_TMP here.

        TERM = ((1.033*(1.0+1.382937*EXP(-4.015292*(DBH/10.0))))&
             &* (BA + 0.087266) - 0.174533 )

        if (DBH.ge.6.0) then
           CV4 = CF4 * BA * HT
           TARIF = (CV4 * 0.912733)/(BA - 0.087266)
           if (TARIF.le.0.0) TARIF = 0.01
           CVTS = (CV4*TERM )/(BA - 0.087266)
           CVT = TARIF*(0.9679-0.1051*0.5523**(DBH-1.5))*TERM/0.912733
        else
           CV4_TMP = CF4_TMP*BA_TMP*HT
           TARIF_TMP = (CV4_TMP * 0.912733)/(BA_TMP - 0.087266)
           if (TARIF_TMP.le.0.0) TARIF_TMP = 0.01
           !           CALCULATE An ADJUSTED  TARIF FOR SMALL TREES (Both DBH and TMP_DBH are used)
           TARIF = 0.5*(TMP_DBH-DBH)**2&
                &+(1.0+0.063*(TMP_DBH-DBH)**2)*TARIF_TMP

           if (TARIF.le.0.0) TARIF = 0.01
           CVTS = TARIF * TERM
           CVT = TARIF*(0.9679-0.1051*0.5523**(DBH-1.5))*TERM !/0.912733
           CV4 = CF4 * BA * HT
           !          CV4 calculated with actual DBH and BA
        end if
        VOL(1) = CVT
        VOL(4) = CV4
        if (DBH.lt.5.0) CV4 = 0.0
        call GET_MERCH_VOL(SPN,DBH,MTOPP,SCRBLOGL,&
             &CV4,CVT,TARIF,VOL,ERRFLAG)
     else if (SPEC.eq.'475') then
        DRC = DBHOB
        FCLASS = 1.
        call R4D2H (VOLEQ,HTTOT,DBHOB,DRC,FCLASS,VOL,ERRFLAG)
     else
        !       OTHER HARDWOOD IN REGION 5 CALL R5HARV
        call R5HARV(VOLEQ,DBHOB,HTTOT,MTOPP,VOL,BFPFLG,CUPFLG,ERRFLAG)
     end if
  end if
  !     get Stump volume which is CVTS-CVT
  if (VOL(14).le.0.0) VOL(14) = CVTS-CVT
  if (VOL(14).lt.0.0) VOL(14) = 0.0
  !     get stem tip volume which is cvt-cv4
  if (VOL(15).le.0.0) VOL(15) = CVT-CV4
  if (VOL(15).lt.0.0) VOL(15) = 0.0

  999 return
  end subroutine PNWTARIF

  !     CALCULATE MERCH VOLUME
subroutine GET_MERCH_VOL(SPN,DBH,MTOPP,SCRBLOGL,&
     &CV4,CVT,TARIF,VOL,ERRFLAG)
  ! Variables
  real    :: DBH,MTOPP,CV4,CVT,CV6,SV616,SV632,XINT6,TARIF,VOL(15)
  real    :: RC6,CUBUS,B4,RS616L,RS616,RS632,RI6,RI816,SV816,XINT8
  integer :: SPN,SCRBLOGL,ERRFLAG
  CV6 = 0.0
  CV8 = 0.0
  SV616 = 0.0
  SV632 = 0.0
  XINT6 = 0.0
  XINT8 = 0.0
  if (DBH.ge.9.0.and.MTOPP.ge.6.0) then
     RC6 = 0.993-0.993*0.62**(DBH-6.0)
     !     CALCULATION FOR SOFTWOOD AND HARDWOOD ARE DIFFERENT
     !        IF(SPN.LT.300) THEN
     !          CV6 = RC6*CV4
     !        ELSE
     !          CV6=RC6*CVT
     !        ENDIF
     CV6 = RC6*CV4
     if (CV6.gt.CV4) CV6 = CV4
     VOL(4) = CV6
     CUBUS = CV4-CV6
     VOL(7) = CUBUS
     B4 = TARIF/0.912733
     RS616L = (.174439+.117594*log10(DBH)*log10((TARIF/.912733))&
          &-8.210585/DBH**2+.236693*log10((TARIF/.912733))&
          &-.00001345*(TARIF/.912733)**2-.00001937*DBH**2)
     RS616 = 10**RS616L
     SV616 = RS616*CV6
     VOL(2) = SV616
     if (SCRBLOGL.eq.32) then
        RS632 = 1.001491-6.924097/TARIF+0.00001351*DBH**2
        SV632 = RS632*SV616
        VOL(2) = SV632
     end if
     RI6 = (-2.904154+3.466328*log10(DBH*TARIF)&
          &-.02765985*DBH-.00008205*TARIF**2+11.29598/DBH**2)
     XINT6 = RI6*CV6
     VOL(10) = XINT6
     !        IF(DBH.GE.11.0)THEN
     if (MTOPP.ge.8.0) then
        CV8 = CV4*(.983-.983*.65**(DBH-8.6))
        VOL(4) = CV8
        VOL(7) = CV4-CV8
        RS816 = (.99-.58*.484**(DBH-9.5))
        SV816 = RS816*SV616
        VOL(2) = SV816
        RI8 = 0.99-0.55*0.485 ** (DBH-9.5)
        XINT8 = XINT6*RI8
        VOL(10) = XINT8
     end if
  end if
  ! THE VOL HAS BEEN SET UP IN THE ABOVE, SO COMMENT OUT THE BELOW LINES (2018/11/02)
  !      IF(MTOPP.LT.0.1)THEN
  !        VOL(4)=CV4
  !      ELSEIF(MTOPP.LT.3.0)THEN
  !        VOL(4)=CVT
  !      ELSEIF(MTOPP.LT.5.0)THEN
  !        VOL(4)=CV4
  !      ELSEIF(MTOPP.LT.7.0)THEN
  !        VOL(4)=CV6
  !      ELSE
  !        VOL(4)=CV8
  !      ENDIF
  !      IF(MTOPP.LT.7.0.OR.(MTOPP.LT.0.1.AND.SPN.LT.300))THEN
  !        VOL(2)=SV616
  !        IF(SCRBLOGL.EQ.32)THEN
  !          VOL(2)=SV632
  !        ENDIF
  !        IF(MTOPP.GT.4.0) VOL(7)=CV4-CV6
  !        VOL(10)=XINT6
  !      ELSEIF(MTOPP.LE.9.0.OR.(MTOPP.LT.0.1.AND.SPN.GE.300))THEN
  !        VOL(2)=SV816
  !        IF(MTOPP.GT.4.0) VOL(7)=CV4-CV8
  !        VOL(10)=XINT8
  !      ENDIF
  return
  end subroutine GET_MERCH_VOL

  !     CHECK CF4 AND CF4_TMP ARE IN THE RANGE OF 0.3 AND 0.4
subroutine CHECK_CF(CF4)
  ! Variables
  real    :: CF4
  if (CF4.lt.0.3) CF4 = 0.3
  if (CF4.gt.0.4) CF4 = 0.4
  return
  end subroutine CHECK_CF
  ! --Browne, J. E. 1962
  ! --Standard Cubic Foot Volume for Commercial Tree Species of British Columbia
  ! --British Columbia Forest Service
  ! --Cubic foot volume of total stem from ground to tip of tree
subroutine BROWNE_CVTS(DBH,HT,COFA,COFB,COFC,CVTS)
  ! Variables
  real    :: DBH,HT,COFA,COFB,COFC,CVTS
  CVTS = 10**COFA*DBH**COFB*HT**COFC
  return
  end subroutine BROWNE_CVTS

  ! BRACKETT 1973, NOTES ON TARIS TREE VOLUME COMPUTATION, WASHING STATE DNR REPORT 24
  ! THE FOLLOWING ROUTINES CALCULATE TARIF BASED CVTS, CVT, CV4 AND CV8
subroutine GET_TARIF_FROM_CVTS(DBH, CVTS, TARIF)
  ! Variables
  real    :: DBH,CVTS,TARIF
  TARIF = (CVTS*.912733)/((1.033*(1+1.382937*&
       &EXP(-4.015292*(DBH/10.0))))&
       &*(.005454*DBH**2+.087266)-.174533)
  return
  end subroutine GET_TARIF_FROM_CVTS

subroutine GET_TARIF_FROM_CVT(DBH, CVT, TARIF)
  ! Variables
  real    :: DBH,CVT,TARIF
  TARIF = CVT*0.912733/((0.9679-0.1051*0.5523**(DBH-1.5))&
       &*((1.033*(1+1.382937*Exp(-4.015292*DBH/10.0)))&
       &*(.005454154*DBH**2+0.087266)-0.174533))
  if (TARIF.le.0.0) TARIF = 0.01
  return
  end subroutine GET_TARIF_FROM_CVT

subroutine GET_TARIF_FROM_CV4(DBH, CV4, TARIF)
  ! Variables
  real    :: DBH,CV4,TARIF
  TARIF = CV4*.912733/(.005454*DBH**2+.087266)
  return
  end subroutine GET_TARIF_FROM_CV4

subroutine GET_TARIF_FROM_CV8(DBH, CV8, TARIF)
  ! Variables
  real    :: DBH,CV8,TARIF
  TARIF = (CV8*0.912733)/((0.983-0.983*0.65**(DBH-8.6))&
       &*(.005454154*DBH**2-0.087266))
  if (TARIF.le.0.0) TARIF = 0.01
  return
  end subroutine GET_TARIF_FROM_CV8

  ! SOFTWOOD VOLUME EQUATION
  ! NVEL EQN   FIA EQN                    REFERENCE

  ! 632TRFW202 EQUATION 1 DOUGLAS-FIR  (Brackett, 1973; WEYERHAUSER-DNR RPT#24,  updated in 1977)
  ! 616TRFW202 EQUATION 2 DOUGLAS-FIR  (DNR MEMO--SUMMERFIELD,11/7/80)
  ! 516TRFW202 EQUATION 3 DOUGLAS-FIR  (USDA-FS RES NOTE PNW-266)
  ! 616TRFW122 EQUATION 4 PONDEROSA PINE   (DNR MEMO--SUMMERFIELD,11/7/80)
  ! 632TRFW122
  ! 516TRFW122 EQUATION 5 PONDEROSA PINE   (USDA-FS RES NOTE PNW-266)
  ! 532TRFW122
  ! 616TRFW263 EQUATION 6 W.HEMLOCK        (DNR NOTE 27,4/79)
  ! 632TRFW263
  !            EQUATION 7 W.HEMLOCK        (BROWNE (1962) BC FOREST SERV,P33)
  ! 616TRFW242 EQUATION 8 REDCEDAR         (REDCEDAR INTERIOR--DNR RPT#24,1977)
  ! 632TRFW242 EQUATION 9 REDCEDAR         (REDCEDAR COAST--DNR RPT#24,1977)
  ! 616TRFW019 EQUATION10 TRUE FIRS        (INTERIOR BALSAM--DNR RPT#24,1977)
  ! 632TRFW011 EQUATION11 TRUE FIRS        (COAST BALSAM--DNR RPT#24,1977)
  ! 616TRFW094 EQUATION12 SPRUCE       (SITKA SPRUCE INTERIOR--DNR RPT#24,1977)
  ! 616TRFW098 EQUATION13 SPRUCE       (SITKA SPRUCE MATURE--DNR RPT#24,1977)
  ! 632TRFW098
  ! 616TRFW108 EQUATION15 LODGEPOLE PINE   (LODGEPOLE PINE--DNR RPT#24,1977)
  ! 632TRFW108
  ! 516TRFW108 EQUATION16 LODGEPOLE PINE   (USDA-FS RES NOTE PNW-266)
  ! 616TRFW264 EQUATION17 MTN.HEMLOCK  (BELL, OSU RES.BULL 35)
  ! 632TRFW264
  ! 516TRFW021 EQUATION18 SHASTA RED FIR   (USDA-FS RES NOTE PNW-266)
  ! 532TRFW021
  ! 516TRFW081 EQUATION19 INCENSE CEDAR    (USDA-FS RES NOTE PNW-266)
  ! 532TRFW081
  ! 516TRFW117 EQUATION20 SUGAR PINE       (USDA-FS RES NOTE PNW-266)
  ! 532TRFW117
  ! 516TRFW060 EQUATION21 W.JUNIPER        (CHITTESTER,1984)
  ! 532TRFW060
  ! 616TRFW073 EQUATION22 W.LARCH      (LARCH--DNR RPT#24,1977)
  ! 632TRFW073
  ! 516TRFW015 EQUATION23 WHITE FIR        (USDA-FS RES NOTE PNW-266)
  ! 532TRFW015
  ! 616TRFW211 EQUATION24 REDWOOD      (Krumland, B.E. and L.E. Wensel. 1975. And DNR RPT#24,1977)
  ! 632TRFW211

  ! HARDWOOD VOLUME EQUATION

  ! 616TRFW351 EQUATION 25     ALDER       (CURTIS/BRUCE, PNW-56)
  ! 500DVEW351 EQUATION 26     ALDER       (BC-ALDER--DNR RPT#24,1977)
  ! 616TRFW747 EQUATION 27     COTTONWOOD  (BC-COTTONWOOD--DNR RPT#24,1977)
  ! 616TRFW746 EQUATION 28     ASPEN       (BC-ASPEN--DNR RPT#24,1977)
  !            EQUATION 29     BIRCH           (BC-BIRCH--DNR RPT#24,1977)
  !            EQUATION 30     BIGLEAF MAPLE   (BC-MAPLE--DNR RPT#24,1977)
  ! 616TRFW998 EQUATION 31     EUCALYPTUS      (MEMO,COLIN D. MacLEAN 1/27/83,(REVISED 2/7/83) )
  ! 500DVEW431 EQUATION 32     G.CHINQUAPIN    (PILLSBURY (H,D), CHARLES BOLSINGER 1/3/83)
  ! 500DVEW981 EQUATION 33     C.LAUREL        (PILLSBURY (H,D), CHARLES BOLSINGER 1/3/83)
  ! 500DVEW631 EQUATION34      TANOAK      (PILLSBURY (H,D), CHARLES BOLSINGER 1/3/83)
  ! 500DVEW821 EQUATION35      CALIF WHITE OAK     (PILLSBURY (H,D), CHARLES BOLSINGER 1/3/83)
  ! 500DVEW811 EQUATION36      ENGELMANN OAK   (PILLSBURY (H,D), CHARLES BOLSINGER 1/3/83)
  ! 500DVEW312 EQUATION37      BIGLEAF MAPLE   (PILLSBURY (H,D,FC), CHARLES BOLSINGER 1/3/83)
  ! 500DVEW818 EQUATION38      CALIF BLACK OAK     (PILLSBURY (H,D,FC), CHARLES BOLSINGER 1/3/83)
  ! 500DVEW807 EQUATION39      BLUE OAK        (PILLSBURY (H,D,FC), CHARLES BOLSINGER 1/3/83)
  ! 500DVEW361 EQUATION40      PACIFIC MADRONE     (PILLSBURY (H,D,FC), CHARLES BOLSINGER 1/3/83)
  ! 500DVEW815 EQUATION41      ORE WHITE OAK   (PILLSBURY (H,D,FC), CHARLES BOLSINGER 1/3/83)
  ! 500DVEW805 EQUATION42      CANYON LIVE OAK     (PILLSBURY (H,D,FC), CHARLES BOLSINGER 1/3/83)
  ! 500DVEW801 EQUATION43      COAST LIVE OAK  (PILLSBURY (H,D,FC), CHARLES BOLSINGER 1/3/83)
  ! 500DVEW839 EQUATION44      INT LIVE OAK        (PILLSBURY (H,D,FC), CHARLES BOLSINGER 1/3/83)
  ! 400DVEW475 EQUATION45  MTN. MAHOGANY   (Chojnacky, 1985)
