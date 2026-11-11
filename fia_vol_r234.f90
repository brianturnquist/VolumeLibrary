!==================================!
!   VolumeLibrary : fia_vol_r234   !
!==================================!
  ! This is from FIA package code NIMS_VOL_R234
  ! Created by YW 2018/09/04

  ! Chojnacky, 1994. Volume Equations for New Mexico's Pinyon-Juniper Dryland Forests
  ! USDA Forest Service Intermountain Station Research Paper INT-471
  ! Cubic Foot volume of all wood and bark to a 1.5 and 3.0  inch branch diameter
  ! CU064004 CV1.5 FUNCTION JUNIP4_CH_CU(DRC IN NUMBER, THT IN NUMBER)
  ! CU064005 CV3   FUNCTION JUNIP5_CH_CU(DRC IN NUMBER, THT IN NUMBER)
  ! CU064006 CV1.5 FUNCTION JUNIP6_CH_CU(DRC IN NUMBER, THT IN NUMBER)
  ! CU064007 CV3   JUNIP7_CH_CU(DRC IN NUMBER, THT IN NUMBER)
  ! CU106005 CV1.5 PIED1_CH_CU(DRC IN NUMBER, THT IN NUMBER)
  ! CU106006 CV3   PIED2_CH_CU(DRC IN NUMBER, THT IN NUMBER)
  ! valid species code: 63, 66, 65, 69, 106
  ! NVEL EQUATION NUMBER:
  ! R03CHO0063, R03CHO0066,R03CHO0065,R03CHO0069,R03CHO0106,
subroutine CHOJNACKY15_30(VOLEQ,DRCOB,HTTOT,MTOPP,VOL,ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  integer :: SPN,ERRFLG
  real    :: DRCOB,HTTOT,MTOPP,VOL(15),D2H,X
  real    :: B0,B1,B2,X0,CV15,CV3
  V1(B0,B1,B2,X) = B0+B1*X+B2*X**2
  V2(B0,B1,B2,X0,X) = B0+B1*X+B2*(3.0*X0**2-2.0*X0**3/X)
  ERRFLG = 0
  VOL = 0.0
  !Set to 0.1 for small tree DIA < 3
  if (DRCOB.lt.3) then
     VOL(1) = 0.1
     return
  end if
  D2H = DRCOB*DRCOB*HTTOT
  X = D2H/1000.0
  read(VOLEQ(8:10),'(I3)')SPN
  if (MTOPP.lt.1.5) MTOPP = 1.5
  if (MTOPP.gt.1.5) MTOPP = 3.0
  if (SPN.eq.63.or.SPN.eq.66) then
     B0 = 0.0255
     B1 = 1.7479
     B2 = 0.1994
     X0 = 4.0021
  else if (SPN.eq.65.or.SPN.eq.69) then
     B0 = -.0192
     B1 = 2.1297
     B2 = .1100
     X0 = 2.5757
  else if (SPN.eq.106) then
     B0 = -.0594
     B1 = 2.6358
     B2 = .3248
     X0 = 2.0773
  else
     ERRFLG = 6
     return
  end if
  !     CALCULATE WOOD AND BARK VOLUME TO 1.5 INCH DIAMETER
  if (X.le.X0) then
     CV15 = V1(B0,B1,B2,X)
  else
     CV15 = V2(B0,B1,B2,X0,X)
  end if
  VOL(1) = CV15
  VOL(4) = CV15
  !     CALCULATE WOOD AND BARK VOLUME TO 3.0 INCH DIAMETER
  if (MTOPP.eq.3.0) then
     if (SPN.eq.63.or.SPN.eq.66) then
        B0 = -.0601
        B1 = 1.3846
        B2 = .1566
        X0 = 5.2101
     else if (SPN.eq.65.or.SPN.eq.69) then
        B0 = -.1063
        B1 = 1.4373
        B2 = .1324
        X0 = 4.0243
     else if (SPN.eq.106) then
        B0 = -.1231
        B1 = 2.0741
        B2 = .1831
        X0 = 3.5503
     end if
     if (X.le.X0) then
        CV3 = V1(B0,B1,B2,X)
     else
        CV3 = V2(B0,B1,B2,X0,X)
     end if
     VOL(4) = CV3
  end if
  return
  end subroutine CHOJNACKY15_30

  !--Southwestern Ponderosa Pine
  !--McTague, J.P. and Stansfied, W.F. 1988
  !--Total and Merchantable Volume Equations and Taper Functoins for Southwestern Ponderosa Pine
  !--West J. appl. For. 3(4):123-125
  !CU122016 - CVT - FUNCTION PIPO_MCT_ICU(DBH IN NUMBER, THT IN NUMBER)
  !CU122017 - CVT(outside bark) - FUNCTION PIPO_MCT_OCU(DBH IN NUMBER, THT IN NUMBER)
  ! NVEL EQUATION NUMBER:
  ! R00MCT0122(inside bark), R00MCT1122 (outside bark)
subroutine MACTAGUE_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,MTOPP,VOL,ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  real    :: DBHOB,HTTOT,BFMIND,MTOPP,VOL(15),F,D2H,CVT,CV4,CV6,TOPD,HTUP
  integer :: ERRFLG
  real    :: TOPV,DBH,THT
  ERRFLG = 0
  VOL = 0.0
  DBH = DBHOB
  THT = HTTOT
  D2H = DBHOB*DBHOB*HTTOT
  F = 77.73 + (0.000056598*D2H) - (40042.8/D2H)
  if (VOLEQ(7:7).eq.'0') then !INSIDE BARK
     CVT = 0.00000528*(DBH**2.019660)*(THT**0.720826)*(F**1.629650)
     TOPD = 4.0
     CV4 = CVT - CVT*(1934.82*TOPD**2.967686/&
          &(DBHOB**2.902258*F**1.755814))
     VOL(4) = CV4
     if (DBHOB.ge.BFMIND.and.MTOPP.gt.4.0) then
        TOPD = MTOPP
        CV6 = CVT - CVT*(1934.82*TOPD**2.967686/&
             &(DBHOB**2.902258*F**1.755814))
        TOPV = CV4-CV6
        VOL(4) = CV6
        VOL(7) = TOPV
     end if
  else !OUTSIDE BARK
     CVT = 0.00003213*(DBH**1.962963)*(THT**0.710346)*(F**1.316366)
     TOPD = 4.0
     CV4 = CVT - CVT*(63.2146*TOPD**2.866527/&
          &(DBHOB**2.592541*F**1.202693))
     VOL(4) = CV4
     if (DBHOB.ge.BFMIND.and.MTOPP.gt.4.0) then
        TOPD = MTOPP
        CV6 = CVT - CVT*(63.2146*TOPD**2.866527/&
             &(DBHOB**2.592541*F**1.202693))
        TOPV = CV4-CV6
        VOL(4) = CV6
        VOL(7) = TOPV
     end if
  end if
  VOL(1) = CVT
  VOL(15) = CVT - CV4
  return
  end subroutine MACTAGUE_VOL

  !MACTAGUE TAPER EQUATION TO CALCULATE HEIGHT TO A TOPD OR TOPD AT A GIVEN HEIGHT
subroutine MACTAGUE_TAP(VOLEQ,DBHOB,HTTOT,HTUP,TOPD)
  ! Variables
  character(len=10) :: VOLEQ
  real    :: DBHOB,HTTOT,HTUP,TOPD,F,D2H
  D2H = DBHOB*DBHOB*HTTOT
  F = 77.73 + (0.000056598*D2H) - (40042.8/D2H)
  if (VOLEQ(7:7).eq.'0') then
     !     INSIDE BARK
     if (HTUP.gt.0.1) then
        TOPD = 0.0692939*(HTTOT-HTUP)**0.732737*DBHOB**0.977953&
             &*F**0.61725/HTTOT**0.729358
     else if (TOPD.gt.0.0) then
        HTUP = HTTOT-38.208303*TOPD**1.364747*HTTOT**0.995426/&
             &(DBHOB**1.334659*F**0.84239)
     end if
  else
     !     OUTSIDE BARK
     if (HTUP.gt.0.1) then
        TOPD = 0.194206*(HTTOT-HTUP)**0.766379*DBHOB**0.904419&
             &*F**0.463624/HTTOT**0.764885
     else if (TOPD.gt.0.0) then
        HTUP = HTTOT-8.485946*TOPD**1.304838*HTTOT**0.998051/&
             &(DBHOB**1.18012*F**0.604954)
     end if
  end if
  return
  end subroutine MACTAGUE_TAP

  ! Southwestern Ponderosa Pine
  ! Myers, C.A. 1963. Volume, taper, and related tables for southwestern ponderosa pine.
  ! USDA Forest Service Rocky Mountain Forest and Range Experiment Station Research Paper RM-2
  ! CU122018 CVT FUNCTION PIPO1_MYRM2_CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU122019 CVT FUNCTION PIPO2_MYRM2_CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU122020 CV4 FUNCTION PIPO3_MYRM2_MCU(DBH IN NUMBER, THT IN NUMBER)
  ! CU122021 CV4 FUNCTION PIPO4_MYRM2_MCU(DBH IN NUMBER, THT IN NUMBER)
  ! CU122022 CV6 FUNCTION PIPO5_MYRM2_SCU(DBH IN NUMBER, THT IN NUMBER)
  ! CU122023 CV6 FUNCTION PIPO6_MYRM2_SCU(DBH IN NUMBER, THT IN NUMBER)
  ! BD122012 SV6 FUNCTION PIPO1_MYRM2_BD(DBH IN NUMBER, THT IN NUMBER)
  ! BD122013 SV6 FUNCTION PIPO2_MYRM2_BD(DBH IN NUMBER, THT IN NUMBER)
  ! BD122014 IV6 FUNCTION PIPO3_MYRM2_IBD(DBH IN NUMBER, THT IN NUMBER)
  ! BD122015 IV6 FUNCTION PIPO4_MYRM2_IBD(DBH IN NUMBER, THT IN NUMBER)
  ! NVEL EQUATION NUMBER:
  ! R00MYE0122 (BLACKJACK), R00MYE1122(OLDGROWTH)
subroutine MYERS_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,MTOPP,VOL,ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  real    :: DBHOB,HTTOT,BFMIND,MTOPP,VOL(15)
  integer :: ERRFLG
  real    :: CVT,CV4,CV6,SV6,IV6,D2H
  ERRFLG = 0
  VOL = 0.0
  D2H = DBHOB*DBHOB*HTTOT
  if (VOLEQ(7:7).eq.'0') then
     !blackjack
     if (D2H.le.6000.0) then
        CVT = (0.001824*D2H) + 0.5870
     else if (D2H.gt.6000.0) then
        CVT = (0.002103*D2H) - 1.091458
     end if
     if (D2H.le.6500.0) then
        CV4 = (0.001770*D2H) - 0.2055
     else if (D2H.gt.6500.0) then
        CV4 = (0.002056*D2H) - 2.061477
     end if
     VOL(4) = CV4
     if (DBHOB.ge.BFMIND.and.MTOPP.gt.4.0) then
        CV6 = (0.002043*D2H) - 6.744303
        VOL(4) = CV6
        VOL(7) = CV4 - CV6
        if (D2H.le.20000.0) then
           SV6 = (0.009481*D2H) - 26.48230
           IV6 = (0.011744*D2H) - 37.47260
        else if (D2H.gt.20000.0) then
           SV6 = (0.013887*D2H) - 114.596390
           IV6 = (0.014270*D2H) - 87.992784
        end if
        VOL(2) = SV6
        VOL(10) = IV6
     end if
  else
     !old-growth
     CVT = (0.002302*D2H) - 0.402357
     CV4 = (0.002255*D2H) - 1.440832
     VOL(4) = CV4
     if (DBHOB.ge.BFMIND.and.MTOPP.gt.4.0) then
        CV6 = (0.002226*D2H) - 5.161622
        VOL(4) = CV6
        VOL(7) = CV4 - CV6
        if (D2H.le.22900.0) then
           SV6 = (0.011962*D2H) - 44.1260
        else if (D2H.gt.22900.0) then
           SV6 = (0.015666*D2H) - 129.672480
        end if
        VOL(2) = SV6
        if (D2H.le.21800.0) then
           IV6 = (0.0140*D2H) - 48.0
        else if (D2H.gt.21800) then
           IV6 = (0.016041*D2H) - 95.013379
        end if
        VOL(10) = IV6
     end if
  end if
  VOL(1) = CVT
  return
  end subroutine MYERS_VOL

  !-- Clendenen,  Memo dated 12/06/76
  !-- Used by RMRS-FIA for amall trees
  !-- Total stem wood fiber cubic foot volume of trees 1.0 thru 4.9 inches DBH
  ! CU000061 FUNCTION SMALL_DBH_CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU000062 FUNCTION SMALL_DRC_CU(DRC IN NUMBER, THT IN NUMBER)
  ! CU000063 FUNCTION SMALL_DBH_MT_CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU000064 FUNCTION SMALL_DRC_MT_CU(DRC IN NUMBER, THT IN NUMBER)
  ! NVEL EQUATION NUMBER:
  ! R00CLE0000, R01CLE0000
subroutine CLENDENEN_SMALLTREE(VOLEQ,DBHOB,DRCOB,HTTOT,VOL,ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  real    :: DBHOB,DRCOB,HTTOT,VOL(15),DBH,HT,TREEFORM
  integer :: ERRFLG
  ERRFLG = 0
  VOL = 0.0
  DBH = DBHOB
  if (DRCOB.gt.0.0) DBH = DRCOB
  if (DBH.ge.5.0) return
  HT = HTTOT
  if (HT.lt.0.1) HT = 10.0
  if ((VOLEQ(2:3).eq.'01').and.HT.gt.20.0) HT = 20.0
  if (HT.le.18.0) then
     TREEFORM = .406098*(HT-0.9)**2/(HT-4.5)**2-&
          &0.0762998*DBH*(HT-0.9)**3/(HT-4.5)**3+&
          &0.00262615*DBH*HT*(HT-0.9)**3/(HT-4.5)**3
  else
     TREEFORM = .480961+42.46542/HT**2-10.99643*DBH/HT**2&
          &-.107809*DBH/HT-.00409083*DBH
  end if;
  if (TREEFORM.lt.0.3333) then
     TREEFORM = 0.33333
  end if;
  VOL(1) = 0.005454154 * TREEFORM *DBH**2*HT
  return
  end subroutine CLENDENEN_SMALLTREE
