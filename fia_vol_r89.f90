!=================================!
!   VolumeLibrary : fia_vol_r89   !
!=================================!
  ! This is from FIA package code NIMS_VOL_R89
  ! Created by YW 2018/09/12

  !--Clark, Phillips, Hitchcock
  !--Predicted Weights and Volumes of Scarlet Oak Trees on the Tennessee Cumberland Plateau
  !--SE-214
  ! CU806001 CVT (W+B) FUNCTION QUCO2_CK_1CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU806002 CVT (W)   FUNCTION QUCO2_CK_2CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU806003 CV8 (W+B) FUNCTION QUCO2_CK_3CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU806004 CV8 (W)   FUNCTION QUCO2_CK_4CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU806005 CV4 (W+B) FUNCTION QUCO2_CK_5CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU806006 CV4 (W)   FUNCTION QUCO2_CK_6CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU806007 CROWN (W+B) FUNCTION QUCO2_CK_7CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU806008 CROWN (W)   FUNCTION QUCO2_CK_8CU(DBH IN NUMBER, THT IN NUMBER)
  !--Clark, Phillips, Schroeder
  !--Predicted Weights and Volumes of Northern Red Oak Trees in Western North Carolina
  !--SE-209
  ! CU833001 CVT (W+B) FUNCTION QURU_CK_1CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU833002 CVT (W)   FUNCTION QURU_CK_2CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU833003 CV8 (W+B) FUNCTION QURU_CK_3CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU833004 CV8 (W)   FUNCTION QURU_CK_4CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU833005 CV4 (W+B) FUNCTION QURU_CK_5CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU833006 CV4 (W)   FUNCTION QURU_CK_6CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU833007 CROWN (W+B) FUNCTION QURU_CK_7CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU833008 CROWN (W)   FUNCTION QURU_CK_8CU(DBH IN NUMBER, THT IN NUMBER)
  ! NVEL EQUATION NUMBER:
  ! S00CLK0806, S00CLK0833, S00CLK1806, S00CLK1833
  ! THE 7TH CHARACTER IN THE EQUATION NUMBER: 0 = WOOD, 1 = WOOD AND BARK
subroutine CLARK_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG,CROWN)
  ! Variables
  character(len=10) :: VOLEQ
  integer :: ERRFLG,SPN
  real    :: DBHOB,HTTOT,BFMIND,VOL(15),D2H
  real    :: A0,B0,A4,B4,A8,B8,AC,BC,CV4,CV8,CROWN
  CV(D2H,A0,B0) = A0*D2H**B0
  ERRFLG = 0
  VOL = 0.0
  read(VOLEQ(8:10),'(I3)')SPN
  if (SPN.eq.806) then
     if (VOLEQ(7:7).eq.'0') then ! WOOD ONLY
        A0 = 0.00233
        B0 = 1.01465
        A4 = 0.00104
        B4 = 1.06736
        A8 = 0.00054
        B8 = 1.09480
        AC = 0.00153
        BC = 0.91833
     else ! WOOD AND BARK
        A0 = 0.00311
        B0 = 1.00368
        A4 = 0.00119
        B4 = 1.05828
        A8 = 0.00074
        B8 = 1.07831
        AC = 0.00194
        BC = 0.92165
     end if
  else if (SPN.eq.833) then
     if (VOLEQ(7:7).eq.'0') then ! WOOD ONLY
        A0 = 0.002439
        B0 = 1.00568
        A4 = 0.001763
        B4 = 1.01258
        A8 = 0.000633
        B8 = 1.09537
        AC = 0.000697
        BC = 0.97993
     else ! WOOD AND BARK
        A0 = 0.00305
        B0 = 1.00014
        A4 = 0.002183
        B4 = 1.00496
        A8 = 0.000753
        B8 = 1.09162
        AC = 0.000889
        BC = 0.98263
     end if
  else
     ERRFLG = 6
     return
  end if
  D2H = DBHOB*DBHOB*HTTOT
  VOL(1) = CV(D2H,A0,B0)
  CV4 = CV(D2H,A4,B4)
  VOL(4) = CV4
  if (BFMIND.lt.0.1) BFMIND = 11.0
  if (DBHOB.ge.BFMIND) then
     CV8 = CV(D2H,A8,B8)
     if (CV4.gt.CV8) then
        VOL(4) = CV8
        VOL(7) = CV4 - CV8
     end if
  end if
  !     THE CROWN VOLUME IS CALCULATED BUT NOT SAVED IN VOL BECAUSE THERE IS NO PLACE IN VOL TO HOLD IT
  !     This actualy is branches
  CROWN = CV(D2H,AC,BC)
  return
  end subroutine CLARK_VOL

  ! --Aspen
  ! Schlaegel, B.E. 1975.
  ! Estimating Aspen Volume and Weight for Individual Trees, Diameter Classes or Entire Stands
  ! NC-20
  ! CU746007 CVT (W)   FUNCTION POTRWOOD_SCH_CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU746008 CVT (W+B) FUNCTION POTRBARK_SCH_CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU746009 CVT (W+B+BRANCH) FUNCTION POTRTOTAL_SCH_CU(DBH IN NUMBER, THT IN NUMBER)
  ! --Willow Oak
  ! Schlaegel, B.E. 1981.
  ! Willow Oak VOlume and Weight Tables for the Mississippi Delta
  ! SO-173
  ! CU831001 CVT (W) FUNCTION QUPHWOOD_SCH_CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU831002 CVT (W+B) FUNCTION QUPHBARK_SCH_CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU831001 CVT (W+B+BRANCH) FUNCTION QUPHTOTAL_SCH_CU(DBH IN NUMBER, THT IN NUMBER)
  ! NVEL EQUATION NUMBER:
  ! N00SCH0746, N00SCH1746, S00SCH0831, S00SCH1831
  ! THE 7TH CHARACTER IN NVEL EQUATION: 0 = WOOD, 1 = WOOD+BARK
subroutine SCHLAEGEL_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG,&
     &TOTTREE)
  ! Variables
  character(len=10) :: VOLEQ
  integer :: ERRFLG,SPN
  real    :: DBHOB,HTTOT,BFMIND,VOL(15),BH,D2H,TOTTREE
  ERRFLG = 0
  VOL = 0.0
  read(VOLEQ(8:10),'(I3)')SPN
  if (SPN.eq.746) then
     BH = 3.141592/144.0/4.0*DBHOB*DBHOB*HTTOT
     if (VOLEQ(7:7).eq.'0') then ! WOOD ONLY
        VOL(1) = EXP(0.99554*LOG(BH)-0.85373)
     else !WOOD + BARK
        VOL(1) = EXP(0.99788*LOG(BH)-0.73529)
     end if
     TOTTREE = EXP(0.98717*LOG(BH)-0.47559)
  else if (SPN.eq.831) then
     D2H = DBHOB*DBHOB*HTTOT
     if (VOLEQ(7:7).eq.'0') then ! WOOD ONLY
        VOL(1) = EXP(0.97301*LOG(D2H)-6.047)
     else !WOOD + BARK
        VOL(1) = EXP(0.96166*LOG(D2H)-5.798)
     end if
     TOTTREE = EXP(0.98243*LOG(D2H)-5.719)
  else
     ERRFLG = 6
     return
  end if
  return
  end subroutine SCHLAEGEL_VOL

  ! Barnard, Bickford and Mayer 1975
  ! Forest Survey Cubic-foot Volume Equations
  ! NE-166
  ! CU000036 FUNCTION BARN_NE166_CU(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
  ! ACTUALLY THIS FUNCTION REQUIRE HEIGHT TO 4" TOP AND VOLUME IS CV4
  ! VALID SPECIES CODE:
  ! 98, 12, 260, 91, 40, 318, 310, 540, 762, 370, 531, 951, 693, 832, 400, 999
  ! NVEL EQUATION NUMBER:
  ! N02BAR0***
subroutine BARNARD_VOL(VOLEQ,DBHOB,HT2PRD,VOL,ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  integer :: ERRFLG,SPN
  real    :: DBHOB,HT2PRD,VOL(15),A,B,D2H
  ERRFLG = 0
  VOL = 0.0
  if (HT2PRD.lt.4.5) then
     ERRFLG = 10
     return
  end if
  read(VOLEQ(8:10),'(I3)')SPN
  if (SPN.eq.129) then
     A = 3.5142
     B = .00236
  else if (SPN.eq.98) then
     A = 2.1998
     B = .00257
  else if (SPN.eq.12) then
     A = 1.4793
     B = .00272
  else if (SPN.eq.260) then
     A = 2.4784
     B = .00242
  else if (SPN.eq.91) then
     A = -.00496
     B = .00303
  else if (SPN.eq.40) then
     A = 1.5817
     B = .00259
  else if (SPN.eq.318) then
     A = 1.6823
     B = .00310
  else if (SPN.eq.310) then
     A = 1.1763
     B = .00310
  else if (SPN.eq.540) then
     A = 1.0067
     B = .00293
  else if (SPN.eq.762) then
     A = 1.1809
     B = .00292
  else if (SPN.eq.370) then
     A = 1.1339
     B = .00281
  else if (SPN.eq.531) then
     A = 1.2851
     B = .00326
  else if (SPN.eq.951) then
     A = 0.8976
     B = .00349
  else if (SPN.eq.693) then
     A = 1.2027
     B = .00301
  else if (SPN.eq.832) then
     A = 1.0009
     B = .00300
  else if (SPN.eq.400) then
     A = 1.4438
     B = .00316
  else if (SPN.eq.999) then
     A = 0.8591
     B = .00309
  else
     ERRFLG = 6
     return
  end if;
  D2H = DBHOB*DBHOB*HT2PRD
  VOL(4) = A + (B*D2H)
  return
  end subroutine BARNARD_VOL

  ! Burkhart, H.E. 1977. Cubic-Foot Volume of Loblolly Pine to Any Merchantable Top Limit. S. J. Appl. For. 1:7-9
  ! CU131001 CVT (W+B) plantation-grown -- FUNCTION PITA_BUR_DOBP_CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU131002 CVT (W)   plantation-grown --  FUNCTION PITA_BUR_DIBP_CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU131003 CVT (W+B) natural stand-grown -- FUNCTION PITA_BUR_DOBN_CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU131004 CVT (W)   natural stand-grown -- FUNCTION PITA_BUR_DIBN_CU(DBH IN NUMBER, THT IN NUMBER)
  ! NVEL EQUATION NUMBER:
  ! S00BUR0131, S01BUR0131, S00BUR1131, S01BUR1131
  ! THE 2&3 CHARACTER IN NVEL EQUQTION: 00 = NATURAL, 01 = PLANTATION
  ! THE 7TH CHARACTER IN NVEL EQUATION: 0 = INSIDE BARK, 1 = OUTSIDE BARK
subroutine BURKHART_VOL(VOLEQ,DBHOB,HTTOT,VOL,ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  real    :: DBHOB,HTTOT,VOL(15),D2H
  integer :: ERRFLG
  ERRFLG = 0
  VOL = 0.0
  D2H = DBHOB*DBHOB*HTTOT
  if (VOLEQ(2:3).eq.'01') then
     if (VOLEQ(7:7).eq.'0') then
        VOL(1) = 0.11691 + (0.00185*D2H)
     else
        VOL(1) = 0.34864 + (0.00232*D2H)
     end if
  else if (VOLEQ(2:3).eq.'00') then
     if (VOLEQ(7:7).eq.'0') then
        VOL(1) = 0.00828 + (0.00205*D2H)
     else
        VOL(1) = 0.27611 + (0.00253*D2H)
     end if
  end if
  return
  end subroutine BURKHART_VOL

  ! Farrar, R.M. 1981.
  ! Cubic-Foot Volume, Surface Area, and Merchantable Height Functions for Longleaf Pine Trees
  ! Research paper SO-166
  ! CU121001 FUNCTION PIPA2_FAR_0CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU121002 FUNCTION PIPA2_FAR_1CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU121003 FUNCTION PIPA2_FAR_2CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU121004 FUNCTION PIPA2_FAR_3CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU121005 FUNCTION PIPA2_FAR_4CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU121006 FUNCTION PIPA2_FAR_5CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU121007 FUNCTION PIPA2_FAR_6CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU121008 FUNCTION PIPA2_FAR_7CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU121009 FUNCTION PIPA2_FAR_8CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU121010 FUNCTION PIPA2_FAR_9CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU121011 FUNCTION PIPA2_FAR_10CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU121012 FUNCTION PIPA2_FAR_11CU(DBH IN NUMBER, THT IN NUMBER)
  ! CU121013 FUNCTION PIPA2_FAR_12CU(DBH IN NUMBER, THT IN NUMBER)
  ! NVEL EQUATION NUMBER:
  ! S00FAR0121
subroutine FARRAR_VOL(DBHOB,HTTOT,MTOPP,VOL,ERRFLG)
  ! Variables
  real    :: DBHOB,HTTOT,MTOPP,VOL(15),B0,B1,CVT,D2H,CV4

  integer :: ERRFLG
  ERRFLG = 0
  VOL = 0.0
  D2H = DBHOB*DBHOB*HTTOT
  CVT = 0.00535 + (0.0021971*D2H)
  B0 = -0.030701
  B1 = -1.3732
  CV4 = CVT/(1.0 + ((10.0**B0)*(CVT**B1)))
  if (MTOPP.lt.0.1) MTOPP = 4.0
  if (MTOPP.lt.1.5) then
     B0 = -2.6629
     B1 = -1.1188
  else if (MTOPP.lt.2.5) then
     B0 = -1.2415
     B1 = -1.2107
  else if (MTOPP.lt.3.5) then
     B0 = -0.5493
     B1 = -1.3024
  else if (MTOPP.lt.4.5) then
     B0 = -0.030701
     B1 = -1.3732
  else if (MTOPP.lt.5.5) then
     B0 = 0.45856
     B1 = -1.4739
  else if (MTOPP.lt.6.5) then
     B0 = 0.93421
     B1 = -1.6004
  else if (MTOPP.lt.7.5) then
     B0 = 1.1610
     B1 = -1.5844
  else if (MTOPP.lt.8.5) then
     B0 = 1.4899
     B1 = -1.6501
  else if (MTOPP.lt.9.5) then
     B0 = 1.7427
     B1 = -1.6701
  else if (MTOPP.lt.10.5) then
     B0 = 1.8512
     B1 = -1.6074
  else if (MTOPP.lt.11.5) then
     B0 = 2.2608
     B1 = -1.7210
  else
     B0 = 2.3442
     B1 = -1.6435
  end if
  VOL(4) = CVT/(1.0 + ((10.0**B0)*(CVT**B1)))
  VOL(1) = CVT
  if (MTOPP.gt.4.0) VOL(7) = CV4 - VOL(4)
  return
  end subroutine FARRAR_VOL

  ! Green and Strawderman 1986
  ! Stein-rule estimation of coefficients for 18 eastern hardwood cubic volume equations
  ! Can. J. For. Res. 16:249-255
  ! CU000039 CVT (W+B) FUNCTION GREEN_HARD_CU(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
  ! VALID SPECIES CODE:
  ! 316, 318, 372, 371, 400, 531,541,621,651,690,762,802,806,832,812,837,901,951
  ! NVEL EQUATION NUMBER:
  ! N00GRE0***
subroutine GREEN_VOL(VOLEQ,DBHOB,HTTOT,VOL,ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  real    :: DBHOB,HTTOT,VOL(15),D2H,B,DBHCM,HTM,CVM
  integer :: ERRFLG,SPN
  ERRFLG = 0
  VOL = 0.0
  read(VOLEQ(8:10),'(I3)')SPN
  if (SPN.eq.316) then
     B = 0.0000366
  else if (SPN.eq.318) then
     B = 0.0000363
  else if (SPN.eq.372) then
     B = 0.0000375
  else if (SPN.eq.371) then
     B = 0.0000371
  else if (SPN.eq.400) then
     B = 0.0000371
  else if (SPN.eq.531) then
     B = 0.0000371
  else if (SPN.eq.541) then
     B = 0.0000372
  else if (SPN.eq.621) then
     B = 0.0000384
  else if (SPN.eq.651) then
     B = 0.0000381
  else if (SPN.eq.690) then
     B = 0.0000393
  else if (SPN.eq.762) then
     B = 0.0000382
  else if (SPN.eq.802) then
     B = 0.0000390
  else if (SPN.eq.806) then
     B = 0.0000386
  else if (SPN.eq.832) then
     B = 0.0000371
  else if (SPN.eq.812) then
     B = 0.0000376
  else if (SPN.eq.837) then
     B = 0.0000366
  else if (SPN.eq.901) then
     B = 0.0000373
  else if (SPN.eq.951) then
     B = 0.0000405
  else
     ERRFLG = 6
     return
  end if;
  DBHCM = DBHOB*2.54
  HTM = HTTOT*0.3048
  D2H = DBHCM*DBHCM*HTM
  CVM = B*D2H
  VOL(1) = CVM*35.3147
  return
  end subroutine GREEN_VOL

  ! Hilt, D.E. 1980. Taper-Based System for Estimating Stem Volumes of Upland Oaks. NE-458
  ! CU800003 CVT FUNCTION QUERC_HILT_CU(DBH IN NUMBER, THT IN NUMBER)
  ! NVEL EQUATION NUMBER:
  ! N03HIL0800
subroutine HILT_VOL(DBHOB,HTTOT,VOL,ERRFLG)
  ! Variables
  real    :: DBHOB,HTTOT,VOL(15),F,Z,DBH,THT
  integer :: ERRFLG
  ERRFLG = 0
  VOL = 0.0
  DBH = DBHOB
  THT = HTTOT
  Z = ((THT-1.0)/(THT-4.5))
  F = (0.4*(Z**(5.0/2.0))) + (1.21752E-3*THT*(Z**(5.0/2.0))) -&
       &(7.6098E-4*THT*(Z**4)) + (9.041E-5*DBH*THT*(Z**(5.0/2.0))) -&
       &(5.651E-5*DBH*THT*(Z**4)) + (1.28405E-3*DBH*(Z**(5.0/2.0))) -&
       &(1.0355E-4*DBH*(Z**31)) - (8.48E-5*DBH*THT*(Z**(5.0/2.0))) +&
       &(6.84E-6*DBH*THT*(Z**31))
  VOL(1) = 0.91*0.00545415*F*DBH*DBH*(THT-4.5)
  return
  end subroutine HILT_VOL
