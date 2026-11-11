!============================!
!   VolumeLibrary : r6dibs   !
!============================!
  !== last modified  08-13-2003
subroutine R6DIBS (IAPZ,DBHOB,IBTR,FCLASS,MTOPP,TLH,TH,&
     &XLOGS,LOGDIA,SL,XL,A)

  ! THIS IS THE BASIC TREE MENSURATION ALGORITHM USED AS A SUBROUTINE
  ! BY ANY PROGRAM OF THE CRS6 SYSTEM.

  ! Variables
  integer :: FCLASS,I,IAPZ,IRET,ILH,ID
  real    :: IBTR,LOGDIA(21,3),SL(20),XL(20),S,XLOGS,AT,A,BT,H1,HX,HR,DR
  real    :: FC16,FC32,AIFC,MTOPP,TLH,TH,DBHOB,DOB16,T,B,H,LOGS,H2,BTR

  AIFC = FCLASS
  !     ***** CLEAR PREVIOUS TREE *****
  do I = 1,20
     SL(I) = 0.0
     5 XL(I) = 0.0
  end do

  do I = 1,21
     LOGDIA(I,1) = 0.0
     LOGDIA(I,2) = 0.0
     7 continue
  end do

  FC16 = 0.0
  FC32 = 0.0
  S = 0.0
  XLOGS = 0.0

  if (IAPZ.eq.2) go to 200

  !********************** EASTSIDE ***************************

  FC16 = AIFC / 100.0
  !                             btr diameter logic removed 8/98
  !      IF (IBTR.GT.0) GO TO 10

  !     ***** DBHOB-SIGHTING - COMPUTE LOGDIA(1,2) WITH FC *****
  LOGDIA(1,2) = DBHOB * FC16
  go to 20

  !     ***** 16-FT SIGHTING - COMPUTE LOGDIA(1,2) WITH BTR *****
  10 DOB16 = DBHOB
  LOGDIA(1,2) = DOB16 * IBTR
  DBHOB = LOGDIA(1,2) / FC16

  !    ***** COMPUTE TAPER COEFFICIENT *****
  20 IRET = 1
  go to 1000
  25 if (TLH.eq.0.0) go to 70

  !     ***************** LOG HEIGHT ******************
  SL(1) = 16.3
  if (TLH.gt.1.0.and.LOGDIA(1,2).gt.MTOPP) go to 30

  !     ***** ONLY ONE LOG IN TREE *****
  XLOGS = 1.0
  LOGDIA(1,2) = MTOPP
  !      IF (IBTR.GT.0) DBHOB=LOGDIA(1,2)/FC16
  go to 130

  !     ***** MORE THAN ONE LOG IN TREE *****
  30 ILH = TLH
  S = TLH - ILH
  if (S.eq.0.0) go to 40

  !     ***** TREE HAS A HALF LOG AT THE TOP *****
  S = 8.0
  SL(ILH+1) = 8.0
  LOGDIA(ILH+1,2) = MTOPP
  XLOGS = 0.5
  if (ILH.gt.1) go to 40

  !     ***** 1.5-LOG TREE *****
  XLOGS = 1.5
  go to 130

  !     ***** COMPUTE DIBS OF ALL FULL LOGS *****
  40 T = MTOPP / LOGDIA(1,2)
  AT = A / (1.0 - (A*T))
  BT = (1.0 / (1.0-T)) -AT
  H1 = (ILH - 1) * 16.3 + S
  do I = 2,ILH
     HX = H1 - (I-1)*16.3
     HR = HX / H1
     DR = T + (HR / (AT*HR+BT))
     LOGDIA(I,2) = LOGDIA(1,2) * DR
     SL(I) = 16.3
     50 continue
  end do
  if (S.eq.0.0) LOGDIA(ILH,2) = MTOPP
  XLOGS = XLOGS + ILH
  go to 130

  !     ****************** TOTAL HEIGHT ******************
  70 B = 1.0 - A
  SL(1) = 16.3
  XL(1) = 16.3
  if (LOGDIA(1,2).gt.MTOPP) go to 80

  !     ***** ONLY ONE LOG IN TREE *****
  LOGDIA(1,2) = MTOPP
  XLOGS = 1.0
  !      IF (IBTR.GT.0) DBHOB=LOGDIA(1,2)/FC16
  go to 130

  !     ***** COMPUTE DIBS OF ALL FULL LOGS *****
  80 H1 = TH - 16.3
  do I = 2,19
     HX = H1 - ((I-1) * 16.3)
     HR = HX / H1
     if (HR.gt.0.0) go to 85
     LOGDIA(I,2) = 1.0
     go to 100
     85 DR = HR / (A*HR+B)
     LOGDIA(I,2) = DR * LOGDIA(1,2)
     SL(I) = 16.3
     XL(I) = 16.3
     if (LOGDIA(I,2).lt.MTOPP) go to 100
     if (LOGDIA(I,2).gt.MTOPP) go to 90
     XLOGS = I-1
     SL(I) = 16.3
     XL(I) = 16.3
     go to 130
     90 continue
  end do

  !     ***** ROUND ANY SHORT LENGTH AT TOP OF TREE *****
  100 DR = MTOPP / LOGDIA(1,2)
  HX = (DR*B*H1) / (1.0 - (A*DR))
  H = (I-2) * 16.3
  S = H1 - HX - H
  if (S.ge.4.0) go to 110
  XLOGS = I-1
  LOGDIA(I,2) = 0.0
  SL(I) = 0.0
  XL(I) = S
  go to 130
  110 if (S.gt.12.0) go to 120
  XLOGS = (I-1) + 0.5
  LOGDIA(I,2) = MTOPP
  SL(I) = 8.0
  XL(I) = S
  go to 130
  120 XLOGS = I
  LOGDIA(I,2) = MTOPP
  SL(I) = 16.3
  XL(I) = S

  !     ***** ROUND DIBS TO NEAREST INCH *****
  130 do I = 1,20
     ID = LOGDIA(I,2) + 0.5
     LOGDIA(I,1) = ID
     140 continue
  end do
  return

  !******************* WESTSIDE ********************

  200 FC32 = AIFC / 100.0
  SL(1) = 16.3
  XL(1) = 16.3

  !     ***** GET TAPER COEFFICIENT *****
  IRET = 2
  go to 1000
  210 if ( TLH .eq.0.0 ) go to 500
  if ( IBTR .gt.0 ) go to 400
  go to 300
  220 if ( IBTR .gt.0 ) go to 600
  go to 500

  !     *********** LOG HEIGHT WITH DBHOB-SIGHTING ***********
  300 LOGS = TLH*10.0 / 5.0

  !     ***** ONLY ONE 16-FT SEGMENT IN TREE *****
  if (LOGS.gt.1) go to 320
  XLOGS = 1.0
  LOGDIA(1,2) = MTOPP
  !      ID = LOGDIA(1,2) + 0.5
  !      LOGDIA(1,1) = ID
  !      RETURN
  go to 370

  !     ***** TWO 16'S *****
  320 if (LOGS.gt.2) go to 330
  XLOGS = 2.0
  LOGDIA(2,2) = MTOPP
  ID = LOGDIA(2,2) + 0.5
  LOGDIA(2,1) = ID
  SL(2) = 16.3
  LOGDIA(1,2) = IFIX( (LOGDIA(2,2)+DBHOB) / 2.0 + 0.5 )
  !      ID = LOGDIA(1,2) + 0.5
  !      LOGDIA(1,1) = ID
  !      RETURN
  go to 370

  !     ***** REDUCE TOPDIB IF CONFLICT WITH LOGDIA(2,2) *****
  330 LOGDIA(2,2) = INT(DBHOB*FC32+0.5 )
  SL(2) = 16.3
  if (LOGDIA(2,2).le.MTOPP) MTOPP = LOGDIA(2,2)-1.0

  !     ***** PROJECT TAPER FROM LOGDIA(2,2) TO LOGDIA(1,2) *****
  T = MTOPP/LOGDIA(2,2)
  AT = A / (1.0 -(A*T))
  BT = (1.0 / (1.0-T)) -AT
  H1 = (LOGS-1) * 16.3
  H2 = H1 - 16.3
  HR = H2/H1
  DR = T + (HR / (AT*HR+BT))
  LOGDIA(1,2) = IFIX( LOGDIA(2,2)/DR+0.5 )

  !     ***** THREE 16'S *****
  if (LOGS.gt.3) go to 340
  XLOGS = 3.0
  LOGDIA(3,2) = MTOPP
  SL(3) = 16.3
  !      DO 331 I=1,3
  !  331  LOGDIA(I,1) = IFIX( LOGDIA(I,2)+0.5 )
  !      RETURN
  go to 370

  !     ***** FOUR OR MORE 16'S *****
  340 H1 = (LOGS-2) * 16.3
  do I = 3,INT(LOGS)
     HX = H1 - (I-2)*16.3
     HR = HX/H1
     DR = T + (HR / (AT*HR+BT))
     LOGDIA(I,2) = LOGDIA(2,2)*DR
     350 SL(I) = 16.3
  end do
  XLOGS = LOGS

  !     ***** ROUND DIBS *****
  370 do I = 1,20
     360 LOGDIA(I,1) = IFIX( LOGDIA(I,2)+0.5 )
  end do
  return

  !     *********** LOG HEIGHT WITH 16-FT SIGHTING *************
  400 LOGS = TLH*10.0 / 5.0

  !     ***** ONE 16-FT SEGMENT IN TREE *****
  if (LOGS.gt.1) go to 410
  XLOGS = 1.0
  LOGDIA(1,2) = MTOPP
  !      LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
  DBHOB = MTOPP+2.0
  !      RETURN
  go to 370

  !     ***** REDUCE TOPDIB IF CONFLICT WITH LOGDIA(1,2) *****
  410 DOB16 = DBHOB
  BTR = IBTR
  LOGDIA(1,2) = IFIX( DOB16*BTR+0.5 )
  if (LOGDIA(1,2).le.MTOPP) MTOPP = LOGDIA(1,2)-1.0

  !     ***** TWO 16'S *****
  if (LOGS.gt.2) go to 420
  XLOGS = 2.0
  LOGDIA(2,2) = MTOPP
  SL(2) = 16.3
  DBHOB = LOGDIA(2,2)/FC32
  if (DBHOB.le.LOGDIA(1,2)) DBHOB = LOGDIA(1,2)+2.0
  !      DO 411 I=1,2
  !  411   LOGDIA(I,1) = IFIX( LOGDIA(I,2)+0.5 )
  !      RETURN
  go to 450

  !     ***** THREE OR MORE 16'S *****
  420 T = MTOPP/LOGDIA(1,2)
  AT = A / (1.0 - (A*T))
  BT = (1.0 / (1.0-T)) -AT
  H1 = (LOGS-1) * 16.3
  do I = 2,INT(LOGS)
     HX = H1 - (I-1)*16.3
     HR = HX/H1
     DR = T + (HR / (AT*HR+BT))
     LOGDIA(I,2) = LOGDIA(1,2)*DR
     430 SL(I) = 16.3
  end do
  XLOGS = LOGS

  !     ***** COMPUTE DBHOB *****
  DBHOB = LOGDIA(2,1) / FC32
  if (DBHOB.le.LOGDIA(1,1)) DBHOB = LOGDIA(1,1)+2.0

  !     ***** ROUND DIBS *****
  450 do I = 1,20
     440 LOGDIA(I,1) = IFIX( LOGDIA(I,2)+0.5 )
  end do
  return

  !     ********* TOTAL HEIGHT WITH DBHOB-SIGHTING *********
  500 B = 1.0 - A

  !     ***** START WITH ROUNDED LOGDIA(2,2) *****
  LOGDIA(2,2) = DBHOB*FC32
  SL(2) = 16.3
  XL(2) = 16.3

  !     ***** REDUCE HEIGHT IF CONFLICT WITH LOGDIA(2,2) *****
  if (LOGDIA(2,2).gt.MTOPP) go to 510
  XLOGS = 2.0
  LOGDIA(2,2) = MTOPP
  !      LOGDIA(2,1) = IFIX( LOGDIA(2,2)+0.5 )
  SL(2) = 16.3
  XL(2) = 16.3

  LOGDIA(1,2) = MTOPP + 1.0
  !      LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
  if (TH.ge.40.0) go to 560

  LOGDIA(2,2) = 0.0
  LOGDIA(2,1) = 0.0
  SL(2) = 0.0
  XL(2) = 0.0
  XLOGS = 1.0
  LOGDIA(1,2) = MTOPP
  !      LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
  !      RETURN
  go to 560

  !     ***** PROJECT TAPER FROM LOGDIA(2,2) TO LOGDIA(1,2) *****
  510 H1 = TH - 16.3
  H2 = H1 - 16.3
  HR = H2/H1
  if (HR.gt.0.0) go to 520
  LOGDIA(2,1) = 0.0
  SL(2) = 0.0
  XL(2) = 0.0
  XLOGS = 1.0
  LOGDIA(1,2) = MTOPP
  !      LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
  !      RETURN
  go to 560

  520 DR = HR / (A*HR+B)
  LOGDIA(1,2) = IFIX( LOGDIA(2,2)/DR + 0.5 )

  !     ***** COMPUTE DIBS ABOVE LOGDIA(2,22 *****
  H1 = TH - 32.6
  do I = 3,19
     HX = H1 - (I-2)*16.3
     HR = HX/H1
     if (HR.gt.0.0) go to 530
     XLOGS = I-1
     go to 560
     530 DR = HR / (A*HR+B)
     LOGDIA(I,2) = DR*LOGDIA(2,2)
     SL(I) = 16.3
     XL(I) = 16.3
     if (LOGDIA(I,2).le.MTOPP) go to 550
     540 continue
  end do

  !     ***** ROUND ANY SHORT LENGTH AT TOP OF TREE *****
  550 XLOGS = I

  LOGDIA(I,2) = MTOPP
  DR = MTOPP/LOGDIA(2,2)
  HX = (DR*B*H1) / (1.0 - (A*DR))
  H = (I-3)*16.3
  S = H1 - HX - H
  XL(I) = S
  if (S.ge.8.0) go to 560
  XLOGS = I-1
  LOGDIA(I,2) = 0.0
  SL(I) = 0.0

  !     ***** ROUND DIBS *****
  560 do I = 1,20
     570 LOGDIA(I,1) = IFIX( LOGDIA(I,2)+0.5 )
  end do
  return

  !     *********** TOTAL HEIGHT WITH 16-FT SIGHTING ***********
  600 B = 1.0 - A

  !     ***** START WITH ROUNDED LOGDIA(1,2) *****
  DOB16 = DBHOB
  BTR = IBTR
  LOGDIA(1,2) = IFIX( DOB16*BTR + 0.5 )

  !     ***** REDUCE HEIGHT IF CONFLICT WITH LOGDIA(1,2) *****
  if (LOGDIA(1,2).gt.MTOPP) go to 610
  XLOGS = 1.0
  LOGDIA(1,2) = MTOPP
  !      LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
  DBHOB = MTOPP+2.0
  !      RETURN
  go to 650

  !     ***** COMPUTE DIBS ABOVE LOGDIA(1,2) *****
  610 H1 = TH - 16.3
  do I = 2,19
     HX = H1 - (I-1)*16.3
     HR = HX/H1
     if (HR.gt.0.0) go to 620
     XLOGS = I-1
     go to 650
     620 DR = HR / (A*HR+B)
     LOGDIA(I,2) = DR*LOGDIA(1,2)
     SL(I) = 16.3
     if (LOGDIA(I,2).le.MTOPP) go to 640
     630 continue
  end do

  !     ***** ROUND ANY SHORT LENGTH AT TOP OF TREE *****
  640 XLOGS = I
  LOGDIA(I,2) = MTOPP
  DR = MTOPP/LOGDIA(1,2)
  HX = (DR*B*H1) / (1.0 - (A*DR))
  H = (I-2)*16.3
  S = H1 - HX - H
  XL(I) = S
  if (S.ge.8.0) go to 650
  XLOGS = I-1
  LOGDIA(I,2) = 0.0
  SL(I) = 0.0

  !     ***** COMPUTE DBHOB *****
  DBHOB = LOGDIA(2,2)/FC32
  if (DBHOB.le.LOGDIA(1,2)) DBHOB = LOGDIA(1,2)+2.0

  !     ***** ROUND DIBS *****
  650 do I = 1,20
     660 LOGDIA(I,1) = IFIX( LOGDIA(I,2)+0.5 )
  end do
  return

  !     ************* COMPUTE TAPER COEFFICIENT *************

  1000 if (A.ge.1.0) go to 1010
  if (A.eq.0.0) A = 0.62
  go to (25,210), IRET

  1010 if (A.ne.1.0) go to 1020
  A = 0.71469 - 0.002536 * DBHOB + 0.000004719 * DBHOB**2
  go to (25,210), IRET

  1020 if (A.ne.2.0) go to 1030
  A = 0.98104 - .0226*DBHOB + .000692*DBHOB**2 - .000007387*DBHOB**3
  go to (25,210), IRET

  1030 A = 0.62
  go to (25,210), IRET

  return
end
  ! USES A HYPERBOLIC CURVE TO DETERMINE DIAM OF ALL LOGS IN A TREE.
  ! IF TREE HEIGHT WAS MEASURED IN FEET THEN ALSO DETERMINE HOW MANY
  ! LOGS ARE IN THE TREE.

  ! ENTER WITH:

  ! IAPZ  = 1  (EASTSIDE) TOTAL # LOGS WILL RETURN AS 16.3-FT SGMTS
  !       = 2  (WESTSIDE)                             32.6-FT
  ! DBHOB   ---  WILL BE XXX.0 FOR NON-3P AND XXX.X FOR 3P CRUISES
  ! IBTR = 0   ASSUMES SIGHTING POINT AT DBHOB
  !      > 0     "        "      "    AT 16 FT & DBHOB IS DOB16
  ! FCLASS   ---  ALWAYS PRESENT
  ! MTOPP    ---  ALWAYS LESS THAN DBHOB
  ! TLH   ---  IF > 0 IT IS TREE HT EXPRESSED AS TOTAL # LOGS TO MTOPP
  !             (EASTSIDE) MIN. TLH = 1.0, (WESTSIDE) MIN. TLH = 0.5
  ! TH    ---  IF > 0 IT IS TREE HT EXPRESSED IN FT FROM STUMP TO TIP
  !             (EITHER TH OR TLH WILL ENTER AS ZERO.)
  ! A     ---  IF = 0 USE STANDARD TAPER COEFFICIENT.
  !            IF > 0 IT IS A FLAG FOR COMPUTING SPECIAL COEFFICIENTS
  ! RETURN WITH:
  ! DBHOB   ---  REPLACED WITH COMPUTED DBHOB IF SIGHTING WAS AT 16 FT.
  ! MTOPP    ---  IF COMPUTED BASAL DIAM FOR CURVE (DBHOB*FC) OR
  !            (DOB16*BTR) IS < MTOPP, MEASUREMENTS ARE ILLOGICAL AND MTOPP
  !            WILL BE REDUCED

  ! XLOGS ---  THE TOTAL # OF 16.3-FT SGMTS IN THE TREE (1.0 - 20.0)
  !             (EASTSIDE) MAY RETURN AS XX.5, (WESTSIDE) AS XX.0

  ! D     ---  THE DIBS OF THE LOG SEGMENTS.  NOTE --
  !            IF HT IS MEASURED AS TOTAL TREE HT (TH BEING USED),
  !            LAST DIB (D) COMPUTED MAY NOT EQUAL MTOPP.  I.E. CANT GET
  !            A FULL # OF LOG SGMTS TO THE TOP THAT WAS REQUESTED.

  ! SL    ---  THE SCALING LENGTH FOR EACH LOG SEGMENT.

  ! XL    ---  THE LENGTH IN FT BETWEEN LAST FULL 16.3-FT SEG & MTOPP.
  !            CORRESPONDING SL OF 0.0, 8.0, OR 16.3 SHOWS WHICH WAY
  !            IT WAS ROUNDED.  XL ONLY APPLIES FOR TOTAL-HEIGHT
  !            MEASUREMENT.
