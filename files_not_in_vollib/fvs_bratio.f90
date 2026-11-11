!================================!
!   VolumeLibrary : fvs_bratio   !
!================================!
  ! This file has BRATIO function for all FVS variants
  ! All functions are renamed as SUNROUTINE, such as AK_BRATIO(IS,D,H,BRATIO)

  ! 01/15/2015 Y. Wang created

  ! AK
subroutine AK_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--AK   DATE OF LAST REVISION:  05/08/12

  !     SPECIES LIST FOR ALASKA VARIANT.

  !     1 = WHITE SPRUCE (WS)           PICEA GLAUCA
  !     2 = WESTERN REDCEDAR (RC)       THUJA PLICATA
  !     3 = PACIFIC SILVER FIR (SF)     ABIES AMABILIS
  !     4 = MOUNTAIN HEMLOCK (MH)       TSUGA MERTENSIANA
  !     5 = WESTERN HEMLOCK (WH)        TSUGA HETEROPHYLLA
  !     6 = ALASKA CEDAR (YC)           CALLITROPSIS NOOTKATENSIS
  !     7 = LODGEPOLE PINE (LP)         PINUS CONTORTA
  !     8 = SITKA SPRUCE (SS)           PICEA SITCHENSIS
  !     9 = SUBALPINE FIR (AF)          ABIES LASIOCARPA
  !    10 = RED ALDER (RA)              ALNUS RUBRA
  !    11 = BLACK COTTONWOOD (CW)       POPULUS TRICHOCARPA
  !    12 = OTHER HARDWOODS (OH)
  !    13 = OTHER SOFTWOODS (OS)

  !  FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER

  ! Variables
  integer :: IS
  real    :: H,D,BRATIO,DIB,DD
  !  RED ALDER AND COTTONWOOD (FROM PN VARIANT)

  if (IS.eq.10.or. IS.eq.11) then
     DIB = 0.075256 + 0.94373*D
     BRATIO = DIB/D
  else

     !  EQUATIONS FOR OTHER SPECIES
     !  COMPUTE DOUBLE BARK THICKNESS AND STORE IN BRATIO

     DD = D
     if (DD .lt.1.)DD = 1.
     BRATIO = 0.186 * (DD ** 0.45417)

     ! SUBTRACT DOUBLE BARK THICKNESS FROM DIAMETER, THEN DIVIDE BY
     ! DIAMETER TO GET BARK RATIO.

     BRATIO = (DD - BRATIO) / DD
  end if

  return
end

  ! BM
subroutine BM_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--BM   DATE OF LAST REVISION:  04/16/09

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BLKDAT.

  !      INCLUDE 'PRGPRM.F77'

  !  SPECIES ORDER:
  !   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
  !  17=OS, 18=OH

  !  SPECIES EXPANSION
  !  WJ USES SO JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
  !  WB USES SO WB (ORIGINALLY FROM TT VARIANT)
  !  LM USES UT LM
  !  PY USES SO PY (ORIGINALLY FROM WC VARIANT)
  !  YC USES WC YC
  !  AS USES SO AS (ORIGINALLY FROM UT VARIANT)
  !  CW USES SO CW (ORIGINALLY FROM WC VARIANT)
  !  OS USES BM PP BARK COEFFICIENT
  !  OH USES SO OH (ORIGINALLY FROM WC VARIANT)

  ! Variables
  real    :: BARK1(MAXSP),BARK2(MAXSP),H,D,BRATIO,TEMD,DIB
  integer :: IS

  data BARK1/&
       &0.859045,  0.859045,  0.903563,  0.904973,  0.903563,&
       &0.,       0.9,       0.9,  0.904973,  0.809427,&
       &0.969,    0.9625,  0.933290,  0.837291,     0.950,&
       &0.075256,  0.809427,    0.9000/

  data BARK2/&
       &1.,        1.,  0.989388,        1.,  0.989388,&
       &0.,        1.,        1.,        1.,  1.016866,&
       &0.,   -0.1141,        1.,        1.,        0.,&
       &0.949670,  1.016866,        1./

  select case (IS)

     !  ORIGINAL BM VARIANT SPECIES
     !     BARK COEFFICIENTS:
     !     202,15,122,116,81 FROM WALTERS ET.AL. RES BULL 50
     !     242,93,108   FROM WYKOFF ET.AL. RES PAPER INT 133

  case(1:5,7:10,17)
     DIB = BARK1(IS)*D**BARK2(IS)
     BRATIO = DIB/D
     if (BRATIO .gt.1.0.or. BRATIO .le.0.0) then
        BRATIO = .999
        go to 10
     end if

     !  PACIFIC YEW (13=PY)
     !  ALASKA CEDAR (14=YC)
     !  OTHER HARDWOODS (18=OH)

  case(13,14,18)
     DIB = BARK1(IS)*D**BARK2(IS)
     BRATIO = DIB/D

     !  WESTERN JUNIPER (6 = WJ)

  case(6)
     TEMD = D
     if (TEMD.lt.1.)TEMD = 1.
     if (TEMD.gt.19.)TEMD = 19.
     BRATIO = 0.9002 - 0.3089*(1/TEMD)

     !  WHITEBARK PINE (11 = WB)
     !  QUAKING ASPEN (15 = AS)

  case(11,15)
     BRATIO = BARK1(IS)

     !  LIMBER PINE (12 = LM)

  case(12)
     TEMD = D
     if (TEMD.lt.1.)TEMD = 1.
     BRATIO = BARK1(IS)+BARK2(IS)*(1.0/TEMD)

     !  BLACK COTTONWOOD (16 = CW)

  case(16)
     DIB = BARK1(IS) + BARK2(IS)*D
     BRATIO = DIB/D
  end select

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  10 continue
  return
end

  ! CA
subroutine CA_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--CA     DATE OF LAST REVISION:   02/22/08

  ! FUNCTION TO COMPUTE BARK RATIOS.  THIS ROUTINE IS VARIANT SPECIFIC
  ! AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS PASSED TO IT.

  !      INCLUDE 'PRGPRM.F77'

  ! Variables
  real    :: BARKB(5,28),H,D,BRATIO,DIB
  integer :: JBARK(MAXSP),IS,J,I

  data JBARK/&
       &12,  5, 12,  2,  2,  2,  1, 13, 13, 10,&
       &27, 10,  3, 10,  4, 25, 25,  3,  3, 26,&
       &12,  9, 12, 10,  3, 19, 18, 17, 20,  8,&
       &16, 24, 21,  6, 16, 14, 23,  7, 16, 16,&
       &16, 28, 16, 14, 16, 16, 16, 22, 16/

  !  202=DF, 015=WF, 122=PP, 116=JP, 081=IC, 312=BM, 431=GC, 815=WO,
  !  093=ES, 108=LP, 098=SS, 242=RC, 263=WH, 351=RA, 001=OTHER SOFTWOODS,
  !  818=BLK OAK, 807=BLUE OAK, 805=CAN LIVE OAK, 801=COAST LIVE OAK,
  !  811=ENGELMANN OAK, 839=INTERIOR LIVE OAK, 981=BAY LAURAL,
  !  361=MADRONE, 821=CALIF WHITE OAK, 631=TANOAK
  !  117=SUGAR PINE, 127=DIGGER PINE, 103=KNOBCONE PINE

  ! BARK COEFFICIENT SOURCES:
  !  202 --------------- FROM WALTERS ET.AL. RES BULL 50  TABLE 2
  !  807,805,801,811,
  !  839,981,821,631,
  !  818,631,361
  !  312,431,815 ------- FROM PILLSBURY AND KIRKLEY RES NOTE PNW 414
  !  93,108,242,263 ---- FROM WYKOFF ET.AL. RES PAPER INT 133  TABLE 7

  !  98 ---------------- FROM HARLOW & HARRAR, TEXTBOOK OF DENDRO PG 129
  !  351 --------------- AVERAGE OF 312,361,431 VALUES FROM PILLSBURY &
  !                      KIRKLEY TABLE 2, PLUS INFO FROM HARLOW & HARRAR
  !  001 --------------- AVERAGE OF 019,081,093,108,119
  !  015,116,117,122 --- FROM DOLPH PSW-368
  !  127,103,101 ------- FROM WYKOFF AVERAGE OF AF,IC,ES,LP,WP
  !  81 ---------------- FROM Dolph PSW-368

  !  NOTE: COEFFICIENTS FOR SPECIES IN PILLSBURY & KIRKLEY ARE METRIC.
  !  INTERCEPT WERE DIVIDED BY 2.54 TO CONVERT THESE
  !  EQUATIONS TO ENGLISH.

  data ((BARKB(I,J),I = 1,5),J = 1,14)/&
       &202.,  0.903563,  0.989388, 0.0    , 1.,&
       &15., -0.1593  ,  0.8911  , 0.0    , 2.,&
       &122., -0.4448  ,  0.8967  , 0.0    , 2.,&
       &116., -0.4448  ,  0.8967  , 0.0    , 2.,&
       &81., -0.0549  ,  0.8374  , 0.0    , 2.,&
       &312.,  0.08360 ,  0.94782 , 0.0    , 2.,&
       &431.,  0.15565 ,  0.90182 , 0.0    , 2.,&
       &815., -0.30722 ,  0.95956 , 0.0    , 2.,&
       &93.,  0.9     ,  0.0     , 0.0    , 3.,&
       &108.,  0.9     ,  0.0     , 0.0    , 3.,&
       &98.,  0.958330,  1.0     , 0.0    , 1.,&
       &242.,  0.949670,  1.0     , 0.0    , 1.,&
       &263.,  0.933710,  1.0     , 0.0    , 1.,&
       &351.,  0.075256,  0.94373 , 0.0    , 2./

  data ((BARKB(I,J),I = 1,5),J = 15,28)/&
       &001.,  0.933290,  1.0     , 0.0    , 1.,&
       &818., -0.26824 ,  0.95767 , 0.0    , 2.,&
       &807., -0.17324 ,  0.94403 , 0.0    , 2.,&
       &805., -0.19128 ,  0.96147 , 0.0    , 2.,&
       &801., -0.75739 ,  0.93475 , 0.0    , 2.,&
       &811., -0.78572 ,  0.92472 , 0.0    , 2.,&
       &839.,  0.04817 ,  0.92953 , 0.0    , 2.,&
       &981., -0.12791 ,  0.96579 , 0.0    , 2.,&
       &361., -0.013484,  0.98155 , 0.0    , 2.,&
       &821., -0.38289 ,  0.93545 , 0.0    , 2.,&
       &117., -0.1429  ,  0.8863  , 0.0    , 2.,&
       &127.,  0.93290 ,  0.0     , 0.0    , 3.,&
       &101.,  0.93290 ,  0.0     , 0.0    , 3.,&
       &631., -0.26824 ,  0.95354 , 0.0    , 2./

  !  EQUATION TYPES:
  !  1  DIB = a * DOB ** b
  !  2  DIB = a + bDOB
  !  3  DIB = a*DOB

  if (BARKB(5,JBARK(IS)) .eq.1.) then
     DIB = BARKB(2,JBARK(IS))*D**BARKB(3,JBARK(IS))
     BRATIO = DIB/D
  else if (BARKB(5,JBARK(IS)) .eq.2.) then
     DIB = BARKB(2,JBARK(IS)) + BARKB(3,JBARK(IS))*D
     BRATIO = DIB/D
  else if (BARKB(5,JBARK(IS)) .eq.3.) then
     BRATIO = BARKB(2,JBARK(IS))
  end if

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end

  ! CI
subroutine CI_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--CI  DATE OF LAST REVISION:  06/06/11

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BASE MODEL.

  !      INCLUDE 'PRGPRM.F77'

  !     SPECIES LIST FOR CENTRAL IDAHO VARIANT.

  !     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
  !     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
  !     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
  !     4 = GRAND FIR (GF)                   ABIES GRANDIS
  !     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
  !     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
  !     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
  !     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
  !     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
  !    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
  !    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
  !    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
  !    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
  !    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
  !    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
  !    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
  !    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
  !    18 = OTHER SOFTWOODS (OS)
  !    19 = OTHER HARDWOODS (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !  FROM THE IE VARIANT:
  !      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
  !      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
  !      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
  !      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)

  !  FROM THE UT VARIANT:
  !      USE 12(WJ) FOR 14(WJ)
  !      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
  !                                                  REALLY WC39=OT)

  ! Variables
  real    :: BARK1(MAXSP),BARK2(MAXSP),H,D,BRATIO,DIB,TEMD
  integer :: IS

  !  DATA STATEMENTS

  data BARK1/&
       &.859045, .900000, .903563, .904973, .903563,&
       &.837291, .900000, .900000, .903563, .809427,&
       &0.969,   0.969,   0.950,     0.0,     0.9,&
       &0.969,   0.892, .900000,   0.892/
  data BARK2/&
       &0.0,     0.0, .989388,     0.0, .989388,&
       &0.0,     0.0,     0.0, .989388,1.016866,&
       &0.0,     0.0,     0.0,     0.0,     0.0,&
       &0.0,  -0.086,     0.0,  -0.086/

  select case (IS)

  case(3,5,9,10)
     DIB = BARK1(IS)*D**BARK2(IS)
     BRATIO = DIB/D
     if (BRATIO .gt.0.97) BRATIO = 0.97

  case(14)
     TEMD = D
     if (TEMD.lt.1.)TEMD = 1.
     if (TEMD.gt.19.)TEMD = 19.
     BRATIO = 0.9002 - 0.3089*(1/TEMD)
     if (BRATIO .gt.0.99) BRATIO = 0.99
     if (BRATIO .lt.0.80) BRATIO = 0.80

  case(17,19)
     TEMD = D
     if (TEMD.lt.1.)TEMD = 1.
     BRATIO = BARK1(IS)+BARK2(IS)*(1.0/TEMD)
     if (BRATIO .gt.0.99) BRATIO = 0.99
     if (BRATIO .lt.0.80) BRATIO = 0.80

  case default
     BRATIO = BARK1(IS)

  end select

  return
end

  ! CS

  ! CR
subroutine CR_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--CR   DATE OF LAST REVISION:  06/16/09

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BASE MODEL. COEFFICIENTS FOR BARK1 AND BARK2
  ! ARE SET IN BLKDAT AND SITSET.  IF BARK1 AND BARK2 ARE BOTH ZERO THEN
  ! ITS THE BLACK HILL MODEL TYPE AND USE CORMIER'S FMSC MODEL.

  !      INCLUDE 'PRGPRM.F77'

  !      INCLUDE 'GGCOM.F77'

  !  SPECIES ORDER:
  !   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
  !  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
  !  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
  !  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH

  !  SPECIES EXPANSION:
  !  UJ,AJ,RM,OJ,ER USE CR JU
  !  NC,PW USE CR CO
  !  GO,AW,EM,BK,SO USE CR OA
  !  PB USES CR AS
  !  PM,PD,AZ USE CR PI
  !  CI USES CR PP

  ! Variables
  integer :: IMAP(MAXSP),IEQN,IS
  real    :: H,D,BRATIO,TEMD
  data IMAP/ 2, 2, 2, 2, 2, 2, 2, 3, 3, 3,&
       &3, 1, 1, 3, 2, 1, 3, 3, 3, 2,&
       &3, 3, 3, 3, 3, 3, 3, 2, 1, 1,&
       &1, 1, 1, 1, 1, 1, 1, 3/

  !  PI, PP, UJ, AJ, RM, OJ, ER AND OS USE PP BARK EQUATION.
  !  PP EQN IS DIFFERENT FOR BHPP, S-F, AND LP MODEL TYPES.
  !  BARKi() COEFFICIENTS SET TO ZERO IN SITSET TO TRIGGER
  !  DIFFERENT EQUATION IN THESE MODEL TYPES.
  !  PP EQN FOR BLACK HILLS WAS FIT BY CORMIER, FMSC
  !  PP FOR MODEL TYPES 1 & 2 USES EQUATION FROM CA VARIANT
  !  AF, CB, GF, WF USE THE CONSTANT RATIO FROM NI VARIANT

  IEQN = IMAP(IS)
  TEMD = D
  if (TEMD.lt.1.)TEMD = 1.

  if (IEQN .eq.1) then
     if (BARK1(IS).eq.0.0.and. BARK2(IS).eq.0.0) then
        if (TEMD.gt.19.)TEMD = 19.
        BRATIO = 0.9002 - 0.3089*(1/TEMD)
     else
        BRATIO = BARK1(IS)+BARK2(IS)*(1/TEMD)
     end if

  else if (IEQN .eq.2) then
     BRATIO = BARK1(IS)

  else if (IEQN .eq.3) then
     BRATIO = BARK1(IS)+BARK2(IS)*(1.0/TEMD)
  end if

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end

  ! EC
subroutine EC_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--EC   DATE OF LAST REVISION:  05/09/12

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BLKDAT.

  !      INCLUDE 'PRGPRM.F77'

  !  SPECIES LIST FOR EAST CASCADES VARIANT.

  !   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
  !   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
  !   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
  !   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
  !   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
  !   6 = GRAND FIR               (GF)    ABIES GRANDIS
  !   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
  !   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
  !   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
  !  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
  !  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
  !  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
  !  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
  !  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
  !  15 = NOBLE FIR               (NF)    ABIES PROCERA
  !  16 = WHITE FIR               (WF)    ABIES CONCOLOR
  !  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
  !  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
  !  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
  !  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
  !  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
  !  22 = RED ALDER               (RA)    ALNUS RUBRA
  !  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
  !  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA
  !  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
  !  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
  !  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var. TRICHOCARPA
  !  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
  !  29 = CHERRY AND PLUM SPECIES (PL)    PRUNUS sp.
  !  30 = WILLOW SPECIES          (WI)    SALIX sp.
  !  31 = OTHER SOFTWOODS         (OS)
  !  32 = OTHER HARDWOODS         (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !  FROM THE EC VARIANT:
  !      USE 6(GF) FOR 16(WF)
  !      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)

  !  FROM THE WC VARIANT:
  !      USE 19(WH) FOR 11(WH)
  !      USE 33(PY) FOR 13(PY)
  !      USE 31(WB) FOR 14(WB)
  !      USE  7(NF) FOR 15(NF)
  !      USE 30(LL) FOR 17(LL)
  !      USE  8(YC) FOR 18(YC)
  !      USE 29(WJ) FOR 19(WJ)
  !      USE 21(BM) FOR 20(BM) AND 21(VN)
  !      USE 22(RA) FOR 22(RA)
  !      USE 24(PB) FOR 23(PB)
  !      USE 25(GC) FOR 24(GC)
  !      USE 34(DG) FOR 25(DG)
  !      USE 26(AS) FOR 26(AS) AND 32(OH)
  !      USE 27(CW) FOR 27(CW)
  !      USE 28(WO) FOR 28(WO)
  !      USE 36(CH) FOR 29(PL)
  !      USE 37(WI) FOR 30(WI)

  ! Variables
  real    :: BARK1(MAXSP),BARK2(MAXSP),H,D,BRATIO,TEMD,DIB
  integer :: IS

  data BARK1/&
       &0.964,     0.851,     0.844,     0.903,     0.950,&
       &0.903,     0.963,     0.956,     0.903,     0.889,&
       &0.93371,     0.934,   0.93329,   0.93329,  0.904973,&
       &0.903,       0.9,  0.837291,   0.94967,    0.0836,&
       &0.0836,  0.075256,    0.0836,   0.15565,  0.075256,&
       &0.075256,  0.075256,    0.8558,  0.075256,  0.075256,&
       &0.934,  0.075256/

  data BARK2/&
       &1.,        1.,        1.,        1.,        1.,&
       &1.,        1.,        1.,        1.,        1.,&
       &1.,        1.,        1.,        1.,        1.,&
       &1.,        1.,        1.,        1.,   0.94782,&
       &0.94782,   0.94967,   0.94782,   0.90182,   0.94967,&
       &0.94967,   0.94967,    1.0213,   0.94967,   0.94967,&
       &1.,   0.94967/

  select case (IS)

     !  ORIGINAL EC VARIANT SPECIES (WP,WL,DF,SF,RC,GF,LP,ES,AF,PP,OS)
     !  THOSE SPECIES USING EC COEFFICIENTS (WF,VN)

  case(1:10,12,16,31)
     BRATIO = BARK1(IS)

     !  THOSE SPECIES USING WC COEFFICIENTS
     !  WH, MH, PY, WB, NF, LL, YC, WJ

  case(11,13:15,17:19)
     BRATIO = BARK1(IS)

     !  THOSE SPECIES USING WC COEFFICIENTS; DIB = a + b*DOB
     !  BM, VN, RA, PB, GC, DG, AS, CW, PL, WI, OH

  case(20:27,29:30,32)
     DIB = BARK1(IS) + BARK2(IS)*D
     BRATIO = DIB/D
     if (BRATIO .gt.0.99) BRATIO = 0.99
     if (BRATIO .lt.0.80) BRATIO = 0.80

     !  THOSE SPECIES USING WC COEFFICIENTS; DIB = a * DOB ** b
     !  WO

  case(28)
     DIB = BARK1(IS)*D**BARK2(IS)
     BRATIO = DIB/D
     if (BRATIO .gt.0.99) BRATIO = 0.99
     if (BRATIO .lt.0.80) BRATIO = 0.80
  end select

  return
end

  ! EM
subroutine EM_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--EM   DATE OF LAST REVISION:  03/26/09

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BLKDAT.

  !      INCLUDE 'PRGPRM.F77'

  !  SPECIES ORDER:
  !   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
  !  17=PB, 18=OS, 19=OH

  !  OS USES EM WB BARK COEFFICIENT

  !  SPECIES EXPANSION
  !  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
  !  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
  !  RM USES IE JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
  !  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
  !  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)

  ! Variables
  real    :: BARK1(MAXSP),BARK2(MAXSP),H,D,BRATIO,TEMD
  integer :: IMAP(MAXSP),IS,IEQN

  data BARK1/0.934, 0.934, 0.867, 0.969, 0.937, 0.000, 0.969,&
       &0.956, 0.937, 0.890, 0.892, 0.950, 0.892, 0.892, 0.892,&
       &0.892, 0.950, 0.934, 0.892/
  data BARK2/0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       &0.000, 0.000, 0.000, -.086, 0.000, -.086, -.086, -.086,&
       &-.086, 0.000, 0.000, -.086/
  data IMAP/     2,     2,     2,     2,     2,     1,     2,&
       &2,     2,     2,     3,     2,     3,     3,     3,&
       &3,     2,     2,     3/

  IEQN = IMAP(IS)
  TEMD = D
  if (TEMD.lt.1.)TEMD = 1.

  if (IEQN .eq.1) then
     if (BARK1(IS).eq.0.0.and. BARK2(IS).eq.0.0) then
        if (TEMD.gt.19.)TEMD = 19.
        BRATIO = 0.9002 - 0.3089*(1/TEMD)
     else
        BRATIO = BARK1(IS)+BARK2(IS)*(1/TEMD)
     end if

  else if (IEQN .eq.2) then
     BRATIO = BARK1(IS)

  else if (IEQN .eq.3) then
     BRATIO = BARK1(IS)+BARK2(IS)*(1.0/TEMD)
  end if

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end

  ! IE
subroutine IE_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--NI23   DATE OF LAST REVISION:  05/20/11

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN NI VARIANT.

  !      INCLUDE 'PRGPRM.F77'

  !  SPECIES ORDER:
  !   1=WP,  2=WL,  3=DF,  4=GF,  5=WH,  6=RC,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=MH, 12=WB, 13=LM, 14=LL, 15=PI, 16=JU,
  !  17=PY, 18=AS, 19=CO, 20=MM, 21=PB, 22=OH, 23=OS

  ! Variables
  real    :: BARK1(MAXSP),BARK2(MAXSP),H,D,BRATIO,TEMD
  integer :: IMAP(MAXSP),IS,IEQN

  data BARK1/0.964, 0.851, 0.867, 0.915, 0.934, 0.950, 0.969,&
       &0.956, 0.937, 0.890, 0.934, 0.851, 0.969, 0.937, 0.000,&
       &0.000, 0.969, 0.950, 0.892, 0.950, 0.950, 0.892, 0.934/
  data BARK2/0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       &0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       &0.000, 0.000, 0.000, -.086, 0.000, 0.000, -.086, 0.000/
  data IMAP/     2,     2,     2,     2,     2,     2,     2,&
       &2,     2,     2,     2,     2,     2,     2,     1,&
       &1,     2,     2,     3,     2,     2,     3,     2/

  !  PI, JU USE PP BARK EQUATION FROM CR VARIANT.
  !  WB USE COEFFICIENTS FOR WL
  !  LM AND PY USE COEFFICIENTS FROM TT FOR LM
  !  LL USE COEFFICIENTS FOR AF
  !  AS, MM, PB USE COEFFIECIENTS FOR AS FROM UT
  !  CO, OH USE COEFFIECIENTS FOR OH FROM CR
  !  OS USE COEFFEICIENTS FOR MH

  IEQN = IMAP(IS)
  TEMD = D
  if (TEMD.lt.1.)TEMD = 1.

  if (IEQN .eq.1) then
     if (BARK1(IS).eq.0.0.and. BARK2(IS).eq.0.0) then
        if (TEMD.gt.19.)TEMD = 19.
        BRATIO = 0.9002 - 0.3089*(1/TEMD)
     else
        BRATIO = BARK1(IS)+BARK2(IS)*(1/TEMD)
     end if

  else if (IEQN .eq.2) then
     BRATIO = BARK1(IS)

  else if (IEQN .eq.3) then
     BRATIO = BARK1(IS)+BARK2(IS)*(1.0/TEMD)
  end if

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end

  ! KT

  ! LS
subroutine LS_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--LS DATE OF LAST REVISION:  07/11/08

  ! FUNCTION TO COMPUTE BARK RATIOS. THIS ROUTINE IS VARIANT SPECIFIC
  ! AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS PASSED TO IT.

  !      INCLUDE 'PRGPRM.F77'

  !      INCLUDE 'COEFFS.F77'

  ! Variables
  integer :: IS
  real    :: H,D,BRATIO

  data BKRAT/5*.91,2*.95,3*.94,.95,3*.91,2*.94,.93,3*.94,3*.93,&
       &.94,.92,3*.94,.92,4*.91,3*.92,3*.94,3*.92,6*.94,&
       &20*.92/

  if (IS .eq.0) then
     BRATIO = 0.93
  else
     BRATIO = BKRAT(IS)
  end if
  return
end

  ! NC
subroutine NC_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--NC  DATE OF LAST REVISION:  02/17/09

  !  FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  !  REPLACES ARRAY BKRAT IN BASE MODEL.

  !  SPECIES LIST FOR KLAMATH MOUNTAINS VARIANT.
  !  1=OC,2=SP,3=DF,4=WF,5=M,6=IC,7=BO,8=TO,9=RF,10=PP,11=OH

  ! Variables
  real    :: BRKRAT(4,11),H,D,BRATIO,DBT,DIB
  integer :: IS,J,I

  data ((BRKRAT(I,J),I = 1,4),J = 1,11)/&
       &1.,  0.1429,  0.1137,    1.0,&
       &2.,  0.1429,  0.1137,    1.0,&
       &3.,  0.1045,  0.1661,    1.0,&
       &4.,  0.1593,  0.1089,    1.0,&
       &5., -0.01348, 0.98155,   2.0,&
       &6., -0.0549,  0.1626,    1.0,&
       &7., -0.26824, 0.95767,   2.0,&
       &8., -0.26824, 0.95354,   2.0,&
       &9.,  0.1593,  0.1089,    1.0,&
       &10., 0.4448,  0.1033,    1.0,&
       &11.,-0.26824, 0.95767,   2.0/

  ! Equation Types:
  ! 1 DBT= a+b*DOB
  ! 2 DIB= a+b*DOB

  if (BRKRAT(4,IS) .eq.1.) then
     DBT = BRKRAT(2,IS) + BRKRAT(3,IS)*D
     BRATIO = (D - DBT) / D
  else if (BRKRAT(4,IS) .eq.2.) then
     DIB = BRKRAT(2,IS) + BRKRAT(3,IS)*D
     BRATIO = DIB/D
  end if

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end

  ! NE

  ! NI
subroutine NI_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--NI DATE OF LAST REVISION:  04/09/08

  ! FUNCTION TO COMPUTE BARK RATIOS. THIS ROUTINE IS VARIANT SPECIFIC
  ! AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS PASSED TO IT.

  !      INCLUDE 'PRGPRM.F77'

  !      INCLUDE 'COEFFS.F77'

  ! Variables
  integer :: IS
  real    :: H,D,BRATIO
  data  BKRAT/0.964,0.851,0.867,0.915,0.934,0.950,0.969,0.956,&
       &0.937,0.890,0.934/

  BRATIO = BKRAT(IS)
  return
end

  ! PN
subroutine PN_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--PN    DATE OF LAST REVISION:  04/22/10

  ! FUNCTION TO COMPUTE BARK RATIOS.  THIS ROUTINE IS VARIANT SPECIFIC
  ! AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS PASSED TO IT.

  !  SPECIES LIST CODES: SEQUENCE NO.-SPECIES ALPHA CODE-FIA NO.

  !   1-SF-011, 2-WF-015, 3-GF-017  4-AF-019, 5-RF-020, 6-SS-098,
  !   7-NF-022, 8-YC-042, 9-IC-081,10-ES-093,11-LP-108,12-JP-116,
  !  13-SP-117,14-WP-119,15-PP-122,16-DF-202,17-RW-211,18-RC-242,
  !  19-WH-263,20-MH-264,21-BM-312,22-RA-351,23-WA-352,24-PB-376,
  !  25-GC-431,26-AS-746,27-CW-747,28-WO-815,29-J0-060,30-LL-072,
  !  31-WB-101,32-KP-103,33-PY-231,34-DG-492,35-HT-500,36-CH-764,
  !  37-WI-920,   ---   ,39-OT-999

  ! Variables
  real    :: BARKB(4,15),H,D,BRATIO,DIB
  integer :: JBARK(39),IS

  data JBARK/&
       &2,  2,  2,  2,  2, 11,  2,  5,  5,  9,&
       &10,  4,  4,  4,  3,  1,  5, 12, 13, 12,&
       &6, 14, 14,  6,  7, 14, 14,  8, 12, 10,&
       &15, 15, 15, 14, 14, 14, 14, 10, 10/

  !  MASTER SPECIES
  !  202=DF, 015=WF, 122=PP, 116=JP, 081=IC, 312=BM, 431=GC, 815=WO,
  !  093=ES, 108=LP, 098=SS, 242=RC, 263=WH, 351=RA, 001=OTHER SOFTWOODS

  !  NOTE: COEFFICIENTS FOR SPECIES 312 & 431 IN PILLSBURY &
  !  KIRKLEY ARE METRIC. INTERCEPT IS DIVIDED BY 2.54 TO CONVERT THESE
  !  EQUATIONS TO ENGLISH.

  data BARKB/&
       &202.,  0.903563,  0.989388, 1.,&
       &15.,  0.904973,  1.0     , 1.,&
       &122.,  0.809427,  1.016866, 1.,&
       &116.,  0.859045,  1.0     , 1.,&
       &81.,  0.837291,  1.0     , 1.,&
       &312.,  0.08360 ,  0.94782 , 2.,&
       &431.,  0.15565 ,  0.90182 , 2.,&
       &815.,  0.8558  ,  1.0213 ,  1.,&
       &93.,   .9     ,  0.0     , 3.,&
       &108.,   .9     ,  0.0     , 3.,&
       &98.,  0.958330,  1.0     , 1.,&
       &242.,  0.949670,  1.0     , 1.,&
       &263.,  0.933710,  1.0     , 1.,&
       &351.,  0.075256,  0.94373 , 2.,&
       &001.,  0.933290,  1.0     , 1./

  ! BARK COEFS
  !  202,15,122,116,81   FROM WALTERS ET.AL. RES BULL 50  TABLE 2
  !  312,431             FROM PILLSBURY AND KIRKLEY RES NOTE PNW 414
  !  93,108,242,263      FROM WYKOFF ET.AL. RES PAPER INT 133  TABLE 7
  !  98                  FROM HARLOW & HARRAR, TEXTBOOK OF DENDRO PG 129
  !  351                 AVERAGE OF 312,361,431 VALUES FROM PILLSBURY &
  !                      KIRKLEY TABLE 2, PLUS INFO FROM HARLOW & HARRAR
  !  001                 AVERAGE OF 019,081,093,108,119
  !  815                 FROM GOULD AND HARRINGTON

  !  EQUATION TYPES
  !  1  DIB = a * DOB ** b
  !  2  DIB = a + bDOB
  !  3  DIB = a*DOB

  if (BARKB(4,JBARK(IS)) .eq.1.) then
     DIB = BARKB(2,JBARK(IS))*D**BARKB(3,JBARK(IS))
     BRATIO = DIB/D
  else if (BARKB(4,JBARK(IS)) .eq.2.) then
     DIB = BARKB(2,JBARK(IS)) + BARKB(3,JBARK(IS))*D
     BRATIO = DIB/D
  else if (BARKB(4,JBARK(IS)) .eq.3.) then
     BRATIO = BARKB(2,JBARK(IS))
  else
     BRATIO = 0.9
  end if

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end

  ! SN
subroutine SN_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--SN    DATE OF LAST REVISION:  06/05/08

  !  FUNCTION TO COMPUTE BARK RATIOS.  THIS ROUTINE IS VARIANT SPECIFIC
  !  AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS PASSED TO IT.
  !  COFFICIENTS ARE FROM CLARK(RPSE, 1991:TABLE9, PAGE54) FOR THE LINEAR
  !  EQUATION DIB= A + B*DBH

  !  SPECIES LIST FOR SOUTHERN VARIANT.
  !  1=FR,2=JU,3=PI,4=PU,5=SP,6=SA,7=SR,8=LL,9=TM,10=PP,11=PD,12=WP,
  !  13=LP,14=VP,15=BY,16=PC,17=HM,18=FM,19=BE,20=RM,21=SV,22=SM,
  !  23=BU,24=BB,25=SB,26=AH,27=HI,28=CA,29=HB,30=RD,31=DW,32=PS,
  !  33=AB,34=AS,35=WA,36=BA,37=GA,38=HL,39=LB,40=HA,41=HY,42=BN,
  !  43=WN,44=SU,45=YP,46=MG,47=CT,48=MS,49=MV,50=ML,51=AP,52=MB,
  !  53=WT,54=BG,55=TS,56=HH,57=SD,58=RA,59=SY,60=CW,61=BT,62=BC,
  !  63=WO,64=SO,65=SK,66=CB,67=TO,68=LK,69=OV,70=BJ,71=SN,72=CK
  !  73=WK,74=CO,75=RO,76=QS,77=PO,78=BO,79=LO,80=BK,81=WI,82=SS,
  !  83=BW,84=EL,85=WE,86=AE,87=RL,88=OS,89=OH,90=OT

  !      VARIABLES
  !      BARKC(I,J) - I1= COEFF A, I2= COEF B,
  !                   ONE RECORD (J) FOR EACH SPECIES (IS)
  !      D          - DBH
  !      DIB        - DIAM. INSIDE BARK

  !      INCLUDE 'PRGPRM.F77'

  !      INCLUDE 'PLOT.F77'

  ! Variables
  integer :: IS,J,I
  real    :: H,D,BRATIO,DIB,BARKC(2,90)

  data ((BARKC(I,J),I = 1,2),J = 1,15)/&
       &0.05119,0.89372,&
       &-0.27012,0.97546,&
       &-0.17289,0.91572,&
       &-0.39956,0.95183,&
       &-0.44121,0.93045,&
       &-0.55073,0.91887,&
       &-0.13301,0.93755,&
       &-0.45903,0.92746,&
       &0.05119,0.89372,&
       &-0.58808,0.91852,&
       &-0.51271,0.90245,&
       &-0.31608,0.92054,&
       &-0.48140,0.91413,&
       &-0.31137,0.95011,&
       &-0.27012,0.97546/
  data ((BARKC(I,J),I = 1,2),J = 16,30)/&
       &-0.94204,0.96735,&
       &-0.04931,0.92272,&
       &-0.09800,0.94646,&
       &-0.09800,0.94646,&
       &-0.09800,0.94646,&
       &-0.09800,0.94646,&
       &-0.09800,0.94646,&
       &-0.35332,0.95955,&
       &0.21790,0.92290,&
       &0.21790,0.92290,&
       &-0.13040,0.97071,&
       &-0.60912,0.94347,&
       &-0.33014,0.94215,&
       &-0.18338,0.95768,&
       &-0.33014,0.94215/
  data ((BARKC(I,J),I = 1,2),J = 31,45)/&
       &-0.33014,0.94215,&
       &-0.42001,0.94264,&
       &-0.13040,0.97071,&
       &-0.34316,0.93964,&
       &-0.48735,0.93847,&
       &-0.25063,0.94349,&
       &-0.34316,0.93964,&
       &-0.42001,0.94264,&
       &-0.33014,0.94215,&
       &-0.33014,0.94215,&
       &-0.33014,0.94215,&
       &-0.42001,0.94264,&
       &-0.42001,0.94264,&
       &-0.39271,0.95997,&
       &-0.22976,0.92408/
  data ((BARKC(I,J),I = 1,2),J = 46,60)/&
       &-0.21140,0.94461,&
       &-0.21140,0.94461,&
       &-0.21140,0.94461,&
       &-0.17978,0.92381,&
       &-0.21140,0.94461,&
       &-0.33014,0.94215,&
       &-0.33014,0.94215,&
       &-0.38140,0.97327,&
       &0.19899,0.88941,&
       &-0.15231,0.93442,&
       &-0.42001,0.94264,&
       &-0.25063,0.94349,&
       &-0.33014,0.94215,&
       &-0.09192,0.96411,&
       &-0.25063,0.94349/
  data ((BARKC(I,J),I = 1,2),J = 61,75)/&
       &-0.25063,0.94349,&
       &-0.12958,0.94152,&
       &-0.24096,0.93789,&
       &-0.40860,0.94613,&
       &-0.42141,0.93008,&
       &-0.21801,0.93540,&
       &-0.61021,0.95803,&
       &-0.04612,0.93127,&
       &-0.37973,0.94380,&
       &-0.61021,0.95803,&
       &-0.49699,0.94832,&
       &-0.34225,0.93494,&
       &-0.30330,0.95826,&
       &-0.43197,0.92120,&
       &-0.52266,0.95215/
  data ((BARKC(I,J),I = 1,2),J = 76,90)/&
       &-0.61021,0.95803,&
       &-0.26493,0.91899,&
       &-0.70754,0.94821,&
       &-0.70754,0.94821,&
       &-0.37166,0.89193,&
       &-0.25063,0.94349,&
       &-0.25063,0.94349,&
       &-0.35979,0.95322,&
       &-0.42027,0.96305,&
       &-0.42027,0.96305,&
       &-0.42027,0.96305,&
       &-0.42027,0.96305,&
       &-0.38344,0.91915,&
       &-0.33014,0.94215,&
       &-0.25063,0.94349/

  ! MODEL TYPE FROM CLARK

  if (IFOR .eq.20) then
     if (IS .eq.5) then
        DIB = 0.1713 + 0.87459*D
     else if (IS .eq.6) then
        DIB = -0.26207 + 0.87347 * D
     else if (IS .eq.8) then
        DIB = -0.43439 + 0.91382 * D
     else if (IS .eq.11) then
        DIB = -0.62033 + 0.91645 * D
     else if (IS .eq.13) then
        DIB = -0.4671 + 0.90198 * D
     else
        DIB = BARKC(1,IS) + BARKC(2,IS)*D
     end if
  else
     DIB = BARKC(1,IS) + BARKC(2,IS)*D
  end if
  BRATIO = DIB/D

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end

  ! SO
subroutine SO_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--SO  DATE OF LAST REVISION:  05/06/09

  !  FUNCTION TO COMPUTE BARK RATIOS, OR CONSTANTS.  THIS ROUTINE IS
  !  VARIANT SPECIFIC AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS
  !  PASSED TO IT.

  !  SPECIES LIST FOR SO VARIANT
  !  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
  ! 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
  ! 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
  ! 31=MB, 32=OS, 33=OH

  !      INCLUDE 'PRGPRM.F77'

  ! Variables
  real    :: BARKB(3,MAXSP),BRDAT(MAXSP),H,D,BRATIO,DIB,TEMD
  integer :: IS

  data BRDAT /&
       &.964, .851, .867, .915, .934, .950, .969, .956, .000, .890,&
       &.000, .915, .937, .903, .000, .969, .851, .950, .000, .000,&
       &.000, .000, .000, .950, .000, .000, .000, .000, .000, .000,&
       &.000, .867, .000/

  data BARKB/&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &-0.1593,    0.8911,        2.,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.904973,  1.000000,  1.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.933710,  1.000000,  1.000000,&
       &0.933290,  1.000000,  1.000000,&
       &0.075256,  0.949670,  2.000000,&
       &0.075256,  0.949670,  2.000000,&
       &0.083600,  0.947820,  2.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.075256,  0.949670,  2.000000,&
       &0.075256,  0.949670,  2.000000,&
       &-0.30722,   0.95956,        2.,&
       &0.075256,  0.949670,  2.000000,&
       &0.075256,  0.949670,  2.000000,&
       &0.900000,  1.000000,  1.000000,&
       &0.900000,  1.000000,  1.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.900000,  1.000000,  1.000000/

  !  SPECIES FORM WC VARAINT
  !  NOTE: COEFFICIENTS FOR SPECIES 312, 431, & 815 IN PILLSBURY &
  !  KIRKLEY ARE METRIC. INTERCEPT IS DIVIDED BY 2.54 TO CONVERT THESE
  !  EQUATIONS TO ENGLISH.
  !  BARK COEFS
  !  202,15,122,116,81 FROM WALTERS ET.AL. RES BULL 50
  !  312,431,815  FROM PILLSBURY AND KIRKLEY RES NOTE PNW 414
  !  242,93,108   FROM WYKOFF ET.AL. RES PAPER INT 133

  !  EQUATION TYPES
  !  1  DIB = a * DOB ** b
  !  2  DIB = a + bDOB
  !  3  DIB = a*DOB = a * DOB ** b, (eQ.1), WITH b= 1.0
  !  MODEL TYPE 1

  select case (IS)

     !  SPECIES FROM WC,CA VARIANT

  case(9,15,19:23,25,26,27,28:31,33)

     if (BARKB(3,IS) .eq.1.) then
        DIB = BARKB(1,IS)*D**BARKB(2,IS)
        BRATIO = DIB/D
     else if (BARKB(3,IS) .eq.2.) then
        DIB = BARKB(1,IS) + BARKB(2,IS)*D
        BRATIO = DIB/D
     end if

     !  FROM UT(12) JU

  case(11)
     TEMD = D
     if (TEMD.lt.1.)TEMD = 1.
     if (TEMD.gt.19.)TEMD = 19.
     BRATIO = 0.9002 - 0.3089*(1/TEMD)

     !  BARK RATIO IS CONSTANT

  case default
     BRATIO = BRDAT(IS)
  end select

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end

  ! TT
subroutine TT_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--TT   DATE OF LAST REVISION:  04/09/10

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BASE MODEL.

  ! SPECIES ORDER FOR TETONS VARIANT:

  !  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH

  ! VARIANT EXPANSION:
  ! BS USES ES EQUATIONS FROM TT
  ! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
  ! PP USES PP EQUATIONS FROM CI
  ! UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
  ! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
  ! MM USES MM EQUATIONS FROM IE
  ! NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
  ! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
  ! OS USES OT (OTHER SP.) EQUATIONS FROM TT

  ! Variables
  real    :: BARK1(18),BARK2(18),H,D,BRATIO,TEMD,DIB
  integer :: IMAP(18),IS,IEQN

  data BARK1/  0.969,   0.969,    0.867,       0.,    0.956,&
       &0.969,   0.969,    0.956,    0.937, 0.809427,&
       &0.,      0.,  0.94782,    0.950,    0.892,&
       &0.9,   0.969,    0.892/

  data BARK2/     0.,      0.,       0.,       0.,       0.,&
       &0.,      0.,       0.,       0., 1.016866,&
       &0.,      0.,   0.0836,       0.,   -0.086,&
       &0.,      0.,   -0.086/

  data IMAP/ 2, 2, 2, 1, 2, 2, 2, 2, 2, 4,&
       &1, 1, 3, 2, 3, 2, 2, 3/

  IEQN = IMAP(IS)
  TEMD = D
  if (TEMD.lt.1.)TEMD = 1.

  if (IEQN .eq.1) then
     if (BARK1(IS).eq.0.0.and. BARK2(IS).eq.0.0) then
        if (TEMD.gt.19.)TEMD = 19.
        BRATIO = 0.9002 - 0.3089*(1/TEMD)
     else
        BRATIO = BARK1(IS)+BARK2(IS)*D
        BRATIO = 1.0/BRATIO
     end if

  else if (IEQN .eq.2) then
     BRATIO = BARK1(IS)

  else if (IEQN .eq.3) then
     BRATIO = BARK1(IS)+BARK2(IS)*(1.0/TEMD)
  else if (IEQN .eq.4) then
     DIB = BARK1(IS)*D**BARK2(IS)
     BRATIO = DIB/D
     if (BRATIO .gt.0.97) BRATIO = 0.97
     go to 100
  end if

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  100 continue
  return
end

  ! UT
subroutine UT_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--UT   DATE OF LAST REVISION:  11/04/09

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BASE MODEL.

  !  SPECIES ORDER:
  !  1=WB,  2=LM,  3=DF,  4=WF,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=PI, 12=WJ, 13=GO, 14=PM, 15=RM, 16=UJ, 17=GB, 18=NC, 19=FC, 20=MC,
  ! 21=BI, 22=BE, 23=OS, 24=OH

  ! VARIANT EXPANSION:
  ! GO AND OH USE OA (OAK SP.) EQUATIONS FROM UT
  ! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
  ! RM AND UJ USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
  ! GB USES BC (BRISTLECONE PINE) EQUATIONS FROM CR
  ! NC, FC, AND BE USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
  ! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
  ! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
  ! OS USES OT (OTHER SP.) EQUATIONS FROM UT

  ! Variables
  real    :: BARK1(24),BARK2(24),H,D,BRATIO,TEMD
  integer :: IMAP(24),IS,IEQN

  data BARK1/ 0.9625,  0.9625,    0.867,    0.890,  0.9502,&
       &0.950,  0.9625,   0.9502,    0.890,  0.8967,&
       &0.,      0.,  0.93789,       0.,      0.,&
       &0.,  0.9625,    0.892,    0.892,     0.9,&
       &0.94782,   0.892,   0.9625,  0.93789/

  data BARK2/ -.1141,  -.1141,       0.,       0., -0.2528,&
       &0.,  -.1141,   -.2528,       0.,  -.4448,&
       &0.,      0., -0.24096,       0.,      0.,&
       &0., -0.1141,   -0.086,   -0.086,      0.,&
       &0.0836,  -0.086,   -.1141, -0.24096/

  data IMAP/ 3, 3, 2, 2, 3, 2, 3, 3, 2, 3,&
       &1, 1, 3, 1, 1, 1, 3, 3, 3, 2,&
       &3, 3, 3, 3/

  !  PI, JU, BS, AND OA USE EQUATIONS FROM CR MODEL TYPE 3-5
  !  WB, LM, LP, OT USE EQUATIONS FROM CR VARIANT
  !  ES USES THE EQUATION FROM THE CR VARIANT
  !  PP USES THE EQUATION FROM THE CA VARIANT
  !  WF AND AF USE THE CONSTANT RATIO FROM NI VARIANT

  IEQN = IMAP(IS)
  TEMD = D
  if (TEMD.lt.1.)TEMD = 1.

  if (IEQN .eq.1) then
     if (BARK1(IS).eq.0.0.and. BARK2(IS).eq.0.0) then
        if (TEMD.gt.19.)TEMD = 19.
        BRATIO = 0.9002 - 0.3089*(1/TEMD)
     else
        BRATIO = BARK1(IS)+BARK2(IS)*D
        BRATIO = 1.0/BRATIO
     end if

  else if (IEQN .eq.2) then
     BRATIO = BARK1(IS)

  else if (IEQN .eq.3) then
     BRATIO = BARK1(IS)+BARK2(IS)*(1.0/TEMD)
  end if

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end

  ! WC
subroutine WC_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--WC    DATE OF LAST REVISION:  04/22/10

  !  FUNCTION TO COMPUTE BARK RATIOS.  THIS ROUTINE IS VARIANT SPECIFIC
  !  AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS PASSED TO IT.

  !  SPECIES LIST FOR WEST CASCADES VARIANT
  !  1=SF,2=WF,3=GF,4=AF,5=RF,6=---,7=NF,8=YC,9=IC,10=ES,11=LP,12=JP,
  !  13=SP,14=WP,15=PP,16=DF,17=RW,18=RC,19=WH,20=MH,21=BM,22=RA,23=WA,
  !  24=PB,25=GC,26=AS,27=CW,28=WO,29=J,30=LL,31=WB,32=KP,33=PY,34=DG
  !  35=HT,36=CH,37=WI,38=---,39=OT

  ! Variables
  real    :: BARKB(4,13),H,D,BRATIO,DIB
  integer :: JBARK(39),IS
  data JBARK/&
       &2,  2,  2,  2,  2,  2,  2,  5,  5,  10,&
       &10,  4,  4,  4,  3,  1,  5, 11, 12,  11,&
       &6,  9,  9,  6,  7,  9,  9,  8, 11,  10,&
       &13, 13, 13,  9,  9,  9,  9,  1, 10/

  !  NOTE: COEFFICIENTS FOR SPECIES 312 & 431 IN PILLSBURY &
  !  KIRKLEY ARE METRIC. INTERCEPT IS DIVIDED BY 2.54 TO CONVERT THESE
  !  EQUATIONS TO ENGLISH.

  data BARKB/&
       &202.,  0.903563,  0.989388, 1.,&
       &15.,  0.904973,  1.0     , 1.,&
       &122.,  0.809427,  1.016866, 1.,&
       &116.,  0.859045,  1.0     , 1.,&
       &81.,  0.837291,  1.0     , 1.,&
       &312.,  0.08360 ,  0.94782 , 2.,&
       &431.,  0.15565 ,  0.90182 , 2.,&
       &815.,  0.8558  ,  1.0213 ,  1.,&
       &351.,  0.075256,  0.949670, 2.,&
       &108.,  0.9     ,  1.0     , 1.,&
       &242.,  0.949670,  1.0     , 1.,&
       &263.,  0.933710,  1.0     , 1.,&
       &101.,  0.933290,  1.0     , 1./

  !  BARK COEFS
  !  202,15,122,116,81 FROM WALTERS ET.AL. RES BULL 50
  !  312,431      FROM PILLSBURY AND KIRKLEY RES NOTE PNW 414
  !  242,93,108   FROM WYKOFF ET.AL. RES PAPER INT 133
  !  815          FROM GOULD AND HARRINGTON

  !  EQUATION TYPES
  !  1  DIB = a * DOB ** b
  !  2  DIB = a + bDOB
  !  3  DIB = a*DOB = a * DOB ** b, (eQ.1), WITH b= 1.0
  !  MODEL TYPE 1

  if (BARKB(4,JBARK(IS)) .eq.1.) then
     DIB = BARKB(2,JBARK(IS))*D**BARKB(3,JBARK(IS))
     BRATIO = DIB/D
  else if (BARKB(4,JBARK(IS)) .eq.2.) then
     DIB = BARKB(2,JBARK(IS)) + BARKB(3,JBARK(IS))*D
     BRATIO = DIB/D
  else
     BRATIO = 0.9
  end if

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end

  ! WS
subroutine WS_BRATIO(IS,D,H,BRATIO)
  !      FUNCTION BRATIO(IS,D,H)
  implicit none

  !  **BRATIO--WS   DATE OF LAST REVISION:  05/09/12

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BASE MODEL.

  !     SPECIES LIST FOR WESTERN SIERRAS VARIANT.

  !     1 = SUGAR PINE (SP)                   PINUS LAMBERTIANA
  !     2 = DOUGLAS-FIR (DF)                  PSEUDOTSUGA MENZIESII
  !     3 = WHITE FIR (WF)                    ABIES CONCOLOR
  !     4 = GIANT SEQUOIA (GS)                SEQUOIADENDRON GIGANTEAUM
  !     5 = INCENSE CEDAR (IC)                LIBOCEDRUS DECURRENS
  !     6 = JEFFREY PINE (JP)                 PINUS JEFFREYI
  !     7 = CALIFORNIA RED FIR (RF)           ABIES MAGNIFICA
  !     8 = PONDEROSA PINE (PP)               PINUS PONDEROSA
  !     9 = LODGEPOLE PINE (LP)               PINUS CONTORTA
  !    10 = WHITEBARK PINE (WB)               PINUS ALBICAULIS
  !    11 = WESTERN WHITE PINE (WP)           PINUS MONTICOLA
  !    12 = SINGLELEAF PINYON (PM)            PINUS MONOPHYLLA
  !    13 = PACIFIC SILVER FIR (SF)           ABIES AMABILIS
  !    14 = KNOBCONE PINE (KP)                PINUS ATTENUATA
  !    15 = FOXTAIL PINE (FP)                 PINUS BALFOURIANA
  !    16 = COULTER PINE (CP)                 PINUS COULTERI
  !    17 = LIMBER PINE (LM)                  PINUS FLEXILIS
  !    18 = MONTEREY PINE (MP)                PINUS RADIATA
  !    19 = GRAY PINE (GP)                    PINUS SABINIANA
  !         (OR CALIFORNIA FOOTHILL PINE)
  !    20 = WASHOE PINE (WE)                  PINUS WASHOENSIS
  !    21 = GREAT BASIN BRISTLECONE PINE (GB) PINUS LONGAEVA
  !    22 = BIGCONE DOUGLAS-FIR (BD)          PSEUDOTSUGA MACROCARPA
  !    23 = REDWOOD (RW)                      SEQUOIA SEMPERVIRENS
  !    24 = MOUNTAIN HEMLOCK (MH)             TSUGA MERTENSIANA
  !    25 = WESTERN JUNIPER (WJ)              JUNIPERUS OCIDENTALIS
  !    26 = UTAH JUNIPER (UJ)                 JUNIPERUS OSTEOSPERMA
  !    27 = CALIFORNIA JUNIPER (CJ)           JUNIPERUS CALIFORNICA
  !    28 = CALIFORNIA LIVE OAK (LO)          QUERCUS AGRIFOLIA
  !    29 = CANYON LIVE OAK (CY)              QUERCUS CHRYSOLEPSIS
  !    30 = BLUE OAK (BL)                     QUERCUS DOUGLASII
  !    31 = CALIFORNIA BLACK OAK (BO)         QUERQUS KELLOGGII
  !    32 = VALLEY OAK (VO)                   QUERCUS LOBATA
  !         (OR CALIFORNIA WHITE OAK)
  !    33 = INTERIOR LIVE OAK (IO)            QUERCUS WISLIZENI
  !    34 = TANOAK (TO)                       LITHOCARPUS DENSIFLORUS
  !    35 = GIANT CHINQUAPIN (GC)             CHRYSOLEPIS CHRYSOPHYLLA
  !    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
  !    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
  !    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
  !    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
  !    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
  !    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
  !    42 = OTHER SOFTWOODS (OS)
  !    43 = OTHER HARDWOODS (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !    FROM EXISTING WS EQUATIONS --
  !      USE 1(SP) FOR 11(WP) AND 24(MH)
  !      USE 2(DF) FOR 22(BD)
  !      USE 3(WF) FOR 13(SF)
  !      USE 4(GS) FOR 23(RW)
  !      USE 8(PP) FOR 18(MP)
  !      USE 34(TO) FOR 35(GC), 36(AS), 37(CL), 38(MA), AND 39(DG)
  !      USE 31(BO) FOR 28(LO), 29(CY), 30(BL), 32(VO), 33(IO), 40(BM), AND
  !                     43(OH)

  !    FROM CA VARIANT --
  !      USE CA11(KP) FOR 12(PM), 14(KP), 15(FP), 16(CP), 17(LM), 19(GP), 20(WE),
  !                       25(WJ), 26(WJ), AND 27(CJ)
  !      USE CA12(LP) FOR 9(LP) AND 10(WB)

  !    FROM SO VARIANT --
  !      USE SO30(MC) FOR 41(MC)

  !    FROM UT VARIANT --
  !      USE UT17(GB) FOR 21(GB)

  ! Variables
  real    :: BARK1(43),BARK2(43),H,D,BRATIO,TEMD
  integer :: IMAP(43),IS,IEQN

  data BARK1/&
       &0.8863, 0.8839, 0.8911, 0.8863, 0.8374,&
       &0.8967, 0.8911, 0.8967, 0.9000, 0.9000,&
       &0.8863, 0.9329, 0.8911, 0.9329, 0.9329,&
       &0.9329, 0.9329, 0.8967, 0.9329, 0.9329,&
       &0.9625, 0.8839, 0.8863, 0.8863, 0.9329,&
       &0.9329, 0.9329, 0.8374, 0.8374, 0.8374,&
       &0.8374, 0.8374, 0.8374, 0.8374, 0.8374,&
       &0.8374, 0.8374, 0.8374, 0.8374, 0.8374,&
       &0.9000, 0.8967, 0.8374/

  data BARK2/&
       &0.,     0.,     0.,     0.,     0.,&
       &0.,     0.,     0.,     0.,     0.,&
       &0.,     0.,     0.,     0.,     0.,&
       &0.,     0.,     0.,     0.,     0.,&
       &-0.1141,     0.,     0.,     0.,     0.,&
       &0.,     0.,     0.,     0.,     0.,&
       &0.,     0.,     0.,     0.,     0.,&
       &0.,     0.,     0.,     0.,     0.,&
       &0.,     0.,     0./

  data IMAP/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,&
       &1, 1, 1, 1, 1, 1, 1, 1, 1, 1,&
       &2, 1, 1, 1, 1, 1, 1, 1, 1, 1,&
       &1, 1, 1, 1, 1, 1, 1, 1, 1, 1,&
       &1, 1, 1/

  !  PI, JU, BS, AND OA USE EQUATIONS FROM CR MODEL TYPE 3-5
  !  WB, LM, LP, OT USE EQUATIONS FROM CR VARIANT
  !  ES USES THE EQUATION FROM THE CR VARIANT
  !  PP USES THE EQUATION FROM THE CA VARIANT
  !  WF AND AF USE THE CONSTANT RATIO FROM NI VARIANT

  IEQN = IMAP(IS)
  TEMD = D
  if (TEMD.lt.1.)TEMD = 1.

  if (IEQN .eq.1) then
     BRATIO = BARK1(IS)

  else if (IEQN .eq.2) then
     BRATIO = BARK1(IS)+BARK2(IS)*(1.0/TEMD)
     if (BRATIO .gt.0.99) BRATIO = 0.99
     if (BRATIO .lt.0.80) BRATIO = 0.80
  end if

  return
end
