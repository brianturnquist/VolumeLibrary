!=============================!
!   VolumeLibrary : jenkins   !
!=============================!
subroutine JENKINS(SPEC, DBHOB, BIOMS)
  !     This subroutine calculate biomass using DBH (inches) and return dry biomass in lb.
  !     08/15/2016 YW For woodland species, dismater is usually measured as DRC, Jenkins converted DRC to DBH
  !     for the generilized equation.
  ! Common blocks
  include 'wdbkwtdata.inc' !'WDBKWTDATA.INC'
  ! Variables
  integer :: SPEC,SPCD,SPGRPCD,FIRST,LAST,HALF,DONE
  integer :: I0,J0
  real    :: DBHOB,B0,B1,DBHIN,DBHCM
  real    :: BIOMS(8),COEF(10,3)
  real    :: FOL,ROOT,WOOD,BARK,STMTOT,ABT,BRANCHES,CROWN,STUMP
  real    :: A0F,A1F,A0R,A1R,A0W,A1W,A0B,A1B
  real    :: FOLRATIO,ROTRATIO,WDRATIO,BKRATIO,KG2LB
  real    :: STUMPVIB,STUMPVOB,STUMPHT,WDEN,BDEN

  !     COEF IS THE COEFFICIENT FOR ABOVE GROUND TOTAL BIOMASS FOR 10 SPECIES GROUP
  !     SPECIES CLASS SPECIES GROUP                 GROUP CODE  B0         B1

  !     SOFTWOOD      CEDAR/LARCH                   1           -2.0336    2.2592
  !                   DOUGLAS-FIR                   2           -2.2304    2.4435
  !                   TRUE FIR/HEMLOCK              3           -2.5384    2.4814
  !                   PINE                          4           -2.5356    2.4349
  !                   SPRUCE                        5           -2.0773    2.3323
  !     HARDWOOD      ASPEN/ALDER/COTTONWOOD/WILLOW 6           -2.2094    2.3867
  !                   SOFT MAPLE/BIRCH              7           -1.9123    2.3651
  !                   MIXED HARDWOOD                8           -2.4800    2.4835
  !                   HARD MAPLE/OAK/HICKORY/BEECH  9           -2.0127    2.4342
  !     WOODLAND      JUNIPER/OAK/MESQUITE          10          -0.7152    1.7029

  data ((COEF(I0,J0),J0 = 1,3),I0 = 1,10) /&
       &1,           -2.0336,    2.2592,&
       &2,           -2.2304,    2.4435,&
       &3,           -2.5384,    2.4814,&
       &4,           -2.5356,    2.4349,&
       &5,           -2.0773,    2.3323,&
       &6,           -2.2094,    2.3867,&
       &7,           -1.9123,    2.3651,&
       &8,           -2.4800,    2.4835,&
       &9,           -2.0127,    2.4342,&
       &10,          -0.7152,    1.7029/

  !    SOFTCOEF AND HARDCOEF ARE COEF FOR COMPONENT RATIO
  !    SPECIES CLASS COMPONENT    COMPONENT CODE A0       A1

  !    HARDWOOD      FOLIAGE      1              -4.0813  5.8816
  !                  COARSE ROOTS 2              -1.6911  0.8160
  !                  STEM BARK    3              -2.0129  -1.6805
  !                  STEM WOOD    4              -0.3065  -5.4240
  !    SOFTWOOD      FOLIAGE      1              -2.9584  4.4766
  !                  COARSE ROOTS 2              -1.5619  0.6614
  !                  STEM BARK    3              -2.0980  -1.1432
  !                  STEM WOOD    4              -0.3737  -1.8055

  SPCD = SPEC
  KG2LB = 2.20462
  STUMP = 0
  if (SPCD.lt.10) then
     !     The SPCD is jenkins group code
     SPCLS = 1
     if (SPCD.lt.6) SPCLS = 0
     if (SPCD.eq.0) SPCD = 10
     SPGRPCD = SPCD
  else
     !     The SPCD is the species FIA code
     FIRST = 1
     !LAST = 455
     LAST = TOTSPC
     DONE = 0
     !     FIND THE SPECIES GROUP CODE FROM THE ARRAY
     do 5, while (DONE.eq.0)
     HALF = (LAST - FIRST +1)/2 + FIRST
     if (WDBKWT(HALF,1) .eq. SPCD) then
        DONE = HALF
     else if (FIRST .eq. LAST) then
        DONE = -1
     else if (WDBKWT(HALF,1) .lt. SPCD) then
        FIRST = HALF
     else
        LAST = HALF - 1
     end if
     5 continue
     !IF(DONE.LT.0) DONE = TOTSPC  !455
     if (DONE.lt.0) DONE = CNT999
     SPCLS = WDBKWT(DONE,2)
     SPGRPCD = WDBKWT(DONE,3)
  end if

  DBHIN = DBHOB

  !    GET VOLUME FOR 1 FOOT STUMP USING RAILE
  STUMPHT = 1.0
  if (DBHIN.ge.5.0) then
     call RAILEVOL(SPCD, DBHIN, STUMPHT, STUMPVIB, STUMPVOB)
     call WOODDEN(SPCD, WDEN, BDEN)
     STUMP = STUMPVIB*WDEN + (STUMPVOB-STUMPVIB)*BDEN
     !       The STUMP calculated in above is in lb. Convert it to KG to calculate with Jenkins
     STUMP = STUMP/KG2LB
  end if

  do I = 1, 10
     if (COEF(I,1).eq.SPGRPCD) then
        B0 = COEF(I,2)
        B1 = COEF(I,3)
        exit
     end if
  end do
  !     CALCULATE BIOMASS FOR ABOVE GROUND TOTAL
  !     First convert DBH from inch to cm because Jenkins equation using cn for DBH
  DBHCM = 2.54*DBHIN
  !     For woodland species, convert DRC(DBHOB) to real DBH
  !     The DRC to DBH conversion from Chojnacky etal 2014.
  !      IF(SPGRPCD.EQ.10) DBHCM = EXP(-0.35031+1.03991*LOG(DBHCM))
  !     The conversion from DRC to DBH should be done before call Jenkins

  ABT = EXP(B0 + B1 * LOG(DBHCM))
  !     CALCULATE COMPONENT RATIO
  if (SPCLS.eq.0) then
     A0F = -2.9584
     A1F = 4.4766
     A0R = -1.5619
     A1R = 0.6614
     A0B = -2.0980
     A1B = -1.1432
     A0W = -0.3737
     A1W = -1.8055
  else if (SPCLS.eq.1) then
     A0F = -4.0813
     A1F = 5.8816
     A0R = -1.6911
     A1R = 0.8160
     A0B = -2.0129
     A1B = -1.6805
     A0W = -0.3065
     A1W = -5.4240
  end if
  FOLRATIO = EXP(A0F + A1F/DBHCM)
  ROTRATIO = EXP(A0R + A1R/DBHCM)
  BKRATIO = EXP(A0B + A1B/DBHCM)
  WDRATIO = EXP(A0W + A1W/DBHCM)
  FOL = ABT*FOLRATIO
  ROOT = ABT*ROTRATIO
  if (DBHIN.lt.5) then
     BARK = 0
     WOOD = 0
     STMTOT = 0
     BRANCHES = ABT - FOL
     CROWN = ABT
  else
     BARK = ABT*BKRATIO
     WOOD = ABT*WDRATIO
     STMTOT = WOOD + BARK
     BRANCHES = ABT - STMTOT - FOL - STUMP
     CROWN = ABT - STMTOT - STUMP
  end if
  !     ADD BIOMASS COMPONENT VALUE TO RETURN VARIABLE BIOMS(8)
  !     1 ABOVE GROUND TOTAL
  !     2 STEM WOOD
  !     3 STEM BARK
  !     4 FOLIAGE
  !     5 ROOTS
  !     6 BRANCHES
  !     7 CROWN
  !     8 STEM WOOD AND BARK
  !     Biomass calculated from Jenkins in Unit kg, need to convert to lb
  BIOMS(1) = ABT*KG2LB
  BIOMS(2) = WOOD*KG2LB
  BIOMS(3) = BARK*KG2LB
  BIOMS(4) = FOL*KG2LB
  BIOMS(5) = ROOT*KG2LB
  BIOMS(6) = BRANCHES*KG2LB
  BIOMS(7) = CROWN*KG2LB
  BIOMS(8) = STMTOT*KG2LB

end
