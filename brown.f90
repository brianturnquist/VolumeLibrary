!===========================!
!   VolumeLibrary : brown   !
!===========================!
subroutine BROWN(BIOEQ, DBHOB, HTTOT, CR, BIOMAS, ERRFLG)
  implicit none
  ! Variables
  real    :: DBHOB,HTTOT,CR,CFWT(7),BIOMAS
  character(len=12) :: BIOEQ
  integer :: SPEC,ERRFLG,SPLIST(24),LAST,DONE,I
  character(len=3) :: CMP
  !  YW 2019/05/01 changed the biomass component code to B0Q and BQ1
  ! Valid species code for Brown's fraction biomasss
  ! 011,015,017,019,020,021,022,041,042,073,081,093,098
  ! 101,108,116,117,119,121,122,202,242,260,263
  ! List of Brown fraction equations:
  ! BRN***FOT00D, BRN***B0Q00D,BRN***BQ100D,BRN***B1300D,BRN***B3P00D,
  ! BRN***LCR00D,BRN***DCR00D,
  data (SPLIST(I), I = 1,24)/&
       &011,015,017,019,020,021,022,041,042,073,081,093,&
       &098,101,108,116,117,119,121,122,202,242,260,263/
  CMP = BIOEQ(7:9)
  read (BIOEQ(4:6),'(I3)') SPEC
  !     First check the species for in the SPLIST
  LAST = 24
  DONE = 0
  call SEARCH(LAST,SPLIST,SPEC,DONE,ERRFLG)
  if (DONE.le.0) then
     ERRFLG = 15
     return
  end if
  call BROWNCROWNFRACTION(SPEC, DBHOB, HTTOT,CR, CFWT)
  if (CMP.eq.'FOT') then
     !     1 - NEEDLES
     BIOMAS = CFWT(1)
  else if (CMP.eq.'B0Q'.or.CMP.eq.'B04') then
     !     2 - 0 TO 1/4"
     BIOMAS = CFWT(2)
  else if (CMP.eq.'BQ1'.or.CMP.eq.'B41') then
     !     3 - 1/4 TO 1"
     BIOMAS = CFWT(3)
  else if (CMP.eq.'B13') then
     !     4 - 1 TO 3"
     BIOMAS = CFWT(4)
  else if (CMP.eq.'B3P') then
     !     5 - 3" AND UP
     BIOMAS = CFWT(5)
  else if (CMP.eq.'LCR') then
     !     6 - Live Crown
     BIOMAS = CFWT(6)
  else if (CMP.eq.'DCR') then
     !     7 - Dead crown
     BIOMAS = CFWT(7)
  else
     ERRFLG = 15
     return
  end if

  return
end

  !     THIS SUBROUTINE CALCULATE CROWN BIOMASS (DRY WEIGHT) FRACTIONS BASED ON BROWN 1976
  !     THE RESULT FOR SIZE CATEGORY IN CWF AS:
  !     1 - NEEDLES
  !     2 - 0 TO 1/4"
  !     3 - 1/4 TO 1"
  !     4 - 1 TO 3"
  !     5 - 3" AND UP
  !     6 - Live Crown
  !     7 - Dead crown
subroutine BROWNCROWNFRACTION(SPCD, DBH, THT,CR, CFWT)
  ! Expose subroutine BrownCrownFraction to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::BROWNCROWNFRACTION
  implicit none
  ! Variables
  real    :: DBH,THT,CFWT(7),CR
  integer :: SPCD,PROP
  real    :: LCW,DCW,LP1,LP2,LP3,LP4,DP1,DP2
  real    :: DST1,DST2,DST3,SW
  real    :: CONST2,CONST3,CONST4
  real    :: LIVEPROP,DEADPROP
  ! test biomass equation 2019/05/08
  !      CHARACTER*12 BMEQ
  !      REAL tmpD,tmpH,tmpCR
  !      BMEQ = 'AFF019BRL01D'
  !      tmpD = 15.0
  !      tmpH = 65.0
  !      tmpCR = 30.0/65.0
  !      CALL CALCFROMBIOEQDB(BMEQ,tmpD,tmpH,tmpCR,DCW)
  ! end test biomass equation 2019/05/08
  LCW = 0.0
  DCW = 0.0
  !     ASSOCIATE SPECIES CODE WITH BROWN SPECIES CODE
  if (SPCD.eq.15.or.SPCD.eq.11.or.SPCD.eq.21.or.SPCD.eq.22) then
     SPCD = 17
  else if (SPCD.eq.20) then
     SPCD = 19
  else if (SPCD.eq.116.or.SPCD.eq.117.or.SPCD.eq.121) then
     SPCD = 108
  else if (SPCD.eq.98) then
     SPCD = 93
  else if (SPCD.eq.260) then
     SPCD = 263
  else if (SPCD.eq.41.or.SPCD.eq.42.or.SPCD.eq.81) then
     SPCD = 242
  end if
  call LIVECROWNWEIGHT(SPCD, DBH, THT, CR, LCW)

  !     SMALL TREES AND BIG TREES ARE CALCULATED DIFFERENT FOR PROPORTION
  if (DBH.le.6) then
     call SMALLTREESTEMWEIGHT(SPCD, DBH, THT, SW)
     DST1 = LIVEPROP(SPCD, DBH, 1)
     DST2 = LIVEPROP(SPCD, DBH, 2)
     DST3 = LIVEPROP(SPCD, DBH, 3)
     !       NEEDLES
     CFWT(1) = LCW*DST1
     !       0 TO 1/4"
     CFWT(2) = LCW*(DST2-DST1)
     !       1/4 TO 1"
     if (DBH.gt.1.and.DBH.le.2) then
        CONST2 = 0.02
     else
        CONST2 = 1.0
     end if
     CFWT(3) = LCW*(DST3-DST2) + SW*CONST2
     !       1 TO 3"
     if (DBH.le.2) then
        CONST3 = 0.96
     else if (DBH.le.3) then
        CONST3 = 0.5
     else if (DBH.le.4) then
        CONST3 = 0.1
     else
        CONST3 = 0
     end if
     CFWT(4) = LCW*(1-DST3) + CONST3*SW
     !       3" AND UP
     if (DBH.le.3) then
        CONST4 = 0.5
     else if (DBH.le.4) then
        CONST4 = 0.9
     else
        CONST4 = 1.0
     end if
     CFWT(5) = SW*CONST4
     if (DBH.lt.3) CFWT(5) = 0
  else
     call DEADCROWNWEIGHT(SPCD, DBH, THT,CR, DCW)
     LP1 = LIVEPROP(SPCD, DBH, 1)
     LP2 = LIVEPROP(SPCD, DBH, 2)
     LP3 = LIVEPROP(SPCD, DBH, 3)
     LP4 = LIVEPROP(SPCD, DBH, 4)
     DP1 = DEADPROP(SPCD, DBH, 1)
     DP2 = DEADPROP(SPCD, DBH, 2)
     !NEEDLES
     CFWT(1) = LP1*LCW
     ! 0 TO 1/4"
     CFWT(2) = (LP2-LP1)*LCW +DP1*DCW
     ! 1/4 TO 1"
     CFWT(3) = (LP3-LP2)*LCW + (DP2-DP1)*DCW
     ! 1 TO 3" (ONLY CALCULATE FOR PP, DF, WP, GF, WC AND LP
     if (SPCD.eq.122.or.SPCD.eq.202) then
        CFWT(4) = (LP4-LP3)*LCW + (1-DP2)*DCW
     else if (SPCD.eq.17.or. SPCD.eq.119&
          &.or. SPCD.eq.108.or. SPCD.eq.242) then
        CFWT(4) = (1-LP3)*LCW + (1-DP2)*DCW
     else
        CFWT(4) = 0
     end if
     ! 3" AND UP (ONLY FOR PP AND DF)
     if (SPCD.eq.122.or.SPCD.eq.202) then
        CFWT(5) = (1-LP4)*LCW
     else
        CFWT(5) = 0
     end if
  end if
  CFWT(6) = LCW
  CFWT(7) = DCW
end

  ! Brown topwood dry weight = gross CUFT sedondary volume times breakage ratio times wood density
subroutine BROWNTOPWOOD(SPN, GCUFTS, WT)
  ! Expose subroutine BrownTopwood to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::BROWNTOPWOOD
  implicit none
  ! Variables
  real    :: GCUFTS,WT,WOODDRYWEIGHT
  integer :: SPN
  !      !test===============================================================
  !      CHARACTER*10 VOLEQ,FIAVTYPE,VOLEQ2,GEOSUB,FIAEQ
  !      REAL DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,VOL(15),BFMIND,STUMP
  !      REAL DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,FIAVOL
  !      REAL CENTROID_HT,CENTROID_DIA
  !      INTEGER BA,SI,ERRFLG
  !      VOLEQ = 'CU108102'
  !      SPN = 108
  !      DBHOB = 15
  !      HTTOT = 76
  !      HT1PRD = 0.0
  !      HT2PRD = 0.0
  !      MTOPP = 6.0
  !      STUMP = 1.0
  !      DRCOB = 0.0
  !      UPSHT1 = 0.0
  !      UPSD1 = 0.0
  !      UPSHT2 = 0.0
  !      UPSD2 = 0.0
  !      VOL = 0.0
  !      BA = 85
  !      SI = 65
  !      GEOSUB = '0'
  !      ERRFLG = 0
  !      FIAVOL = 0.0
  !      BFMIND = 9
  !      CALL FIA_VOLINIT(VOLEQ,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,
  !     & STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLG,
  !     & FIAVTYPE,FIAVOL,BFMIND)
  !      !end test==========================================================
  WT = WOODDRYWEIGHT(SPN, GCUFTS, 1)
end

  ! BROWN CULLLOG WEIGHT = GROSS LOG VOLUME TIMES BARK WEIGHT/STEM WEIGHT RATIO TIMES WOOD DENSITY
subroutine BROWNCULLLOG(SPN, GCUFTS, WT)
  ! Expose subroutine BrownCullLog to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::BROWNCULLLOG
  implicit none
  ! Variables
  real    :: GCUFTS,WT,WOODDRYWEIGHT
  integer :: SPN
  WT = WOODDRYWEIGHT(SPN, GCUFTS, 0)
end

  ! BROWN CULL CHUNK WEIGHT = GROSS CUFT MINUS NET CUFT VOLUME TIMES FRACTION LEFT IN THE WOOD
subroutine BROWNCULLCHUNK(SPN, GCUFT, NCUFT, FLIW, WT)
  ! Expose subroutine BrownCullChunk to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::BROWNCULLCHUNK
  implicit none
  ! Variables
  real    :: GCUFT,NCUFT,FLIW,WT,WOODDRYWEIGHT,VOL
  integer :: SPN
  VOL = (GCUFT - NCUFT)*FLIW
  WT = WOODDRYWEIGHT(SPN, VOL, 0);
end

subroutine LIVECROWNWEIGHT(SPCD, DBH, THT, CR, LCW)
  !     THIS SUBROUTINE CALCULATE LIVE CROWN DRY WEIGHT
  implicit none
  ! Variables
  real    :: DBH,THT,LCW,CR,DF34PLUSLCW
  integer :: SPCD
  character(len=12) :: BMEQ
  DF34PLUSLCW(DBH) = EXP(0.0168 + 1.949*LOG(DBH))
  LCW = -1
  if (DBH.gt.50) DBH = 50
  if (DBH.lt.1) DBH = 1
  if (SPCD.eq.17) then
     !       eqn 305
     BMEQ = 'BRN017LCR02D'
     if (DBH.gt.40) LCW = 1.03*DF34PLUSLCW(DBH)
  else if (SPCD.eq.19) then
     !       eqn 312
     BMEQ = 'BRN019LCR01D'
  else if (SPCD.eq.108) then
     !       eqn 323
     BMEQ = 'BRN108LCR01D'
  else if (SPCD.eq.73) then
     !       eqn 315
     BMEQ = 'BRN073LCR01D'
     if (DBH.gt.34) LCW = 0.58*DF34PLUSLCW(DBH)
  else if (SPCD.eq.93) then
     !       eqn 317
     BMEQ = 'BRN093LCR01D'
     if (DBH.gt.28) LCW = 1.24*DF34PLUSLCW(DBH)
  else if (SPCD.eq.202) then
     if (DBH.le.16) then
        !       eqn 337
        BMEQ = 'BRN202LCR01D'
     else if (DBH.le.32) then
        !       eqn 339
        BMEQ = 'BRN202LCR03D'
     else
        LCW = DF34PLUSLCW(DBH)
     end if
  else if (SPCD.eq.263) then
     if (DBH.le.32) then
        !       eqn 350
        BMEQ = 'BRN263LCR01D'
     else
        !       eqn 4146
        BMEQ = 'BRN263LCR04D'
     end if
  else if (SPCD.eq.119) then
     !       eqn 324
     BMEQ = 'BRN119LCR01D'
     if (DBH.gt.42) LCW = 0.45*DF34PLUSLCW(DBH)
  else if (SPCD.eq.242) then
     !       eqn 345
     BMEQ = 'BRN242LCR01D'
     if (DBH.gt.36) LCW = 0.77*DF34PLUSLCW(DBH)
  else if (SPCD.eq.122) then
     !       eqn 330
     BMEQ = 'BRN122LCR01D'
     if (DBH.gt.34) LCW = 2.0*DF34PLUSLCW(DBH)
  else if (SPCD.eq.101) then
     if (DBH.le.6) then
        !       eqn 324
        BMEQ = 'BRN119LCR01D'
     else
        !       eqn 323
        BMEQ = 'BRN108LCR01D'
     end if
  end if
  if (LCW.lt.0) then
     LCW = 0
     call CALCFROMBIOEQDB(BMEQ, DBH, THT, CR,LCW)
  end if
end

subroutine DEADCROWNWEIGHT(SPCD, DBH, THT, CR, DCW)
  implicit none
  !     THIS SUBROUTINE CALCULATE LIVE CROWN DRY WEIGHT
  ! Variables
  real    :: DBH,THT,DCW,P3,LCW,CR
  integer :: SPCD
  character(len=12) :: BMEQ
  P3 = 0
  DCW = -1
  if (DBH.le.6) return
  if (DBH.gt.50) DBH = 50
  if (SPCD.eq.17) then
     if (DBH.le.18) then
        !       eqn 310
        BMEQ = 'BRN017DCR02D'
     else if (DBH.le.20) then
        !       eqn 309
        BMEQ = 'BRN017LCR02D'
        call CALCFROMBIOEQDB(BMEQ, DBH, THT, CR,LCW)
        DCW = LCW*0.38
     else
        DBH = 20
        !       eqn 310
        BMEQ = 'BRN017DCR02D'
     end if
  else if (SPCD.eq.19) then
     if (DBH.le.16) then
        !       eqn 313
        BMEQ = 'BRN019DCR01D'
     else
        !       eqn 312
        BMEQ = 'BRN019LCR01D'
        call CALCFROMBIOEQDB(BMEQ, DBH, THT, CR,LCW)
        DCW = LCW*0.31
     end if
  else if (SPCD.eq.108) then
     !       eqn 323
     BMEQ = 'BRN108LCR01D'
     call CALCFROMBIOEQDB(BMEQ, DBH, THT, CR,LCW)
     DCW = LCW*0.235
  else if (SPCD.eq.73) then
     DCW = 0
  else if (SPCD.eq.93) then
     if (DBH.gt.22) DBH = 22
     !       eqn 318
     BMEQ = 'BRN093DCR01D'
  else if (SPCD.eq.202) then
     if (DBH.gt.34) DBH = 34
     !       eqn 341
     BMEQ = 'BRN202DCR02D'
  else if (SPCD.eq.263) then
     if (DBH.gt.20) DBH = 21
     !       eqn 351
     BMEQ = 'BRN263DCR01D'
  else if (SPCD.eq.119) then
     if (DBH.gt.24) DBH = 25
     !       eqn 326
     BMEQ = 'BRN119DCR01D'
  else if (SPCD.eq.242) then
     if (DBH.gt.26) DBH = 27
     !       eqn 347
     BMEQ = 'BRN242DCR01D'
  else if (SPCD.eq.122) then
     if (DBH.gt.34) DBH = 34
     !       eqn 332
     BMEQ = 'BRN122DCR01D'
  else if (SPCD.eq.101) then
     !       eqn 324
     BMEQ = 'BRN119LCR01D'
     call CALCFROMBIOEQDB(BMEQ, DBH, THT, CR,LCW)
     DCW = LCW*0.235
  end if
  if (DCW.lt.0) call CALCFROMBIOEQDB(BMEQ, DBH, THT, CR,DCW)
  if (DCW.lt.0) DCW = 0
end

  !     DAM SMALL TREE STEM WEIGHT
subroutine SMALLTREESTEMWEIGHT(SPN, DBH, THT, STSWT)
  implicit none
  ! Variables
  real    :: DBH,THT,STSWT,P3
  integer :: SPN
  character(len=12) :: BMEQ
  STSWT = -1
  P3 = 0

  if (SPN.eq.17) then
     if (DBH.le.4) then
        !       eqn 304
        BMEQ = 'BRN017STT01D'
     else if (DBH.le.5) then
        STSWT = 62
     else if (DBH.le.6) then
        STSWT = 106
     end if
  else if (SPN.eq.19) then
     if (DBH.le.4) then
        !       eqn 5025
        BMEQ = 'BRN019STT11D'
     else
        STSWT = 0
     end if
  else if (SPN.eq.108) then
     if (DBH.le.4) then
        !       eqn 322
        BMEQ = 'BRN108STT01D'
     else if (DBH.le.5) then
        STSWT = 83
     else if (DBH.le.6) then
        STSWT = 134
     end if
  else if (SPN.eq.73) then
     if (DBH.le.4) then
        !       eqn 5022
        BMEQ = 'BRN073STT11D'
     else
        STSWT = 0
     end if
  else if (SPN.eq.93) then
     if (DBH.le.4) then
        !       eqn 4148
        BMEQ = 'BRN093STT02D'
     else
        STSWT = 0
     end if
  else if (SPN.eq.202) then
     if (DBH.le.4) then
        !       eqn 327
        BMEQ = 'BRN122STT01D'
     else if (DBH.le.5) then
        STSWT = 69
     else if (DBH.le.6) then
        STSWT = 113
     end if
  else if (SPN.eq.263) then
     if (DBH.le.4) then
        !       eqn 5021
        BMEQ = 'BRN263STT11D'
     else
        STSWT = 0
     end if
  else if (SPN.eq.119) then
     if (DBH.le.4) then
        !       eqn 325
        BMEQ = 'BRN119STT01D'
     else if (DBH.le.5) then
        STSWT = 50
     else if (DBH.le.6) then
        STSWT = 82
     end if
  else if (SPN.eq.242) then
     if (DBH.le.4) then
        !       eqn 342
        BMEQ = 'BRN242STT01D'
     else if (DBH.le.5) then
        STSWT = 45
     else if (DBH.le.6) then
        STSWT = 73
     end if
  else if (SPN.eq.122) then
     if (DBH.le.4) then
        !       eqn 4147
        BMEQ = 'BRN122STT04D'
     else if (DBH.le.5) then
        STSWT = 50
     else if (DBH.le.6) then
        STSWT = 82
     end if
  else if (SPN.eq.101) then
     if (DBH.le.4) then
        !       eqn 325
        BMEQ = 'BRN119STT01D'
     else if (DBH.le.5) then
        STSWT = 50
     else if (DBH.le.6) then
        STSWT = 82
     end if
  else
     STSWT = 0
  end if
  if (STSWT.lt.0) call CALCFROMBIOEQDB(BMEQ, DBH, THT, P3, STSWT)
  if (STSWT.lt.0) STSWT = 0
  return
end

  !     FUNCTION TO CALCULATE DOUGLAS FIR LIVE CROWN FOR DBH > 34
  !      REAL FUNCTION DF34PLUSLCW(DBH)
  !      IMPLICIT NONE
  !      REAL DBH
  !      DF34PLUSLCW = EXP(0.0168 + 1.949*LOG(DBH))
  !      RETURN
  !      END

  ! Variables
real function PROPFUNC1(A, B, DBH)
  implicit none
  real    :: A,B,DBH
  PROPFUNC1 = A + B*DBH
  return
end

  ! Variables
real function PROPFUNC2(A, B, DBH)
  implicit none
  real    :: A,B,DBH
  PROPFUNC2 = A*EXP(B*DBH)
  return
end

  !     THE FUNCTION CALC LIVE PROP FOR LP1, LP2, LP3 AND LP4
  ! Variables
real function LIVEPROP(SPN, DBH, PROP)
  implicit none
  real    :: DBH,LP,PROPFUNC1,PROPFUNC2
  integer :: SPN,PROP
  if (SPN.eq.17) then
     if (PROP.eq.1) then
        if (DBH.le.34) then
           LP = 1/PROPFUNC1(1.5916, 0.05294, DBH)
        else
           LP = 0.286
        end if
     else if (PROP.eq.2) then
        if (DBH.le.34) then
           LP = 1/PROPFUNC1(1.1495, 0.04165, DBH)
        else
           LP = 0.378
        end if
     else if (PROP.eq.3) then
        if (DBH.le.2) then
           LP = 1
        else if (DBH.le.34) then
           LP = PROPFUNC1(1.0267, -0.01495, DBH)
        else
           LP = 0.488
        end if
     end if
  else if (SPN.eq.19) then
     if (PROP.eq.1) then
        LP = PROPFUNC2(0.5966, -0.04247, DBH)
     else if (PROP.eq.2) then
        LP = PROPFUNC2(0.8643, -0.03733, DBH)
     else if (PROP.eq.3) then
        if (DBH.le.2) then
           LP = 1.0
        else
           LP = PROPFUNC1(1.0221, -0.01083, DBH)
        end if
     end if
  else if (SPN.eq.108) then
     if (PROP.eq.1) then
        LP = PROPFUNC1(0.4933, -0.01167, DBH)
     else if (PROP.eq.2) then
        LP = PROPFUNC1(0.7767, -0.01464, DBH)
     else if (PROP.eq.3) then
        if (DBH.le.3) then
           LP = 1.0
        else
           LP = PROPFUNC1(1.0494, -0.01464, DBH)
        end if
     end if
  else if (SPN.eq.73) then
     if (PROP.eq.1) then
        LP = PROPFUNC2(0.3468, -0.04343, DBH)
     else if (PROP.eq.2) then
        LP = PROPFUNC2(0.745, -0.03622, DBH)
     else if (PROP.eq.3) then
        if (DBH.le.2) then
           LP = 1.0
        else
           LP = PROPFUNC2(1.05448, -0.0213, DBH)
        end if
     else if (PROP.eq.4) then
        if (DBH.le.12) then
           LP = 1.0
        else
           LP = 0.9223 + 0.7197/DBH
        end if
     end if
  else if (SPN.eq.93) then
     if (PROP.eq.1) then
        if (DBH.le.40) then
           LP = PROPFUNC2(0.5783, -0.325, DBH)
        else
           LP = 0.158
        end if
     else if (PROP.eq.2) then
        if (DBH.le.40) then
           LP = PROPFUNC2(0.8519, -0.02811, DBH)
        else
           LP = 0.277
        end if
     else if (PROP.eq.3) then
        if (DBH.le.2) then
           LP = 1.0
        else if (DBH.le.40) then
           LP = PROPFUNC1(1.03781, -0.01537, DBH)
        else
           LP = 0.423
        end if
     end if
  else if (SPN.eq.202) then
     if (PROP.eq.1) then
        if (DBH.le.36) then
           LP = PROPFUNC2(0.484, -0.02102, DBH)
        else
           LP = 0.227
        end if
     else if (PROP.eq.2) then
        if (DBH.le.36) then
           LP = PROPFUNC2(0.7289, -0.02332, DBH)
        else
           LP = 0.315
        end if
     else if (PROP.eq.3) then
        if (DBH.le.2) then
           LP = 1.0
        else if (DBH.le.36) then
           LP = PROPFUNC1(1.0342, -0.01584, DBH)
        else
           LP = 0.465
        end if
     else if (PROP.eq.4) then
        if (DBH.le.14) then
           LP = 1.0
        else
           LP = PROPFUNC1(1.0221, -0.001821, DBH)
        end if
     end if
  else if (SPN.eq.263) then
     if (PROP.eq.1) then
        if (DBH.le.40) then
           LP = PROPFUNC2(0.5474, -0.03697, DBH)
        else
           LP = 0.125
        end if
     else if (PROP.eq.2) then
        if (DBH.le.40) then
           LP = PROPFUNC2(0.8352, -0.03802, DBH)
        else
           LP = 0.183
        end if
     else if (PROP.eq.3) then
        if (DBH.le.2) then
           LP = 1.0
        else if (DBH.le.40) then
           LP = PROPFUNC2(1.0781, -0.02735, DBH)
        else
           LP = 0.361
        end if
     end if
  else if (SPN.eq.119) then
     if (PROP.eq.1) then
        LP = PROPFUNC2(0.5497, -0.03453, DBH)
     else if (PROP.eq.2) then
        LP = PROPFUNC2(0.9138, -0.09782, DBH)
     else if (PROP.eq.3) then
        if (DBH.le.2) then
           LP = 1.0
        else
           LP = PROPFUNC2(1.0564, -0.0181, DBH)
        end if
     end if
  else if (SPN.eq.242) then
     if (PROP.eq.1) then
        LP = PROPFUNC2(0.6174, -0.02326, DBH)
     else if (PROP.eq.2) then
        LP = PROPFUNC2(0.7562, -0.02411, DBH)
     else if (PROP.eq.3) then
        if (DBH.le.2) then
           LP = 1.0
        else
           LP = PROPFUNC2(1.0602, -0.02226, DBH)
        end if
     end if
  else if (SPN.eq.122) then
     if (PROP.eq.1) then
        LP = PROPFUNC2(0.5578, -0.04754, DBH)
     else if (PROP.eq.2) then
        if (DBH.le.30) then
           LP = PROPFUNC2(0.6254, -0.05114, DBH)
        else
           LP = PROPFUNC2(0.5578, -0.04754, DBH) + 0.01
        end if
     else if (PROP.eq.3) then
        LP = PROPFUNC2(0.985, -0.03102, DBH)
     else if (PROP.eq.4) then
        LP = PROPFUNC1(1.083, -0.01306, DBH)
     end if
  else if (SPN.eq.101) then
     if (PROP.eq.1) then
        if (DBH.le.6) then
           LP = PROPFUNC2(0.5497, -0.0345, DBH)
        else
           LP = PROPFUNC1(0.4933, -0.01167, DBH)
        end if
     else if (PROP.eq.2) then
        if (DBH.le.6) then
           LP = 0.9138 - 0.0978*DBH**0.5
        else
           LP = PROPFUNC1(0.7767, -0.01464, DBH)
        end if
     else if (PROP.eq.3) then
        if (DBH.le.6) then
           LP = PROPFUNC2(1.0564, -0.0181, DBH)
        else
           LP = PROPFUNC1(1.0494, -0.01402, DBH)
        end if
     end if
  end if
  LIVEPROP = LP
  return
end

  ! Variables
real function DEADPROP(SPN, DBH, PROP)
  implicit none
  real    :: DBH,DP,PROPFUNC1,PROPFUNC2
  integer :: SPN,PROP
  DP = 0
  if (SPN.eq.17) then
     if (PROP.eq.1) then
        if (DBH.le.26) then
           DP = PROPFUNC2(1.4336, -0.1816, DBH)
        else
           DP = 0.01
        end if
     else if (PROP.eq.2) then
        DP = PROPFUNC2(1.2623, -0.0347, DBH)
     end if
  else if (SPN.eq.19) then
     if (PROP.eq.1) DP = 1.2105*DBH**(-0.565)
  else if (SPN.eq.108) then
     if (PROP.eq.1) then
        if (DBH.le.20) then
           DP = 1.3527*DBH**(-0.7585)
        else
           DP = 0.139
        end if
     else if (PROP.eq.2) then
        if (DBH.le.20) then
           DP = PROPFUNC2(2.7979, -0.1257, DBH)
        else
           DP = 0.226
        end if
     end if
  else if (SPN.eq.93) then
     if (PROP.eq.1) then
        DP = 1.4657*DBH**(-0.6454)
     else if (PROP.eq.2) then
        DP = 1/PROPFUNC1(0.847, 0.01678, DBH)
     end if
  else if (SPN.eq.202) then
     if (PROP.eq.1) then
        DP = 0.08355 + 1.5893/DBH
     else if (PROP.eq.2) then
        DP = PROPFUNC2(1.5673, -0.05232, DBH)
     end if
  else if (SPN.eq.263) then
     if (PROP.eq.1) then
        if (DBH.le.28) then
           DP = PROPFUNC2(1.9608, -0.2064, DBH)
        else
           DP = 0.005
        end if
     else if (PROP.eq.2) then
        if (DBH.le.12) then
           DP = 1.0
        else
           DP = 1/PROPFUNC1(0.2772, 0.06141, DBH)
        end if
     end if
  else if (SPN.eq.119) then
     if (PROP.eq.1) then
        DP = 1.0077*DBH**(-0.4556)
     else if (PROP.eq.2) then
        DP = PROPFUNC1(1.0291, -0.004964, DBH)
     end if
  else if (SPN.eq.242) then
     if (PROP.eq.1) then
        DP = -0.01578 + 1.4673/DBH
     else if (PROP.eq.2) then
        DP = PROPFUNC2(1.4534, -0.05395, DBH)
     end if
  else if (SPN.eq.122) then
     if (PROP.eq.1) then
        if (DBH.le.30) then
           DP = -0.04345 + 1.4114/DBH
        else
           DP = 0.0004
        end if
     else if (PROP.eq.2) then
        if (DBH.le.30) then
           DP = PROPFUNC1(1.0621, -0.03342, DBH)
        else
           DP = 0.06
        end if
     end if
  else if (SPN.eq.101) then
     if (PROP.eq.1) then
        DP = 1.3527*DBH**(-0.7585)
     else if (PROP.eq.2) then
        DP = PROPFUNC2(2.7979, -0.1257, DBH)
     end if
  end if
  DEADPROP = DP
  return
end

  ! Variables
real function WOODDRYWEIGHT(SPN, GCUFTS, TWFLG)
  implicit none
  real    :: GCUFTS,DRYWT,WD,RATIO,RATIO2
  integer :: SPN,TWFLG
  !     TWFLAG IS THE FLAG FOR: 1 = TOPWOOD, 0 = LOG / CHUNK
  DRYWT = 0
  RATIO = 1.68
  RATIO2 = 1.188
  WD = 20
  if (SPN.eq.17) then
     WD = 23.1
     RATIO2 = 1.353
  else if (SPN.eq.19) then
     RATIO2 = 1.26
  else if (SPN.eq.108) then
     WD = 25.6
     RATIO2 = 1.114
  else if (SPN.eq.73) then
     WD = 32.4
     RATIO2 = 1.194
  else if (SPN.eq.93) then
     WD = 21.8
     RATIO2 = 1.274
  else if (SPN.eq.202) then
     WD = 30
     RATIO2 = 1.188
  else if (SPN.eq.263) then
     WD = 28.1
     RATIO2 = 1.207
  else if (SPN.eq.119) then
     WD = 23.7
     RATIO = 1.97
     RATIO2 = 1.232
  else if (SPN.eq.242) then
     RATIO2 = 1.168
  else if (SPN.eq.122) then
     WD = 25
     RATIO = 1.97
     RATIO2 = 1.209
  else if (SPN.eq.101) then
     WD = 23.7
     RATIO = 1.97
     RATIO2 = 1.232
  end if
  if (TWFLG.eq.0) RATIO = RATIO2
  DRYWT = GCUFTS*RATIO*WD
  WOODDRYWEIGHT = DRYWT
  return
end

subroutine CALCFROMBIOEQDB(BMEQ,DBH,THT,CR,BIOMS)
  ! Variables
  character(len=12) :: BMEQ
  real    :: DBH,THT,CR,LCW,HT1,HT2,BIOMS,TOPD
  integer :: ERRFLG
  !C      For equations saved in the SQLite database, calculate biomass with its equation formula
  call BIOEQDB(BMEQ,DBH,THT,HT1,HT2,CR,TOPD,BIOMS,ERRFLG)
  return
end
