!=================================!
!   VolumeLibrary : calcbiomass   !
!=================================!
  ! WARNING! The codes were created by NBEL Tester program. Please do NOT modify
  ! Last modified on 7/17/2015
  ! This sunroutine is generated from NBEL Tester for selected biomass equations
subroutine CalcBiomass(BEQ, DBH, THT, CR, BMS)
  ! Variables
  character(len=12) :: BEQ
  real    :: DBH,THT,CR,VOL(15),BMS
  integer :: STEMS,ERRFLG
  real    :: HT1PRD,HT2PRD,TOPD
  BMS = 0

  call BiomassLibrary(BEQ,DBH,THT,CR,HT1PRD,&
       &HT2PRD,TOPD, STEMS, VOL, BMS, ERRFLG)

  return
end

  ! This subroutine calculates biomass for equation stored in the National Biomass Estimator Library (NBEL).
  ! The file fiabioeq.inc lists all FIA Regional biomass equation.
  ! The file bioeqinfo.inc lists published biomass equations in NBEL and the file bioeqcoef.inc contains the
  ! coefficients for all biomass equations.
  ! Input variables are:
  ! BIOEQ -- biomass equatiom number
  ! DBHOB -- outside bark diameter at breast height
  ! HTTOT -- total height
  ! CR    -- crown ratio (=crown length/total height)
  ! HT1PRD -- height for primary prod (saw timber)
  ! HT2PRD -- height for secondary prod (pulp)
  ! TOPD -- top diameter for merch part
  ! STEMS -- number of stems for woodland species
  ! VOL -- the volume calculated from volume library
  ! BIOMS -- the calculated dry/green biomass for the given BIOEQ.
  ! ERRFLG -- error code returnd from calculation:
  !        15 - invalid BIOEQ
  !        16 - primary prod height required
  !        17 - secondary prod height is required
  !        18 - stem cubic volume is required for FIA biomass equation
subroutine BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,&
     &HT2PRD,TOPD,STEMS,VOL,BIOMS,ERRFLG)
  ! Variables
  character(len=12) :: BIOEQ
  character(len=12) :: GEOSUB
  real    :: DBHOB,HTTOT,HT1PRD,HT2PRD,TOPD,CR,VOL(15),BIOMS
  integer :: STEMS,ERRFLG,SPN
  SPN = 0
  GEOSUB = '0'
  call BiomassLibrary2(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,&
       &HT2PRD,TOPD,STEMS,VOL,BIOMS,ERRFLG,SPN,GEOSUB)
  return
end

subroutine BiomassLibrary2(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,&
     &HT2PRD,TOPD,STEMS,VOL,BIOMS,ERRFLG,SPN,GEOSUB)
  ! Common blocks
  include 'fiabioeq.inc'
  !       INCLUDE 'bioeqinfo.inc'
  !       INCLUDE 'bioeqcoef.inc'
  ! Variables
  character(len=12) :: BIOEQ
  character(len=12) :: BIOEQTMP
  character(len=12) :: GEOSUB
  character(len=12) :: NVELBEQ
  real    :: DBHOB,HTTOT,TOPD,CR,VOL(15),BIOMS,BIOMSTMP
  real    :: HT1PRD,HT2PRD,VOLM(15)
  character(len=3) :: MDL
  character(len=3) :: COMP
  real    :: DIA,HT
  integer :: STEMS,IDX,I,SPEC,ERRFLG,SPN
  character(len=2) :: DiaUnit
  character(len=2) :: HtUnit
  character(len=2) :: BioUnit
  character(len=3) :: DiaType
  character(len=3) :: HtType
  real    :: IN2CM,IN2MM,IN2M,FT2M,FT2CM,KG2LB,G2LB,MG2LB
  real    :: SG(11),MC,JNKBMS(8),RATIO
  real    :: DTMP,HTMP,HT1,HT2,CV4,WDEN,BDEN,CV4MS
  integer :: EqForm,BEQNUM,stat,LEN,isint

  ERRFLG = 0
  ! Check if BIOEQ is FIA equation number
  call str2int(BIOEQ,BEQNUM,stat)
  if (stat.eq.0) then
     if (BEQNUM.eq.109) then
        BIOMS = 0.0
        return
     else
        call FIABEQ2NVELBEQ(BEQNUM,SPN,NVELBEQ,GEOSUB,ERRFLG)
     end if
  else
     NVELBEQ = BIOEQ
  end if
  DIA = DBHOB
  HT = HTTOT
  MDL = NVELBEQ(1:3)
  COMP = NVELBEQ(7:9)
  VOLM = VOL
  BIOMS = 0.0
  if (MDL.eq.'CRM') then
     !      Jenkins component ratio method
     CV4 = VOL(4)+VOL(7)
     !         IF(CV4.LT.0.1)THEN
     !           ERRFLG = 18
     !           RETURN
     !         ENDIF
     !      CALL JENKINS FUNCTION TO GET BIOMASS FOR TREE COMPONENT
     !      NOTE: JENKINS FUNCTION RETURN DRY BIOMASS FOR THE FOLLOWING COMPONENT
     !     1 ABOVE GROUND TOTAL
     !     2 STEM WOOD
     !     3 STEM BARK
     !     4 FOLIAGE
     !     5 ROOTS
     !     6 BRANCHES
     !     7 CROWN
     !     8 STEM WOOD AND BARK
     call JENKINS(SPN, DIA, JNKBMS)
     !Convert CV4 to mass
     call WOODDEN(SPN, WDEN, BDEN)
     CV4MS = CV4*WDEN
     if (CV4MS.gt.0.0.and.JNKBMS(2).gt.0.0) then
        RATIO = CV4MS/JNKBMS(2)
     else
        if (DIA.lt.5.0) then
           call SAPLINGADJ(SPN,RATIO)
        else
           RATIO = 1.0
        end if
     end if
     !return results for components
     if (COMP.eq.'AGT') then
        BIOMS = JNKBMS(1)*RATIO
     else if (COMP.eq.'MSW') then
        BIOMS = JNKBMS(2)*RATIO
     else if (COMP.eq.'MSB') then
        BIOMS = JNKBMS(3)*RATIO
     else if (COMP.eq.'FOT') then
        BIOMS = JNKBMS(4)*RATIO
     else if (COMP.eq.'RTT') then
        BIOMS = JNKBMS(5)*RATIO
     else if (COMP.eq.'BRT'.or.COMP.eq.'BRL') then
        BIOMS = JNKBMS(6)*RATIO
     else if (COMP.eq.'CRW') then
        BIOMS = JNKBMS(7)*RATIO
     else if (COMP.eq.'MST') then
        BIOMS = JNKBMS(8)*RATIO
     end if

  else if (MDL.eq.'FNW'.or.MDL.eq.'FRM'.or.MDL.eq.'FNC'&
       &.or.MDL.eq.'FNE'.or.MDL.eq.'FSE'.or.MDL.eq.'FPI'&
       &.or.MDL.eq.'CAB'.or.MDL.eq.'CHV'.or.MDL.eq.'ASN') then
     !      Check if the equation is a valid
     IDX = 0
     ! No need to check equation number for FNW***STW01D and FNW***MSW01D
     if (MDL.eq.'FNW'.and.(COMP.eq.'STW'.or.COMP.eq.'MSW')) then
        continue
     else
        do I = 1, TOTEQ
           if (MDL.eq.'FSE'.and.FIABEQ(I)(1:11).eq.NVELBEQ(1:11)) then
              IDX = I
              exit
           else if (FIABEQ(I).eq.NVELBEQ) then
              IDX = I
              exit
           end if
        end do
        if (IDX.eq.0) then
           !      Flag for invalid BIOEQ
           ERRFLG = 15
           return
        end if
     end if
     !      Vol(4) should be the volume to 4" top
     !      If top wood volume is saved in VOL(7), add it back to VOL(4)
     if (VOLM(7).gt.0) VOLM(4) = VOLM(4)+VOLM(7)
     !         Stem cubic volume is required for FIA biomass equation
     !         IF(VOLM(1).EQ.0.AND.VOLM(4).EQ.0)THEN
     !           ERRFLG = 18
     !           RETURN
     !         ENDIF
     !      For the FIA biomass equation call FIA subroutine
     if (MDL.eq.'FNW') then
        call FIA_NW(NVELBEQ, DIA, HT, VOLM, BIOMS)
     else if (MDL.eq.'FRM') then
        call FIA_RM(NVELBEQ, DIA, HT, STEMS, VOLM, BIOMS)
     else if (MDL.eq.'FNC') then
        call FIA_NC(NVELBEQ, DIA, HT, VOLM, BIOMS)
     else if (MDL.eq.'FNE') then
        call FIA_NE(NVELBEQ, DIA, HT, VOLM, BIOMS)
     else if (MDL.eq.'FSE') then
        call FIA_SE(NVELBEQ, DIA, HT, VOL, BIOMS)
     else
        ! PACIFIC ISLANDS
        CV4 = VOLM(4)
        call PI_BIOMASS(SPN,NVELBEQ,DIA,HT,CV4,BIOMS,ERRFLG)
     end if
  else if (MDL.eq.'BRN'.and.NVELBEQ(10:12).eq.'00D') then
     !      Brown's fraction biomasss
     !      List of Brown fraction equations:
     !      BRN***FOT00D, BRN***B0Q00D,BRN***BQ100D,BRN***B1300D,
     !      BRN***B3P00D, BRN***LCR00D,BRN***DCR00D
     !      Valid species code:
     !      011,015,017,019,020,021,022,041,042,073,081,093,
     !      098,101,108,116,117,119,121,122,202,242,260,263
     call BROWN(NVELBEQ, DIA, HT, CR, BIOMS,ERRFLG)
  else
     !C      For equations saved in the SQLite database, calculate biomass with its equation formula
     !       Affleck 2019 component sum equations
     if (MDL.eq.'AFF'.and.(COMP.eq.'LCR'.or.&
          &COMP.eq.'MST'.or.COMP.eq.'AST'.or.COMP.eq.'B0Q')) then
        BIOEQTMP = NVELBEQ
        if (COMP.eq.'MST') then
           !           MAIN STEM TOTAL (BARK + WOOD)
           BIOEQTMP(7:9) = 'MSB'
           DTMP = DIA
           HTMP = HT
           BIOMSTMP = 0.0
           call BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,&
                &BIOMSTMP,ERRFLG)
           BIOMS = BIOMS+BIOMSTMP
           BIOEQTMP(7:9) = 'MSW'
           DTMP = DIA
           HTMP = HT
           BIOMSTMP = 0.0
           call BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,&
                &BIOMSTMP,ERRFLG)
           BIOMS = BIOMS+BIOMSTMP
        else if (COMP.eq.'LCR') then
           !          LIVE CROWN (LiveBranches + Foliage)
           BIOEQTMP(7:9) = 'BRL'
           DTMP = DIA
           HTMP = HT
           BIOMSTMP = 0.0
           call BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,&
                &BIOMSTMP,ERRFLG)
           BIOMS = BIOMS+BIOMSTMP
           BIOEQTMP(7:9) = 'FOT'
           DTMP = DIA
           HTMP = HT
           BIOMSTMP = 0.0
           call BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,&
                &BIOMSTMP,ERRFLG)
           BIOMS = BIOMS+BIOMSTMP
        else if (COMP.eq.'AST') then
           !          ABOVE STUMP TOTAL (DeadBranched + LiveBranches + Foliage + StemBARK + StemWOOD)
           BIOEQTMP(7:9) = 'BRD'
           DTMP = DIA
           HTMP = HT
           BIOMSTMP = 0.0
           call BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,&
                &BIOMSTMP,ERRFLG)
           BIOMS = BIOMS+BIOMSTMP
           BIOEQTMP(7:9) = 'BRL'
           DTMP = DIA
           HTMP = HT
           BIOMSTMP = 0.0
           call BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,&
                &BIOMSTMP,ERRFLG)
           BIOMS = BIOMS+BIOMSTMP
           BIOEQTMP(7:9) = 'FOT'
           DTMP = DIA
           HTMP = HT
           BIOMSTMP = 0.0
           call BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,&
                &BIOMSTMP,ERRFLG)
           BIOMS = BIOMS+BIOMSTMP
           BIOEQTMP(7:9) = 'MSB'
           DTMP = DIA
           HTMP = HT
           BIOMSTMP = 0.0
           call BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,&
                &BIOMSTMP,ERRFLG)
           BIOMS = BIOMS+BIOMSTMP
           BIOEQTMP(7:9) = 'MSW'
           DTMP = DIA
           HTMP = HT
           BIOMSTMP = 0.0
           call BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,&
                &BIOMSTMP,ERRFLG)
           BIOMS = BIOMS+BIOMSTMP
        else if (COMP.eq.'B0Q') then
           !          BRANCHES < 1/4 INCHES
           BIOEQTMP(7:9) = 'BRL'
           DTMP = DIA
           HTMP = HT
           call BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,&
                &BIOMS,ERRFLG)
           DTMP = DIA
           HTMP = HT
           BIOMSTMP = 0.0
           call BIOEQDB(BIOEQ,DTMP,HTMP,HT1,HT2,CR,TOPD,&
                &BIOMSTMP,ERRFLG)
           BIOMS = BIOMS*BIOMSTMP
        end if
     else if (MDL.eq.'SNE'.and.COMP.eq.'BRL') then
        ! Snell live branches = live crown - foliage total
        BIOEQTMP = NVELBEQ
        BIOEQTMP(7:9) = 'CRW'
        DTMP = DIA
        HTMP = HT
        BIOMSTMP = 0.0
        call BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,&
             &BIOMSTMP,ERRFLG)
        BIOMS = BIOMSTMP
        BIOEQTMP(7:9) = 'FOT'
        DTMP = DIA
        HTMP = HT
        BIOMSTMP = 0.0
        call BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,&
             &BIOMSTMP,ERRFLG)
        BIOMS = BIOMS - BIOMSTMP
     else
        call BIOEQDB(NVELBEQ,DIA,HT,HT1PRD,HT2PRD,CR,TOPD,&
             &BIOMS,ERRFLG)
     end if
  end if
  return
end

  ! This subroutine the specific gravity data for a given species
  ! The wood and bark specific gravity data for a species are given as:
  !      Column# Name
  !            1 Species FIA code
  !            2 soft or hard wood (0=soft and 1=hard)
  !            3 Jenkins species group code
  !            4 Wood dry weight (lb/cf)
  !            5 bark dry weight (lb/cf)
  !            6 Wood moist content(%)
  !            7 Bark moist cotent (%)
  !            8 bark to wood vol percent (%)
  !            9 Green weight factor (lb/cf)
  !           10 Dry weight factor (lb/cf)
  !           11 Sapling adjustment factor
subroutine MILESDATA(SPCD, SG)
  ! Common blocks
  include 'wdbkwtdata.inc' !'WDBKWTDATA.INC'
  ! Variables
  integer :: SPCD,FIRST,LAST,HALF,DONE
  real    :: SG(11)
  DONE = 0
  FIRST = 1
  LAST = TOTSPC
  do 50, while (DONE.eq.0)
  HALF = (LAST - FIRST +1)/2 + FIRST
  if (WDBKWT(HALF,1).eq.SPCD) then
     DONE = HALF
  else if (FIRST .eq. LAST) then
     DONE = -1
  else if (WDBKWT(HALF,1).lt.SPCD) then
     FIRST = HALF
  else
     LAST = HALF - 1
  end if
  50 continue
  !IF(DONE.LT.0) DONE = TOTSPC
  if (DONE.lt.0) DONE = CNT999
  SG(1) = WDBKWT(DONE,1)
  SG(2) = WDBKWT(DONE,2)
  SG(3) = WDBKWT(DONE,3)
  SG(4) = WDBKWT(DONE,4)
  SG(5) = WDBKWT(DONE,5)
  SG(6) = WDBKWT(DONE,6)
  SG(7) = WDBKWT(DONE,7)
  SG(8) = WDBKWT(DONE,8)
  SG(9) = WDBKWT(DONE,9)
  SG(10) = WDBKWT(DONE,10)
  SG(11) = WDBKWT(DONE,11)
  return
end

subroutine BIOEQDB(BIOEQ,DIA,HT,HT1PRD,HT2PRD,CR,TOPD,&
     &BIOMS,ERRFLG)
  !    The BIOMS returned from this subroutine is DRY or GREEN based the BIOEQ (2022/04/13)
  ! Common blocks
  include 'bioeqinfo.inc'
  include 'bioeqcoef.inc'
  ! Variables
  character(len=12) :: BIOEQ
  real    :: DIA,HT,HT1PRD,HT2PRD,CR,CL,TOPD,BIOMS
  real    :: a,b,c,d,e
  integer :: STEMS,IDX,I,SPEC,ERRFLG,EqForm
  character(len=2) :: DiaUnit
  character(len=2) :: HtUnit
  character(len=2) :: BioUnit
  character(len=3) :: DiaType
  character(len=3) :: HtType
  real    :: IN2CM,IN2MM,IN2M,FT2M,FT2CM,KG2LB,G2LB,MG2LB
  real    :: SG(11),MC
  !      For equations saved in the SQLite database, calculate biomass with its equation formula
  !      1. check if the BIOEQ is valid. EQNTOT is total number of biomass equations in the database
  IDX = 0
  a = 0.0
  b = 0.0
  c = 0.0
  d = 0.0
  e = 0.0
  !      List of unit conversion factor
  IN2CM = 2.54
  IN2MM = 25.4
  IN2M = 0.0254
  FT2M = 0.3048
  FT2CM = 30.48
  KG2LB = 2.20462
  G2LB = 0.00220462
  MG2LB = 2204.62
  do I = 1, EQNTOT
     if (BIOMSEQ(I)%BIOEQ.eq.BIOEQ) then
        IDX = I
        exit
     end if
  end do
  if (IDX.eq.0) then
     ERRFLG = 15
     return
  end if
  if (BIOEQ(1:2).eq.'AS') then
     read(BIOEQ(3:6),'(I4)') SPEC
  else
     read(BIOEQ(4:6),'(I3)') SPEC
  end if

  !      2. get the EQN info and coef
  DiaUnit = BIOMSEQ(IDX)%DiaUnit
  DiaType = BIOMSEQ(IDX)%DiaType
  HtType = BIOMSEQ(IDX)%HtType
  HtUnit = BIOMSEQ(IDX)%HtUnit
  if (HtType.eq.'HT1') then
     if (HT1PRD.le.0) then
        !          Primary prod height is required for the BIOEQ
        ERRFLG = 16
        return
     else
        HT = HT1PRD
     end if
  else if (HtType.eq.'HT2') then
     if (HT2PRD.le.0) then
        !          Secondary prod height is required for the BIOEQ
        ERRFLG = 17
        return
     else
        HT = HT2PRD
     end if
  end if

  if (DiaUnit.eq.'CM'.or.DiaUnit.eq.'cm') then
     DIA = DIA*IN2CM
  else if (DiaUnit.eq.'MM'.or.DiaUnit.eq.'mm') then
     DIA = DIA*IN2MM
  else if (DiaUnit.eq.'M'.or.DiaUnit.eq.'m') then
     DIA = DIA*IN2M
  end if
  if (HtUnit.eq.'M'.or.HtUnit.eq.'m') then
     HT = HT*FT2M
  else if (HtUnit.eq.'CM'.or.HtUnit.eq.'cm') then
     HT = HT*FT2CM
  end if
  CL = CR*HT

  if (DiaType.eq.'CBH'.or.DiaType.eq.'CRC'.or.DiaType.eq.'CBA') then
     DIA = DIA*3.14
  else if (DiaType.eq.'BA') then
     DIA = (DIA/2)**2*3.14
  else if (DiaType.eq.'D2') then
     DIA = DIA*DIA
  else if (DiaType.eq.'BAH') then
     DIA = DIA**2*0.005454*HT
  else if (DiaType.eq.'D2H') then
     DIA = DIA*DIA*HT
  else if (DiaType.eq.'D2R') then
     DIA = DIA*DIA*CR
  end if
  EqForm = BEQCOEF(IDX,1)
  a = BEQCOEF(IDX,2)
  b = BEQCOEF(IDX,3)
  c = BEQCOEF(IDX,4)
  d = BEQCOEF(IDX,5)
  e = BEQCOEF(IDX,6)

  !      3. call the EQN formula to calculate the biomass
  call BioeqFormula(EqForm,DIA,HT,CR,TOPD,a,b,c,d,e,BIOMS)
  if (BIOEQ(12:12).eq.'R'.or.EqForm.eq.41.or.EqForm.eq.19) return
  !      4. return dry biomass in lb
  BioUnit = BIOMSEQ(IDX)%BioUnit
  if (BioUnit.eq.'KG') then
     BIOMS = BIOMS*KG2LB
  else if (BioUnit.eq.'G') then
     BIOMS = BIOMS*G2LB
  else if (BioUnit.eq.'MG') then
     BIOMS = BIOMS*MG2LB
  end if
  read(BIOEQ(4:6),'(I3)') SPEC
  !      Convert to dry biomass if the original equation is for green biomass
  ! Chenged to no conversion from green to dry if the BIOEQ is green
  !IF(BIOEQ(12:12).EQ.'G')THEN
  !  CALL MILESDATA(SPEC, SG)
  !  MC = (SG(9)-SG(10))/SG(10)
  !  BIOMS = BIOMS/(1.0+MC)
  !ENDIF
  return
end

subroutine biomasslib_r(BIOEQ,DBH_d,THT_d,CL_d,HT1_d,HT2_d,CV4_d,&
     &TOPD_d,STEMS,DRYBIOMAS_d,GRNBIOMAS_d,ERRFLAG,SPN,GEOSUB)
  ! This subroutine is for R user to calculate biomass      !
  ! YW 05/09/2019
  ! YW 2019/08/05 Added input variables SPN and GEOSUB and also make BIOEQ
  ! be able to use FIA biomass equation number.

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::biomasslib_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'biomasslib_r_'::biomasslib_r

  implicit none

  double precision :: DBH_d,THT_d,CL_d,HT1_d,HT2_d,CV4_d,TOPD_d
  double precision :: DRYBIOMAS_d,GRNBIOMAS_d
  ! Variables
  character(len=12) :: BIOEQ
  character(len=12) :: GEOSUB
  character(len=12) :: NVELBEQ
  integer :: SPEC,STEMS,ERRFLAG,SPN
  real    :: DBHOB,HTTOT,HT1PRD,HT2PRD,CV4,VOL(15),TOPD,CL,CR
  real    :: DRYBIOMAS,GRNBIOMAS,BIOMASS
  real    :: SG(11),MC

  DBHOB = real(DBH_d)
  HTTOT = real(THT_d)
  HT1PRD = real(HT1_d)
  HT2PRD = real(HT2_d)
  CV4 = real(CV4_d)
  TOPD = real(TOPD_d)
  VOL = 0.0
  VOL(4) = CV4
  CL = real(CL_d)
  ERRFLAG = 0
  DRYBIOMAS = 0.0
  GRNBIOMAS = 0.0
  if (HTTOT.gt.1.0.and.CL.gt.1.0) CR = CL/HTTOT
  call BiomassLibrary2(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,&
       &HT2PRD,TOPD,STEMS,VOL,BIOMASS, ERRFLAG,SPN,GEOSUB)
  if (SPN.eq.0) then
     read(BIOEQ(4:6),'(I3)') SPEC
  else
     SPEC = SPN
  end if
  call MILESDATA(SPEC, SG)
  MC = (SG(9)-SG(10))/SG(10)
  if (BIOEQ(12:12).eq.'G') then
     GRNBIOMAS = BIOMASS
     DRYBIOMAS = BIOMASS/(1.0+MC)
  else
     DRYBIOMAS = BIOMASS
     GRNBIOMAS = BIOMASS*(1.0+MC)
  end if
  DRYBIOMAS_d = DBLE(DRYBIOMAS)
  GRNBIOMAS_d = DBLE(GRNBIOMAS)
  return
end

  ! This subroutine return the species default weight factor for R
subroutine getwtfactor_r(REGN,FORST,SPEC,GRNWF_d,DRYWF_d)
      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::getwtfactor_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'getwtfactor_r_'::getwtfactor_r

  implicit none

  double precision :: GRNWF_d,DRYWF_d
  ! Variables
  real    :: WF(3),DRYWF
  integer :: REGN,SPEC
  character(len=2) :: FORST
  character(len=12) :: BMSEQ(8)
  character(len=12) :: BIOEQ
  character(len=50) :: REF(8)
  WF = 0.0
  call CRZSPDFT(REGN,FORST,SPEC,WF,BMSEQ,REF)

  DRYWF = WF(1)/(1.0+WF(3)/100.0)
  GRNWF_d = DBLE(WF(1))
  DRYWF_d = DBLE(DRYWF)
  return
end
