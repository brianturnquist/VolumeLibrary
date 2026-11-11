!==========================!
!   VolumeLibrary : nsvb   !
!==========================!
  ! A national-scale tree volume, biomass, and carbon modeling system for the United States
  ! Westfall, J.A. etal 2023
  ! The subroutine calculate stem wood and bark volumes and also biomass for stem wood and bark, branches, foliage, and above ground total
  ! The elements in the variable DRYBIO and GRNBIO are weight of following:
  ! 1 above ground biomass (no foliage)
  ! 2 total stem wood
  ! 3 total stem bark
  ! 4 stump wood
  ! 5 stump bark
  ! 6 sawtimber wood (primary)
  ! 7 sawtimber bark
  ! 8 topwood wood (secondary)
  ! 9 topwood bark
  ! 10 tip wood
  ! 11 tip bark
  ! 12 branches
  ! 13 foliage
  ! 14 top and limb
  ! 15 carbon content
subroutine NVBC(REGN,FORST,DIST,VOLEQ,DBHOB,HTTOT,MTOPP,MTOPS,&
     &HT1PRD,HT2PRD,STUMP,PROD,BRKHT,BRKHTD,LIVE,CR,CULL,DECAYCD,&
     &LOGLEN,LOGDIA,LOGVOL,BOLHT,LOGST,NOLOGP,NOLOGS,VOL,DRYBIO,GRNBIO,&
     &ERRFLG,FIASPCD,CTYPE)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: PROD2
  character(len=1) :: LIVE
  character(len=1) :: CTYPE
  !CTYPE as I = FIA, F = FVS, C = Cruise, B = when other VOLEQ was used in voinitnvb
  integer :: REGN,ERRFLG,DECAYCD,FIASPCD,SFTHRD,ERRFLG2
  real    :: DBHOB,HTTOT,MTOPP,MTOPS,HT1PRD,HT2PRD,STUMP,BRKHT,BRKHTD
  real    :: CR,CULL
  real    :: VOL(15),DRYBIO(15),GRNBIO(15)
  ! Common blocks
  include 'tables5.inc'
  !Mrules variables
  integer :: EVOD,OPT
  character(len=1) :: COR
  character(len=1) :: JKSPGRP
  real    :: MAXLEN,MINLEN,MERCHL,MINLENT,TRIM,BTR,DBTBH,MINBFD
  character(len=10) :: V_EQN
  !Variables for calculation
  real    :: a,b,HT2,DIB,DIBL,DIBS,LENTH,LOGV,BFINT,R
  real    :: Vtotib,Vtotbk,Vtotob,Rstump,Rsaw,Rmrch
  real    :: Vsawib,Vsawbk,Vsawob,Vmrchib,Vmrchbk,Vmrchob
  real    :: Vtwib,Vtwob,Vtwbk,Vstumpib,Vstumpob,Vstumpbk
  real    :: Vtipib,Vtipob,Vtipbk,LMERCH,HTsaw,HTmrch
  real    :: LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3),BOLHT(21),LOGLENT(20) !,SG(11)
  integer :: RatioEQ,LOGST,NUMSEG,I,SPCD,SPGRPCD,DIVISION,EcoProv
  real    :: NOLOGP,NOLOGS,NLOGP,NLOGS,WDSG
  real    :: Wtotib,Wtotbk,Wbrch,AGBpred,AGBcomp,AGBred,AGBdiff
  real    :: WoodR,BarkR,BrchR,WoodAdd,BarkAdd,BrchAdd,WoodHarm,BarkHarm
  real    :: BrchHarm,WDSGadj,BKSGadj,Wsawib,Wsawbk,Wtwib,Wtwbk,Wtipib
  real    :: Wtipbk,Wstumpib,Wstumpbk,Wfol,Rrem,Vibmiss,Vbkmiss,Vobmiss
  !CHARACTER*12 BMSEQ(8)
  !CHARACTER*40 REF(8)
  real    :: GRNWF,DRYWF,MC,CF,VtotibSound,VtotbkSound,VtotobSound !WF(3),
  real    :: VstumpibSound,VtipibSound,VtipbkSound,VtipobSound,AGBpredRed
  real    :: VsawibSound,VtwibSound
  real    :: CullDenProp,DenProp,RemBkProp,RemBrchProp,WfolRem
  real    :: WtotibRed,WtotbkRed,WtotobRed,WbrchRed,AGBcompRed,BrchRem
  real    :: Vtotob2,Vsawib2,Vsawob2,Vsawbk2,Vmrchib2,Vmrchob2,Vmrchbk2
  real    :: Vtwib2,Vtwob2,Vtwbk2,Vtipib2,Vtipbk2,Rsaw2,Rmrch2
  real    :: DeadCF,SPGRNWF,SPDRYWF,SPREGNWF,DeadWF,WtFac2
  real    :: Ib2ObRatio,MTOPPob,MTOPSob,aOB,bOB,TOTLOGV,Vmerch,DOB
  real    :: VolFactor
  character(len=3) :: SPC
  !Check VOLEQ is valid
  if (VOLEQ(1:3).ne.'NVB') then
     ERRFLG = 1
     return
  end if
  !Check if EcoPROV is provided in the VOLEQ
  EcoProv = 0
  if (VOLEQ(7:7).ne.'0') then
     read (VOLEQ(5:7),*) EcoProv
     if (VOLEQ(4:4).eq.'M') then
        EcoProv = 1000 + EcoProv
     end if
     VOLEQ(7:7) = '0'
  end if
  do I = 7, 10
     if (VOLEQ(I:I).lt.'0'.or.VOLEQ(I:I).gt.'9') then
        ERRFLG = 1
        return
     end if
  end do
  !Reset the species code for R10 second grow species(2024/10/07)
  if (FIASPCD.eq.2042) FIASPCD = 42
  if (FIASPCD.eq.2098) FIASPCD = 98
  if (FIASPCD.eq.2242) FIASPCD = 242
  if (FIASPCD.eq.2263) FIASPCD = 263
  VolFactor = 1.0
  VOL = 0.0
  DRYBIO = 0.0
  GRNBIO = 0.0
  LOGLEN = 0.0
  LOGVOL = 0.0
  LOGDIA = 0.0
  BOLHT = 0.0
  ERRFLG = 0
  ERRFLG2 = 0
  AGBpred = 0
  WoodHarm = 0
  BARKHarm = 0
  Wstumpib = 0
  Wstumpbk = 0
  Wsawib = 0
  Wsawbk = 0
  Wtwib = 0
  Wtwbk = 0
  Wtipib = 0
  Wtipbk = 0
  BrchHarm = 0
  Wfol = 0
  LOGST = 0
  NOLOGP = 0
  NOLOGS = 0
  Rrem = 1
  BrchRem = 1
  Vtotib = 0
  Vtotbk = 0
  Vtotob = 0
  Vsawib = 0
  Vsawbk = 0
  Vsawob = 0
  Vtwib = 0
  Vtwob = 0
  Vtwbk = 0
  Vibmiss = 0
  Vbkmiss = 0
  Vobmiss = 0
  RatioEQ = 6
  WfolRem = 0
  Vtotob2 = 0
  BTR = 0
  DBTBH = 0
  DeadCF = 0
  !Move the MTOPS check to after call MRULES 2023/08/16
  !IF(MTOPS.GT.MTOPP) MTOPS = MTOPP
  SPGRPCD = 0
  WDSG = 0
  SFTHRD = 0
  CF = 0
  !should get the division from a crowsswalk using district and ecoregion
  read (VOLEQ(5:7),'(i3)') DIVISION
  if (VOLEQ(4:4).eq.'M') DIVISION = DIVISION + 1000
  if (FIASPCD.le.0) then
     if (LEN_TRIM(VOLEQ).eq.10) then
        read (VOLEQ(8:10),'(i3)') SPCD
     else if (LEN_TRIM(VOLEQ).eq.11) then
        if (VOLEQ(11:11).ge.'0'.and.VOLEQ(11:11).le.'9') then
           !Allow 4-digit SPCD i the VOLEQ
           read (VOLEQ(8:11),'(i4)') SPCD
        else
           read (VOLEQ(8:10),'(i3)') SPCD
        end if
     end if
  else
     SPCD = FIASPCD
  end if
  if (SPCD.eq.204) SPCD = 202
  if (SPCD.lt.10) then
     !FIASPCD is required for Jenkins SPGRP equation
     ERRFLG = 6
     return
  end if
  !Added default DECAYCD for Dead tree (2025/06/04)
  if (LIVE.eq.'D'.and.DECAYCD.eq.0) DECAYCD = 3
  !FIA is not using the CR input value for biomass calculation, but NVEL still keep it
  !IF(LIVE.EQ.'D'.AND.CR.GT.0) CR = 0

  call NVB_RefSpcData(SPCD,SPGRPCD,WDSG,SFTHRD,CF,ERRFLG,&
       &SPGRNWF, SPDRYWF)
  if (ERRFLG.gt.0.and.ERRFLG.ne.6) return
  !Reset VOLEQ if it has 4-digit SPCD
  if (LEN_TRIM(VOLEQ).eq.11.and.VOLEQ(11:11).ne.'P'&
       &.and.(VOLEQ(11:11).ge.'0'.and.VOLEQ(11:11).le.'9')) then
     if (SPCD.gt.999) then
        write (JKSPGRP, '(i1)') SPGRPCD
        VOLEQ = 'NVB000000'//JKSPGRP
     else
        write (SPC, '(i3)') SPCD
        if (SPCD.lt.10) then
           ERRFLG = 6
        else if (SPCD.lt.100) then
           VOLEQ = VOLEQ(1:7)//'0'//SPC(2:3)
        else
           VOLEQ = VOLEQ(1:7)//SPC
        end if
     end if
  end if
  !First call the mrules to get the region defaults
  V_EQN = VOLEQ(1:10)
  call MRULES(REGN,FORST,V_EQN,DBHOB,COR,EVOD,OPT,MAXLEN,&
       &MINLEN,MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,&
       &PROD)
  if (MTOPS.gt.MTOPP) MTOPS = MTOPP
  !If HTTOT is not provided, try to get an estimate from HT1PRD/MTOPP, HT2PRD/MTOPS
  if (HTTOT.le.0) then
     if (HT2PRD.gt.0.and.MTOPS.gt.0) then
        call NVB_ESTTHT(VOLEQ,DBHOB,HT2PRD,MTOPS,HTTOT,ERRFLG,&
             &SPGRPCD,WDSG)
     else if (HT1PRD.gt.0.and.MTOPP.gt.0) then
        call NVB_ESTTHT(VOLEQ,DBHOB,HT1PRD,MTOPP,HTTOT,ERRFLG,&
             &SPGRPCD,WDSG)
     end if
  end if
  !If HTTOT still 0, check broken height
  if (HTTOT.le.0) then
     if (BRKHT.le.0.and.BRKHTD.le.0) then
        ERRFLG = 4
        !RETURN
     else if (BRKHT.le.0.or.BRKHTD.le.0) then
        ERRFLG = 9
        !RETURN
     else if (BRKHT.gt.0.and.BRKHTD.gt.0) then
        call NVB_ESTTHT(VOLEQ,DBHOB,BRKHT,BRKHTD,HTTOT,ERRFLG,&
             &SPGRPCD,WDSG)
     end if
  end if
  if (HTTOT.le.0) ERRFLG = 4
  if (ERRFLG.gt.0) return
  if (BRKHT.lt.4.5) BRKHT = 0
  if (BRKHT.gt.0) then
     call NVB_GetRatioCOEF(VOLEQ,Tbl5Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
     call CalcRatio(HTTOT,BRKHT,RatioEQ,a,b,Rrem)
     !this DIVISION needs to be accurate, like 331 not 330
     if (EcoProv.eq.0) call NVB_EcoProv(REGN,FORST,DIST,EcoProv)
     if (LIVE.eq.'L'.and.CR.eq.0) CR = 1
     call NVB_BrchRem(EcoProv,SPCD,BRKHT,HTTOT,CR,BrchRem)
  end if
  CullDenProp = 0.54
  !IF(SPCD.LT.300) CullDenProp = 0.92
  if (SFTHRD.eq.0) CullDenProp = 0.92
  DenProp = 1
  RemBkProp = 1
  RemBrchProp = 1
  !Table 1 FIA dead tree wood density proportion and remaining bark and branch proportion
  !For live trees, ignore DECAYCD
  if (LIVE.eq.'L'.and.DECAYCD.gt.0) DECAYCD = 0
  if (DECAYCD.gt.0) then
     if (DECAYCD.eq.1) then
        RemBkProp = 1
        RemBrchProp = 1
     else if (DECAYCD.eq.2) then
        RemBkProp = 0.8
        RemBrchProp = 0.5
     else if (DECAYCD.eq.3) then
        RemBkProp = 0.5
        RemBrchProp = 0.1
     else if (DECAYCD.eq.4) then
        RemBkProp = 0.2
        RemBrchProp = 0
     else if (DECAYCD.eq.5) then
        RemBkProp = 0
        RemBrchProp = 0
     end if
     !IF(SPCD.LT.300)THEN
     !Move the following code to DecayDenProp subroutine (2025/05/08)
     !IF(SFTHRD.EQ.0)THEN
     !    IF(DECAYCD.eq.1)THEN
     !        DenProp = 0.97
     !        DeadCF = 0.501
     !    ELSEIF(DECAYCD.EQ.2)THEN
     !        DenProp = 1
     !        DeadCF = 0.504
     !   ELSEIF(DECAYCD.EQ.3)THEN
     !        DenProp = 0.92
     !        DeadCF = 0.506
     !   ELSEIF(DECAYCD.EQ.4)THEN
     !        DenProp = 0.55
     !        DeadCF = 0.52
     !   ELSEIF(DECAYCD.EQ.5)THEN
     !        DenProp = 0.55
     !        DeadCF = 0.527
     !   ENDIF
     !ELSE
     !    IF(DECAYCD.eq.1)THEN
     !        DenProp = 0.99
     !        DeadCF = 0.47
     !    ELSEIF(DECAYCD.EQ.2)THEN
     !        DenProp = 0.8
     !        DeadCF = 0.473
     !   ELSEIF(DECAYCD.EQ.3)THEN
     !        DenProp = 0.54
     !        DeadCF = 0.481
     !   ELSEIF(DECAYCD.EQ.4)THEN
     !        DenProp = 0.43
     !        DeadCF = 0.48
     !   ELSEIF(DECAYCD.EQ.5)THEN
     !        DenProp = 0.43
     !        DeadCF = 0.472
     !   ENDIF
     !ENDIF
     call DecayDenProp(SFTHRD,DECAYCD,DenProp,DeadCF)
  end if
  ! (1) calculate total stem wood volume inside bark
  call NVB_Vib(VOLEQ,DBHOB,HTTOT,Vtotib,ERRFLG,SPGRPCD,WDSG)
  if (ERRFLG.gt.0) return
  if (Vtotib.le.0) then
     ERRFLG = 1
     return
  end if
  VtotibSound = Vtotib*Rrem*(1-CULL/100)
  Vibmiss = Vtotib*(1-Rrem)
  VOL(1) = VtotibSound
  ! (2) calculate total stem bark volume
  call NVB_Vbk(VOLEQ,DBHOB,HTTOT,Vtotbk,ERRFLG,SPGRPCD,WDSG)
  Vbkmiss = Vtotbk*(1-Rrem)
  VtotbkSound = Vtotbk*Rrem
  Ib2ObRatio = sqrt(Vtotib/(Vtotib+Vtotbk))
  ! (3) addup for the Vtotob
  Vtotob = Vtotib + Vtotbk
  Vobmiss = Vibmiss + Vbkmiss
  VtotobSound = VtotibSound+VtotbkSound
  call NVB_Vob(VOLEQ,DBHOB,HTTOT,Vtotob2,ERRFLG,SPGRPCD,WDSG)
  ! (4) calculate stump vol
  call NVB_GetRatioCOEF(VOLEQ,Tbl5Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
  call CalcRatio(HTTOT,STUMP,RatioEQ,a,b,Rstump)
  Vstumpib = Vtotib * Rstump
  Vstumpob = Vtotob * Rstump
  Vstumpbk = Vstumpob - Vstumpib
  VstumpibSound = Vstumpib*(1-CULL/100)
  VOL(14) = VstumpibSound
  ! (5) calculate saw volume
  !FIA calculate merch volume different than cruise and FVS
  !FIA uses the very simple way, i.e. the volume from stump to merch top (DOB)
  !Timber Cruise and FVS calculates it as sum of log volumes with trim removed between logs
  if (HT1PRD.le.0.and.MTOPP.lt.DBHOB) then
     if (CTYPE.eq.'I'.or.CTYPE.eq.'i') then
        MTOPPob = MTOPP
        !              CALL NVB_HT2TOPDob(VOLEQ,DBHOB,HTTOT,Vtotob2,MTOPP,HT1PRD,
        !     &        ERRFLG,SPGRPCD,WDSG)
     else
        MTOPPob = MTOPP/Ib2ObRatio
        !              CALL NVB_HT2TOPDib(VOLEQ,DBHOB,HTTOT,Vtotib,MTOPP,HT1PRD,
        !     &        ERRFLG,SPGRPCD,WDSG)
     end if
     !Merch height calculation using DOB and outside bark coefficient
     call NVB_HT2TOPDob(VOLEQ,DBHOB,HTTOT,Vtotob2,MTOPPob,HT1PRD,&
          &ERRFLG,SPGRPCD,WDSG)
     !Check the broken height
     if (BRKHT.gt.0.and.BRKHT.lt.HT1PRD) HT1PRD = BRKHT
  end if
  if (HT1PRD.lt.STUMP) HT1PRD = STUMP
  !Calculate sawtimber volume for FIA
  !CTYPE = B is set in volinitnvb when VOLEQ is not NSVB EQ
  if (CTYPE.eq.'I'.or.CTYPE.eq.'B') then
     call CalcRatio(HTTOT,HT1PRD,RatioEQ,a,b,Rsaw2)
     Vsawib2 = Vtotib*Rsaw2 - Vstumpib
     Vsawob2 = Vtotob*Rsaw2 - Vstumpob
     Vsawbk2 = Vsawob2 - Vsawib2
  end if
  LMERCH = HT1PRD - STUMP
  if (LMERCH.lt.0) LMERCH = 0
  LOGST = 0
  NLOGP = 0
  NUMSEG = 0
  TOTLOGV = 0.0
  R = 0
  HTsaw = STUMP
  !Moved the DIBL (DBHIN) calculation here avoid DIBL=0 for trees with no saw prod
  HT2 = 4.5
  call NVB_GetRatioCOEFob(VOLEQ,SPGRPCD,aOB,bOB)
  call NVB_CalcDiaAtHT(Vtotob2,aOb,bOB,HTTOT,HT2,DOB)
  DIB = DOB*Ib2ObRatio
  !CALL NVB_CalcDiaAtHT(Vtotib,a,b,HTTOT,HT2,DIB)
  LOGDIA(1,2) = DIB
  LOGDIA(1,1) = NINT(DIB)
  BOLHT(1) = HT2
  !--   USE DIB AT DBHOB FOR LARGE END BUTT LOG
  DIBL = LOGDIA(1,1)
  HT2 = STUMP
  if (LMERCH.ge.MERCHL) then
     !Added max number of logs (20) check (20250626)
     if ((LMERCH/(MAXLEN+TRIM)).gt.20) then
        ERRFLG2 = 12
        !RETURN
     end if
     call NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG)
     if (NUMSEG.gt.20) then
        ERRFLG = 12
        return
     end if
     call SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG,LOGLEN)
     call NVB_CalcLOGVOL(LOGST,NUMSEG,DIBL,HT2,Vtotob2,TRIM,HTTOT,&
          &LOGLEN,LOGDIA,LOGVOL,BOLHT,VOL,COR,aOB,bOB,Ib2ObRatio,TOTLOGV)
     LOGST = NUMSEG
     NOLOGP = NUMSEG
     HTsaw = HT2
     call CalcRatio(HTTOT,HTsaw,RatioEQ,a,b,Rsaw)
     Vsawib = Vtotib * Rsaw - Vstumpib
     Vsawob = Vtotob * Rsaw - Vstumpob
     Vsawbk = Vsawob - Vsawib
     R = Rsaw
  end if
  HTmrch = HTsaw
  if (HT1PRD.le.0) HT1PRD = HTsaw
  ! (6) calculate merch, topwood(tw) and tip volumes
  NLOGS = 0
  !Added check MTOPS < DBHOB (2025/10/20)
  if (HT2PRD.le.0.and.MTOPS.lt.DBHOB) then
     if (CTYPE.eq.'I'.or.CTYPE.eq.'i') then
        MTOPSob = MTOPS
        !              CALL NVB_HT2TOPDob(VOLEQ,DBHOB,HTTOT,Vtotob2,MTOPS,HT2PRD,
        !     &        ERRFLG,SPGRPCD,WDSG)
     else
        MTOPSob = MTOPS/Ib2ObRatio
        !              CALL NVB_HT2TOPDib(VOLEQ,DBHOB,HTTOT,Vtotib,MTOPS,HT2PRD,
        !     &        ERRFLG,SPGRPCD,WDSG)
     end if
     call NVB_HT2TOPDob(VOLEQ,DBHOB,HTTOT,Vtotob2,MTOPSob,HT2PRD,&
          &ERRFLG,SPGRPCD,WDSG)
     !Check the broken height
     if (BRKHT.gt.0.and.BRKHT.lt.HT2PRD) HT2PRD = BRKHT
  end if
  !Calculate topwood volume for FIA
  if (CTYPE.eq.'I'.or.CTYPE.eq.'B') then
     call CalcRatio(HTTOT,HT2PRD,RatioEQ,a,b,Rmrch2)
     Vmrchib2 = Vtotib*Rmrch2 - Vstumpib
     Vmrchob2 = Vtotob*Rmrch2 - Vstumpob
     Vmrchbk2 = Vmrchob2 - Vmrchib2
     Vtwib2 = Vmrchib2 - Vsawib2
     Vtwob2 = Vmrchob2 - Vsawob2
     Vtwbk2 = Vtwob2 - Vtwib2
  end if
  if (HT2PRD.lt.HTsaw) HT2PRD = HTsaw
  LMERCH = HT2PRD - HTsaw
  if (LMERCH.ge.MINLENT) then
     NUMSEG = 0
     !Added max number of logs (20) check (20250626)
     if ((LMERCH/(MAXLEN+TRIM)).gt.20) then
        ERRFLG2 = 12
        !RETURN
     end if
     call NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG)
     call SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLENT,TRIM,NUMSEG,&
          &LOGLENT)
     NOLOGS = NUMSEG
     if ((LOGST+NUMSEG).gt.20) then
        ERRFLG2 = 12
        !RETURN
     end if
     if ((LOGST+NUMSEG).le.20) then
        !Add secondary log length to LOGLEN
        do I = 1, NUMSEG
           LOGLEN(I+LOGST) = LOGLENT(I)
           600 continue
        end do

        call NVB_CalcLOGVOL(LOGST,NUMSEG,DIBL,HT2,Vtotob2,TRIM,HTTOT,&
             &LOGLEN,LOGDIA,LOGVOL,BOLHT,VOL,COR,aOB,bOB,Ib2ObRatio,TOTLOGV)
        LOGST = LOGST + NUMSEG
     end if
     HTmrch = HT2
     call CalcRatio(HTTOT,HTmrch,RatioEQ,a,b,Rmrch)
     Vmrchib = Vtotib * Rmrch - Vstumpib
     Vmrchob = Vtotob * Rmrch - Vstumpob
     Vmrchbk = Vmrchob - Vmrchib
     ! topwood volume
     Vtwib = Vmrchib - Vsawib
     Vtwob = Vmrchob - Vsawob
     Vtwbk = Vtwob - Vtwib
  end if
  if (HT2PRD.le.0) HT2PRD = HTmrch
  call CalcRatio(HTTOT,HTmrch,RatioEQ,a,b,Rmrch)
  !Adjust log cubic volume to line up with merch vol from stump to merch top
  Vmerch = Vtotib*Rmrch - Vstumpib;
  !Reset VOL(4) and VOL(7)
  VOL(4) = 0.0
  VOL(7) = 0.0
  if (TOTLOGV.gt.0.0.and.Vmerch.gt.0.0) VolFactor = Vmerch/TOTLOGV
  do I = 1, 20
     LOGVOL(4,I) = LOGVOL(4,I)*VolFactor
     if (I.le.NOLOGP) then
        VOL(4) = VOL(4) + LOGVOL(4,I)
     else
        VOL(7) = VOL(7) + LOGVOL(4,I)
     end if
     700 continue
  end do
  ! tip volume
  !IF(NLOGS.GT.0) R = Rmrch
  if (CTYPE.eq.'I'.or.CTYPE.eq.'i') then
     R = Rmrch2
  else
     R = Rmrch
  end if
  Vtipib = Vtotib * (1 - R)
  Vtipob = Vtotob * (1 - R)
  Vtipbk = Vtipob - Vtipib
  VtipibSound = (Vtipib-Vibmiss)*(1-CULL/100)
  VtipbkSound = Vtipbk - Vbkmiss
  if (VtipibSound.lt.0) VtipibSound = 0
  if (VtipbkSound.lt.0) VtipbkSound = 0
  VtipobSound = VtipibSound+VtipbkSound
  VOL(15) = VtipibSound
  !FIA is not using LOGVOL
  if (CTYPE.eq.'I'.or.CTYPE.eq.'B') then
     VOL(4) = Vsawib2*(1-CULL/100)
     VOL(7) = Vtwib2*(1-CULL/100)
     Vsawib = Vsawib2
     Vsawbk = Vsawbk2
     Vtwib = Vtwib2
     Vtwbk = Vtwbk2
     !ELSE
     !    VOL(4) = VOL(4)*(1-CULL/100)*Vmerch/TOTLOGV
     !    VOL(7) = VOL(7)*(1-CULL/100)*Vmerch/TOTLOGV
  end if
  !Apply CULL to VOL(2),VOL(4),VOL(7),VOL(10) calculated in LOGVOL
  VOL(2) = VOL(2)*(1-CULL/100)
  VOL(10) = VOL(10)*(1-CULL/100)
  LOGVOL = LOGVOL*(1-CULL/100)
  VsawibSound = Vsawib*(1-CULL/100)
  VtwibSound = Vtwib*(1-CULL/100)
  if (VsawibSound.lt.0.0) VsawibSound = 0.0
  if (VtwibSound.lt.0.0) VtwibSound = 0.0
  if (VOL(4).lt.0.0) VOL(4) = 0.0
  if (VOL(7).lt.0.0) VOL(7) = 0.0
  ! (7) calculate stem wood weight
  ! CULL is not deducted for Dead trees to avoid double deduction
  if (LIVE.eq.'D') CULL = 0
  Wtotib = Vtotib*WDSG
  WtotibRed = Wtotib*Rrem*(1-CULL/100*(1-CullDenProp))*DenProp
  ! (8) calculate total stem bark weight
  call NVB_Wbk(VOLEQ,DBHOB,HTTOT,Wtotbk,ERRFLG,SPGRPCD,WDSG)
  WtotbkRed = Wtotbk*Rrem*DenProp*RemBkProp
  ! (9) calculate total branches weight
  call NVB_Wbrch(VOLEQ,DBHOB,HTTOT,Wbrch,ERRFLG,SPGRPCD,WDSG)
  WbrchRed = Wbrch*DenProp*RemBrchProp*BrchRem
  ! (10) calculate total above ground biomass
  call NVB_AGB(VOLEQ,DBHOB,HTTOT,AGBpred,ERRFLG,SPGRPCD,WDSG)
  AGBcompRed = WtotibRed + WtotbkRed + WbrchRed
  AGBred = AGBcompRed/(Wtotib+Wtotbk+Wbrch)
  AGBpredRed = AGBpred * AGBred
  AGBdiff = AGBpredRed - AGBcompRed
  WoodR = WtotibRed/AGBcompRed
  BarkR = WtotbkRed/AGBcompRed
  BrchR = WbrchRed/AGBcompRed
  WoodAdd = AGBdiff*WoodR
  BarkAdd = AGBdiff*BarkR
  BrchAdd = AGBdiff*BrchR
  WoodHarm = WtotibRed + WoodAdd
  BarkHarm = WtotbkRed + BarkAdd
  BrchHarm = WbrchRed + BrchAdd

  ! (11) calculate foliage weight for LIVE only
  !IF(LIVE.EQ.'L')THEN
  call NVB_Wfo(VOLEQ,DBHOB,HTTOT,Wfol,ERRFLG,SPGRPCD,WDSG)
  WfolRem = Wfol*BrchRem
  !ENDIF
  if (LIVE.eq.'D') WfolRem = 0
  ! (12) Adjust wood and bark density
  WDSGadj = WoodHarm/VtotibSound
  BKSGadj = BarkHarm/VtotbkSound
  ! (13) calculate weights for primary(saw), secondary(topwood) and tip using the adjusted density
  Wsawib = VsawibSound*WDSGadj
  Wsawbk = Vsawbk*BKSGadj
  Wtwib = VtwibSound*WDSGadj
  Wtwbk = Vtwbk*BKSGadj
  if (Wtwbk.lt.0.0) Wtwbk = 0.0
  Wtipib = VtipibSound*WDSGadj
  Wtipbk = VtipbkSound*BKSGadj
  Wstumpib = VstumpibSound*WDSGadj
  Wstumpbk = Vstumpbk*BKSGadj
  ! (14) add weight to DRYBIO and GRMBIO
  DRYBIO(1) = AGBpredRed
  DRYBIO(2) = WoodHarm
  DRYBIO(3) = BARKHarm
  DRYBIO(4) = Wstumpib
  DRYBIO(5) = Wstumpbk
  DRYBIO(6) = Wsawib
  DRYBIO(7) = Wsawbk
  DRYBIO(8) = Wtwib
  DRYBIO(9) = Wtwbk
  DRYBIO(10) = Wtipib
  DRYBIO(11) = Wtipbk
  DRYBIO(12) = BrchHarm
  DRYBIO(13) = WfolRem
  DRYBIO(14) = AGBpredRed-Wstumpib-Wstumpbk-Wsawib-Wsawbk&
       &-Wtwib-Wtwbk
  !Calculate carbon
  if (LIVE.eq.'D'.and.DECAYCD.gt.0) then
     CF = DeadCF
  end if
  !Round CF to 3 decimals
  CF = ANINT(CF*1000)/1000
  DRYBIO(15) = DRYBIO(1)*CF
  ! (15) get the regional green weight factor to calculate green biomass (GRNBIO)
  !SPREGNWF = 0
  !CALL GetRegnWF(REGN,FORST,SPCD,SPREGNWF)
  !IF(SPREGNWF.GT.0) THEN
  !    GRNWF = SPREGNWF
  !ELSE
  !    GRNWF = SPGRNWF
  !ENDIF
  GRNWF = SPGRNWF
  call GetRegnWF(REGN,FORST,SPCD,GRNWF,DeadWF,PROD)
  DRYWF = (WoodHarm+BarkHarm)/Vtotib
  if (DRYWF.lt.10) then
     DRYWF = SPDRYWF
  end if
  !MC = (GRNWF-DRYWF)/DRYWF
  if (LIVE.eq.'L') then
     MC = (GRNWF-DRYWF)/DRYWF
  else
     MC = (DeadWF-DRYWF)/DRYWF
  end if
  GRNBIO = DRYBIO*(1+MC)
  !20240626 Add calculate cord volume
  VOL(6) = VOL(4)/90
  if (REGN.eq.3.or.REGN.eq.8.or.REGN.eq.9) VOL(6) = VOL(4)/79
  VOL(6) = ANINT(VOL(6)*1000)/1000
  !Set the log weight to LOGVOL for CTYPE ='C' (Cruise only)
  if (CTYPE.eq.'C') then
     WtFac2 = GRNWF
     if (NOLOGS.gt.0) then
        PROD2 = '20'
        call GetRegnWF(REGN,FORST,SPCD,WtFac2,DeadWF,PROD2)
     end if
     if (LIVE.eq.'L') then
        call CruiseLogWt(LOGVOL,NOLOGP,NOLOGS,GRNWF,WtFac2)
     else
        call CruiseLogWt(LOGVOL,NOLOGP,NOLOGS,DeadWF,DeadWF)
     end if
  end if
  !Set errflag for tree with more than 20 logs (2025/06/27)
  if (ERRFLG2.gt.0.and.ERRFLG.eq.0) ERRFLG = ERRFLG2
  return
end

  ! Total stem inside bark volume from TableS1
subroutine NVB_Vib(VOLEQ,DBHOB,HTTOT,Vib,ERRFLG,SPGRPCD,WDSG)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: ERRFLG,SPGRPCD
  real    :: DBHOB,HTTOT,Vib,WDSG
  ! Common blocks
  include "tables1.inc"

  call CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl1Cnt,SPcoef,JKcoef,Vib,ERRFLG,&
       &SPGRPCD,WDSG)
  return
end

  ! Total stem bark volume from TableS2
subroutine NVB_Vbk(VOLEQ,DBHOB,HTTOT,Vbk,ERRFLG,SPGRPCD,WDSG)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: ERRFLG,SPGRPCD
  real    :: DBHOB,HTTOT,Vbk,WDSG
  ! Common blocks
  include "tables2.inc"

  call CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl2Cnt,SPcoef,JKcoef,Vbk,ERRFLG,&
       &SPGRPCD,WDSG)
  return
end

  ! Total stem outside bark volume from TableS3
subroutine NVB_Vob(VOLEQ,DBHOB,HTTOT,Vob,ERRFLG,SPGRPCD,WDSG)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: ERRFLG,SPGRPCD
  real    :: DBHOB,HTTOT,Vob,WDSG
  ! Common blocks
  include "tables3.inc"

  call CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl3Cnt,SPcoef,JKcoef,Vob,ERRFLG,&
       &SPGRPCD,WDSG)
  return
end

  ! Stem height to a top diameter outside bark
subroutine NVB_HT2TOPDob(VOLEQ,DBHOB,HTTOT,Vob,TOPD,HT2,ERRFLG,&
     &SPGRPCD,WDSG)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: ERRFLG,SPGRPCD
  real    :: DBHOB,HTTOT,Vob,TOPD,HT2,a,b,WDSG
  ! Common blocks
  include "tables4.inc"
  HT2 = 0
  call NVB_GetRatioCOEF(VOLEQ,Tbl4Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
  if (Vob.le.0) then
     call NVB_Vob(VOLEQ,DBHOB,HTTOT,Vob,ERRFLG,SPGRPCD,WDSG)
  end if
  call NVB_CalcHT2TOPD(Vob,a,b,HTTOT,TOPD,HT2)
  !Added HT2 floor to 4.5 for merch height (2025/05/02)
  if (HT2.lt.5.and.TOPD.lt.DBHOB) HT2 = 5
  return
end

  ! Stem height to a top diameter inside bark
subroutine NVB_HT2TOPDib(VOLEQ,DBHOB,HTTOT,Vib,TOPD,HT2,ERRFLG,&
     &SPGRPCD,WDSG)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: ERRFLG,SPGRPCD
  real    :: DBHOB,HTTOT,Vib,TOPD,HT2,a,b,WDSG,Vbk,Vob
  !INCLUDE "tables5.inc"
  HT2 = 0
  !CALL NVB_GetRatioCOEF(VOLEQ,Tbl5Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
  if (Vib.le.0) then
     call NVB_Vib(VOLEQ,DBHOB,HTTOT,Vib,ERRFLG,SPGRPCD,WDSG)
  end if
  !CALL NVB_CalcHT2TOPD(Vib,a,b,HTTOT,TOPD,HT2)
  call NVB_Vbk(VOLEQ,DBHOB,HTTOT,Vbk,ERRFLG,SPGRPCD,WDSG)
  TOPD = TOPD/sqrt(Vib/(Vib+Vbk))
  call NVB_Vob(VOLEQ,DBHOB,HTTOT,Vob,ERRFLG,SPGRPCD,WDSG)
  call NVB_HT2TOPDob(VOLEQ,DBHOB,HTTOT,Vob,TOPD,HT2,ERRFLG,&
       &SPGRPCD,WDSG)
  return
end

  ! Calc Dia outside bark at a given height
subroutine NVB_DobAtHT(VOLEQ,DBHOB,HTTOT,Vob,HT2,DOB,ERRFLG,&
     &SPGRPCD,WDSG)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: ERRFLG,SPGRPCD
  real    :: DBHOB,HTTOT,Vob,DOB,HT2,a,b,WDSG
  ! Common blocks
  include "tables4.inc"
  DOB = 0
  call NVB_GetRatioCOEF(VOLEQ,Tbl4Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
  if (Vob.le.0) then
     call NVB_Vob(VOLEQ,DBHOB,HTTOT,Vob,ERRFLG,SPGRPCD,WDSG)
  end if
  call NVB_CalcDiaAtHT(Vob,a,b,HTTOT,HT2,DOB)
  return
end

  ! Calc Dia inside bark at a given height
subroutine NVB_DibAtHT(VOLEQ,DBHOB,HTTOT,Vib,HT2,DIB,ERRFLG,&
     &SPGRPCD,WDSG)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: ERRFLG,SPGRPCD
  real    :: DBHOB,HTTOT,Vib,DIB,HT2,a,b,WDSG
  real    :: Vob,Vbk,Ib2ObRatio,DOB
  !INCLUDE "tables5.inc"
  DIB = 0
  !CALL NVB_GetRatioCOEF(VOLEQ,Tbl5Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
  if (Vib.le.0) then
     call NVB_Vib(VOLEQ,DBHOB,HTTOT,Vib,ERRFLG,SPGRPCD,WDSG)
  end if
  !CALL NVB_CalcDiaAtHT(Vib,a,b,HTTOT,HT2,DIB)
  call NVB_Vob(VOLEQ,DBHOB,HTTOT,Vob,ERRFLG,SPGRPCD,WDSG)
  call NVB_Vbk(VOLEQ,DBHOB,HTTOT,Vbk,ERRFLG,SPGRPCD,WDSG)
  Ib2ObRatio = sqrt(Vib/(Vib+Vbk))
  call NVB_DobAtHT(VOLEQ,DBHOB,HTTOT,Vob,HT2,DOB,ERRFLG,&
       &SPGRPCD,WDSG)
  !calculated DOB mutily ratio factor to get DIB
  !this is to avoid DIB > DOB
  DIB = DOB*Ib2ObRatio
  return
end

  ! Total stem bark dry weight from TableS6
subroutine NVB_Wbk(VOLEQ,DBHOB,HTTOT,Wbk,ERRFLG,SPGRPCD,WDSG)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: ERRFLG,SPGRPCD
  real    :: DBHOB,HTTOT,Wbk,WDSG
  ! Common blocks
  include "tables6.inc"

  call CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl6Cnt,SPcoef,JKcoef,Wbk,ERRFLG,&
       &SPGRPCD,WDSG)
  return
end

  ! Total branch dry weight from TableS7
subroutine NVB_Wbrch(VOLEQ,DBHOB,HTTOT,Wbrch,ERRFLG,SPGRPCD,WDSG)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: ERRFLG,SPGRPCD
  real    :: DBHOB,HTTOT,Wbrch,WDSG
  ! Common blocks
  include "tables7.inc"

  call CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl7Cnt,SPcoef,JKcoef,Wbrch,&
       &ERRFLG,SPGRPCD,WDSG)
  return
end

  ! Total above ground biomass from TableS8
subroutine NVB_AGB(VOLEQ,DBHOB,HTTOT,AGB,ERRFLG,SPGRPCD,WDSG)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: ERRFLG,SPGRPCD
  real    :: DBHOB,HTTOT,AGB,WDSG
  ! Common blocks
  include "tables8.inc"

  call CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl8Cnt,SPcoef,JKcoef,AGB,ERRFLG,&
       &SPGRPCD,WDSG)
  return
end

  ! Total foliage dry weight from TableS6
subroutine NVB_Wfo(VOLEQ,DBHOB,HTTOT,Wfo,ERRFLG,SPGRPCD,WDSG)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: ERRFLG,SPGRPCD
  real    :: DBHOB,HTTOT,Wfo,WDSG
  ! Common blocks
  include "tables9.inc"

  call CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl9Cnt,SPcoef,JKcoef,Wfo,ERRFLG,&
       &SPGRPCD,WDSG)
  return
end

  ! Calculate the volume or weight from a given VOLEQ
subroutine CALC_EQVW(VOLEQ,D,H,CoefCnt,SPcoef,JKcoef,VW,ERRFLG,&
     &SPGRPCD,WDSG )
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: I,EQN,ERRFLG,SPCD,SPGRPCD,CoefCnt
  real    :: D,H,VW,WDSG,SPcoef(CoefCnt,13),JKcoef(9,5)
  real    :: a,a0,a1,b,b0,b1,b2,c,c1

  ERRFLG = 0
  EQN = 0
  !WDSG = 0
  if (D.lt.1.0) then
     ERRFLG = 3
     return
  end if
  if (H.lt.5.0) then
     ERRFLG = 4
     return
  end if
  read (VOLEQ(8:10),'(i3)') SPCD
  if (SPCD.gt.0.and.SPCD.lt.10) then
     !SPCD is Jenkins group code
     call JKEQCOEF(SPCD,JKcoef,EQN,a,b,c)
     !CALL JKSPGRP(SPCD, SPGRPCD, WDSG)
     call CalcVOLWT(SPCD,D,H,EQN,a,a0,a1,b,b0,b1,b2,c,c1,WDSG,VW)
     return
  end if
  call SPEQCOEF(VOLEQ,CoefCnt,SPcoef,SPCD,EQN,&
       &a,a0,a1,b,b0,b1,b2,c,c1)
  if (EQN.eq.0) then
     !If the equation is not in the list, then using Jenkins group equation
     !CALL JKSPGRP(SPCD, SPGRPCD, WDSG)
     call JKEQCOEF(SPGRPCD,JKcoef,EQN,a,b,c)
     if (EQN.eq.0) then
        !No species match, then use species 999
        call OTEQCOEF(CoefCnt,SPcoef,EQN,a,a0,a1,b,b0,b1,b2,c,c1)
     end if
  end if
  !Calculate total inside bark volume
  call CalcVOLWT(SPCD,D,H,EQN,a,a0,a1,b,b0,b1,b2,c,c1,WDSG,VW)
  return
end

  ! Search FIA_REF_Species to get species Jenkins SPGRPCD
subroutine JKSPGRP(SPCD, SPGRPCD)
  implicit none
  ! Variables
  integer :: SPCD,SPGRPCD,FIRST,LAST,HALF,DONE
  !REAL WDSG, CF
  ! Common blocks
  include 'wdbkwtdata.inc'

  FIRST = 1
  LAST = CNT999
  if (SPCD.gt.999) then
     FIRST = CNT999+1
     LAST = TOTSPC
  end if
  DONE = 0
  SPGRPCD = 0
  !CF = 0
  !WDSG = 0
  if (SPCD.lt.10) then
     SPGRPCD = SPCD
     return
  end if
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
  if (DONE.gt.0) then
     SPGRPCD = WDBKWT(DONE,3)
  else
     SPGRPCD = WDBKWT(CNT999,3)
  end if
  return
end

  ! Get Jenkins group equation coefficient
subroutine JKEQCOEF(SPGRPCD,JKCOEF,equation,a,b,c)
  implicit none
  ! Variables
  integer :: SPGRPCD,equation,DONE,I
  real    :: JKCOEF(9,5),a,b,c

  equation = 0
  DONE = 0
  do I = 1, 9
     if (JKCOEF(I,1).eq.SPGRPCD) then
        DONE = I
        exit
     end if
  end do
  if (DONE.gt.0) then
     equation = JKCOEF(DONE,2)
     a = JKCOEF(DONE,3)
     b = JKCOEF(DONE,4)
     c = JKCOEF(DONE,5)
  end if
  return
end

  ! Calculate volume or weight from the equations
subroutine CalcVOLWT(SPCD,D,H,equation,a,a0,a1,b,b0,b1,b2,c,c1,&
     &WDSG,VOLWT)
  implicit none
  ! Variables
  integer :: equation,k,SPCD
  real    :: D,H,a,a0,a1,b,b0,b1,b2,c,c1,WDSG,VOLWT
  VOLWT = 0
  if (equation.gt.0) then
     if (equation.eq.1) then
        VOLWT = a*D**b*H**c
     else if (equation.eq.2) then
        k = 11
        if (SPCD.lt.300) k = 9
        if (D.lt.k) then
           VOLWT = a0*D**b0*H**c
        else
           VOLWT = a0*k**(b0-b1)*D**b1*H**c
        end if
     else if (equation.eq.3) then
        VOLWT = a*D**(a1*(1-EXP(-b1*D))**c1)*H**c
     else if (equation.eq.4) then
        VOLWT = a*D**b*H**c*EXP(-(b2*D))
     else if (equation.eq.5) then
        VOLWT = a*D**b*H**c*WDSG/62.4
     end if
  end if
  return
end

  ! Calculate volme ratio from Table S4 or S5
subroutine CalcRatio(H,h1,equation,a,b,R)
  implicit none
  ! Variables
  integer :: equation
  real    :: H,h1,a,b,R
  R = 0
  if (equation.eq.6.and.(h1.gt.0.and.h1.le.H)) then
     R = (1-(1-h1/H)**a)**b
  end if
  return
end

  ! Get the equation coefficients for SPCD
subroutine SPEQCOEF(VOLEQ,TOTROW,SPCOEF,SPCD,&
     &equation,a,a0,a1,b,b0,b1,b2,c,c1)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: SPCD,DIVISION,STDORG,TOTROW,equation,I,DONE
  real    :: SPCOEF(TOTROW,13),a,a0,a1,b,b0,b1,b2,c,c1

  STDORG = 0
  equation = 0
  DONE = 0
  read (VOLEQ(8:10),'(i3)') SPCD
  do I = 5, 7
     if (VOLEQ(I:I).lt.'0'.or.VOLEQ(I:I).gt.'9') then
        VOLEQ(I:I) = '0'
     end if
  end do
  read (VOLEQ(5:6), '(i2)') DIVISION
  DIVISION = DIVISION*10
  if (VOLEQ(4:4).eq.'M') DIVISION = DIVISION+1000
  if (LEN_TRIM(VOLEQ).eq.11.and.VOLEQ(11:11).eq.'P') STDORG = 1
  do I = 1, TOTROW
     if (SPCOEF(I,1).eq.SPCD) then
        if (SPCOEF(I,2).eq.DIVISION.and.SPCOEF(I,3).eq.STDORG) then
           DONE = I
           exit
        else if (SPCOEF(I,2).eq.0.and.SPCOEF(I,3).eq.STDORG) then
           DONE = I
           exit
        end if
     end if
  end do
  if (DONE.gt.0) then
     equation = SPCOEF(DONE,4)
     a = SPCOEF(DONE,5)
     a0 = SPCOEF(DONE,6)
     a1 = SPCOEF(DONE,7)
     b = SPCOEF(DONE,8)
     b0 = SPCOEF(DONE,9)
     b1 = SPCOEF(DONE,10)
     b2 = SPCOEF(DONE,11)
     c = SPCOEF(DONE,12)
     c1 = SPCOEF(DONE,13)
  end if
  return
end

  ! Get the coef for species 999
subroutine OTEQCOEF(TOTROW,SPCOEF,equation,&
     &a,a0,a1,b,b0,b1,b2,c,c1)
  implicit none
  ! Variables
  integer :: TOTROW,equation
  real    :: SPCOEF(TOTROW,13),a,a0,a1,b,b0,b1,b2,c,c1
  equation = SPCOEF(TOTROW,4)
  a = SPCOEF(TOTROW,5)
  a0 = SPCOEF(TOTROW,6)
  a1 = SPCOEF(TOTROW,7)
  b = SPCOEF(TOTROW,8)
  b0 = SPCOEF(TOTROW,9)
  b1 = SPCOEF(TOTROW,10)
  b2 = SPCOEF(TOTROW,11)
  c = SPCOEF(TOTROW,12)
  c1 = SPCOEF(TOTROW,13)
  return
end

  ! Calculate height to a given TOPD
  ! For TOPD outside bark, input TCUFT outside bark and outside bark ratio coef (a,b)
  ! For TOPD inside bark, input TCUFT inside bark and inside bark coef (a,b)
subroutine NVB_CalcHT2TOPD(TCUFT,a,b,HTTOT,TOPD,HT2)
  implicit none
  ! Variables
  real    :: HTTOT,TOPD,HT2,HTLOW
  real    :: low,hi,diff,X,a,b,TCUFT,mid
  integer :: loopcnt

  HT2 = 0
  low = 0
  hi = HTTOT
  diff = 1.0
  loopcnt = 0
  do while (ABS(diff).gt.0.001)
     mid = (low+hi)/2
     !Stop calculation when mid < stump ht (2025/10/21)
     if (mid.lt.0.5) then
        mid = 0
        exit
     end if
     X = mid/HTTOT
     diff = TOPD - ((TCUFT/0.005454154/HTTOT*a*b*&
          &(1-X)**(a-1)*(1-(1-X)**a)**(b-1)))**0.5
     if (ABS(diff).lt.0.001) then
        exit
     end if
     if (diff.lt.0.0) then
        low = mid
     else
        hi = mid
     end if
     loopcnt = loopcnt +1
     if (loopcnt.gt.1000) exit
  end do
  HT2 = mid
  return
end

  ! Calculate the diameter at a given height
  ! For outside bark diameter, input TCUFT outside bark and outside bark ratio coef (a,b)
  ! For inside bark diameter, input TCUFT inside bark and inside bark coef (a,b)
subroutine NVB_CalcDiaAtHT(TCUFT,a,b,THT,HT2,DIA)
  implicit none
  ! Variables
  real    :: HT2,DIA,a,b,TCUFT,THT

  DIA = 0
  if (HT2.ge.THT) return
  DIA = SQRT((TCUFT/ 0.005454154 / THT) * (a * b *&
       &(1-HT2/THT)**(a-1)*(1-(1 - HT2/THT)**a)**(b-1)))
  return
end

subroutine NVB_CalcDibAtHT(Vtotob,aOB,bOB,THT,HT2,DIB,Ib2ObRatio)
  implicit none
  ! Variables
  real    :: HT2,DIA,aOb,bOB,TCUFT,THT,DOB,DIB,Ib2ObRatio,Vtotob

  DOB = 0
  if (HT2.ge.THT) return
  DOB = SQRT((Vtotob/ 0.005454154 / THT) * (aOB * bOB *&
       &(1-HT2/THT)**(aOB-1)*(1-(1 - HT2/THT)**aOB)**(bOB-1)))
  DIB = DOB*Ib2ObRatio
  return
end

  ! Get ratio coef alpha (a) and belta (b) for the VOLEQ
subroutine NVB_GetRatioCOEF(VOLEQ,COEFCNT,SPCOEF,JKCOEF,a,b,&
     &SPGRPCD)
  ! Variables
  character(len=11) :: VOLEQ
  integer :: COEFCNT,EQN,SPCD,SPGRPCD
  real    :: SPCOEF(COEFCNT,13),JKCOEF(9,5),WDSG
  real    :: a,a0,a1,b,b0,b1,b2,c,c1

  call SPEQCOEF(VOLEQ,COEFCNT,SPCOEF,SPCD,EQN,&
       &a,a0,a1,b,b0,b1,b2,c,c1)
  if (EQN.eq.0) then
     !If the equation is not in the list, then using Jenkins group equation
     !CALL JKSPGRP(SPCD, SPGRPCD)
     call JKEQCOEF(SPGRPCD,JKCOEF,EQN,a,b,c)
     if (EQN.eq.0) then
        !No species match, then use species 999
        call OTEQCOEF(COEFCNT,SPCOEF,EQN,a,a0,a1,b,b0,b1,b2,c,c1)
     end if
  end if
  return
end

subroutine NVB_GetRatioCOEFob(VOLEQ,SPGRPCD,aOB,bOB)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  integer :: EQN,SPGRPCD,SPCD
  real    :: aOB,bOB
  real    :: a,a0,a1,b,b0,b1,b2,c,c1
  ! Common blocks
  include "tables4.inc"
  call SPEQCOEF(VOLEQ,Tbl4CNT,SPCOEF,SPCD,EQN,&
       &a,a0,a1,b,b0,b1,b2,c,c1)
  if (EQN.eq.0) then
     !If the equation is not in the list, then using Jenkins group equation
     call JKEQCOEF(SPGRPCD,JKCOEF,EQN,a,b,c)
     if (EQN.eq.0) then
        !No species match, then use species 999
        call OTEQCOEF(Tbl4CNT,SPCOEF,EQN,a,a0,a1,b,b0,b1,b2,c,c1)
     end if
  end if
  aOB = a
  bOB = b
  return
end

  ! Calculate LOGDIA, LOGVOL, BOLHT and VOL
  ! Added input parameter Ib2ObRatio
  ! calculation use outside bark coefficient a and b, then DOB is converted to DIB using ratio
subroutine NVB_CalcLOGVOL(LOGST,NUMSEG,DIBL,HT2,TCUFT,TRIM,THT,&
     &LOGLEN,LOGDIA,LOGVOL,BOLHT,VOL,COR,a,b, Ib2ObRatio,TOTLOGV)
  implicit none
  ! Variables
  integer :: LOGST,NUMSEG,I
  real    :: DIBL,HT2,TCUFT,TRIM,DIB,a,b,THT,DIBS,LOGCV,LENTH,LOGV,BFINT
  real    :: LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3),BOLHT(21),VOL(15)
  real    :: Ib2ObRatio,DOB,TOTLOGV,LOGVT
  character(len=1) :: COR

  do I = 1+LOGST,NUMSEG+LOGST
     HT2 = HT2+TRIM+LOGLEN(I)
     call NVB_CalcDiaAtHT(TCUFT,a,b,THT,HT2,DOB)
     DIB = DOB*Ib2ObRatio
     LOGDIA(I+1,2) = DIB
     LOGDIA(I+1,1) = NINT(DIB)
     BOLHT(I+1) = HT2
     DIBS = LOGDIA(I+1,1)
     !LOGVT = .00272708*(DIBL*DIBL+DIBS*DIBS)*(LOGLEN(I)+TRIM)
     !TOTLOGV = TOTLOGV+LOGVT
     LOGCV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LOGLEN(I)
     TOTLOGV = TOTLOGV+LOGCV
     LOGVOL(4,I) = ANINT(LOGCV*10)/10
     DIBL = LOGDIA(I+1,1)
     !Calculate boardfoot volume
     DIB = LOGDIA(I+1,1)
     LENTH = LOGLEN(I)
     call SCRIB (DIB,LENTH,COR,LOGV)
     if (cor.eq.'Y') then
        LOGVOL(1,I) = LOGV * 10
     else
        LOGVOL(1,I) = ANINT(LOGV)
     end if
     call INTL14(DIB,LENTH,BFINT)
     LOGVOL(7,I) = BFINT
     if (LOGST.gt.0) then
        VOL(7) = VOL(7)+LOGVOL(4,I)
        VOL(12) = VOL(12) + LOGVOL(1,I)
     else
        VOL(4) = VOL(4)+LOGVOL(4,I)
        VOL(2) = VOL(2) + LOGVOL(1,I)
        VOL(10) = VOL(10) + BFINT
     end if
     500 continue
  end do

  return
end

  ! Estimate total height for a broken top tree
subroutine NVB_ESTTHT(VOLEQ,DBHOB,BRKHT,BRKHTD,HTTOT,ERRFLG,&
     &SPGRPCD,WDSG)
  implicit none
  ! Variables
  character(len=11) :: VOLEQ
  real    :: DBHOB,BRKHT,BRKHTD,HTTOT
  integer :: ERRFLG,I,SPGRPCD
  ! Common blocks
  include 'tables4.inc'
  real    :: TCUFT,a,b,THT,diff,X,WDSG
  !Height to top broken and diameter at broken point are required
  if (BRKHT.le.0.or.BRKHTD.le.0) then
     ERRFLG = 9
     return
  end if
  if (BRKHTD.le.1.0) then
     HTTOT = BRKHT + 1.0
     return
  end if
  !Get the outbark ratio coefficient
  call NVB_GetRatioCOEF(VOLEQ,Tbl4Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
  THT = BRKHT
  I = 3
  diff = 1
  do while (I < 300)
     THT = THT + 1.0
     call NVB_Vob(VOLEQ,DBHOB,THT,TCUFT,ERRFLG,SPGRPCD,WDSG)
     X = BRKHT/THT
     diff = BRKHTD - ((TCUFT/0.005454154/THT*a*b*&
          &(1-X)**(a-1)*(1-(1-X)**a)**(b-1)))**0.5
     if (diff.lt.0.05) then
        exit
     end if
     I = I+1
  end do
  HTTOT = THT
  return
end

  ! Get species carbon fraction
subroutine NVB_CarbonFrac(SPCD,CF)
  implicit none
  ! Variables
  integer :: SPCD,FIRST,LAST,DONE,HALF
  real    :: CF
  ! Common blocks
  include 'tables10.inc'
  DONE = 0
  FIRST = 1
  LAST = Tbl10Cnt
  do while (DONE.eq.0)
     HALF = (LAST - FIRST +1)/2 + FIRST
     if (SPCF(HALF,1).eq.SPCD) then
        DONE = HALF
     else if (FIRST .eq. LAST) then
        DONE = -1
     else if (SPCF(HALF,1).lt.SPCD) then
        FIRST = HALF
     else
        LAST = HALF - 1
     end if
  end do
  if (DONE.le.0) DONE = Tbl10Cnt
  CF = SPCF(DONE,2)/100
  return
end

  ! Get average Crown ration
subroutine NVB_AvgCR(DIVISION,SPCD,CR)
  implicit none
  ! Variables
  integer :: DIVISION,SPCD,I,DONE
  real    :: CR
  ! Common blocks
  include 'tables11.inc'
  !DIVCR = DIVCRh
  if (SPCD.lt.300) then
     DIVCR = DIVCRs
  else
     DIVCR = DIVCRh
  end if
  if (DIVISION.eq.0) then
     CR = DIVCR(Tbl11Cnt,2)/100
     return
  end if
  DONE = 0
  do I = 1, Tbl11Cnt
     if (DIVCR(I,1).eq.DIVISION) then
        DONE = I
        exit
     end if
  end do
  if (DONE.eq.0) DONE = Tbl11Cnt
  CR = DIVCR(DONE,2)/100
  return
end

  ! Calculate branches remaining portion for broken top tree
  ! CR is in decimal, not percent
  ! BrchRem is the remaining portion of the crown in decimal, not percent
subroutine NVB_BrchRem(DIVISION,SPCD,BRKHT,HTTOT,CR,BrchRem)
  implicit none
  ! Variables
  integer :: DIVISION,SPCD
  real    :: BRKHT,HTTOT,CR,BrchRem,CrownLenRem,CRh,CrownLen
  BrchRem = 1
  if (CR.gt.0) then
     if (CR.ge.1) CR = CR/100
     !CrownLen = BRKHT*CR
     !BrchRem = CrownLen/(CrownLen+HTTOT-BRKHT)
     CRh = (HTTOT-BRKHT*(1-CR))/HTTOT
     BrchRem = (BRKHT-HTTOT*(1-CRh))/(HTTOT*CRh)
  else
     call NVB_AvgCR(DIVISION,SPCD,CRh)
     if (CRh.ge.1) CRh = CRh/100
     CrownLen = HTTOT*CRh
     BrchRem = (CrownLen - (HTTOT-BRKHT))/CrownLen
  end if
  if (BrchRem.le.0.01) BrchRem = 0
  return
end

  ! Get EcoProvince for a given national forest district
subroutine NVB_EcoProv(REGN, FORST, DIST, iPROV)
  implicit none
  ! Variables
  integer :: REGN,iPROV,DistNum,iFORST,iDist,ForstNum
  character(len=2) :: FORST
  character(len=2) :: DIST
  ! Common blocks
  include 'dist_ecoprov.inc'
  integer :: DONE,FIRST,LAST,HALF,I
  iPROV = 0
  if (FORST(2:2) .lt. '0') then
     FORST(2:2) = FORST(1:1)
     FORST(1:1) = '0'
     if (FORST(2:2) .lt. '0') FORST(2:2) = '0'
  end if
  read (FORST,'(i2)') iFORST
  if (DIST(2:2) .lt. '0') then
     DIST(2:2) = DIST(1:1)
     DIST(1:1) = '0'
     if (DIST(2:2) .lt. '0') DIST(2:2) = '0'
  end if
  read (DIST,'(i2)') iDIST
  DistNum = REGN*10000 + iFORST*100 + iDIST
  DONE = 0
  FIRST = 1
  LAST = DistCnt
  do while (DONE.eq.0)
     HALF = (LAST - FIRST +1)/2 + FIRST
     if (DistProv(HALF,1).eq.DistNum) then
        DONE = HALF
     else if (FIRST .eq. LAST) then
        DONE = -1
     else if (DistProv(HALF,1).lt.DistNum) then
        FIRST = HALF
     else
        LAST = HALF - 1
     end if
  end do
  if (DONE.gt.0) then
     iPROV = DistProv(DONE,2)
  else
     !Checking forest default EcoProv
     ForstNum = REGN*100+iFORST
     DONE = 0
     FIRST = 1
     LAST = ForstCnt
     do while (DONE.eq.0)
        HALF = (LAST - FIRST +1)/2 + FIRST
        if (ForstProv(HALF,1).eq.ForstNum) then
           DONE = HALF
        else if (FIRST .eq. LAST) then
           DONE = -1
        else if (ForstProv(HALF,1).lt.ForstNum) then
           FIRST = HALF
        else
           LAST = HALF - 1
        end if
     end do
     if (DONE.gt.0) then
        iPROV = ForstProv(DONE,2)
     else
        !Check Regional default EcoProv
        DONE = 0
        I = 0
        do while (DONE.eq.0)
           I = I + 1
           if (RegnProv(I,1).eq.REGN) then
              DONE = I
           else if (I.eq.RegnCnt) then
              DONE = -1
           end if
        end do
        if (DONE.gt.0) iPROV = RegnProv(DONE,2)
     end if
  end if
  return
end

  ! Get default for a species in a given national forest district
subroutine NVB_DefaultEq(REGN,FORST,DIST,SPCD,NVBEQ,ERRFLG)
  implicit none
  ! Variables
  integer :: REGN,SPCD,ERRFLG,V_SPCD
  character(len=10) :: NVBEQ
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=3) :: SPC
  integer :: iPROV,I,DONE,SPCDFIND,SPGRPCD,SFTHRD
  character(len=4) :: PROV
  real    :: WDSG,CF,SPGRNWF,SPDRYWF
  ! Common blocks
  include "tables1.inc"

  ERRFLG = 0
  V_SPCD = SPCD
  call NVB_EcoProv(REGN,FORST,DIST,iPROV)
  iPROV = (iPROV/10)*10
  call NVB_RefSpcData(SPCD,SPGRPCD,WDSG,SFTHRD,CF,ERRFLG,&
       &SPGRNWF,SPDRYWF)
  if (ERRFLG.gt.0) then
     ERRFLG = 6
     return
  end if
  if (SPGRPCD.eq.0.or.SPGRPCD.eq.10) then
     ERRFLG = 1
     return
  end if

  !Check if the SPCD has a equation for the DIVISION
  !IF(SPCD.GT.999) SPCD = 999
  !DONE = 0
  !SPCDFIND = 0
  !DO I = 1, Tbl1Cnt
  !    IF(SPCOEF(I,1).EQ.SPCD)THEN
  !        SPCDFIND = 1
  !        IF(SPCOEF(I,2).EQ.iPROV.OR.SPCOEF(I,2).EQ.0)THEN
  !            DONE = I
  !            IF(SPCOEF(I,2).EQ.0) iPROV = 0
  !            EXIT
  !        ENDIF
  !    ENDIF
  !END DO
  !When no equation for the SPCD, set iPROV to 0 and check Jenkin's species group
  !Not set default equation for Jenkins coef. It causes error for biomass calc for some species.
  !      IF(DONE.EQ.0)THEN
  !          iPROV = 0
  !          CALL NVB_RefSpcData(SPCD,SPGRPCD,WDSG,SFTHRD,CF,ERRFLG,
  !     &      SPGRNWF,SPDRYWF)
  !          IF(ERRFLG.GT.0) RETURN
  !          IF(SPGRPCD.EQ.0.OR.SPGRPCD.EQ.10)THEN
  !              ERRFLG = 1
  !              RETURN
  !          ENDIF
  !          V_SPCD = SPGRPCD
  !      ENDIF
  write (PROV, '(I4)') iPROV
  if (iPROV.eq.0) PROV = '0000'
  if (iPROV.gt.999) then
     NVBEQ = 'NVBM'//PROV(2:4)
  else
     NVBEQ = 'NVB0'//PROV(2:4)
  end if

  write (SPC, '(I3)') V_SPCD
  if (V_SPCD.lt.10) then
     NVBEQ = NVBEQ(1:7)//'00'//SPC(3:3)
  else if (V_SPCD.lt.100) then
     NVBEQ = NVBEQ(1:7)//'0'//SPC(2:3)
  else if (V_SPCD.lt.1000) then
     NVBEQ = NVBEQ(1:7)//SPC(1:3)
  else
     ERRFLG = 1
  end if
  return
end

  ! Search FIA_REF_Species to get species Jenkins SPGRPCD and WDSG (wood density lb/cf), carbon ratio, soft or hard wood
subroutine NVB_RefSpcData(SPCD,SPGRPCD,WDSG,SFTHRD,CF,ERRFLG,&
     &SPGRNWF,SPDRYWF)
  implicit none
  ! Variables
  integer :: SPCD,SPGRPCD,FIRST,LAST,HALF,DONE,ERRFLG,SFTHRD
  real    :: WDSG,CF,SPGRNWF,SPDRYWF
  ! Common blocks
  include 'wdbkwtdata.inc'

  FIRST = 1
  LAST = CNT999
  ERRFLG = 0
  if (SPCD.lt.10) then
     ERRFLG = 6
     return
  end if
  if (SPCD.gt.999) then
     FIRST = CNT999+1
     LAST = TOTSPC
  end if
  DONE = 0
  SPGRPCD = 0
  CF = 0
  WDSG = 0
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
  if (DONE.gt.0) then
     SFTHRD = WDBKWT(DONE,2)
     SPGRPCD = WDBKWT(DONE,3)
     WDSG = WDBKWT(DONE,4)
     CF = WDBKWT(DONE,12)
     SPGRNWF = WDBKWT(DONE,9)
     SPDRYWF = WDBKWT(DONE,10)
  else
     ! no species match, use species 999 for calc
     ERRFLG = 6
     SFTHRD = WDBKWT(CNT999,2)
     SPGRPCD = WDBKWT(CNT999,3)
     WDSG = WDBKWT(CNT999,4)
     CF = WDBKWT(CNT999,12)
     SPGRNWF = WDBKWT(CNT999,9)
     SPDRYWF = WDBKWT(CNT999,10)
  end if
  if (WDSG.eq.0) WDSG = WDBKWT(CNT999,4)
  if (SPGRNWF.eq.0) SPGRNWF = WDBKWT(CNT999,9)
  if (SPDRYWF.eq.0) SPDRYWF = WDBKWT(CNT999,10)
  return
end

  !get species regional weight factor
subroutine GetRegnWF(REGN,FORST,SPCD,WtFac,DeadWF,PROD)
  implicit none
  ! Variables
  integer :: REGN,SPCD,DONE,I,IFORST
  character(len=2) :: FORST
  character(len=2) :: PROD
  real    :: WtFac,DeadWF,WtFac2
  ! Common blocks
  include 'regndftdata.inc'
  integer :: SPGRPCD,ERRFLG,SFTHRD
  real    :: WDSG,CF,SPGRNWF,SPDRYWF
  DONE = 0
  I = 0
  WtFac = 0
  WtFac2 = 0
  DeadWF = 0
  read(FORST,'(i2)') IFORST
  if (REGN.eq.0) DONE = -1
  do while (DONE.eq.0)
     I = I + 1
     if (SPREGNDFTWF(I,1).eq.REGN) then
        if ((SPREGNDFTWF(I,2).eq.IFORST .and.&
             &SPREGNDFTWF(I,3).eq.SPCD) .or.&
             &(SPREGNDFTWF(I,2).eq.0.and.&
             &SPREGNDFTWF(I,3).eq.SPCD)) then
           DONE = I
           WtFac = SPREGNDFTWF(I,4)
           WtFac2 = SPREGNDFTWF(I,5)
           DeadWF = SPREGNDFTWF(I,7)
        end if
     end if
     if (I.ge.TOTDFT.and.DONE.eq.0) DONE = -1
  end do
  if (WtFac2.eq.0) WtFac2 = WtFac
  !For Non-saw product using the non-saw/secondary weightfactor
  !IF(REGN.EQ.1.AND.PROD.NE.'01') WtFac = WtFac2
  !IF(REGN.EQ.5.AND.PROD.EQ.'20') WtFac = WtFac2
  !YW 2025/02/19
  if (PROD.ne.'01') WtFac = WtFac2
  !Species does not have a regional default, get green weight from ref_species
  if (DONE.eq.-1.and.WtFac.lt.1) then
     call NVB_RefSpcData(SPCD,SPGRPCD,WDSG,SFTHRD,CF,ERRFLG,&
          &SPGRNWF,SPDRYWF)
     WtFac = SPGRNWF
  end if
  !Dead weight factor is not defined by region, using regional average
  if (DeadWF.lt.1) then
     if (REGN.eq.1) then
        DeadWF = WtFac*0.6749
     else if (REGN.eq.2) then
        DeadWF = WtFac*0.6381
     else if (REGN.eq.4) then
        DeadWF = WtFac*0.6113
     else if (REGN.eq.5) then
        DeadWF = WtFac*0.8254
     else if (REGN.eq.7) then
        DeadWF = WtFac*0.7951
     else
        DeadWF = WtFac*0.7036
     end if
  end if
  return
end

subroutine DecayDenProp(SFTHRD,DECAYCD,DenProp,DeadCF)
  implicit none
  ! Variables
  integer :: SFTHRD,DECAYCD
  real    :: DenProp,DeadCF

  if (SFTHRD.eq.0) then
     if (DECAYCD.eq.1) then
        DenProp = 0.97
        DeadCF = 0.501
     else if (DECAYCD.eq.2) then
        DenProp = 1
        DeadCF = 0.504
     else if (DECAYCD.eq.3) then
        DenProp = 0.92
        DeadCF = 0.506
     else if (DECAYCD.eq.4) then
        DenProp = 0.55
        DeadCF = 0.52
     else if (DECAYCD.eq.5) then
        DenProp = 0.55
        DeadCF = 0.527
     end if
  else
     if (DECAYCD.eq.1) then
        DenProp = 0.99
        DeadCF = 0.47
     else if (DECAYCD.eq.2) then
        DenProp = 0.8
        DeadCF = 0.473
     else if (DECAYCD.eq.3) then
        DenProp = 0.54
        DeadCF = 0.481
     else if (DECAYCD.eq.4) then
        DenProp = 0.43
        DeadCF = 0.48
     else if (DECAYCD.eq.5) then
        DenProp = 0.43
        DeadCF = 0.472
     end if
  end if
  return
end
