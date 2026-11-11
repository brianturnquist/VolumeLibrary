!=============================!
!   VolumeLibrary : volinit   !
!=============================!
subroutine VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
     &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
     &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
     &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
     &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
     &BA,SI,CTYPE,ERRFLAG,IDIST)

  !...  Prepare parameters and data for call to appropriate volume
  !...  equations.

  use CHARMOD
  use DEBUG_MOD
  use VOLINPUT_MOD
  implicit none

  !REV  Created TDH 04/01/09
  !REV  Revised TDH 11/18/09
  !REV  YW 08/23/12 added stump and tip volume calc and saved in vol(14) and vol(15)
  !REV  YW 01/18/2013 modified stump and tip calculation logic. Here is the calculation from FIA.
  !     If volume equation does not calculate stump and tip, they will ba calculated here.
  !REV  YW 05/16/2013 set HTTYPE to F if total height is entered
  !REV  YW 02/27/2014 For region 6 Behre equation, if merch height is entered in feet, convert it to log ht
  !REV  YW 07/30/2014 For region 6 Behre equation, if merch height is entered in feet, use log rules to convert to log ht.
  !     YW 2016/01/13 Added BTR default value for Region 3 Santa Fe forest DF and PP
  !     YW 04/19/2016 Added input variable DIST.
  !     YW 09/15/2016 Added output variable LOGDIA,LOGLEN,LOGVOL to R4vol subroutine
  !REV  Added manual debugging for use with pro vollib09 calls and
  !REV  code to check for forest = null which caused blm problems
  !     YW 04/13/2017 changed stump DIB calc using CALCDIA for profile model and stump volume as cylinder of stump DIB and stump height.
  !                   also changed stem tip volume calc using DIB from last log and Samlian method with tip length
  ! 2018/11/07 YW ADDED HTTOT TO R12VOL FOR TOTAL CUBIC VOLUME
  ! YW 2019/02/14 Set the default stump and MTOPP for FIA equation only
  ! YW 2019/04/04 Added call to BIA behr and johnson equation
  ! YW 2020/03/18 R6 requests to use 102.4 for cuft to Cord
  ! YW 2020/11/06 Changed the subroutine name to call FIA volume equations
  ! YW 2022/05/03 Set the total cubic vol to VOL(4) if MTOPP=0.1
  ! YW 2022/08/08 added variable STUMP to the call DVEST
  ! YW 2022/10/25 Fix the change made on 2022/05/03 to check VOL(1) before reset VOL(4)
  ! YW 2022/11/23 Added the call TOP6LEN R2 Black Hills equation 223DVEW122
  ! YW 2023/06/05 Modified VOLINITNVB for resetting CTYPE
  ! YW 2023/08/18 Check STUMP>0 before calc stump vol
  ! YW 2025/02/19 Set log weight to LOGVOL for CTYPE = 'C' (Cruise only)
  ! YW 2025/12/16 Changed R4 MAT equation for Forest 17 (HT NF) to use R5 Merch rules

  ! Variables
  character(len=1) :: HTTYPE
  character(len=1) :: LIVE
  character(len=1) :: CTYPE
  character(len=1) :: VOLEQREGN
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=2) :: PROD2
  character(len=4) :: CONSPEC
  character(len=10) :: VOLEQ
  character(len=10) :: FIAVTYPE
  character(len=10) :: GEOSUB
  character(len=10) :: NVELEQ
  character(len=3) :: MDL
  character(len=3) :: SPECIES
  character(len=2) :: DIST
  character(len=2) :: VAR

  character(len=10) :: EQNUM
  integer :: SPEC

  !   MERCH VARIABLES
  integer :: REGN,HTTFLL,BA,SI
  real    :: STUMP,MTOPP,MTOPS,THT1,MAXLEN
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
  real    :: TIPDIB,TIPLEN

  !   Tree variables
  real    :: HTTOT,HT1PRD,HT2PRD,LEFTOV
  real    :: DBHOB,DRCOB,DBTBH,BTR,CR,TRIM
  integer :: FCLASS,HTLOG,SPCODE,WHOLELOGS

  !    3RD POINT VARIABLES
  real    :: UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2
  integer :: HTREF

  !   OUTPUTS
  real    :: NOLOGP,NOLOGS
  integer :: TLOGS,IFORST,IDIST

  !   ARRAYS
  integer :: I15,I21,I20,I7,I3,I,J
  real    :: VOL(I15),LOGVOL(I7,I20)
  real    :: LOGDIA(I21,I3),LOGLEN(I20),BOLHT(I21)
  !  variables for stump dia and vol
  integer :: SPN
  real    :: STUMPDIB,STUMPDOB,VOLIB,VOLOB
  real    :: DIB,DOB,HTUP,MHT,FIAVOL,BFMIND,LMERCH

  !  test biomass calc variable
  real    :: WF(3),BMS(8),GRNWF,DeadWF,WtFac2
  integer :: SPCD,FOREST

  ! Test fiaeq2nveleq 2019/08/21
  !      CHARACTER*12 FIABEQ,NVELBEQ,GEOSUB2
  !      INTEGER BEQNUM,STEMS
  !      REAL TOPD,BIOMS
  !      VOL(1) = 20.0
  !      VOL(4) = 20.0
  !      SPN = HTREF
  !      BEQNUM = FCLASS
  !      GEOSUB2 = '1'
  !      CALL FIABEQ2NVELBEQ(BEQNUM,SPN,NVELBEQ,GEOSUB2,ERRFLAG)
  !      ERRFLAG = 0
  !      CALL BiomassLibrary2(NVELBEQ,DBHOB,HTTOT,CR,HT1PRD,
  !     + HT2PRD,TOPD,STEMS,VOL,BIOMS,ERRFLAG,SPN,GEOSUB2)
  !      CONTINUE

  !--------the following code is for manual debugging of older-----------
  !--------versions of the call.  comment or uncomment to use------------
  !--------also need to comment/uncomment close at bottom----------------

  !     SPECIES = VOLEQ(8:10)
  !     READ(SPECIES,'(i3)') SPCODE

  !     IF(SPCODE .EQ. 202 .AND. DBHOB .EQ. 10.0 .AND.
  !    &     HTTOT == 90.0) THEN
  !      IF(VOLEQ .EQ.'F06FW2W202') THEN
  !        ANY_DEBUG = .TRUE.
  !        DEBUG%VOLEQ = .TRUE.
  !        DEBUG%MODEL = .TRUE.
  !      ELSE
  !        ANY_DEBUG = .FALSE.
  !        DEBUG%VOLEQ = .FALSE.
  !        DEBUG%MODEL = .FALSE.
  !      ENDIF
  !     ANY_DEBUG = .TRUE.
  !     DEBUG%MODEL = .TRUE.
  if (ANY_DEBUG) then
     open (UNIT = LUDBG, FILE = 'Debug.txt', STATUS = 'UNKNOWN')
     write (LUDBG,1)'Debugging NVEL'
     1 format(A)
  end if

  !---------end of manual debug code----------------------------------------

  if (DEBUG%MODEL) then
     write  (LUDBG, 2) ' -->Enter VOLINIT'
     2 format (A)
  end if

  !  VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,FCLASS,
  !     +               VOL,LOGDIA,LOGLEN,LOGVOL,TLOGS,NOLOGP,NOLOGS,
  !     +               CONSPEC, BFPFLG,CUPFLG,errflag

  if (DEBUG%MODEL) then
     WRITE  (LUDBG, 100)'FORST VOLEQ     MTOPP HTTOT HT1PRD DBHOB    &
          &   HTTYPE FCLASS'
     100 format (A)
     write  (LUDBG, 104)FORST,VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,&
          &FCLASS
     104 format(A,2X,A, 1X, F5.1, 1X, F5.1, 2X, F5.1, 2X, F5.1, 5X,&
          &A, 1X, I5)

     WRITE (LUDBG, 300)'TLOGS NOLOGP NOLOGS CONSPEC                  &
          &    BFPFLG CUPFLG ERRFLAG'
     300 format (A)
     write (LUDBG, 320)TLOGS,NOLOGP,NOLOGS,CONSPEC,BFPFLG,CUPFLG,&
          &ERRFLAG
     320 format(1X, I2, 2X, F5.1, 2X, F5.1, 5X, A,1X, I5, I5, 1X,I5)
  end if

  !Check for a DBH of less than 1.  Drop out of volume if true.  10/97
  if (DBHOB.lt.1.and. DRCOB.lt.1) then
     ERRFLAG = 3
     goto 4000
  end if
  !Check for FIA volume equation (2018/10/02)
  ! P = PACIFIC COAST REGION
  ! R = ROCKY MOUNTAIN REGION
  ! N = NORTHERN REGION
  ! S = SOUTHERN REGION
  if (VOLEQ(1:2).eq.'cu') VOLEQ(1:2) = 'CU'
  if (VOLEQ(1:2).eq.'bd') VOLEQ(1:2) = 'BD'
  if (VOLEQ(1:2).eq.'CU'.or.VOLEQ(1:2).eq.'BD') then
     if (CONSPEC(1:1).ge.'0'.and.CONSPEC(1:1).le.'9') then
        if (CONSPEC(4:4).ge.'0'.and.CONSPEC(4:4).le.'9') then
           read(CONSPEC(1:4),'(I4)')SPN
        else if (CONSPEC(3:3).ge.'0'.and.CONSPEC(3:3).le.'9') then
           read(CONSPEC(1:3),'(I3)')SPN
        else if (CONSPEC(2:2).ge.'0'.and.CONSPEC(2:2).le.'9') then
           read(CONSPEC(1:2),'(I2)')SPN
        else
           read(CONSPEC(1:1),'(I1)')SPN
        end if
     else
        SPN = 0
     end if
     call FIAEQ2NVELEQ(VOLEQ,SPN,GEOSUB,MTOPP,NVELEQ,FIAVTYPE,&
          &ERRFLAG)
     if (ERRFLAG.gt.0) return
     VOLEQ = NVELEQ
  else
     SPN = 0
  end if
  VOLEQREGN = VOLEQ(1:1)
  if (VOLEQREGN.eq.'P'.or.VOLEQREGN.eq.'R'.or.&
       &VOLEQREGN.eq.'N'.or.VOLEQREGN.eq.'S') then
     GEOSUB = '0'
     FIAVOL = 0.0
     VOL = 0.0
     !       the default for stump and mtopp should be here for FIA equation (20190214)
     if (STUMP.eq.0.0) STUMP = 1.0
     if (MTOPP.eq.0.0) MTOPP = 6.0
     if (SI.eq.0) SI = 65
     if (BA.eq.0) BA = 80
     BFMIND = 9.0
     !        CALL FIA_VOLINIT(VOLEQ,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,
     !     &  STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLAG,
     !     &  FIAVTYPE,FIAVOL,BFMIND)
     call FIAVOLUME(VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,&
          &STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLAG,&
          &BFMIND)

     return
  end if
  !-----Set the default DIST to 01---------------------
  !      IF(IDIST.NE.IDIST) IDIST = 1
  if (IDIST.eq.0) IDIST = 1
  write (DIST, '(I2)') IDIST
  if (DIST(1:1) .lt. '0') DIST(1:1) = '0'
  !-----End for DIST (04/19/2016)

  if (VOLEQ .eq. "" .and. CTYPE.eq.'F') then
     VAR = '  '
     read(CONSPEC,'(I3)')SPEC
     !        DIST = '01'
     call VOLEQDEF(VAR,REGN,FORST,DIST,SPEC,PROD,EQNUM,ERRFLAG)
     VOLEQ = EQNUM
     ERRFLAG = 1
     goto 4000
  end if
  !  save FCLASS value
  FORMCLASS = FCLASS
  !-----If forest is null it will cause problems---------------------
  if (FORST(1:1) .eq. CHAR(0)) then
     !        WRITE  (LUDBG, *)' -->VOLINIT FORST = NULL'
     FORST(1:2) = '01'
  end if

  if (FORST(2:2) .lt. '0') then
     FORST(2:2) = FORST(1:1)
     FORST(1:1) = '0'
     if (FORST(2:2) .lt. '0') FORST(2:2) = '0'
  end if
  read(FORST,'(i2)') IFORST

  do I = 1,10
     if (ICHAR(VOLEQ(I:I)).ge.97.and. ICHAR(VOLEQ(I:I)).le.122)&
          &VOLEQ(I:I) = CHAR(ICHAR(VOLEQ(I:I))-32)
     3 continue
  end do

  MDL = VOLEQ(4:6)
  ! When total height is entered, the height type has to be feet. (2013/05/16)
  if (HTTOT.gt.0) HTTYPE = 'F'

  if (MDL.eq.'FW2' .or. MDL.eq.'fw2' .or. MDL.eq.'FW3' .or.&
       &MDL.eq.'fw3' .or. MDL.eq.'CZ2' .or. MDL.eq.'cz2' .or.&
       &MDL.eq.'CZ3' .or. MDL.eq.'cz3' .or. MDL.eq.'WO2' .or.&
       &MDL.eq.'wo2' .or. MDL.eq.'F32' .or. MDL.eq.'f32' .or.&
       &MDL.eq.'F33' .or. MDL.eq.'f33' .or. MDL.eq.'JB2' .or.&
       &MDL.eq.'jb2') then

     !    FLEWELLING MODELS  *
     !    REGION 2 MODELS    *
     !    REGION 5 MODELS    *

     call PROFILE (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,HTTYPE,&
          &HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,&
          &AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,&
          &TLOGS, NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,&
          &DRCOB,CTYPE,FCLASS,PROD,ERRFLAG)
  else if (MDL.eq.'MAT' .or. MDL.eq.'mat') then

     !    REGION 4 MODEL  *

     !        !Region 4 Forest 17 (HT NF uses Region 5 merch rules (20260223)
     if (REGN.eq.4.and.FORST.eq.'17') then
        REGN = 5
        call PROFILE (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,HTTYPE,&
             &HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,&
             &AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,&
             &TLOGS, NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,&
             &DRCOB,CTYPE,FCLASS,PROD,ERRFLAG)
        !Reset REGN variable to 4
        REGN = 4
     else
        call R4VOL(REGN,VOLEQ,MTOPP,HTTOT,DBHOB,HT1PRD,VOL,NOLOGP,&
             &NOLOGS,LOGDIA,LOGLEN,LOGVOL,BOLHT,&
             &CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG)
        TLOGS = ANINT(NOLOGP + NOLOGS)
     end if
  else if (MDL.eq.'TRF' .or. MDL.eq.'trf') then

     !      PNW terif VOLUME EQUATION

     call PNWTARIF(VOLEQ,DBHOB,HTTOT,MTOPP,VOL,BFPFLG,CUPFLG,&
          &ERRFLAG)

  else if (VOLEQ(1:1).eq.'6') then

     !   REGION 6 VOLUME ROUTINES *

     THT1 = 0.0
     if (HTTOT.gt.0) then
        THT1 = HTTOT
     else if (HTTYPE.eq.'L' .or. HTTYPE.eq.'l') then
        THT1 = HT1PRD
     else
        ! Merch height is entered in feet. It need to convert to log height and set HTTYPE to L. (YW 02/27/14)
        if (VOLEQ(1:3).eq.'632') then
           THT1 = INT(HT1PRD/32.6*10)
        else
           !              THT1 = INT(HT1PRD/16.3*10)
           ! ADDED ON 07/30/2014 ROUND LOGS BASED ON JEFF PENMAN LOG RULES
           if (HT1PRD.ge.13.and.HT1PRD.le.20) then
              WHOLELOGS = 1
              THT1 = 10
           else
              WHOLELOGS = INT(HT1PRD/16.5)
              THT1 = WHOLELOGS*10
           end if
           if (HT1PRD.gt.20) then
              LEFTOV = HT1PRD-INT(WHOLELOGS*16.5)
              if (LEFTOV.ge.(12+MOD(WHOLELOGS,2))) then
                 THT1 = WHOLELOGS*10+10
              else if (LEFTOV.ge.(4+MOD(WHOLELOGS,2))) then
                 THT1 = WHOLELOGS*10+5
              end if
           end if

        end if
        HTTYPE = 'L'
        if (THT1.eq.0) then
           ERRFLAG = 4
           return
        end if
     end if

     NOLOGP = 0.0
     NOLOGS = 0.0
     !****   If not top dib is specified then
     !****   use the one from the vol equation

     if ((MDL.eq.'BEH' .or. MDL.eq.'beh').and.&
          &(BFPFLG.eq.1.or. CUPFLG.eq.1)) then
        if (MTOPP.eq.0) then
           MTOPP = 6.0
        end if
        call R6VOL(VOLEQ,FORST,DBHOB,BTR,FCLASS,MTOPP,THT1,&
             &HTTYPE,VOL,LOGVOL,NOLOGP,LOGDIA,LOGLEN,DBTBH,&
             &HT1PRD,CTYPE,errflag)
        TLOGS = ANINT(NOLOGP)
     else if ((MDL.eq.'DVE'.or.MDL.eq.'dve').and.BFPFLG.eq.1) then
        call R6VOL2(VOLEQ,DBHOB,HTTOT,VOL,errflag)
     else
        ERRFLAG = 1
     end if

  else if (VOLEQ(1:1).eq.'B' .or. voleq(1:1).eq.'b') then

     !   BLM VOLUME ROUTINES *

     call BLMVOL(VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,FCLASS,&
          &VOL,LOGDIA,LOGLEN,LOGVOL,TLOGS,NOLOGP,NOLOGS,&
          &BFPFLG,CUPFLG,errflag)

  else if (VOLEQ(1:6).eq.'I16BEH'.or.VOLEQ(1:6).eq.'i16beh'&
       &.or.VOLEQ(1:6).eq.'I00DVE'.or.VOLEQ(1:6).eq.'i00dve') then

     !   BIA behres I16BEHW000 and Johnson I00DVEW000 VOLUME ROUTINES *

     if (FCLASS .le.0) then
        if (CTYPE .eq. 'F') then
           !  GET FORMCLASS
           call GETFCLASS(VOLEQ,FORST,DBHOB,FCLASS)
        end if
     end if
     if (MDL.eq.'BEH' .or. MDL.eq.'beh') then
        call BIA_Behres_Hyperobla(DBHOB,HTTOT,FCLASS,&
             &MTOPP, VOL)
     end if
     if (MDL.eq.'DVE' .or. MDL.eq.'dve') then
        call VolEq_Johnson(DBHOB, HTTOT, FCLASS, VOL)
     end if
  else if (VOLEQ(1:1).eq.'8') then

     !    REGION 8 MODEL  *

     if (MDL.eq.'CLK' .or. MDL.eq.'clk') then
        if (VOLEQ(3:3).eq.'1'.or.VOLEQ(3:3).eq.'I') then
           !         THe new R8 Clark equation using number 1 for the third character in the VOLEQ.
           !         The new equation using R9 Clark equation codes
           ! 02/24/2021 The UPSHT1 can be height to 4 or 7/9. If it is the height to 4, set to HT2PRD
           if (HTTOT.le.0.0) then
              if (UPSHT1.gt.0.0) then
                 if (UPSD1.eq.4.0.or.(UPSD1.eq.0.0.and.PROD.ne.'01')) then
                    HT2PRD = UPSHT1
                    UPSHT1 = 0.0
                 end if
              end if
           end if

           call R9CLARK (VOLEQ,STUMP,MTOPP,MTOPS,DBHOB,HT1PRD,HT2PRD,&
                &HTTOT, LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,CUTFLG,&
                &BFPFLG,CUPFLG, CDPFLG,SPFLG,PROD,ERRFLAG,CTYPE,&
                &UPSHT1,TLOGS,NOLOGP,NOLOGS)
        else
           !         The old R8 Clark equation
           ! ADDED TO TEST R8 CLARK PROFILE FOR LOG BOARDFOOT VOLUME
           call R8CLARK(VOLEQ,FORST,STUMP,MTOPP,MTOPS,DBHOB,HT1PRD,&
                &HT2PRD,HTTOT, LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,CUTFLG,&
                &BFPFLG,CUPFLG, CDPFLG,SPFLG,PROD,ERRFLAG,CTYPE,&
                &UPSHT1,TLOGS,NOLOGP,NOLOGS)
        end if
        ! 04/19/2016
        ! PUT THE INTL BDFT TO VOL(2) FOR GW/JF(08),OUACHITA(09),OZARK-ST FRANCIS(10)
        ! AND ALL OTHER RD (EXCEPT ANDREW PICKENS(02)) OF FRANCIS MARION & SUTTER(12)
        if (IFORST.eq.8.or.IFORST.eq.9.or.IFORST.eq.10.or.&
             &(IFORST.eq.12.and.IDIST.ne.2)) then
           VOL(2) = VOL(10)
        end if
     else
        call R8VOL (VOLEQ,DBHOB,HTTOT,UPSHT1,HT1PRD,MTOPP,PROD,VOL,&
             &FORST,SI,BA,CTYPE,BFPFLG,CUPFLG,SPFLG,ERRFLAG)
     end if
  else if (VOLEQ(1:1).eq.'9' .and.&
       &(MDL.eq.'CLK' .or. MDL.eq.'clk')) then

     !    REGION 9 MODEL  *

     !... need to add NOLOGP,NOLOGS,TLOGS to this call for log volumes
     call R9CLARK (VOLEQ,STUMP,MTOPP,MTOPS,DBHOB,HT1PRD,HT2PRD,&
          &HTTOT, LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,CUTFLG,&
          &BFPFLG,CUPFLG, CDPFLG,SPFLG,PROD,ERRFLAG,CTYPE,&
          &UPSHT1,TLOGS,NOLOGP,NOLOGS)

     if (IFORST.eq.4.or. IFORST.eq.5.or. IFORST.eq.8.or.&
          &IFORST.eq.11.or. IFORST.eq.12.or. IFORST.eq.14.or.&
          &IFORST.eq.19.or. IFORST.eq.20.or. IFORST.eq.21.or.&
          &IFORST.eq.22.or. IFORST.eq.24.or. IFORST.eq.30) then
        !              Put international bdft volumes into vol(2) bucket
        VOL(2) = VOL(10)
     end if

  else if (MDL.eq.'DEM' .or. MDL.eq.'dem' .or.&
       &MDL.eq.'CUR' .or. MDL.eq.'cur' .or.&
       &MDL.eq.'BRU' .or. MDL.eq.'bru') then

     !    REGION 10 MODEL  *

     if (HTTOT.le.0.and.HT1PRD.gt.0) then
        call R10HTS(VOLEQ,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,MTOPP,&
             &LMERCH)
     end if
     if ((VOLEQ(1:3).eq.'A01'.or.VOLEQ(1:3).eq.'A02' .or.&
          &VOLEQ(1:3).eq.'a01'.or.VOLEQ(1:3).eq.'a02' ) .or.&
          &(((HTTYPE.eq.'F'.and.HTTOT.le.40) .or. DBHOB.lt.9.0) .and.&
          &REGN.eq.10)) then

        call R10VOL(VOLEQ,MTOPP,MTOPS,HTTOT,HT1PRD,DBHOB,&
             &HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,LOGVOL,&
             &BFPFLG,CUPFLG,SPFLG,errflag)
     else
        call PROFILE (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
             &HTTYPE, HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
             &UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,&
             &LOGVOL,VOL, TLOGS,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,&
             &CDPFLG,SPFLG, DRCOB,CTYPE,FCLASS,PROD,ERRFLAG)
     end if

  else if (MDL.eq.'SN2') then

     !    HAWAII MODEL  *

     if (FCLASS.eq.0) then
        ERRFLAG = 2
     else
        call R12VOL(VOLEQ,MTOPP,HT1PRD,DBHOB,HTTOT,VOL,NOLOGP,&
             &NOLOGS,FCLASS,CUTFLG,BFPFLG,CUPFLG,errflag)
        TLOGS = ANINT(NOLOGP + NOLOGS)
     end if

  else if (MDL.eq.'DVE' .or. MDL.eq.'dve') then

     !  DVE MODELS FOR  *
     !    REGION 1      *
     !    REGION 2      *
     !    REGION 3      *
     !    REGION 5      *
     !    REGION 9      *
     !    ARMY BASE     *

     if (VOLEQ.eq.'223DVEW122') then
        call BH_NonSawPP(DBHOB,HTTOT,STUMP,MTOPP,MTOPS,&
             &HT1PRD,HT2PRD,VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,&
             &NOLOGP,NOLOGS,SPFLG,PROD,ERRFLAG)
     else
        call DVEST (VOLEQ,DBHOB,DRCOB,HTTOT,MTOPP,FCLASS,HTLOG,&
             &HT1PRD,HT2PRD, FORST,BTR,VOL,CUTFLG,BFPFLG,CUPFLG,&
             &CDPFLG,SPFLG,PROD,HTTYPE,HTTFLL,NOLOGP,LIVE,BA,&
             &SI,CTYPE,errflag,MTOPS,STUMP)
     end if
  else if (MDL.eq.'HAN' .or. MDL.eq.'han') then

     !  HANN PROFILE MODEL FOR BLM *

     SPECIES = VOLEQ(8:10)
     read(SPECIES,'(i3)') SPCODE

     if (VOLEQ(2:3) .eq. '32') then
        MAXLEN = 32
        TRIM = 8.0
     else
        MAXLEN = 16
        TRIM = 4.0
     end if
     !          CALL VOLCAL(SPCODE,MTOPP,STUMP,MAXLEN,0.0,MTOPP,STUMP,
     !      +                TRIM,DBHOB,HTTOT,
     !     &       CR,VERROR,TERROR,VWARNING,TWARNING,IERROR,VOL(4),VOL(2))

  else if (MDL.eq.'HAB' .or. MDL.eq.'hab') then

     !  HANN and Bare equation for Ponderosa pine young-growth and old-growth
     !  blackjack pine and Yellow pine

     call HANN_PP(VOLEQ,DBHOB,HTTOT,PROD,MTOPP,VOL,ERRFLAG)
  else
     !        ERROR MESSAGE
     ERRFLAG = 1
     do I = 1,15
        VOL(I) = 0.0
        5 continue
     end do
     do I = 1,21
        BOLHT(I) = 0.0
        10 continue
     end do
     do I = 1,7
        do J = 1,20
           LOGVOL(I,J) = 0.0
           16 continue
        end do
        15 continue
     end do
     do 17,I = 1,3
     do 18,J = 1,21
     LOGDIA(J,I) = 0.0
     18 continue
     17 continue
     do 19, I = 1,20
     LOGLEN(I) = 0.0
     19 continue
  end if
  if (VOL(7).lt.0.0) VOL(7) = 0.0
  !  calc Tip volume and save to VOL(15)
  if (ERRFLAG.gt.0) return
  if (VOL(14).le.0.0.and.STUMP.gt.0) then !start stump vol calc
     !Do not calculate stump vol for STUMP=0
     !IF(STUMP.LE.0) STUMP = 1.0
     !  calc stump DIB
     HTUP = STUMP
     if ((REGN.eq.9.or.(VOLEQ(1:1).eq.'8'.and.VOLEQ(3:3).eq.'1'))&
          &.and.HTTOT.eq.0) then
        !        UPSHT1=HT1PRD
        !        UPSD1 = MTOPP
        !        UPSHT2 = HT2PRD
        !        UPSD2 = MTOPS
        call CALCDIA2(REGN,FORST,VOLEQ,STUMP,DBHOB,&
             &DRCOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,HTREF,AVGZ1,&
             &AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)

     else
        call CALCDIA2(REGN,FORST,VOLEQ,STUMP,DBHOB,&
             &DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,&
             &AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)
     end if
     !     Not returning error code from stump DIB calc 12/28/2022 (YW)
     if (ERRFLAG.gt.0) ERRFLAG = 0
     if (DIB.gt.0.0) VOL(14) = 0.005454154*DIB**2*STUMP
     !  If stump DIB is not calculated, use the following to calculate stump VOL
     if (VOL(14).le.0.0) then
        SPECIES = VOLEQ(8:10)
        read(SPECIES,'(i3)') SPEC
        HTUP = STUMP
        call RAILEVOL(SPEC, DBHOB, HTUP, VOLIB, VOLOB)
        VOL(14) = VOLIB
     end if
  end if !end stump vol calc

  !     Calculate the tip volume as the volume above last log
  if (TLOGS.gt.0.and.LOGDIA(TLOGS+1,2).gt.0.and.VOL(15).eq.0) then
     MHT = 0.0
     if (BOLHT(TLOGS+1).gt.0.0) then
        MHT = BOLHT(TLOGS+1)
     else if (HT2PRD.gt.0) then
        MHT = HT2PRD
     else if (HT1PRD.gt.0) then
        MHT = HT1PRD
     else
        MHT = STUMP
        TRIM = 0.5
        do I = 1,TLOGS
           MHT = MHT + LOGLEN(I) + TRIM
           30 continue
        end do
     end if
     if (MHT.gt.0.0) then
        VOL(15) = 0.002727*LOGDIA(TLOGS+1,2)*LOGDIA(TLOGS+1,2)&
             &*(HTTOT-MHT)
     end if
  end if
  ! YW comment out the following on 3/20/2017
  !      IF(VOL(15).LT.0.01 .AND. VOL(4).GT.0.0)THEN
  !        VOL(15) = VOL(1)-VOL(4)-VOL(7)-VOL(14)
  !      ENDIF
  if (VOL(15).lt.0.0) VOL(15) = 0.0
  ! YW R3 requests to use 80 for CF to Cord 2019/05/16
  if (REGN.eq.3) VOL(6) = VOL(4)/80.0
  ! YW R6 requests to use 102.4 for cuft to Cord 2020/03/18
  if (REGN.eq.6) VOL(6) = AINT((VOL(4)/102.4)*10)/10.0
  ! YW Set the total cubic vol to VOL(4) for R2 if MTOPP=0.1 (2022/05/03)
  if (REGN.eq.2.and.MTOPP.eq.0.1) then
     if (VOL(1).gt.0.0) then
        VOL(4) = VOL(1)
     end if
  end if
  !Added VOL(1) > 0 check (20240426)
  if (VOL(1).lt.0) VOL(1) = 0
  !Set the log weight to LOGVOL for CTYPE ='C' (Cruise only) 20250219
  if (CTYPE.eq.'C') then
     SPCD = 0
     if (VERIFY(CONSPEC,"0123456789 ").eq.0) then
        read(CONSPEC,'(I3)') SPCD
     end if
     if (SPCD.eq.0) read(VOLEQ(8:10),'(I3)') SPCD
     call GetRegnWF(REGN,FORST,SPCD,GRNWF,DeadWF,PROD)
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
  if (DEBUG%MODEL) then
     WRITE  (LUDBG, 100)'FORST VOLEQ     MTOPP HTTOT HT1PRD DBHOB    &
          &   HTTYPE FCLASS'
     500 format (A)
     write  (LUDBG, 104)FORST,VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,&
          &FCLASS
     504 format(A,2X,A, 1X, F5.1, 1X, F5.1, 2X, F5.1, 2X, F5.1, 5X,&
          &A, 1X, I5)

     WRITE (LUDBG, 300)'TLOGS NOLOGP NOLOGS CONSPEC                  &
          &    BFPFLG CUPFLG ERRFLAG'
     508 format (A)
     write (LUDBG, 320)TLOGS,NOLOGP,NOLOGS,CONSPEC,BFPFLG,CUPFLG,&
          &ERRFLAG
     520 format(1X, I2, 2X, F5.1, 2X, F5.1, 5X, A,1X, I5, I5, 1X,I5)
  end if

  if (DEBUG%MODEL) then

     write  (LUDBG, 600)'VOL(1) VOL(2) VOL(4) ', VOL(1),VOL(2),VOL(4)
     600 format (A, 2x, F8.4, F8.4, F8.4)

  end if

  if (DEBUG%MODEL) then
     write  (LUDBG, 20) ' <--Exit VOLINIT'
     20 format (A//)
  end if

  !--------manual debugging code-----------------------------------------

  !      IF(SPCODE .EQ. 204) THEN
  !         CLOSE(LUDBG)
  !      ENDIF

  4000 return
  end subroutine VOLINIT

  ! This subroutine volinitnew enable the use of new National-scale volume and biomass equation
  !2023/03/21 Changed the subroutine name to volinitnvb and reset CTYPE for non-NVB Eq
subroutine VOLINITNVB(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
     &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
     &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,CR,CULL,DECAYCD,&
     &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
     &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
     &BA,SI,CTYPE,ERRFLAG,IDIST,BRKHT,BRKHTD,FIASPCD,DRYBIO,&
     &GRNBIO,MRULEFLG,MERRULES,CULLMSTOP)
  use CHARMOD
  use MRULES_MOD
  use VOLINPUT_MOD
  implicit none
  ! Variables
  integer :: REGN,HTREF,FCLASS,I3,I7,I15,I20,I21,HTTFLL,BA,SI,IDIST
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG,HTLOG,TLOGS
  character(len=11) :: VOLEQ
  character(len=11) :: NVBEQN
  character(len=10) :: V_EQN
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=2) :: DIST
  character(len=2) :: PROD2
  character(len=1) :: LIVE
  character(len=1) :: CTYPE
  character(len=1) :: HTTYPE
  character(len=4) :: CONSPEC
  real    :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB,HTTOT,HT1PRD,HT2PRD
  real    :: UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,DBTBH,BTR,NOLOGP,NOLOGS
  real    :: VOL(15),LOGVOL(7,20),LOGDIA(21,3),LOGLEN(20),BOLHT(21)
  real    :: NVBVOL(15),NVBLOGVOL(7,20),NVBLOGDIA(21,3),NVBLOGLEN(20)
  real    :: NVBBOLHT(21),CR,CULL,NVBNOLOGP,NVBNOLOGS
  integer :: FIASPCD,MRULEFLG,DECAYCD,ERRFLAG2,I,J,NVBTLOGS
  real    :: BRKHT,BRKHTD,DRYBIO(15),GRNBIO(15),Vfactor,Rwood
  character(len=3) :: SPCD
  TYPE(MERCHRULES)::MERRULES
  integer :: SPGRPCD,SFTHRD,STEMS,EcoProv
  real    :: WDSG,CF,SPGRNWF,SPDRYWF,MC,SPREGNWF,DeadWF,BIOMS(8)
  real    :: NVBHT1PRD,NVBHT2PRD,DecayProp,CV15,CULLMSTOP,FOLIAGE,BrchRem
  real    :: WtPrim,WtPrimWd,WtPrimBk,WtTW,WtTWwd,WtTWbk,WtTip,WtMstemNVB
  real    :: WtTipWd,WtTipBk,WTStem,WtStemWd,WtStemBk,WF,WF2,WtMerchStem
  real    :: Rprim,WtMstmNVBdry,WtTipDiff,WtTipDiffDry,DenProp,DeadCF

  I3 = 3
  I7 = 7
  I15 = 15
  I20 = 20
  I21 = 21
  ERRFLAG = 0
  VOL = 0
  DRYBIO = 0
  GRNBIO = 0
  LOGVOL = 0
  LOGDIA = 0
  LOGLEN = 0
  BOLHT = 0
  TLOGS = 0
  NOLOGP = 0
  NOLOGS = 0
  NVBTLOGS = 0
  NVBNOLOGP = 0
  NVBNOLOGS = 0
  NVBHT1PRD = HT1PRD
  NVBHT2PRD = HT2PRD
  Vfactor = 1
  ERRFLAG2 = 0
  if (IDIST.eq.0) IDIST = 1
  write (DIST, '(I2)') IDIST
  if (FORST(2:2).lt.'0'.or.FORST(2:2).gt.'9') then
     FORST(2:2) = FORST(1:1)
     FORST(1:1) = '0'
     if (FORST(2:2) .lt. '0') FORST(2:2) = '0'
  end if
  !Check LIVE input to be L or D
  if (LIVE.ne.'D') LIVE = 'L'
  !MRULEFLG = 1 indicates using modified merch rules for log segmentation
  if (MRULEFLG.gt.0) then
     MRULEMOD = 'Y'
     NEWCOR = MERRULES%COR
     NEWEVOD = MERRULES%EVOD
     NEWOPT = MERRULES%OPT
     NEWMAXLEN = MERRULES%MAXLEN
     NEWMINLEN = MERRULES%MINLEN
     NEWMERCHL = MERRULES%MERCHL
     NEWMINLENT = MERRULES%MINLENT
     NEWMTOPP = MERRULES%MTOPP
     NEWMTOPS = MERRULES%MTOPS
     NEWSTUMP = MERRULES%STUMP
     NEWTRIM = MERRULES%TRIM
     NEWBTR = MERRULES%BTR
     NEWDBTBH = MERRULES%DBTBH
     NEWMINBFD = MERRULES%MINBFD
  end if
  !Reset the species code for R10 second grow species(2024/10/07)
  if (FIASPCD.eq.2042) FIASPCD = 42
  if (FIASPCD.eq.2098) FIASPCD = 98
  if (FIASPCD.eq.2242) FIASPCD = 242
  if (FIASPCD.eq.2263) FIASPCD = 263
  if (FIASPCD.eq.204) FIASPCD = 202
  !Calculate biomass for trees with only DBH (no HT) using Jenkins (2021/11/01)
  if ((DBHOB.ge.0.1.or.DRCOB.ge.0.1).and.&
       &(HTTOT.lt.1.and.HT1PRD.lt.1.and.HT2PRD.lt.1.and.&
       !     + UPSHT1.LT.1.AND.UPSHT2.LT.1.).AND.CTYPE.EQ.'B')THEN
       &UPSHT1.lt.1.and.UPSHT2.lt.1.and.BRKHT.lt.1.0)) then !Removed the CTYPE = B check (20260326)
     if (DRCOB.ge.1.and.DBHOB.lt.1) DBHOB = DRCOB
     BIOMS = 0
     call JENKINS(FIASPCD,DBHOB,BIOMS)
     !Set the BIOMS component to variable DRYBIO
     DRYBIO(1) = BIOMS(1) - BIOMS(4)
     DRYBIO(6) = BIOMS(2)
     DRYBIO(7) = BIOMS(3)
     DRYBIO(12) = BIOMS(6)
     DRYBIO(13) = BIOMS(4)
     DRYBIO(14) = BIOMS(7)
     if (LIVE.eq.'D') DRYBIO(13) = 0
     !Get weight factor to calculate green weight, also the carbon fraction
     call NVB_RefSpcData(FIASPCD,SPGRPCD,WDSG,SFTHRD,CF,&
          &ERRFLAG,SPGRNWF,SPDRYWF)
     DRYBIO(15) = DRYBIO(1)*CF
     call GetRegnWF(REGN,FORST,FIASPCD,SPGRNWF,DeadWF,PROD)
     if (LIVE.eq.'L') then
        MC = (SPGRNWF-SPDRYWF)/SPDRYWF
     else
        MC = (DeadWF-SPDRYWF)/SPDRYWF
     end if
     GRNBIO = DRYBIO*(1+MC)
     !Added the error flag for no height tree (20260326)
     ERRFLAG = 4
     return
  end if
  !End Jenkins biomass calculation for trees with DBH only

  if (VOLEQ(1:3).eq.'NVB') then
     call NVBC(REGN,FORST,DIST,VOLEQ,DBHOB,HTTOT,MTOPP,MTOPS,&
          &HT1PRD,HT2PRD,STUMP,PROD,BRKHT,BRKHTD,LIVE,CR,CULL,DECAYCD,&
          &LOGLEN,LOGDIA,LOGVOL,BOLHT,&
          &TLOGS,NOLOGP,NOLOGS,VOL,DRYBIO,GRNBIO,ERRFLAG,FIASPCD,CTYPE)
  else
     V_EQN = VOLEQ(1:10)
     !When using FIA equation number like CU000001 or BD000001, the FIASPCD is passed in from CONSPEC variable
     !IF(VOLEQ(1:2).EQ.'CU'.OR.VOLEQ(1:2).EQ.'BD') THEN
     if (FIASPCD.gt.9) then
        if (FIASPCD.gt.999) then
           write(CONSPEC, '(I4)') FIASPCD
        else if (FIASPCD.gt.99) then
           write(CONSPEC, '(I3)') FIASPCD
        else if (FIASPCD.gt.9) then
           write(CONSPEC, '(I2)') FIASPCD
        end if
     end if
     call VOLINIT(REGN,FORST,V_EQN,MTOPP,MTOPS,STUMP,DBHOB,&
          &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
          &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
          &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
          &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
          &BA,SI,CTYPE,ERRFLAG,IDIST)
     if (ERRFLAG.gt.0) return
     if (FIASPCD.eq.0) read (V_EQN(8:10),'(I3)') FIASPCD
     if (FIASPCD.eq.204) FIASPCD = 202
     call NVB_DefaultEq(REGN,FORST,DIST,FIASPCD,NVBEQN,ERRFLAG)
     if (ERRFLAG.gt.0) then
        !Check if woodland species and calculate biomass based FIA equation
        call NVB_RefSpcData(FIASPCD,SPGRPCD,WDSG,SFTHRD,CF,&
             &ERRFLAG,SPGRNWF,SPDRYWF)
        if (SPGRPCD.eq.10) then
           ERRFLAG = 0
           STEMS = FCLASS
           if (DRCOB.lt.0.1.and.DBHOB.ge.0.1) DRCOB = DBHOB
           !FIA adjust missing top VOL using CULLMSTOP (20240426)
           CV15 = VOL(1)
           if (CULLMSTOP.gt.0) CV15 = CV15*(1-CULLMSTOP/100)
           if (REGN.eq.5.or.REGN.eq.6) then
              !For woodland species in pacific NW calculated as CVT*WDDEN
              DRYBIO(1) = CV15*WDSG
           else
              !For woodland species in  RockyMountain region
              call WOODLAND_BIO(FIASPCD, DRCOB,HTTOT,STEMS,CV15,&
                   &DRYBIO,ERRFLAG)
           end if

           !Calculate foliage using Jenkins equation. This is to match FIA foliage calculation
           !Calculate foliage for LIVE tree only
           BIOMS = 0
           call JENKINS(FIASPCD,DRCOB,BIOMS)
           FOLIAGE = BIOMS(4)
           if (LIVE.eq.'D') FOLIAGE = 0
           !Adjust foliage for missing top tree
           if (BRKHT.gt.0.and.BRKHT.lt.HTTOT) then
              call NVB_EcoProv(REGN,FORST,DIST,EcoProv)
              if (LIVE.eq.'L'.and.CR.eq.0) CR = 1
              call NVB_BrchRem(EcoProv,FIASPCD,BRKHT,HTTOT,CR,&
                   &BrchRem)
              FOLIAGE = FOLIAGE*BrchRem
           end if
           !Reset the foliage result
           !DRYBIO(13) = FOLIAGE
           !calc carbon
           DRYBIO(15) = DRYBIO(1)*CF
           !Adhust CULL for VOL and DRYBIO (20240428)
           VOL(1) = VOL(1)*(1-(CULL + CULLMSTOP)/100)
           VOL(4) = VOL(4)*(1-(CULL + CULLMSTOP)/100)
           if (LIVE.eq.'L'.and.CULL.gt.0) then
              DecayProp = 0.92
              if (FIASPCD.ge.300) DecayProp = 0.54
              DRYBIO = DRYBIO*(1.0 - CULL/100 * (1.0 - DecayProp))
              !ENDIF
              !Adjust wood denProp besed on DECAYCD (2025/05/08)
              !IF(DECAYCD.GT.0)THEN
           else if (LIVE.eq.'D') then
              if (DECAYCD.eq.0) DECAYCD = 3
              call DecayDenProp(SFTHRD,DECAYCD,DenProp,DeadCF)
              DRYBIO = DRYBIO*DenProp
              DRYBIO(15) = DRYBIO(1)*DeadCF
           end if
           !Reset the foliage result
           DRYBIO(13) = FOLIAGE
           !calc green weight
           call GetRegnWF(REGN,FORST,FIASPCD,SPGRNWF,DeadWF,PROD)
           if (LIVE.eq.'L') then
              MC = (SPGRNWF-SPDRYWF)/SPDRYWF
           else
              MC = (DeadWF-SPDRYWF)/SPDRYWF
           end if
           GRNBIO = DRYBIO*(1+MC)
        end if
        if (ERRFLAG.gt.0) ERRFLAG = 0
        return
     end if
     !NVBVOL = 0
     !NVBLOGVOL = 0
     !NVBLOGLEN = 0
     !NVBLOGDIA = 0
     !NVBBOLHT = 0

     do 102,I = 1,20
     do 103, J = 1,7
     NVBLOGVOL(J,I) = 0.0
     103 continue
     NVBLOGDIA(I,1) = 0.0
     NVBLOGDIA(I,2) = 0.0
     NVBLOGDIA(I,3) = 0.0
     NVBLOGLEN(I) = 0.0
     NVBBOLHT(I) = 0.0
     102 continue

     do 104, I = 1,15
     NVBVOL(I) = 0.0
     104 continue

     !Set CTYPE to B to make the calc same as FIA except the MTOPP and MTOPS
     if (CTYPE.ne.'I') CTYPE = 'B'
     call NVBC(REGN,FORST,DIST,NVBEQN,DBHOB,HTTOT,MTOPP,MTOPS,&
          &NVBHT1PRD,NVBHT2PRD,STUMP,PROD,BRKHT,BRKHTD,&
          &LIVE,CR,CULL,DECAYCD,NVBLOGLEN,NVBLOGDIA,&
          &NVBLOGVOL,NVBBOLHT,NVBTLOGS,NVBNOLOGP,NVBNOLOGS,NVBVOL,&
          &DRYBIO,GRNBIO,ERRFLAG2,FIASPCD,CTYPE)

     !Recalculate GRNBIO using freen weight factor and VOL from cruise VOLEQ(20241114)
     call GetRegnWF(REGN,FORST,FIASPCD,SPGRNWF,DeadWF,PROD)
     !get the secondary weight factor
     PROD2 = '20'
     WF2 = 0
     call GetRegnWF(REGN,FORST,FIASPCD,WF2,DeadWF,PROD2)
     if (LIVE.eq.'L') then
        WF = SPGRNWF
     else
        WF = DeadWF
        WF2 = WF
     end if
     !calculate moisture content
     if (DRYBIO(1).gt.0) then
        MC = GRNBIO(1)/DRYBIO(1) - 1.0
     else
        call NVB_RefSpcData(FIASPCD,SPGRPCD,WDSG,SFTHRD,CF,&
             &ERRFLAG,SPGRNWF,SPDRYWF)
        MC = (WF - SPDRYWF)/SPDRYWF
     end if
     WtPrim = 0
     WtPrimWd = 0
     WtPrimBk = 0
     WtTW = 0
     WtTWwd = 0
     WtTWbk = 0
     WtTip = 0
     WtTipWd = 0
     WtTipBk = 0
     WtStem = 0
     WtStemWd = 0
     WtStemBk = 0
     if (WF2.lt.1) WF2 = WF
     WtPrim = VOL(4)*WF
     WtTW = VOL(7)*WF2
     WtMerchStem = WtPrim + WtTW
     Rprim = 1
     Rwood = 1
     if (WtMerchStem.gt.0) Rprim = WtPrim/WtMerchStem
     WtMstemNVB = GRNBIO(6)+GRNBIO(7)+GRNBIO(8)+GRNBIO(9)
     Vfactor = 1
     if (WtMstemNVB.gt.0.and.WtMerchStem.gt.0) then
        Vfactor = WtMerchStem/WtMstemNVB
        Rwood = (GRNBIO(6)+GRNBIO(8))/WtMstemNVB
        !ELSE
        !    WtStem = VOL(1)*WF
        !    IF((GRNBIO(2)+GRNBIO(3)).GT.0) THEN
        !        Vfactor = WtStem/(GRNBIO(2)+GRNBIO(3))
        !        Rwood = GRNBIO(2)/(GRNBIO(2)+GRNBIO(3))
        !    ELSE
        !        Vfactor = 1
        !        Rwood = 1
        !    ENDIF
     end if
     if (Vfactor.le.0) Vfactor = 1
     DRYBIO = DRYBIO*Vfactor
     GRNBIO = GRNBIO*Vfactor
     !Reset GRNBIO stem component
     WtMstmNVBdry = DRYBIO(6)+DRYBIO(7)+DRYBIO(8)+DRYBIO(9)
     if (VOL(4).gt.0) then
        GRNBIO(6) = WtPrim*Rwood
        GRNBIO(7) = WtPrim*(1.0-Rwood)
        DRYBIO(6) = Rprim*WtMstmNVBdry*Rwood
        DRYBIO(7) = Rprim*WtMstmNVBdry*(1.0-Rwood)
     else
        GRNBIO(6) = 0
        GRNBIO(7) = 0
        DRYBIO(6) = 0
        DRYBIO(7) = 0
     end if
     if (VOL(7).gt.0) then
        GRNBIO(8) = WtTW*Rwood
        GRNBIO(9) = WtTW*(1.0-Rwood)
        DRYBIO(8) = (1.0-Rprim)*WtMstmNVBdry*Rwood
        DRYBIO(9) = (1.0-Rprim)*WtMstmNVBdry*(1.0-Rwood)
     else
        GRNBIO(8) = 0
        GRNBIO(9) = 0
        DRYBIO(8) = 0
        DRYBIO(9) = 0
     end if
     if (VOL(15).gt.0) then
        WtTip = VOL(15)*WF2
        WtTipDiff = WtTip-GRNBIO(10)-GRNBIO(11)
        WtTipDiffDry = WtTipDiff/(1.0+MC)
        GRNBIO(10) = WtTip*Rwood
        GRNBIO(11) = WtTip*(1.0-Rwood)
        DRYBIO(10) = GRNBIO(10)/(1.0+MC)
        DRYBIO(11) = GRNBIO(11)/(1.0+MC)
     end if
     !Adjust AGB and main stem total
     GRNBIO(2) = GRNBIO(4)+GRNBIO(6)+GRNBIO(8)+GRNBIO(10)
     GRNBIO(3) = GRNBIO(5)+GRNBIO(7)+GRNBIO(9)+GRNBIO(11)
     DRYBIO(2) = DRYBIO(4)+DRYBIO(6)+DRYBIO(8)+DRYBIO(10)
     DRYBIO(3) = DRYBIO(5)+DRYBIO(7)+DRYBIO(9)+DRYBIO(11)
     GRNBIO(1) = GRNBIO(2)+GRNBIO(3)+GRNBIO(12)
     DRYBIO(1) = DRYBIO(2)+DRYBIO(3)+DRYBIO(12)
  end if
  return
end

  !Set the log weight in LOGVOL for CTYPE = Cruise
  !Set the log weight to LOGVOL(7,I) which is Intl BDFT for other CTYPE
subroutine CruiseLogWt(LOGVOL,NOLOGP,NOLOGS,WTFAC,WTFAC2)
  implicit none
  ! Variables
  real    :: LOGVOL(7,20),WTFAC,WTFAC2
  real    :: NOLOGP,NOLOGS
  integer :: NLOGP,NLOGS,I,J
  NLOGP = CEILING(NOLOGP)
  NLOGS = CEILING(NOLOGS)
  if (NLOGP.ge.1) then
     do I = 1, NLOGP
        LOGVOL(7,I) = LOGVOL(4,I)*WTFAC
     end do
  end if
  if (NLOGS.ge.1) then
     do J = 1+NLOGP, NLOGP+NLOGS
        LOGVOL(7,J) = LOGVOL(4,J)*WTFAC2
     end do
  end if
  return
end
