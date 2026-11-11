!==============================!
!   VolumeLibrary : volinit2   !
!==============================!
  !  This version of Volint allows the user to pass in the merrules
  !  defined type.  This type allows the user to set the merch rules
  !  externally.

  !  created TDH 04/01/09

  !  revised YW 05/16/2013 For BEH equation, if total height is entered, the HTTYPE is set to F
  !REV  YW 02/27/2014 For region 6 Behre equation, if merch height is entered in feet, convert it to log ht
  !REV  YW 07/30/2014 For region 6 Behre equation, if merch height is entered in feet, use log rules to convert to log ht.
  !     YW 09/15/2016 Added output variable LOGDIA,LOGLEN,LOGVOL to R4vol subroutine
  !     YW 08/08/2022 Added STUMP to call DVEST subroutine
  !_______________________________________________________________________

subroutine VOLINIT2(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
     &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
     &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
     &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
     &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
     &BA,SI,CTYPE,ERRFLAG, MERRULES,IDIST)
  !_______________________________________________________________________

  use CHARMOD
  use DEBUG_MOD
  use MRULES_MOD
  use VOLINPUT_MOD
  implicit none

  !REV  Created TDH
  !REV  Revised iii ../../..

  ! Variables
  character(len=1) :: HTTYPE
  character(len=1) :: LIVE
  character(len=1) :: CTYPE
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=4) :: CONSPEC
  character(len=10) :: VOLEQ
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
  TYPE(MERCHRULES)::MERRULES

  !   Tree variables
  real    :: HTTOT,HT1PRD,HT2PRD,LEFTOV
  real    :: DBHOB,DRCOB,DBTBH,BTR,CR,TRIM,VOLIB,VOLOB,HTUP
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

  if (DEBUG%MODEL) then
     write  (LUDBG, 2) ' -->Enter VOLINIT2'
     2 format (A)
  end if

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

  call VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST)
  goto 4000

  ! The following codes are almost duplicated from VOLINIT1 and will be deleted 2017/03/17

  !Check for a DBH of less than 1.  Drop out of volume if true.  10/97
  if (DBHOB.lt.1.and. DRCOB.lt.1) then
     ERRFLAG = 3
     goto 4000
  end if
  !-----Set the default DIST to 01---------------------
  if (IDIST.ne.IDIST) IDIST = 1
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
       !     +   MDL.EQ.'jb2') THEN
       &MDL.eq.'jb2' .or. MDL.eq.'BEH') then

     !    FLEWELLING MODELS  *
     !    REGION 2 MODELS    *
     !    REGION 5 MODELS    *

     call PROFILE2 (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,HTTYPE,&
          &HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,&
          &AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,&
          &TLOGS, NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,&
          &DRCOB,CTYPE,FCLASS,PROD,ERRFLAG, MERRULES)

  else if (MDL.eq.'MAT' .or. MDL.eq.'mat') then

     !    REGION 4 MODEL  *

     call R4VOL(REGN,VOLEQ,MTOPP,HTTOT,DBHOB,HT1PRD,VOL,NOLOGP,&
          &NOLOGS, LOGDIA,LOGLEN,LOGVOL,BOLHT,&
          &CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG)
     TLOGS = ANINT(NOLOGP + NOLOGS)
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
        !              THT1 = HT1PRD
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
        if (THT1.eq.0) return
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

  else if (VOLEQ(1:1).eq.'8') then

     !    REGION 8 MODEL  *

     if (MDL.eq.'CLK' .or. MDL.eq.'clk') then
        ! ADDED TO TEST R8 CLARK PROFILE FOR LOG BOARDFOOT VOLUME
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

        call R8CLARK(VOLEQ,FORST,STUMP,MTOPP,MTOPS,DBHOB,HT1PRD,&
             &HT2PRD,HTTOT, LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,CUTFLG,&
             &BFPFLG,CUPFLG, CDPFLG,SPFLG,PROD,ERRFLAG,CTYPE,&
             &UPSHT1,TLOGS,NOLOGP,NOLOGS)
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

     if ((VOLEQ(1:3).eq.'A01'.or.VOLEQ(1:3).eq.'A02' .or.&
          &VOLEQ(1:3).eq.'a01'.or.VOLEQ(1:3).eq.'a02' ) .or.&
          &(HTTOT.le.40.or. DBHOB.lt.9.0)) then

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

     call DVEST (VOLEQ,DBHOB,DRCOB,HTTOT,MTOPP,FCLASS,HTLOG,&
          &HT1PRD,HT2PRD, FORST,BTR,VOL,CUTFLG,BFPFLG,CUPFLG,&
          &CDPFLG,SPFLG,PROD,HTTYPE,HTTFLL,NOLOGP,LIVE,BA,&
          &SI,CTYPE,errflag,MTOPS,STUMP)

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
  !  calc Tip volume and save to VOL(15)
  if (ERRFLAG.gt.0) return
  if (VOL(14).lt.0.01) then
     SPECIES = VOLEQ(8:10)
     read(SPECIES,'(i3)') SPEC
     HTUP = STUMP
     call RAILEVOL(SPEC, DBHOB, HTUP, VOLIB, VOLOB)
     VOL(14) = VOLIB
  end if
  ! now calc tip volume
  !      IF(VOL(15).LT.0.01 .AND. VOL(4).GT.0.0)THEN
  !        VOL(15) = VOL(1)-VOL(4)-VOL(7)-VOL(14)
  !        IF(VOL(15).LT.0.0) VOL(15) = 0.0
  !      ENDIF

  if (DEBUG%MODEL) then
     write  (LUDBG, 20) ' <--Exit VOLINIT2'
     20 format (A)
  end if

  4000 return
  end subroutine VOLINIT2
