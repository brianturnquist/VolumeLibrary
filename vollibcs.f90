!==============================!
!   VolumeLibrary : vollibcs   !
!==============================!
  !  C# .Net entry point to the volume library

  !  created TDH 03/04/10

  !  revised 06/23/11
  !  Cleaned up code and added comments

  !  Revised TDH 10/04/10
  !  Adding logic to expand the profile model tutorial
  !  YW 2016/01/13 Added BTR default value for Region 3 Santa Fe forest DF and PP
  !_______________________________________________________________________

subroutine VOLLIBCS(REGN,FORSTI,VOLEQI,MTOPP,MTOPS,STUMP,&
     &DBHOB,&
     &DRCOB,HTTYPEI,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
     &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
     &VOL,LOGVOLI,LOGDIAI,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
     &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPECI,PRODI,HTTFLL,LIVEI,&
     &BA,SI,CTYPEI,ERRFLAG, INDEB, PMTFLG, MERRULES,IDIST)
  !_______________________________________________________________________

  ! Expose subroutine VOLLIBCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::VOLLIBCS

  use DEBUG_MOD
  use MRULES_MOD

  implicit none

  !     Parameters
  ! Variables
  integer :: REGN
  character(len=*) :: FORSTI
  character(len=*) :: VOLEQI
  real    :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB
  character(len=*) :: HTTYPEI
  real    :: HTTOT
  integer :: HTLOG
  real    :: HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2
  integer :: HTREF
  real    :: AVGZ1,AVGZ2
  integer :: FCLASS
  real    :: DBTBH,BTR
  integer :: I3,I7,I15,I20,I21
  real    :: LOGVOLI(I7,I20),LOGDIAI(I21,I3),LOGLEN(I20)
  real    :: BOLHT(I21)
  integer :: TLOGS
  real    :: NOLOGP,NOLOGS
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,CUSFLG,CDSFLG
  character(len=*) :: PRODI
  character(len=*) :: CONSPECI
  integer :: HTTFLL
  character(len=*) :: LIVEI
  character(len=*) :: CTYPEI
  integer :: ERRFLG,INDEB,PMTFLG
  TYPE(MERCHRULES):: MERRULES

  !     Local variables
  !     Variable required for call to VOLINIT
  integer :: SPFLG
  real    :: VOL(15)
  integer :: BA,SI
  integer :: ERRFLAG

  !     Local variables
  character(2) :: FORST
  character(10) :: VOLEQ
  character(1) :: HTTYPE
  character(4) :: CONSPEC
  character(2) :: PROD
  character(1) :: LIVE
  character(1) :: CTYPE
  character(len=3) :: MDL
  character(len=3) :: SPECIES
  character(len=2) :: DIST
  character(len=2) :: VAR
  character(len=10) :: EQNUM
  integer :: SPEC
  real    :: LOGVOL(I7,I20),LOGDIA(I21,I3),DIBO,TCU
  integer :: IDIST

  !test voinitnew variable
  real    :: CR,CULL,BRKHT,BRKHTD,CULLMSTOP
  integer :: DECAYCD,FIASPCD,MRULEFLG
  real    :: DRYBIO(15),GRNBIO(15)
  character(len=11) :: VOLEQ11

  !Convert the CHAR*256 types to Char(*) so I don't have to rename
  !all references below

  FORST = FORSTI(1:2)
  VOLEQ = VOLEQI(1:10)
  HTTYPE = HTTYPEI(1:1)
  CONSPEC = CONSPECI(1:4)
  PROD = PRODI(1:2)
  LIVE = LIVEI(1:1)
  CTYPE = CTYPEI(1:1)

  !Use array converter to reshape c arrays to fortran notation
  LOGVOL = RESHAPE(LOGVOLI, SHAPE(LOGVOL))
  LOGDIA = RESHAPE(LOGDIAI, SHAPE(LOGDIA))
  ERRFLAG = 0

  ! This is for manual debug only
  !      IF (INDEB.eq.1 .AND. VOLEQ .EQ. 'F06FW2W202') THEN
  !       OPEN (UNIT=LUDBG, FILE='Debug.txt', STATUS='UNKNOWN')
  !       WRITE (LUDBG,2)'Debugging VOLLIBCS'
  !   2     FORMAT(A)
  !         WRITE  (LUDBG, 50)'REGN FORST VOLEQ     HTTYPE CONSPEC PROD
  !     & LIVE CTYPE DBHOB MTOPP STUMP  BTR DBTBH HTTOT HT1PRD'
  !  50     FORMAT (A)
  !         WRITE  (LUDBG, 70) REGN, FORST, VOLEQ, HTTYPE, CONSPEC, PROD,
  !     &    LIVE, CTYPE, DBHOB, MTOPP, STUMP, BTR, DBTBH,
  !     &    HTTOT, HT1PRD
  !   70    FORMAT (I2, 3X, A, 3X, A,1X, A, 2X, A,8X, A,3X,A,4X,
  !     &           A, 2X, F7.1, F7.1, F6.1,F5.1, F5.2, F7.1,F7.1)

  !       CLOSE(LUDBG)
  !      ENDIF
  ! test vollib_r
  !      CALL vollib_r(VOLEQ,REGN,DBHOB,HTTOT,TCU,ERRFLAG)

  if (PMTFLG .eq.1) then
     !         CALL PMTPROFILE (FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     !     >   HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,
     !     >   AVGZ1,AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,
     !     >   TLOGS,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,DRCOB,
     !     >   CTYPE,FCLASS,PROD,DIBO,ERRFLAG)

     !         NOLOGP = DIBO
     !test voinitnew
     CR = 0
     CULL = 0
     CULLMSTOP = 0
     DECAYCD = 0
     FIASPCD = 0
     BRKHT = 0
     BRKHTD = 0
     MRULEFLG = 0
     VOLEQ11 = VOLEQI(1:11)
     call VOLINITNVB(REGN,FORST,VOLEQ11,MTOPP,MTOPS,STUMP,DBHOB,&
          &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
          &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,CR,CULL,DECAYCD,&
          &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
          &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
          &BA,SI,CTYPE,ERRFLAG,IDIST,BRKHT,BRKHTD,FIASPCD,DRYBIO,&
          &GRNBIO,MRULEFLG,MERRULES,CULLMSTOP)

  else if (PMTFLG .eq.2) then

     call VOLINIT2(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
          &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
          &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,3,7,15,20,21,&
          &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
          &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
          &BA,SI,CTYPE,ERRFLAG, MERRULES,IDIST)

  else
     !           IF (INDEB.eq.1 .AND. VOLEQ .EQ. 'F06FW2W202') THEN
     !           WRITE (LUDBG,2)'Before call VOLINIT'
     !           ENDIF

     call VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
          &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
          &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
          &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
          &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
          &BA,SI,CTYPE,ERRFLAG,IDIST)

     !          IF (INDEB.eq.1 .AND. VOLEQ .EQ. 'F06FW2W202') THEN
     !          WRITE (LUDBG,2)'After call VOLINIT'
     !          ENDIF
  end if

  !add null terminator required by C# strings
  FORSTI = FORST // char(0)
  VOLEQI = VOLEQ // char(0)
  HTTYPEI = HTTYPE // char(0)
  CONSPECI = CONSPEC // char(0)
  PRODI = PROD // char(0)
  LIVEI = LIVE // char(0)
  CTYPEI = CTYPE // char(0)
  !MERRULES%OPT = ' '//char(0)

  !copy the logvol and logdia data back into the subroutine
  !paramater for return to c#
  LOGVOLI = RESHAPE(LOGVOL, SHAPE(LOGVOL))
  LOGDIAI = RESHAPE(LOGDIA, SHAPE(LOGDIA))

  !      IF (INDEB.eq.1 .AND. VOLEQ .EQ. 'F06FW2W202') THEN
  !         WRITE  (LUDBG, 80)'After call NVEL: Vol(1)  Vol(2)  Vol(4)'
  !  80     FORMAT (A)
  !         WRITE  (LUDBG, 90) VOL(1), VOL(2), VOL(4)
  !  90    FORMAT (16X, F7.1,F7.1, F7.1)

  !       CLOSE(LUDBG)
  !      ENDIF

  4000 return
  end subroutine VOLLIBCS

  !_______________________________________________________________________

subroutine VOLLIBC2(REGN,FORSTI,VOLEQI,MTOPP,MTOPS,STUMP,&
     &DBHOB,&
     &DRCOB,HTTYPEI,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
     &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
     &VOL,LOGVOLI,LOGDIAI,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
     &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPECI,PRODI,HTTFLL,LIVEI,&
     &BA,SI,CTYPEI,ERRFLAG, INDEB, PMTFLG, MERRULES)
  !_______________________________________________________________________
  ! This function is for the user to use costom merch rules.
  ! The pathon script can call it with user defined merch rules.

  ! Expose subroutine VOLUMELIBRARY2 to users of this DLL

      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLLIBC2
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLLIBC2
  !    !DEC$ ATTRIBUTES DECORATE,ALIAS:'_VOLUMELIBRARY@224'::VOLUMELIBRARY
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLLIBC2'::VOLLIBC2

  use DEBUG_MOD
  use MRULES_MOD

  implicit none

  !     Parameters
  ! Variables
  integer :: REGN
  character(len=*) :: FORSTI
  character(len=*) :: VOLEQI
  real    :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB
  character(len=*) :: HTTYPEI
  real    :: HTTOT
  integer :: HTLOG
  real    :: HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2
  integer :: HTREF
  real    :: AVGZ1,AVGZ2
  integer :: FCLASS
  real    :: DBTBH,BTR
  integer :: I3,I7,I15,I20,I21
  real    :: LOGVOLI(I7,I20),LOGDIAI(I21,I3),LOGLEN(I20)
  real    :: BOLHT(I21)
  integer :: TLOGS
  real    :: NOLOGP,NOLOGS
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,CUSFLG,CDSFLG
  character(len=*) :: PRODI
  character(len=*) :: CONSPECI
  integer :: HTTFLL
  character(len=*) :: LIVEI
  character(len=*) :: CTYPEI
  integer :: ERRFLG,INDEB,PMTFLG
  TYPE(MERCHRULES):: MERRULES

  !     Local variables
  !     Variable required for call to VOLINIT
  integer :: SPFLG
  real    :: VOL(15)
  integer :: BA,SI
  integer :: ERRFLAG

  !     Local variables
  character(2) :: FORST
  character(10) :: VOLEQ
  character(1) :: HTTYPE
  character(4) :: CONSPEC
  character(2) :: PROD
  character(1) :: LIVE
  character(1) :: CTYPE
  character(len=3) :: MDL
  character(len=3) :: SPECIES
  character(len=2) :: DIST
  character(len=2) :: VAR
  character(len=10) :: EQNUM
  integer :: SPEC
  real    :: LOGVOL(I7,I20),LOGDIA(I21,I3),DIBO
  integer :: IDIST
  if (INDEB.eq.1) then
     open (UNIT = LUDBG, FILE = 'Debug.txt', STATUS = 'UNKNOWN')
     write (LUDBG,5)'Debugging VOLLIBCS2'
     5 format(A)
     WRITE  (LUDBG, 108)'  COR EVOD OPT MAXLEN MINLEN MERCHL        &
          &    MINLENT MTOPP MTOPS STUMP TRIM BTR DBTBH MINBFD'
     108 format (A)
     write  (LUDBG, 110) MERRULES%COR, MERRULES%EVOD, MERRULES%OPT,&
          &MERRULES%MAXLEN, MERRULES%MINLEN, MERRULES%MERCHL,&
          &MERRULES%MINLENT, MERRULES%MTOPP, MERRULES%MTOPS,&
          &MERRULES%STUMP,MERRULES%TRIM, MERRULES%BTR, MERRULES%DBTBH,&
          &MERRULES%MINBFD
     110 format (2X, A, 3X, I2, 3X, I3,3X, F7.1, F7.1, F7.1,&
          &F7.1, F7.1, F7.1,F7.1, F7.1, F7.1,F7.1, F7.1)

     close(LUDBG)
  end if
  !  PMTFLG = 2 will user defined rule. otherwise use default rule.
  if (PMTFLG.ne.2) PMTFLG = 3
  !      CALL VOLLIBCS(REGN,FORSTI,VOLEQI,MTOPP,MTOPS,STUMP,
  !     +    DBHOB,
  !     &    DRCOB,HTTYPEI,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
  !     &    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
  !     &    VOL,LOGVOLI,LOGDIAI,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
  !     &    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPECI,PRODI,HTTFLL,LIVEI,
  !     &    BA,SI,CTYPEI,ERRFLAG, INDEB, PMTFLG, MERRULES)

  FORST = FORSTI(1:2)
  VOLEQ = VOLEQI(1:10)
  HTTYPE = HTTYPEI(1:1)
  CONSPEC = CONSPECI(1:4)
  PROD = PRODI(1:2)
  LIVE = LIVEI(1:1)
  CTYPE = CTYPEI(1:1)

  !Use array converter to reshape c arrays to fortran notation
  LOGVOL = RESHAPE(LOGVOLI, SHAPE(LOGVOL))
  LOGDIA = RESHAPE(LOGDIAI, SHAPE(LOGDIA))
  ERRFLAG = 0

  if (PMTFLG .eq.2) then

     call VOLINIT2(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
          &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
          &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,3,7,15,20,21,&
          &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
          &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
          &BA,SI,CTYPE,ERRFLAG, MERRULES,IDIST)

  else
     call VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
          &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
          &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
          &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
          &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
          &BA,SI,CTYPE,ERRFLAG,IDIST)

     !          IF (INDEB.eq.1 .AND. VOLEQ .EQ. 'F06FW2W202') THEN
     !          WRITE (LUDBG,2)'After call VOLINIT'
     !          ENDIF
  end if

  !add null terminator required by C# strings
  FORSTI = FORST // char(0)
  VOLEQI = VOLEQ // char(0)
  HTTYPEI = HTTYPE // char(0)
  CONSPECI = CONSPEC // char(0)
  PRODI = PROD // char(0)
  LIVEI = LIVE // char(0)
  CTYPEI = CTYPE // char(0)

  end subroutine VOLLIBC2

  !_______________________________________________________________________

subroutine MRULESCS(REGN,VOLEQI,PRODI,TRIM,MINLEN,MAXLEN,OPT,&
     &MERCHL)
  !_______________________________________________________________________
  ! 04/15/2014 YW Added this function to get the regional default merch rules.

  ! Expose subroutine MRULESCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::MRULESCS

  implicit none

  !     Parameters
  ! Variables
  integer :: REGN,OPT
  character(len=*) :: PRODI
  character(len=*) :: VOLEQI
  real    :: TRIM,MINLEN,MAXLEN,MERCHL

  !     Local variables
  character(len=1) :: COR
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=3) :: MDL
  character(len=10) :: VOLEQ
  integer :: EVOD
  real    :: MTOPP,MTOPS,STUMP
  real    :: MINLENT,MINBFD,BTR,DBTBH,DBHOB

  VOLEQ = VOLEQI(1:10)
  PROD = PRODI(1:2)

  call MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,&
       &MINLEN,MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,&
       &PROD)

  VOLEQI = VOLEQ // char(0)
  PRODI = PROD // char(0)

  end subroutine MRULESCS

  !_______________________________________________________________________

subroutine MRULESCS2(REGN,VOLEQI,PRODI,TRIM,MINLEN,MAXLEN,OPT,&
     &MERCHL,MTOPP,MTOPS,STUMP,MINLENT)
  !_______________________________________________________________________
  ! 04/15/2014 YW Added this function to get the regional default merch rules.

  ! Expose subroutine MRULESCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::MRULESCS2

  implicit none

  !     Parameters
  ! Variables
  integer :: REGN,OPT
  character(len=*) :: PRODI
  character(len=*) :: VOLEQI
  real    :: TRIM,MINLEN,MAXLEN,MERCHL

  !     Local variables
  character(len=1) :: COR
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=3) :: MDL
  character(len=10) :: VOLEQ
  integer :: EVOD
  real    :: MTOPP,MTOPS,STUMP
  real    :: MINLENT,MINBFD,BTR,DBTBH,DBHOB

  VOLEQ = VOLEQI(1:10)
  PROD = PRODI(1:2)

  call MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,&
       &MINLEN,MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,&
       &PROD)

  !VOLEQI = VOLEQ // char(0)
  PRODI = PROD // char(0)

  end subroutine MRULESCS2
  !____________________________________________________________________
subroutine CALCDIACS(REGN,FORSTI,VOLEQI,STUMP,DBHOB,&
     &DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,&
     &AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)
      !DEC$ ATTRIBUTES DLLEXPORT::CALCDIACS

  implicit none

  ! Variables
  character(len=*) :: FORSTI
  character(len=*) :: VOLEQI
  !     Local variables
  character(2) :: FORST
  character(10) :: VOLEQ
  !      CHARACTER(2)   PROD

  !     MERCH VARIABLES
  integer :: REGN,BA,SI
  integer :: ERRFLAG

  !     TREE VARIABLES
  real    :: HTTOT,HTUP,MHT,MTOPP,UHT,CUVOL
  real    :: DBHOB,DRCOB,DBTBH,BTR,STUMP,TOP6
  integer :: FCLASS
  !      3RD POINT VARIABLES
  real    :: UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2
  integer :: HTREF
  !     OUTPUTS
  real    :: DIB,DOB
  !     Variables to hold flewellings coefficients
  integer :: SETOPT(6),JSP,MFLG,NEXTRA
  real    :: RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
  real    :: HEX(2),dex(2),ZEX(2)
  real    :: mTopS,ht1Prd,ht2Prd
  character(len=2) :: prod
  integer :: SPN
  real    :: TLH
  character(len=3) :: MDL
  ! Variable for R4 taper
  real    :: STUMPD,BUTTCF,CF0,B

  TLH = 0.

  FORST = FORSTI(1:2)
  VOLEQ = VOLEQI(1:10)
  ! Not sure why it does not work if call CALCDIA directly!!!
  ! YW 04/13/2017
  ! But the call CALCDIA2 works fine. I think the reason is that
  ! CALCDIA has the DLLEXPORT and so I added CALCDIA2.
  call CALCDIA2(REGN,FORST,VOLEQ,STUMP,DBHOB,&
       &DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,&
       &AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)
  goto 1000

  ! The following codes are not used will be deleted. (4/13/2017)
  !     ARRAYS
  ! initialize profile model
  !C  heck for a DBH of less than 1.  Drop out of volume if true.  10/97
  if (dbhob.lt.1) then
     errflag = 3
     goto 1000
  end if

  if (VOLEQ .eq. "") then
     ERRFLAG = 1
     goto 1000
  end if
  MFLG = 2
  MHT = 0
  MTOPP = 0
  MDL = VOLEQ(4:6)
  prod = '01'
  !     Modifid BTR for Region 3 Santa Fe forest DF and PP (YW 2016/01/13)
  !      IF(REGN.EQ.3.AND.FORST.EQ.'10'.AND.BTR.EQ.0)THEN
  !        IF(VOLEQ(8:10).EQ.'202') BTR = 87.8
  !        IF(VOLEQ(8:10).EQ.'122') BTR = 88.5
  !      ENDIF

  if (MDL.eq.'FW2' .or. MDL.eq.'fw2' .or. MDL.eq.'FW3' .or.&
       &MDL.eq.'fw3' .or. MDL.eq.'CZ2' .or. MDL.eq.'cz2' .or.&
       &MDL.eq.'CZ3' .or. MDL.eq.'cz3' .or. MDL.eq.'WO2' .or.&
       &MDL.eq.'wo2' .or. MDL.eq.'F32' .or. MDL.eq.'f32' .or.&
       &MDL.eq.'F33' .or. MDL.eq.'f33' .or. MDL.eq.'JB2' .or.&
       &MDL.eq.'jb2' .or. MDL.eq.'DEM' .or. MDL.eq.'CUR' .or.&
       &MDL.eq.'dem' .or. MDL.eq.'cur') then

     !    FLEWELLING MODELS  *
     !    REGION 2 MODELS    *
     !    REGION 5 MODELS    *

     if (VOLEQ(4:4).eq.'F' .or. VOLEQ(4:4).eq.'f') then
        !--   Initialize Flewelling model for this tree
        call FWINIT(VOLEQ,DBHOB,HTTOT,MHT,MTOPP,UPSHT1,UPSHT2,UPSD1,&
             &UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,JSP,RHFW,RFLW,&
             &TAPCOE,F,SETOPT,NEXTRA,HEX,DEX,ZEX,PINV_Z,FMOD,btr,FCLASS,&
             &ERRFLAG)

     else if (VOLEQ(4:6).eq.'CZ3' .or. VOLEQ(4:6).eq.'cz3') then
        !        initialize Czaplewski three point model
        if (HTTOT.le.4.5) then
           ERRFLAG = 4
           goto 1000
        end if
        UHT = HTTOT * 0.95
        if (UPSHT1.le.0.or. UPSD1.le.0) then
           ERRFLAG = 9
           go to 1000
        end if
        if (UPSHT1.le.4.5.or. UPSHT1.gt.UHT) then
           ERRFLAG = 10
           go to 1000
        end if
        HEX(1) = UPSHT1
        DEX(1) = UPSD1
        call TOP6LEN(VOLEQ,HTTOT,DBHOB,DEX,HEX,STUMP,6.0,&
             &TOP6,DBTBH,errflag)
     else
        !C       CHECK FOR TOTAL TREE HEIGHT
        if (HTTOT.le.4.5) then
           ERRFLAG = 4
           goto 1000
        end if
     end if
     ! GET THE DIAMETERS
     call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
          &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HTUP,&
          &MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
     ! Added Clark profile model for region 9
  else if (MDL.eq.'CLK' .or. MDL.eq.'clk') then
     if (VOLEQ(1:1).eq.'9') then
        ht2Prd = UPSHT2
        ht1Prd = UPSHT1
        mTopP = 0.0
        mTopS = 0.0
        errFlag = 0
        ! reset UPSHT1 to 0 (yw 09/24/2012)
        UPSHT1 = 0
        call r9clarkdib (VOLEQ,STUMP,mTopP,mTopS,DBHOB,&
             &ht1Prd,ht2Prd,HTTOT,HTUP,DIB,prod,errFlag,&
             &UPSHT1)
     else
        call R8CLKDIB(VOLEQ, FORST, DBHOB, HTTOT, UPSHT1,HTUP,DIB,&
             &ERRFLAG)
     end if
  else if (MDL.eq.'BEH' .or. MDL.eq.'beh') then
     !     added DIB calculation for Behr equation
     if (FCLASS.le.0) then
        call GETFCLASS(VOLEQ,FORST,DBHOB,FCLASS)
     end if
     call BEHTAP(VOLEQ,DBHOB,HTTOT,TLH,HTUP,FCLASS,MTOPP,DIB)
     !     Added the calculation for R4 taper equation (03/20/2017)
  else if (MDL.eq.'MAT' .or. MDL.eq.'mat') then
     DIB = 0.0
     call R4MATTAPER(VOLEQ,DBHOB,HTTOT,STUMPD,BUTTCF,CF0,B,&
          &HTUP,DIB,ERRFLAG)

     ! calculation for diameter from ground to 4.5 ft heigh for non profile model
     ! added on 7/22/2012 YW
     ! using Raile 1982
  else
     if (HTUP .lt.4.5) then
        read (VOLEQ(8:10),'(I3)') SPN
        if (HTUP .lt.0.0001) HTUP = 1.0
        call STUMPDIA(SPN, DBHOB, HTUP, DIB, DOB)
        return
     end if
  end if

  !add null terminator required by C# strings
  1000 FORSTI = FORST // char(0)
  VOLEQI = VOLEQ // char(0)
  end subroutine CALCDIACS

  !________________________________________________________________________
subroutine CRZSPDFTCS(REGN, FORSTI, SPCD, WF, AGTEQ, LBREQ,&
     &DBREQ, FOLEQ, TIPEQ, WF1REF, WF2REF, MCREF, AGTREF, LBRREF,&
     &DBRREF, FOLREF, TIPREF)
  ! Expose subroutine CRZSPDFTCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::CRZSPDFTCS

  implicit none
  ! Variables
  integer :: REGN,SPCD,I,J
  real    :: WF(3)
  character(len=*) :: FORSTI
  character(len=*) :: AGTEQ
  character(len=*) :: LBREQ
  character(len=*) :: DBREQ
  character(len=*) :: FOLEQ
  character(len=*) :: TIPEQ
  character(len=*) :: WF1REF
  character(len=*) :: WF2REF
  character(len=*) :: MCREF
  character(len=*) :: AGTREF
  character(len=*) :: LBRREF
  character(len=*) :: DBRREF
  character(len=*) :: FOLREF
  character(len=*) :: TIPREF

  character(2) :: FORST
  character(12) :: BMSEQ(8)
  character(12) :: BEQ
  character(50) :: REF(8)

  FORST = FORSTI(1:2)

  call CRZSPDFT(REGN, FORST, SPCD, WF, BMSEQ, REF)
  do 20, J = 1,8
  BEQ = BMSEQ(J)
  if (BEQ(1:1).eq.'-') then
     BMSEQ(J) = ''
  end if
  20 continue
  do J = 1,3
     I = J
     if (J.eq.3) I = J+1
     if (REF(I).eq.'') REF(I) = 'Heath et al 2009'
  end do

  !     !add null terminator required by C# strings
  FORSTI = FORST // char(0)
  AGTEQ = BMSEQ(1) // char(0)
  LBREQ = BMSEQ(2) // char(0)
  DBREQ = BMSEQ(3) // char(0)
  FOLEQ = BMSEQ(4) // char(0)
  TIPEQ = BMSEQ(7) // char(0)
  WF1REF = REF(5) //char(0)
  WF2REF = REF(6) //char(0)
  MCREF = REF(8) //char(0)
  AGTREF = REF(1) //char(0)
  LBRREF = REF(2) //char(0)
  DBRREF = REF(3) //char(0)
  FOLREF = REF(4) //char(0)
  TIPREF = REF(7) //char(0)

end

subroutine CRZBIOMASSCS(REGN,FORSTI,SPCD,DBHOB,DRCOB,HTTOT,FCLASS,&
     &VOL,WF,BMS,ERRFLG,PRODI)
  ! Expose subroutine CRZBIOMASSCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::CRZBIOMASSCS

  implicit none
  ! Variables
  integer :: REGN,SPCD,ERRFLG,FCLASS
  real    :: DBHOB,HTTOT,VOL(15),WF(3),BMS(8),WFI(3),DRCOB
  character(len=*) :: FORSTI
  character(len=*) :: PRODI
  character(2) :: FORST
  character(2) :: PROD

  FORST = FORSTI(1:2)
  PROD = PRODI(1:2)
  call CRZBIOMASS(REGN,FORST,SPCD,DBHOB,DRCOB,HTTOT,FCLASS,&
       &VOL,WF,BMS,ERRFLG,PROD)
  !     !add null terminator required by C# strings
  FORSTI = FORST // char(0)
end

  !      SUBROUTINE BROWNCROWNFRACTIONCS(SPCD, DBH, THT, CFWT)
  !      !DEC$ ATTRIBUTES DLLEXPORT::BrownCrownFractionCS
  !      IMPLICIT NONE
  !      INTEGER SPCD
  !      REAL DBH, THT, CFWT(5)
  !      CALL BrownCrownFraction(SPCD, DBH, THT, CFWT)
  !      END

subroutine BIOLIBCS(REGN,FORST,SPEC,BIOEQ,DBHOB,HTTOT,VOL,&
     &BIOGRN, BIODRY,ERRFLG,HT1PRD,HT2PRD,HTTFLL,GEOSUB)
      !DEC$ ATTRIBUTES DLLEXPORT::BIOLIBCS

  !     The biomass component calculated in BIOGRN and BIODRY as below
  !     1 ABOVE GROUND TOTAL
  !     2 MERCH STEM WOOD
  !     3 MERCH STEM BARK
  !     4 FOLIAGE
  !     5 ROOTS
  !     6 BRANCHES
  !     7 CROWN
  !     8 MERCH STEM WOOD AND BARK
  !     9 Biomass calculated from the biomass Equation BIOEQ

  implicit none
  ! Variables
  integer :: REGN,SPEC,ERRFLG
  character(len=*) :: FORST
  character(len=*) :: BIOEQ
  character(len=*) :: GEOSUB
  real    :: DBHOB,HTTOT,VOL(15),BIOGRN(9),BIODRY(9)
  real    :: HT1PRD,HT2PRD,HTTFLL
  character(len=2) :: FORSTI
  character(len=2) :: GEOSUBI
  character(len=12) :: BIOEQI

  FORSTI = FORST(1:2)
  GEOSUBI = GEOSUB(1:2)
  BIOEQI = BIOEQ(1:12)
  call BIOLIB2(REGN,FORSTI,SPEC,BIOEQI,DBHOB,HTTOT,VOL,&
       &BIOGRN, BIODRY,ERRFLG,HT1PRD,HT2PRD,HTTFLL,GEOSUBI)

end

  ! This new subroutine enable the use of new National-scal volume and biomass equation
subroutine VOLLIBCSNVB(REGN,FORSTI,VOLEQI,MTOPP,MTOPS,STUMP,&
     &DBHOB,DRCOB,HTTYPEI,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,&
     &UPSD1,UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,&
     &VOL,LOGVOLI,LOGDIAI,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
     &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPECI,PRODI,HTTFLL,LIVEI,&
     &BA,SI,CTYPEI,ERRFLAG, PMTFLG, MERRULES,IDIST,&
     &BRKHT,BRKHTD,FIASPCD,DRYBIO,GRNBIO,CR,CULL,DECAYCD)
  !_______________________________________________________________________
  ! Expose subroutine VOLLIBCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::VOLLIBCSNVB
  use MRULES_MOD
  implicit none

  !     Parameters
  ! Variables
  integer :: REGN,HTLOG,HTREF,FCLASS,TLOGS,HTTFLL
  character(len=*) :: FORSTI
  character(len=*) :: VOLEQI
  character(len=*) :: HTTYPEI
  character(len=*) :: PRODI
  character(len=*) :: CONSPECI
  real    :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB,HTTOT
  real    :: HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2
  real    :: AVGZ1,AVGZ2,DBTBH,BTR,NOLOGP,NOLOGS
  real    :: LOGVOLI(7,20),LOGDIAI(21,3),LOGLEN(20),BOLHT(21)
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,CUSFLG,CDSFLG
  character(len=*) :: LIVEI
  character(len=*) :: CTYPEI
  integer :: ERRFLG,PMTFLG
  TYPE(MERCHRULES):: MERRULES
  real    :: BRKHT,BRKHTD,DRYBIO(15),GRNBIO(15),VOL(15)
  !     Local variables
  !     Variable required for call to VOLINIT
  integer :: BA,SI,ERRFLAG

  !     Local variables
  character(2) :: FORST
  character(2) :: PROD
  character(11) :: VOLEQ11
  character(1) :: HTTYPE
  character(1) :: LIVE
  character(1) :: CTYPE
  character(4) :: CONSPEC
  real    :: LOGVOL(7,20),LOGDIA(21,3),CR,CULL,CULLMSTOP
  integer :: IDIST,MRULEFLG,DECAYCD,SPFLG,FIASPCD

  !Convert the CHAR*256 types to Char(*) so I don't have to rename
  !all references below

  FORST = FORSTI(1:2)
  VOLEQ11 = VOLEQI(1:11)
  HTTYPE = HTTYPEI(1:1)
  CONSPEC = CONSPECI(1:4)
  PROD = PRODI(1:2)
  LIVE = LIVEI(1:1)
  CTYPE = CTYPEI(1:1)

  !Use array converter to reshape c arrays to fortran notation
  LOGVOL = RESHAPE(LOGVOLI, SHAPE(LOGVOL))
  LOGDIA = RESHAPE(LOGDIAI, SHAPE(LOGDIA))
  ERRFLAG = 0
  CULLMSTOP = 0
  MRULEFLG = PMTFLG
  call VOLINITNVB(REGN,FORST,VOLEQ11,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,CR,CULL,DECAYCD,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST,BRKHT,BRKHTD,FIASPCD,DRYBIO,&
       &GRNBIO,MRULEFLG,MERRULES,CULLMSTOP)

  !add null terminator required by C# strings
  !FORSTI = FORST // char(0)
  VOLEQI = VOLEQ11 // char(0)
  !HTTYPEI = HTTYPE // char(0)
  !CONSPECI = CONSPEC // char(0)
  !PRODI = PROD // char(0)
  !LIVEI = LIVE // char(0)
  !CTYPEI = CTYPE // char(0)

  !copy the logvol and logdia data back into the subroutine
  !paramater for return to c#
  LOGVOLI = RESHAPE(LOGVOL, SHAPE(LOGVOL))
  LOGDIAI = RESHAPE(LOGDIA, SHAPE(LOGDIA))

  return
  end subroutine VOLLIBCSNVB

subroutine GETREGNWFCS(REGN,FORSTI,SPCD,PRODI,GRNWF,DEADWF)
  ! Expose subroutine GETREGNWFCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::GETREGNWFCS

  implicit none
  ! Variables
  integer :: REGN,SPCD
  character(len=*) :: FORSTI
  character(len=*) :: PRODI
  character(2) :: FORST
  character(2) :: PROD
  real    :: GRNWF,DEADWF

  FORST = FORSTI(1:2)
  PROD = PRODI(1:2)
  call GetRegnWF(REGN,FORST,SPCD,GRNWF,DEADWF,PROD)

  return
end
