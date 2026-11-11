!===================================!
!   VolumeLibrary : volumelibrary   !
!===================================!
subroutine VOLUMELIBRARY(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,&
     &DBHOB,&
     &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
     &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
     &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
     &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
     &BA,SI,CTYPE,ERRFLAG,IDIST)

  ! 04-19-2016     Added IDIST as input variable
  ! 09/21/2016  Added biomass calculation

  ! Expose subroutine VOLUMELIBRARY to users of this DLL

      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLUMELIBRARY
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLUMELIBRARY
  !    !DEC$ ATTRIBUTES DECORATE,ALIAS:'_VOLUMELIBRARY@224'::VOLUMELIBRARY
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLUMELIBRARY'::VOLUMELIBRARY

  use CHARMOD
  use DEBUG_MOD
  use MRULES_MOD
  implicit none

  !     Parameters
  ! Variables
  integer :: REGN
  character(len=*) :: FORST
  character(len=*) :: VOLEQ
  real    :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB
  character(len=*) :: HTTYPE
  real    :: HTTOT
  integer :: HTLOG
  real    :: HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2
  integer :: HTREF
  real    :: AVGZ1,AVGZ2
  integer :: FCLASS
  real    :: DBTBH,BTR
  integer :: I3,I7,I15,I20,I21
  real    :: LOGVOL(I7,I20),LOGDIA(I21,I3),LOGLEN(I20)
  real    :: BOLHT(I21)
  integer :: TLOGS
  real    :: NOLOGP,NOLOGS
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,CUSFLG,CDSFLG
  character(len=*) :: PROD
  character(len=*) :: CONSPEC
  integer :: HTTFLL
  character(len=*) :: LIVE
  character(len=*) :: CTYPE
  integer :: ERRFLG
  character(len=2) :: DIST
  character(len=2) :: VAR
  integer :: IDIST
  !     Local variables
  !     Variable required for call to VOLINIT
  integer :: SPFLG
  real    :: VOL(15)
  integer :: BA,SI
  integer :: ERRFLAG
  !     Variable for biomass
  real    :: BRKHT,BRKHTD,DRYBIO(15),GRNBIO(15),CR,CULL,CULLMSTOP
  integer :: FIASPCD,DECAYCD,MRULEFLG
  TYPE(MERCHRULES)::MERRULES
  BRKHT = 0
  BRKHTD = 0
  if (LEN_TRIM(CONSPEC).gt.0) then
     read (CONSPEC,'(I3)') FIASPCD
  else
     FIASPCD = 0
  end if
  CR = 0
  CULL = 0
  CULLMSTOP = 0
  DECAYCD = 0
  MRULEFLG = 0
  MERRULES%COR = 'Y'
  MERRULES%EVOD = 0
  MERRULES%OPT = 22
  MERRULES%MAXLEN = 16.0
  MERRULES%MINLEN = 2.0
  MERRULES%MERCHL = 8.0
  MERRULES%MINLENT = 2.0
  MERRULES%MTOPP = 6.0
  MERRULES%MTOPS = 4.0
  MERRULES%STUMP = 1
  MERRULES%TRIM = 0.3
  MERRULES%BTR = 0
  MERRULES%DBTBH = 0
  MERRULES%MINBFD = 1

  !      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
  !     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
  !     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
  !     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
  !     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
  !     +    BA,SI,CTYPE,ERRFLAG,IDIST)
  call VOLINITNVB(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,CR,CULL,DECAYCD,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST,BRKHT,BRKHTD,FIASPCD,DRYBIO,&
       &GRNBIO,MRULEFLG,MERRULES,CULLMSTOP)

  4000 return

  end subroutine VOLUMELIBRARY

subroutine VOLUMELIBRARY2(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,&
     &DBHOB,&
     &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
     &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,&
     &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
     &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
     &BA,SI,CTYPE,ERRFLAG,IDIST,BRKHT,BRKHTD,FIASPCD,DRYBIO,GRNBIO,&
     &CR,CULL,DECAYCD)
  ! 02/10/2023  Created volumelibrary2 to enable use the new NSVB equations
  ! Expose subroutine VOLUMELIBRARY2 to users of this DLL
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLUMELIBRARY2
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLUMELIBRARY2
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLUMELIBRARY2'::VOLUMELIBRARY2
  use CHARMOD
  use DEBUG_MOD
  use MRULES_MOD
  implicit none
  !     Parameters
  ! Variables
  integer :: REGN
  character(len=*) :: FORST
  character(len=*) :: VOLEQ
  real    :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB
  character(len=*) :: HTTYPE
  real    :: HTTOT
  integer :: HTLOG
  real    :: HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2
  integer :: HTREF
  real    :: AVGZ1,AVGZ2
  integer :: FCLASS
  real    :: DBTBH,BTR
  !      INTEGER         I3, I7, I15, I20, I21
  real    :: LOGVOL(7,20),LOGDIA(21,3),LOGLEN(20)
  real    :: BOLHT(21)
  integer :: TLOGS
  real    :: NOLOGP,NOLOGS
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,CUSFLG,CDSFLG
  character(len=*) :: PROD
  character(len=*) :: CONSPEC
  integer :: HTTFLL
  character(len=*) :: LIVE
  character(len=*) :: CTYPE
  integer :: ERRFLG
  character(len=2) :: DIST
  character(len=2) :: VAR
  integer :: IDIST
  !     Local variables
  !     Variable required for call to VOLINIT
  integer :: SPFLG
  real    :: VOL(15)
  integer :: BA,SI
  integer :: ERRFLAG
  !     Variable for biomass
  real    :: DRYBIO(15),GRNBIO(15),BRKHT,BRKHTD,CR,CULL,CULLMSTOP
  integer :: FIASPCD,MRULEFLG,DECAYCD
  TYPE(MERCHRULES)::MERRULES
  MRULEFLG = 0
  MERRULES%COR = 'Y'
  MERRULES%EVOD = 0
  MERRULES%OPT = 22
  MERRULES%MAXLEN = 16.0
  MERRULES%MINLEN = 2.0
  MERRULES%MERCHL = 8.0
  MERRULES%MINLENT = 2.0
  MERRULES%MTOPP = 6.0
  MERRULES%MTOPS = 4.0
  MERRULES%STUMP = 1
  MERRULES%TRIM = 0.3
  MERRULES%BTR = 0
  MERRULES%DBTBH = 0
  MERRULES%MINBFD = 1

  call VOLINITNVB(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,CR,CULL,DECAYCD,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST,BRKHT,BRKHTD,FIASPCD,DRYBIO,&
       &GRNBIO,MRULEFLG,MERRULES,CULLMSTOP)

  return
end

subroutine FIAVOL(VOLEQ,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,&
     &STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLG,&
     &FIAVTYPE,FIAEQVOL,BFMIND)
  ! Expose subroutine VOLUMELIBRARY to users of this DLL
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::FIAVOL
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: FIAVOL
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'FIAVOL'::FIAVOL
  implicit none
  ! Variables
  character(len=*) :: VOLEQ
  character(len=*) :: FIAVTYPE
  character(len=*) :: GEOSUB
  real    :: DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,VOL(15),BFMIND,STUMP
  real    :: DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,FIAEQVOL
  integer :: BA,SI,ERRFLG,SPN
  call FIA_VOLINIT(VOLEQ,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,&
       &STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLG,&
       &FIAVTYPE,FIAEQVOL,BFMIND)
  return
end

subroutine FIAVOLTYPE(VOLEQ, MTOPP, VOLTYPE, ERRFLAG)
  ! Expose subroutine VOLUMELIBRARY to users of this DLL
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::FIAVOLTYPE
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: FIAVOLTYPE
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'FIAVOLTYPE'::FIAVOLTYPE
  ! Variables
  character(len=*) :: VOLEQ
  character(len=*) :: VOLTYPE
  character(len=10) :: GEOSUB
  character(len=10) :: NVELEQ
  real    :: MTOPP
  integer :: ERRFLAG,SPN
  SPN = 0
  GEOSUB = '0'
  NVELEQ = "          "
  if (MTOPP.eq.0.0) MTOPP = 4.0
  if (MTOPP.gt.20.0) MTOPP = 99.0
  call FIAEQ2NVELEQ(VOLEQ,SPN,GEOSUB,MTOPP,NVELEQ,VOLTYPE,&
       &ERRFLAG)
  return
end

subroutine VOLLIBVB8(EQNUM, REGN,DBHOB, HTTOT, TOPD,&
     &TOTCU, MERCHCU, BDFT, XINT)
  !... 03-23-2015     This function is make the DLL be called from VB.NET

  ! Expose subroutine VOLLIBVB8 to users of this DLL

      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLLIBVB8
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLLIBVB8
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLLIBVB8'::VOLLIBVB8
      !DEC$ ATTRIBUTES REFERENCE :: EQNUM
  implicit none
  ! Variables
  character(len=10) :: EQNUM
  integer :: REGN,ERRFLG
  real    :: DBHOB,HTTOT,TOPD,TOTCU,MERCHCU,BDFT,XINT,MHT
  real    :: VOL(15)

  call VOLLIBVB8INIT(EQNUM, REGN,DBHOB, HTTOT, TOPD,&
       &VOL, MHT, ERRFLG)

  TOTCU = VOL(1)
  MERCHCU = VOL(4)
  BDFT = VOL(2)
  XINT = VOL(10)

  return

  end subroutine VOLLIBVB8

subroutine VOLLIBVB8XHT(EQNUM, REGN,DBHOB, HTTOT, TOPD,&
     &TOTCU, MERCHCU, BDFT, XINT, MHT)
  !... 12-01-2015     This function is make the DLL be called from VB.NET

  ! Expose subroutine VOLLIBVB8 to users of this DLL

      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLLIBVB8XHT
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLLIBVB8XHT
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLLIBVB8XHT'::VOLLIBVB8XHT
      !DEC$ ATTRIBUTES REFERENCE :: EQNUM
  implicit none
  ! Variables
  character(len=10) :: EQNUM
  integer :: REGN,ERRFLG
  real    :: DBHOB,HTTOT,TOPD,TOTCU,MERCHCU,BDFT,XINT,MHT
  real    :: VOL(15)

  call VOLLIBVB8INIT(EQNUM, REGN,DBHOB, HTTOT, TOPD,&
       &VOL, MHT,ERRFLG)

  TOTCU = VOL(1)
  MERCHCU = VOL(4)
  BDFT = VOL(2)
  XINT = VOL(10)
  return

  end subroutine VOLLIBVB8XHT

subroutine VOLLIBVB8INIT(EQNUM, REGN,DBHOB, HTTOT, TOPD,&
     &VOL, MHT, ERRFLAG)

  use CHARMOD
  use DEBUG_MOD

  implicit none

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
  integer :: IDIST

  !   MERCH VARIABLES
  integer :: REGN,HTTFLL,BA,SI
  real    :: STUMP,MTOPP,MTOPS,THT1,MAXLEN,MHT
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
  integer :: TLOGS,IFORST

  !   ARRAYS
  integer :: I15,I21,I20,I7,I3,I,J
  real    :: VOL(15),LOGVOL(7,20)
  real    :: LOGDIA(21,3),LOGLEN(20),BOLHT(21)

  !     Extra variable
  !      INTEGER IFORST
  real    :: TOPD,TOTCU,MERCHCU,BDFT,XINT

  !     Set default value for unused variables
  if (IFORST.gt.99) then
     FORST = '01'
  else
     write (FORST, '(I2)') IFORST
  end if
  if (FORST(2:2) .lt. '0') then
     FORST(2:2) = FORST(1:1)
     FORST(1:1) = '0'
     if (FORST(2:2) .lt. '0') FORST(2:2) = '0'
  end if
  HT1PRD = 0.0
  HT2PRD = 0.0
  FCLASS = 0
  DBTBH = 0.0
  BTR = 0.0
  PROD = '01'
  HTTYPE = 'F'
  HTLOG = 0
  STUMP = 0.0
  UPSHT1 = 0.0
  UPSD1 = 0.0
  AVGZ1 = 0.0
  HTREF = 0
  UPSHT2 = 0.0
  UPSD2 = 0.0
  AVGZ2 = 0.0
  CONSPEC = '    '
  DRCOB = 0.0
  HTTFLL = 0
  BA = 0
  SI = 0
  CTYPE = 'F'
  CUTFLG = 1
  CUPFLG = 1
  SPFLG = 1
  BFPFLG = 1
  if (TOPD.gt.0.0) then
     MTOPP = TOPD
     MTOPS = TOPD
  else
     MTOPP = 0.0
     MTOPS = 0.0
  end if
  I3 = 3
  I7 = 7
  I15 = 15
  I20 = 20
  I21 = 21

  FORST = '01'
  VOLEQ = EQNUM

  call VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST)

  MHT = HT1PRD
  return

  end subroutine VOLLIBVB8INIT

subroutine EZVOLLIB(VOLEQI,DBHOB,HTTOT,VOL)
  ! ADD THIS EAZY LIBRARY FOR USER WITH ONLY DBH AND HEIGHT
  ! 2017/02/08

      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::EZVOLLIB
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: EZVOLLIB
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'EZVOLLIB'::EZVOLLIB
  implicit none

  ! Variables
  character(len=*) :: VOLEQI
  character(len=10) :: VOLEQ
  real    :: DBHOB,HTTOT,TOPD,VOL(15),MHT
  integer :: REGN,ERRFLG

  VOLEQ = VOLEQI(1:10)
  if (VOLEQ(1:1).eq.'A'.or.VOLEQ(1:1).eq.'a') then
     REGN = 10
  else if (VOLEQ(1:1).eq.'B'.or.VOLEQ(1:1).eq.'B') then
     REGN = 7
  else if (VOLEQ(1:1).eq.'I'.or.VOLEQ(1:1).eq.'i') then
     REGN = 1
  else if (VOLEQ(1:1).eq.'H'.or.VOLEQ(1:1).eq.'h') then
     REGN = 5
  else if (VOLEQ(1:1).eq.'F'.or.VOLEQ(1:1).eq.'f') then
     REGN = 6
  else
     read(VOLEQ(1:1),'(I1)') REGN
  end if

  TOPD = 0.0
  call VOLLIBVB8INIT(VOLEQ, REGN,DBHOB, HTTOT, TOPD,&
       &VOL, MHT,ERRFLG)

  VOLEQI = VOLEQ // char(0)
  return
  end subroutine EZVOLLIB

subroutine fiavoltype_r(FIAVOLEQ,FIAVOLTYPE,ERRFLAG)
  ! This subroutine is for R user to get voltype for a FIA voleq      !
  ! YW 12/03/2018
      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::fiavoltype_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'fiavoltype_r_'::fiavoltype_r
  implicit none
  ! Variables
  character(len=10) :: FIAVOLEQ
  character(len=10) :: FIAVOLTYPE
  character(len=10) :: GEOSUB
  character(len=10) :: NVELEQ
  integer :: ERRFLAG,SPN
  character(len=2) :: EQTYPE
  real    :: MTOPP
  SPN = 0
  GEOSUB = '0'
  NVELEQ = "          "
  MTOPP = 99.0
  EQTYPE = FIAVOLEQ(1:2)
  if (EQTYPE.eq.'CU'.or.EQTYPE.eq.'BD'.or.&
       &EQTYPE.eq.'cu'.or.EQTYPE.eq.'bd') then
     call FIAEQ2NVELEQ(FIAVOLEQ,SPN,GEOSUB,MTOPP,NVELEQ,FIAVOLTYPE,&
          &ERRFLAG)
  else
     ERRFLAG = 1
     FIAVOLTYPE = " "
  end if
  return
  end subroutine fiavoltype_r

subroutine fiavol_r(VOLEQ,SPN,DBHOB_d,HTTOT_d,MTOPP_d,VOLTYPE,&
     &FIAVOL_d,ERRFLAG)
  ! This subroutine is for R user to calc FIA vol using FIA voleq or
  ! NVEL voleq. When using NVEL voleq, VOLTYPE is required !
  ! YW 12/03/2018
      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::fiavol_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'fiavol_r_'::fiavol_r
  implicit none
  double precision :: DBHOB_d,HTTOT_d,MTOPP_d,FIAVOL_d
  ! Variables
  character(len=10) :: VOLEQ
  character(len=10) :: VOLTYPE
  character(len=10) :: GEOSUB
  character(len=10) :: VOLEQ2
  integer :: ERRFLAG
  real    :: DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,VOL(15),BFMIND,STUMP
  real    :: DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,FIAEQVOL
  integer :: BA,SI,SPN
  DBHOB = real(DBHOB_d)
  HTTOT = real(HTTOT_d)
  MTOPP = real(MTOPP_d)
  GEOSUB = "0"
  VOLTYPE = "          "
  VOL = 0.0
  FIAEQVOL = 0.0
  HT1PRD = 0.0
  HT2PRD = 0.0
  MTOPS = 0.0
  BFMIND = 0.0
  STUMP = 1.0
  DRCOB = 0.0
  UPSHT1 = 0.0
  UPSD1 = 0.0
  UPSHT2 = 0.0
  UPSD2 = 0.0
  BA = 85
  SI = 65
  VOLEQ2 = VOLEQ
  call FIA_VOLINIT(VOLEQ2,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,&
       &STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLAG,&
       &VOLTYPE,FIAEQVOL,BFMIND)
  DBHOB_d = DBLE(DBHOB)
  HTTOT_d = DBLE(HTTOT)
  MTOPP_d = DBLE(MTOPP)
  FIAVOL_d = DBLE(FIAEQVOL)
  return
  end subroutine fiavol_r

subroutine advfiavol_r(VOLEQ,SPN,DBHOB_d,HTTOT_d,HT1PRD_d,&
     &HT2PRD_d,MTOPP_d,UPSTEMHT_d,UPSTEMDIA_d,BROKENHT_d,&
     &CENTROIDHT_d,CENTROIDHTDIA_d,STANDBA,SI,GEOSUB,VOLTYPE,&
     &FIAVOL_d,ERRFLAG)
  ! This subroutine is for R user to calc FIA vol using FIA voleq or
  ! NVEL voleq. When using NVEL voleq, VOLTYPE is required !
  ! This is the advanced version and has more input variables.
  ! YW 12/03/2018
      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::advfiavol_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'advfiavol_r_'::advfiavol_r
  implicit none
  double precision :: DBHOB_d,HTTOT_d,HT1PRD_d,HT2PRD_d,MTOPP_d
  double precision :: UPSTEMHT_d,UPSTEMDIA_d,BROKENHT_d,FIAVOL_d
  double precision :: CENTROIDHT_d,CENTROIDHTDIA_d
  ! Variables
  character(len=10) :: VOLEQ
  character(len=10) :: VOLTYPE
  character(len=10) :: GEOSUB
  character(len=10) :: VOLEQ2
  integer :: SPN,ERRFLAG,STANDBA,SI,BA
  real    :: DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,VOL(15),STUMP
  real    :: DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,FIAVOL,BFMIND
  VOL = 0.0
  FIAVOL = 0.0
  ERRFLAG = 0
  DBHOB = real(DBHOB_d)
  HTTOT = real(HTTOT_d)
  HT1PRD = real(HT1PRD_d)
  HT2PRD = real(HT2PRD_d)
  MTOPP = real(MTOPP_d)
  UPSHT1 = real(UPSTEMHT_d)
  UPSD1 = real(UPSTEMDIA_d)
  if (CENTROIDHT_d.gt.0.0.and.CENTROIDHTDIA_d.gt.0.0) then
     UPSHT2 = real(CENTROIDHT_d)
     UPSD2 = real(CENTROIDHTDIA_d)
  else if (BROKENHT_d.gt.0.0) then
     UPSHT2 = real(BROKENHT_d)
     UPSD2 = 0.0
  else
     UPSHT2 = 0.0
     UPSD2 = 0.0
  end if
  BA = STANDBA
  VOLEQ2 = VOLEQ
  call FIA_VOLINIT(VOLEQ2,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,&
       &STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLAG,&
       &VOLTYPE,FIAVOL,BFMIND)
  FIAVOL_d = DBLE(FIAVOL)
  return
  end subroutine advfiavol_r

subroutine vollib_r(VOLEQ,REGN,FORST,DIST,SPEC,DBHOB_d,HTTOT_d,&
     &MTOPP_d,MTOPS_d,HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d,STUMP_d,&
     &FCLASS,DBTBH_d,BTR_d,VOL_d, ERRFLAG)
  ! This subroutine is for R user to calculate volume from vollib      !
  ! YW 02/10/2017

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::vollib_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'vollib_r_'::vollib_r

  use CHARMOD
  use DEBUG_MOD
  use VOLINPUT_MOD

  implicit none

  double precision :: DBHOB_d,HTTOT_d,MTOPP_d,MTOPS_d,STUMP_d
  double precision :: HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d
  double precision :: DBTBH_d,BTR_d,VOL_d(15)

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

  integer :: SPEC,TMPSPEC,NULEQ

  !   MERCH VARIABLES
  integer :: REGN,HTTFLL,BA,SI,IFORST,IDIST
  real    :: STUMP,MTOPP,MTOPS !,THT1,MAXLEN
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
  real    :: TIPDIB,TIPLEN

  !   Tree variables
  real    :: HTTOT,HT1PRD,HT2PRD !,LEFTOV
  real    :: DBHOB,DRCOB,DBTBH,BTR !,CR,TRIM
  integer :: FCLASS,HTLOG !,SPCODE, WHOLELOGS

  !    3RD POINT VARIABLES
  real    :: UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2
  integer :: HTREF

  !   OUTPUTS
  real    :: NOLOGP,NOLOGS
  integer :: TLOGS !,IFORST, IDIST

  !   ARRAYS
  integer :: I15,I21,I20,I7,I3,I,J
  real    :: VOL(15),LOGVOL(7,20)
  real    :: LOGDIA(21,3),LOGLEN(20),BOLHT(21)

  DBHOB = real(DBHOB_d)
  HTTOT = real(HTTOT_d)
  MTOPP = real(MTOPP_d)
  MTOPS = real(MTOPS_d)
  HT1PRD = real(HT1PRD_d)
  HT2PRD = real(HT2PRD_d)
  UPSHT1 = real(UPSHT1_d)
  UPSD1 = real(UPSD1_d)
  STUMP = real(STUMP_d)
  DBTBH = real(DBTBH_d)
  BTR = real(BTR_d)

  read (DIST, '(I2)') IDIST
  !     Set the default value for other variable
  PROD = '01'
  HTTYPE = 'F'
  HTLOG = 0
  AVGZ1 = 0.0
  HTREF = 0
  UPSHT2 = 0.0
  UPSD2 = 0.0
  AVGZ2 = 0.0
  CONSPEC = '    '
  DRCOB = 0.0
  HTTFLL = 0
  BA = 0
  SI = 0
  CTYPE = 'F'
  CUTFLG = 1
  CUPFLG = 1
  SPFLG = 1
  BFPFLG = 1
  I3 = 3
  I7 = 7
  I15 = 15
  I20 = 20
  I21 = 21

  call VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST)

  VOL_d = DBLE(VOL)
  DBHOB_d = DBLE(DBHOB)
  HTTOT_d = DBLE(HTTOT)
  MTOPP_d = DBLE(MTOPP)
  MTOPS_d = DBLE(MTOPS)
  HT1PRD_d = DBLE(HT1PRD)
  HT2PRD_d = DBLE(HT2PRD)
  UPSHT1_d = DBLE(UPSHT1)
  UPSD1_d = DBLE(UPSD1)
  STUMP_d = DBLE(STUMP)
  DBTBH_d = DBLE(DBTBH)
  BTR_d = DBLE(BTR)

  999 continue
  return
  end subroutine vollib_r

subroutine vollib2_r(VOLEQ,REGN,FORST,DIST,SPEC,DBHOB_d,HTTOT_d,&
     &MTOPP_d,MTOPS_d,HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d,STUMP_d,&
     &FCLASS,DBTBH_d,BTR_d,VOL_d,LOGVOL_d,LOGDIA_d,LOGLEN_d,BOLHT_d,&
     &TLOGS,NOLOGP_d,NOLOGS_d,ERRFLAG)
  ! This subroutine is for R user to calculate volume from vollib      !
  ! with output variable for logs LOGDIA,LOGVOL,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS
  ! YW 07/29/2020

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::vollib2_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'vollib2_r_'::vollib2_r

  use CHARMOD
  use DEBUG_MOD
  use VOLINPUT_MOD

  implicit none

  double precision :: DBHOB_d,HTTOT_d,MTOPP_d,MTOPS_d,STUMP_d
  double precision :: HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d
  double precision :: DBTBH_d,BTR_d,VOL_d(15)
  double precision :: LOGVOL_d(7,20),LOGDIA_d(21,3),LOGLEN_d(20)
  double precision :: BOLHT_d(21),NOLOGP_d,NOLOGS_d

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

  integer :: SPEC,TMPSPEC,NULEQ

  !   MERCH VARIABLES
  integer :: REGN,HTTFLL,BA,SI,IFORST,IDIST
  real    :: STUMP,MTOPP,MTOPS !,THT1,MAXLEN
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
  real    :: TIPDIB,TIPLEN

  !   Tree variables
  real    :: HTTOT,HT1PRD,HT2PRD !,LEFTOV
  real    :: DBHOB,DRCOB,DBTBH,BTR !,CR,TRIM
  integer :: FCLASS,HTLOG !,SPCODE, WHOLELOGS

  !    3RD POINT VARIABLES
  real    :: UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2
  integer :: HTREF

  !   OUTPUTS
  real    :: NOLOGP,NOLOGS
  integer :: TLOGS !,IFORST, IDIST

  !   ARRAYS
  integer :: I15,I21,I20,I7,I3,I,J
  real    :: VOL(15),LOGVOL(7,20)
  real    :: LOGDIA(21,3),LOGLEN(20),BOLHT(21)

  DBHOB = real(DBHOB_d)
  HTTOT = real(HTTOT_d)
  MTOPP = real(MTOPP_d)
  MTOPS = real(MTOPS_d)
  HT1PRD = real(HT1PRD_d)
  HT2PRD = real(HT2PRD_d)
  UPSHT1 = real(UPSHT1_d)
  UPSD1 = real(UPSD1_d)
  STUMP = real(STUMP_d)
  DBTBH = real(DBTBH_d)
  BTR = real(BTR_d)

  read (DIST, '(I2)') IDIST
  !     Set the default value for other variable
  PROD = '01'
  HTTYPE = 'F'
  HTLOG = 0
  AVGZ1 = 0.0
  HTREF = 0
  UPSHT2 = 0.0
  UPSD2 = 0.0
  AVGZ2 = 0.0
  CONSPEC = '    '
  DRCOB = 0.0
  HTTFLL = 0
  BA = 0
  SI = 0
  CTYPE = 'F'
  CUTFLG = 1
  CUPFLG = 1
  SPFLG = 1
  BFPFLG = 1
  I3 = 3
  I7 = 7
  I15 = 15
  I20 = 20
  I21 = 21

  call VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST)

  VOL_d = DBLE(VOL)
  DBHOB_d = DBLE(DBHOB)
  HTTOT_d = DBLE(HTTOT)
  MTOPP_d = DBLE(MTOPP)
  MTOPS_d = DBLE(MTOPS)
  HT1PRD_d = DBLE(HT1PRD)
  HT2PRD_d = DBLE(HT2PRD)
  UPSHT1_d = DBLE(UPSHT1)
  UPSD1_d = DBLE(UPSD1)
  STUMP_d = DBLE(STUMP)
  DBTBH_d = DBLE(DBTBH)
  BTR_d = DBLE(BTR)
  LOGVOL_d = DBLE(LOGVOL)
  LOGDIA_d = DBLE(LOGDIA)
  LOGLEN_d = DBLE(LOGLEN)
  BOLHT_d = DBLE(BOLHT)
  NOLOGP_d = DBLE(NOLOGP)
  NOLOGS_d = DBLE(NOLOGS)

  return
  end subroutine vollib2_r

subroutine vollibnvb_r(VOLEQ,REGN,FORST,DIST,SPEC,DBHOB_d,HTTOT_d,&
     &MTOPP_d,MTOPS_d,HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d,STUMP_d,&
     &FCLASS,DBTBH_d,BTR_d,VOL_d,LOGVOL_d,LOGDIA_d,LOGLEN_d,BOLHT_d,&
     &TLOGS,NOLOGP_d,NOLOGS_d,ERRFLAG,BRKHT_d,BRKHTD_d,&
     &DRYBIO_d,GRNBIO_d,CR_d,CULL_d,DECAYCD,CULLMSTOP_d,CTYPE,LIVE,&
     &PROD)
  ! This subroutine is for R user to calculate volume from vollib      !
  ! with output variable for logs LOGDIA,LOGVOL,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS
  ! and new NSVB equations for biomass DRYBIO, GRNBIO
  ! YW 02/10/2023

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::vollibnvb_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'vollibnvb_r_'::vollibnvb_r

  use CHARMOD
  use DEBUG_MOD
  use VOLINPUT_MOD
  use MRULES_MOD

  implicit none

  double precision :: DBHOB_d,HTTOT_d,MTOPP_d,MTOPS_d,STUMP_d
  double precision :: HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d
  double precision :: DBTBH_d,BTR_d,VOL_d(15)
  double precision :: LOGVOL_d(7,20),LOGDIA_d(21,3),LOGLEN_d(20)
  double precision :: BOLHT_d(21),NOLOGP_d,NOLOGS_d
  double precision :: BRKHT_d,BRKHTD_d,CR_d,CULL_d,CULLMSTOP_d
  double precision :: DRYBIO_d(15),GRNBIO_d(15)

  ! Variables
  character(len=1) :: HTTYPE
  character(len=1) :: LIVE
  character(len=1) :: CTYPE
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=4) :: CONSPEC
  character(len=11) :: VOLEQ
  character(len=3) :: MDL
  character(len=3) :: SPECIES
  character(len=2) :: DIST
  character(len=2) :: VAR

  integer :: SPEC,TMPSPEC,NULEQ

  !   MERCH VARIABLES
  integer :: REGN,HTTFLL,BA,SI,IFORST,IDIST
  real    :: STUMP,MTOPP,MTOPS
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
  real    :: TIPDIB,TIPLEN

  !   Tree variables
  real    :: HTTOT,HT1PRD,HT2PRD
  real    :: DBHOB,DRCOB,DBTBH,BTR
  integer :: FCLASS,HTLOG

  !    3RD POINT VARIABLES
  real    :: UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2
  integer :: HTREF

  !   OUTPUTS
  real    :: NOLOGP,NOLOGS
  integer :: TLOGS

  !   ARRAYS
  !      INTEGER   I15,I21,I20,I7,I3,I,J
  real    :: VOL(15),LOGVOL(7,20)
  real    :: LOGDIA(21,3),LOGLEN(20),BOLHT(21)
  real    :: DRYBIO(15),GRNBIO(15),BRKHT,BRKHTD,CR,CULL,CULLMSTOP
  integer :: FIASPCD,MRULEFLG,DECAYCD
  TYPE(MERCHRULES)::MERRULES
  character(len=11) :: NVBEQ

  NVBEQ = VOLEQ
  DBHOB = real(DBHOB_d)
  HTTOT = real(HTTOT_d)
  MTOPP = real(MTOPP_d)
  MTOPS = real(MTOPS_d)
  HT1PRD = real(HT1PRD_d)
  HT2PRD = real(HT2PRD_d)
  UPSHT1 = real(UPSHT1_d)
  UPSD1 = real(UPSD1_d)
  STUMP = real(STUMP_d)
  DBTBH = real(DBTBH_d)
  BTR = real(BTR_d)
  BRKHT = real(BRKHT_d)
  BRKHTD = real(BRKHTD_d)
  CR = real(CR_d)
  CULL = real(CULL_d)
  CULLMSTOP = real(CULLMSTOP_d)

  read (DIST, '(I2)') IDIST
  !     Set the default value for other variable
  !PROD='01'
  HTTYPE = 'F'
  HTLOG = 0
  AVGZ1 = 0.0
  HTREF = 0
  UPSHT2 = 0.0
  UPSD2 = 0.0
  AVGZ2 = 0.0
  CONSPEC = '    '
  DRCOB = 0.0
  HTTFLL = 0
  BA = 0
  SI = 0
  !CTYPE='F'
  CUTFLG = 1
  CUPFLG = 1
  SPFLG = 1
  BFPFLG = 1
  !      I3 = 3
  !      I7 = 7
  !      I15 = 15
  !      I20 = 20
  !      I21 =21
  FIASPCD = SPEC
  MRULEFLG = 0
  call VOLINITNVB(REGN,FORST,NVBEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,CR,CULL,DECAYCD,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST,BRKHT,BRKHTD,FIASPCD,DRYBIO,&
       &GRNBIO,MRULEFLG,MERRULES,CULLMSTOP)
  !      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
  !     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
  !     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
  !     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
  !     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
  !     +    BA,SI,CTYPE,ERRFLAG,IDIST)

  VOL_d = DBLE(VOL)
  !      DBHOB_d = DBLE(DBHOB)
  !      HTTOT_d = DBLE(HTTOT)
  MTOPP_d = DBLE(MTOPP)
  MTOPS_d = DBLE(MTOPS)
  HT1PRD_d = DBLE(HT1PRD)
  HT2PRD_d = DBLE(HT2PRD)
  !      UPSHT1_d = DBLE(UPSHT1)
  !      UPSD1_d = DBLE(UPSD1)
  STUMP_d = DBLE(STUMP)
  !      DBTBH_d = DBLE(DBTBH)
  !      BTR_d = DBLE(BTR)
  LOGVOL_d = DBLE(LOGVOL)
  LOGDIA_d = DBLE(LOGDIA)
  LOGLEN_d = DBLE(LOGLEN)
  BOLHT_d = DBLE(BOLHT)
  NOLOGP_d = DBLE(NOLOGP)
  NOLOGS_d = DBLE(NOLOGS)
  DRYBIO_d = DBLE(DRYBIO)
  GRNBIO_d = DBLE(GRNBIO)

  return
  end subroutine vollibnvb_r

subroutine vollibnvb2_r(VOLEQ,REGN,FORST,DIST,SPEC,DBHOB_d,&
     &HTTOT_d,&
     &MTOPP_d,MTOPS_d,HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d,STUMP_d,&
     &FCLASS,DBTBH_d,BTR_d,VOL_d,LOGVOL_d,LOGDIA_d,LOGLEN_d,BOLHT_d,&
     &TLOGS,NOLOGP_d,NOLOGS_d,ERRFLAG,BRKHT_d,BRKHTD_d,&
     &DRYBIO_d,GRNBIO_d,CR_d,CULL_d,DECAYCD,CULLMSTOP_d,CTYPE,LIVE,&
     &MAXLOGLEN_d)
  ! This subroutine is for R user to calculate volume from vollib      !
  ! with output variable for logs LOGDIA,LOGVOL,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS
  ! and new NSVB equations for biomass DRYBIO, GRNBIO
  ! and also having MAXLOGLEN to allow change log length
  ! YW 08/05/2023

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::vollibnvb2_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'vollibnvb2_r_'::vollibnvb2_r

  use CHARMOD
  use DEBUG_MOD
  use VOLINPUT_MOD
  use MRULES_MOD

  implicit none

  double precision :: DBHOB_d,HTTOT_d,MTOPP_d,MTOPS_d,STUMP_d
  double precision :: HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d
  double precision :: DBTBH_d,BTR_d,VOL_d(15),MAXLOGLEN_d
  double precision :: LOGVOL_d(7,20),LOGDIA_d(21,3),LOGLEN_d(20)
  double precision :: BOLHT_d(21),NOLOGP_d,NOLOGS_d
  double precision :: BRKHT_d,BRKHTD_d,CR_d,CULL_d,CULLMSTOP_d
  double precision :: DRYBIO_d(15),GRNBIO_d(15)

  ! Variables
  character(len=1) :: HTTYPE
  character(len=1) :: LIVE
  character(len=1) :: CTYPE
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=4) :: CONSPEC
  character(len=11) :: VOLEQ
  character(len=3) :: MDL
  character(len=3) :: SPECIES
  character(len=2) :: DIST
  character(len=2) :: VAR

  integer :: SPEC,TMPSPEC,NULEQ

  !   MERCH VARIABLES
  integer :: REGN,HTTFLL,BA,SI,IFORST,IDIST
  real    :: STUMP,MTOPP,MTOPS,MAXLOGLEN
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
  real    :: TIPDIB,TIPLEN

  !   Tree variables
  real    :: HTTOT,HT1PRD,HT2PRD
  real    :: DBHOB,DRCOB,DBTBH,BTR
  integer :: FCLASS,HTLOG

  !    3RD POINT VARIABLES
  real    :: UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2
  integer :: HTREF

  !   OUTPUTS
  real    :: NOLOGP,NOLOGS
  integer :: TLOGS

  !   ARRAYS
  !      INTEGER   I15,I21,I20,I7,I3,I,J
  real    :: VOL(15),LOGVOL(7,20)
  real    :: LOGDIA(21,3),LOGLEN(20),BOLHT(21)
  real    :: DRYBIO(15),GRNBIO(15),BRKHT,BRKHTD,CR,CULL,CULLMSTOP
  integer :: FIASPCD,MRULEFLG,DECAYCD
  TYPE(MERCHRULES)::MERRULES
  character(len=11) :: NVBEQ

  NVBEQ = VOLEQ
  DBHOB = real(DBHOB_d)
  HTTOT = real(HTTOT_d)
  MTOPP = real(MTOPP_d)
  MTOPS = real(MTOPS_d)
  HT1PRD = real(HT1PRD_d)
  HT2PRD = real(HT2PRD_d)
  UPSHT1 = real(UPSHT1_d)
  UPSD1 = real(UPSD1_d)
  STUMP = real(STUMP_d)
  DBTBH = real(DBTBH_d)
  BTR = real(BTR_d)
  BRKHT = real(BRKHT_d)
  BRKHTD = real(BRKHTD_d)
  CR = real(CR_d)
  CULL = real(CULL_d)
  CULLMSTOP = real(CULLMSTOP_d)
  MAXLOGLEN = real(MAXLOGLEN_d)

  read (DIST, '(I2)') IDIST
  !     Set the default value for other variable
  PROD = '01'
  HTTYPE = 'F'
  HTLOG = 0
  AVGZ1 = 0.0
  HTREF = 0
  UPSHT2 = 0.0
  UPSD2 = 0.0
  AVGZ2 = 0.0
  CONSPEC = '    '
  DRCOB = 0.0
  HTTFLL = 0
  BA = 0
  SI = 0
  !CTYPE='F'
  CUTFLG = 1
  CUPFLG = 1
  SPFLG = 1
  BFPFLG = 1
  !      I3 = 3
  !      I7 = 7
  !      I15 = 15
  !      I20 = 20
  !      I21 =21
  FIASPCD = SPEC
  MRULEFLG = 1
  !only set the max log length. all others using default
  MERRULES%EVOD = 0
  MERRULES%OPT = 0
  MERRULES%MAXLEN = MAXLOGLEN
  MERRULES%MINLEN = 0
  MERRULES%MERCHL = 0
  MERRULES%MINLENT = 0
  MERRULES%MTOPP = 0
  MERRULES%MTOPS = 0
  MERRULES%STUMP = 0
  MERRULES%TRIM = 0
  MERRULES%BTR = 0
  MERRULES%DBTBH = 0
  MERRULES%MINBFD = 0
  call VOLINITNVB(REGN,FORST,NVBEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,CR,CULL,DECAYCD,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST,BRKHT,BRKHTD,FIASPCD,DRYBIO,&
       &GRNBIO,MRULEFLG,MERRULES,CULLMSTOP)
  !      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
  !     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
  !     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
  !     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
  !     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
  !     +    BA,SI,CTYPE,ERRFLAG,IDIST)

  VOL_d = DBLE(VOL)
  !      DBHOB_d = DBLE(DBHOB)
  !      HTTOT_d = DBLE(HTTOT)
  MTOPP_d = DBLE(MTOPP)
  MTOPS_d = DBLE(MTOPS)
  HT1PRD_d = DBLE(HT1PRD)
  HT2PRD_d = DBLE(HT2PRD)
  !      UPSHT1_d = DBLE(UPSHT1)
  !      UPSD1_d = DBLE(UPSD1)
  STUMP_d = DBLE(STUMP)
  !      DBTBH_d = DBLE(DBTBH)
  !      BTR_d = DBLE(BTR)
  LOGVOL_d = DBLE(LOGVOL)
  LOGDIA_d = DBLE(LOGDIA)
  LOGLEN_d = DBLE(LOGLEN)
  BOLHT_d = DBLE(BOLHT)
  NOLOGP_d = DBLE(NOLOGP)
  NOLOGS_d = DBLE(NOLOGS)
  DRYBIO_d = DBLE(DRYBIO)
  GRNBIO_d = DBLE(GRNBIO)

  return
  end subroutine vollibnvb2_r

  ! Get regional default weight factor
subroutine GETWTFAC(REGN,FORST,SPCD,WTFAC)
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::GETWTFAC
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: GETWTFAC
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'GETWTFAC'::GETWTFAC
  implicit none
  ! Variables
  integer :: REGN,SPCD
  character(len=*) :: FORST
  character(len=2) :: FORSTI
  real    :: WTFAC,WF(3)
  character(len=12) :: BMSEQ(8)
  character(len=50) :: REF(8)
  FORSTI = FORST(1:2)
  WF = 0
  call CRZSPDFT(REGN,FORSTI,SPCD,WF,BMSEQ,REF)
  WTFAC = WF(1)
  return
end

  ! YW 2019/05/07 ADDED INPUT VARIABLES HT1PRD,HT2PRD,HTTFLL
  ! YW 2019/08/05 Added input variable GROSUB. This is for pacific islands equation
  ! YW 2023/03/15 Corrected subroutine biolib and biolib2
subroutine BIOLIB(REGN,FORST,SPEC,BIOEQ,DBHOB,HTTOT,VOL,&
     &BIOGRN, BIODRY,ERRFLG,HT1PRD,HT2PRD,HTTFLL,GEOSUB)
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::BIOLIB
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: BIOLIB
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'BIOLIB'::BIOLIB

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
  BIOGRN = 0
  BIODRY = 0
  call BIOLIB2(REGN,FORSTI,SPEC,BIOEQI,DBHOB,HTTOT,VOL,&
       &BIOGRN, BIODRY,ERRFLG,HT1PRD,HT2PRD,HTTFLL,GEOSUBI)

  return
end

subroutine BIOLIB2(REGN,FORST,SPEC,BIOEQ,DBHOB,HTTOT,VOL,&
     &BIOGRN, BIODRY,ERRFLG,HT1PRD,HT2PRD,HTTFLL,GEOSUB)
  ! This is duplicated subroutine BIOLIB, but not using pointer for FORST, BIOEQ, GEOSUB
  implicit none
  ! Variables
  integer :: REGN,SPEC,ERRFLG
  character(len=2) :: FORST
  character(len=2) :: GEOSUB
  character(len=12) :: BIOEQ
  real    :: DBHOB,HTTOT,VOL(15),BIOGRN(9),BIODRY(9)

  real    :: BIOMS(8),SG(11),WF(3),MC,RATIO,STMGRNWT,STMDRYWT
  character(len=12) :: BMSEQ(8)
  character(len=12) :: NVELBEQ
  character(len=12) :: FIAEQ
  character(len=12) :: GEOSUB2
  character(len=50) :: REF(8)
  real    :: HT1PRD,HT2PRD,TOPD,CR,BIOMASS,VOLM(15),HTTFLL
  integer :: STEMS,I,EQLEN,NOINT,FIAEQNUM,STAT
  ERRFLG = 0
  do 100, I = 1,9
  BIOGRN(I) = 0.0
  BIODRY(I) = 0.0
  100 continue
  !     Call Jenkin's to calculate biomass
  call JENKINS(SPEC, DBHOB, BIOMS)
  !     The elements in BIOMS are dry weight in pounds as below:
  !     1 ABOVE GROUND TOTAL
  !     2 MERCH STEM WOOD
  !     3 MERCH STEM BARK
  !     4 FOLIAGE
  !     5 ROOTS
  !     6 BRANCHES
  !     7 CROWN
  !     8 MERCH STEM WOOD AND BARK

  !     GET REGIONAL OR NATIONAL DEFAULT weight factor
  WF = 0.0
  call CRZSPDFT(REGN,FORST,SPEC,WF,BMSEQ,REF)

  !     Get the moisture content from Miles $ Smith 2009
  if (WF(3).eq.0) then
     call MILESDATA(SPEC,SG)
     WF(3) = (SG(9)-SG(10))/SG(10)*100.0
  end if
  MC = WF(3)/100.0
  !     Calculate merch stem green weight using cubic feet volume and weight factor
  STMGRNWT = WF(1)*(VOL(4)+VOL(7))
  STMDRYWT = STMGRNWT/(1.0+MC)
  !     GET the ratio for stem calculated from weight factor and Jenkins
  if (BIOMS(8).gt.0) then
     RATIO = STMDRYWT/BIOMS(8)
  else
     RATIO = 1.0
  end if
  if (RATIO.le.0) RATIO = 1
  !     Apply the ratio to biomass calculated from Jenkins and also add MC to get green weight
  do 200, I = 1,8
  BIODRY(I) = BIOMS(I)*RATIO
  BIOGRN(I) = BIODRY(I)*(1+MC)
  200 continue
  !RETURN
  !     If BIOEQ is provided, calculate biomass from it
  EQLEN = LEN_TRIM(BIOEQ)
  FIAEQNUM = -1
  if (EQLEN.gt.0) then
     call str2int(BIOEQ, FIAEQNUM, STAT)
     if (STAT.eq.0) then
        GEOSUB2 = GEOSUB(1:1)
        ERRFLG = 0
        call FIABEQ2NVELBEQ(FIAEQNUM,SPEC,NVELBEQ,GEOSUB2,ERRFLG)
        if (FIAEQNUM.eq.109) then
           BIODRY(9) = 0.0
           BIOGRN(9) = 0.0
           return
        end if
     else
        NVELBEQ = BIOEQ
     end if
     !IF(NVELBEQ(12:12).NE.'D'.OR.NVELBEQ(12:12).NE.'G') RETURN
     !       Set default values
     CR = (HTTOT-HTTFLL)/HTTOT
     if (CR.le.0.0.or.CR.gt.1.0) CR = 0.5
     !        HT1PRD = 0
     !        HT2PRD = 0
     TOPD = 0
     STEMS = 1
     ERRFLG = 0
     VOLM = VOL
     GEOSUB2 = GEOSUB(1:2)
     call BiomassLibrary2(NVELBEQ,DBHOB,HTTOT,CR,HT1PRD,&
          &HT2PRD,TOPD,STEMS,VOLM,BIOMASS,ERRFLG,SPEC,GEOSUB2)
     if (NVELBEQ(12:12).eq.'D') then
        !The result returned from BiomassLibrary2 is dry/green biomass
        !Here save the green and dry
        !biomass into different variables.
        BIODRY(9) = BIOMASS
        BIOGRN(9) = BIOMASS*(1.0+MC)
     else if (NVELBEQ(12:12).eq.'G') then
        BIOGRN(9) = BIOMASS
        BIODRY(9) = BIOMASS/(1.0+MC)
     else
        BIODRY(9) = BIOMASS
        BIOGRN(9) = BIOMASS
     end if
  end if
  ERRFLG = 0
  return
end

subroutine VOLLIBVB(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,&
     &DBHOB,&
     &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
     &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
     &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
     &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
     &BA,SI,CTYPE,ERRFLAG,IDIST,MERRULES)

  ! 2017/09/18 YW Created this subroutine for VB.NET to call the vollib with MRULES input
  ! Expose subroutine VOLUMELIBRARY to users of this DLL

      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLLIBVB
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLLIBVB
  !    !DEC$ ATTRIBUTES DECORATE,ALIAS:'_VOLUMELIBRARY@224'::VOLUMELIBRARY
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLLIBVB'::VOLLIBVB

  use CHARMOD
  use DEBUG_MOD
  use MRULES_MOD

  implicit none

  !     Parameters
  ! Variables
  integer :: REGN
  character(len=*) :: FORST
  character(len=*) :: VOLEQ
  real    :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB
  character(len=*) :: HTTYPE
  real    :: HTTOT
  integer :: HTLOG
  real    :: HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2
  integer :: HTREF
  real    :: AVGZ1,AVGZ2
  integer :: FCLASS
  real    :: DBTBH,BTR
  integer :: I3,I7,I15,I20,I21
  real    :: LOGVOL(I7,I20),LOGDIA(I21,I3),LOGLEN(I20)
  real    :: BOLHT(I21)
  integer :: TLOGS
  real    :: NOLOGP,NOLOGS
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,CUSFLG,CDSFLG
  character(len=*) :: PROD
  character(len=*) :: CONSPEC
  integer :: HTTFLL
  character(len=*) :: LIVE
  character(len=*) :: CTYPE
  integer :: ERRFLG
  character(len=2) :: DIST
  character(len=2) :: VAR
  integer :: IDIST
  !     Local variables
  !     Variable required for call to VOLINIT
  integer :: SPFLG
  real    :: VOL(15)
  integer :: BA,SI
  integer :: ERRFLAG
  TYPE(MERCHRULES):: MERRULES

  call VOLINIT2(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG, MERRULES,IDIST)

  1000 return

  end subroutine VOLLIBVB

subroutine str2int(str,int,stat)
  implicit none
  ! Arguments
  ! stat = 0, convertion OK
  ! stat != 0, cinversion error
  ! Variables
  character(len=*),intent(in) :: str
  integer,intent(out)         :: int
  integer,intent(out)         :: stat
  integer :: i
  !read(str,*,iostat=stat)  int
  stat = 0
  do i = 1, len(str)
     if (str(i:i) < '0' .or. str(i:i) > '9') then
        stat = i
        exit
     end if
  end do
  if (stat.eq.0) read(str, *) int

  end subroutine str2int
