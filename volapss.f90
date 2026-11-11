!=============================!
!   VolumeLibrary : volapss   !
!=============================!
subroutine VOLAPSS(REGN, FORSTC, VOLEQC,I3, I7, I15, I20, I21,&
     &DBHOB, DRCOB, HTTYPEC, HTTOT, HTLOG, HT1PRD, HT2PRD,&
     &HTREF, FCLASS, VOL, LOGVOLC, LOGDIAC, LOGLEN, BOLHT, TLOGS,&
     &NOLOGP, NOLOGS, CUTFLG, BFPFLG, CUPFLG, CDPFLG, SPFLG, PRODC,&
     &LIVEC, CTYPEC, ERRFLAG, INDEB, MERRULES)

  !... Main entry point into dll

  use CHARMOD
  use MRULES_MOD
  use DEBUG_MOD

  implicit none

  !... Expose subroutine VOLLIB09 to users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::VOLAPSS
  !      !DEC$ ATTRIBUTES NOMIXED_STR_LEN_ARG :: VOLLIB09

  !... Compiler adds and underscore to the beginning and @#bytes
  !... to each external call...must create alias to "see" it
  !      !DEC$ ATTRIBUTES ALIAS:'_VOLAPSS@196' :: VOLAPSS

  !   THERE ARE CALLS TO THE FOLLOWING SUBROUTINES
  !     PROFILE - EXTERNAL
  !     DVE     - EXTERNAL
  !     R4VOL  - EXTERNAL
  !     R6VOL  - EXTERNAL
  !     BLMVOL - EXTERNAL
  !     R8VOL  - EXTERNAL
  !     R10VOL - EXTERNAL
  !     R12VOL - EXTERNAL

  !      !DEC$ ATTRIBUTES DLLIMPORT::VOLCAL

  !     Parameters
  ! Variables
  integer :: REGN
  TYPE(CHAR256):: FORSTC, VOLEQC
  real    :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB
  TYPE(CHAR256):: HTTYPEC
  real    :: HTTOT
  integer :: HTLOG
  real    :: HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2
  integer :: HTREF
  real    :: AVGZ1,AVGZ2
  integer :: FCLASS
  real    :: DBTBH,BTR
  integer :: I3,I7,I15,I20,I21
  real    :: VOL(15),LOGVOLC(20,7)
  real    :: LOGDIAC(I3,I21),LOGLEN(I20),BOLHT(I21)
  integer :: TLOGS
  real    :: NOLOGP,NOLOGS
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
  TYPE(CHAR256):: CONSPECC, PRODC
  integer :: HTTFLL
  TYPE(CHAR256):: LIVEC
  integer :: BA,SI
  TYPE(CHAR256):: CTYPEC
  integer :: ERRFLAG
  integer :: INDEB
  TYPE(MERCHRULES):: MERRULES

  !     Local variables
  character(FORSTC%LENGTH) :: FORST
  character(VOLEQC%LENGTH) :: VOLEQ
  character(HTTYPEC%LENGTH) :: HTTYPE
  character(len=5) :: CONSPEC
  character(PRODC%LENGTH) :: PROD
  character(LIVEC%LENGTH) :: LIVE
  character(CTYPEC%LENGTH) :: CTYPE
  character(len=3) :: MDL
  character(len=3) :: SPECIES
  character(len=2) :: DIST
  character(len=2) :: VAR
  character(len=10) :: EQNUM
  integer :: SPEC
  real    :: LOGVOL(I7,I20),LOGDIA(I21,I3)
  integer :: IDIST
  !     MERCH VARIABLES
  real    :: THT1,MAXLEN

  !     TREE VARIABLES
  real    :: CR,TRIM
  integer :: SPCODE

  !     OUTPUTS
  integer :: IFORST

  !     ARRAYS
  integer :: I,J

  if (INDEB .eq.1) then
     ANY_DEBUG = .true.
     DEBUG%VOLEQ = .true.
     DEBUG%MODEL = .true.
  end if

  if (ANY_DEBUG) then
     open (UNIT = LUDBG, FILE = 'Debug.txt', STATUS = 'UNKNOWN')
     write (LUDBG,5)'Debugging NVEL'
     5 format(A)
  end if

  if (DEBUG%MODEL) then
     write  (LUDBG, 10) ' -->Enter VOLAPSS'
     10 format (A)
  end if

  !Convert the CHAR256 types to Char(*) so I don't have to rename
  !all references below
  !CHAR256 is a user defined type used for passing C strings back/
  !forth to Fortran
  !CHAR256 is equivalent to a C struct with an integer LENGTH,and
  !char[] STR

  FORST = FORSTC%STR(1:FORSTC%LENGTH)
  VOLEQ = VOLEQC%STR(1:VOLEQC%LENGTH)
  HTTYPE = HTTYPEC%STR(1:HTTYPEC%LENGTH)
  CONSPEC(1:5) = "" !CONSPECC%STR(1:CONSPECC%LENGTH)
  PROD = PRODC%STR(1:PRODC%LENGTH)
  LIVE = LIVEC%STR(1:LIVEC%LENGTH)
  CTYPE = CTYPEC%STR(1:CTYPEC%LENGTH)

  !Use array converter to reshape c arrays to fortran notation
  LOGVOL = RESHAPE(LOGVOLC, SHAPE(LOGVOL))
  LOGDIA = RESHAPE(LOGDIAC, SHAPE(LOGDIA))
  ERRFLAG = 0

  if (DEBUG%MODEL) then
     write  (LUDBG, 15) REGN, VOLEQ, PROD, DBHOB, MTOPP,HTTOT
     15 format (2X, I2, 1X, A, 1X, A, F5.1, F5.1, F5.1)
  end if

  call VOLINIT2(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,3,7,15,20,21,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG, MERRULES,IDIST)

  !conVert Char(*) back to CHAR256 for return to C++ wrapper including
  !a null terminator required by C strings.  Tack on the null terminator
  call CREATE_C_STRING(FORSTC, FORST)
  call CREATE_C_STRING(VOLEQC, VOLEQ)
  call CREATE_C_STRING(HTTYPEC, HTTYPE)
  call CREATE_C_STRING(CONSPECC, CONSPEC)
  call CREATE_C_STRING(PRODC, PROD)
  call CREATE_C_STRING(LIVEC, LIVE)
  call CREATE_C_STRING(CTYPEC, CTYPE)

  !copy the logvol and logdia data back into the subroutine paramater for
  !return to c++
  LOGVOLC = RESHAPE(LOGVOL, SHAPE(LOGVOL))
  LOGDIAC = RESHAPE(LOGDIA, SHAPE(LOGDIA))

  if (DEBUG%MODEL) then
     !WRITE  (LUDBG,*)'   BDFT    CUFT    TPWDCU  TPWDBF'
     write  (LUDBG, 25)'ERRFLAG = ', ERRFLAG
     25 format (2X, A, 1X, I2)
  end if

  if (DEBUG%MODEL) then
     write  (LUDBG, 30) ' <--Exit VOLAPSS'
     30 format (A)
  end if

  close(LUDBG)

  return
  end subroutine VOLAPSS

  !vollib subroutine for c++ user
subroutine VOLLIBCPP(REGN, FORSTC, VOLEQC,I3, I7, I15, I20, I21,&
     &DBHOB, DRCOB, HTTYPEC, HTTOT, HTLOG, HT1PRD, HT2PRD,&
     &HTREF, FCLASS, VOL, LOGVOLC, LOGDIAC, LOGLEN, BOLHT, TLOGS,&
     &NOLOGP, NOLOGS, CUTFLG, BFPFLG, CUPFLG, CDPFLG, SPFLG, PRODC,&
     &LIVEC, CTYPEC, ERRFLAG,IDIST,&
     &BRKHT,BRKHTD,FIASPCD,DRYBIO,GRNBIO,CR,CULL,DECAYCD)
  use CHARMOD
  use MRULES_MOD
  use DEBUG_MOD
  implicit none
  !... Expose subroutine VOLLIB09 to users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::VOLLIBCPP

  !     Parameters
  ! Variables
  integer :: REGN
  TYPE(CHAR256):: FORSTC, VOLEQC
  real    :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB
  TYPE(CHAR256):: HTTYPEC
  real    :: HTTOT
  integer :: HTLOG
  real    :: HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2
  integer :: HTREF
  real    :: AVGZ1,AVGZ2
  integer :: FCLASS
  real    :: DBTBH,BTR
  integer :: I3,I7,I15,I20,I21
  real    :: VOL(15),LOGVOLC(20,7)
  real    :: LOGDIAC(I3,I21),LOGLEN(I20),BOLHT(I21)
  integer :: TLOGS
  real    :: NOLOGP,NOLOGS
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
  TYPE(CHAR256):: CONSPECC, PRODC
  integer :: HTTFLL
  TYPE(CHAR256):: LIVEC
  integer :: BA,SI
  TYPE(CHAR256):: CTYPEC
  integer :: ERRFLAG

  !     Local variables
  character(FORSTC%LENGTH) :: FORST
  character(VOLEQC%LENGTH) :: VOLEQ
  character(HTTYPEC%LENGTH) :: HTTYPE
  character(len=5) :: CONSPEC
  character(PRODC%LENGTH) :: PROD
  character(LIVEC%LENGTH) :: LIVE
  character(CTYPEC%LENGTH) :: CTYPE
  character(len=3) :: MDL
  character(len=3) :: SPECIES
  character(len=2) :: DIST
  character(len=2) :: VAR
  character(len=10) :: EQNUM
  integer :: SPEC
  real    :: LOGVOL(I7,I20),LOGDIA(I21,I3)
  integer :: IDIST
  TYPE(MERCHRULES):: MERRULES
  real    :: BRKHT,BRKHTD,DRYBIO(15),GRNBIO(15)
  real    :: CR,CULL,CULLMSTOP
  integer :: DECAYCD,FIASPCD,MRULEFLG

  !Convert the CHAR256 types to Char(*) so I don't have to rename
  !all references below
  !CHAR256 is a user defined type used for passing C strings back/
  !forth to Fortran
  !CHAR256 is equivalent to a C struct with an integer LENGTH,and
  !char[] STR

  FORST = FORSTC%STR(1:FORSTC%LENGTH)
  VOLEQ = VOLEQC%STR(1:VOLEQC%LENGTH)
  HTTYPE = HTTYPEC%STR(1:HTTYPEC%LENGTH)
  CONSPEC(1:5) = "" !CONSPECC%STR(1:CONSPECC%LENGTH)
  PROD = PRODC%STR(1:PRODC%LENGTH)
  LIVE = LIVEC%STR(1:LIVEC%LENGTH)
  CTYPE = CTYPEC%STR(1:CTYPEC%LENGTH)

  !Use array converter to reshape c arrays to fortran notation
  LOGVOL = RESHAPE(LOGVOLC, SHAPE(LOGVOL))
  LOGDIA = RESHAPE(LOGDIAC, SHAPE(LOGDIA))
  ERRFLAG = 0
  MRULEFLG = 0

  call VOLINITNVB(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,CR,CULL,DECAYCD,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST,BRKHT,BRKHTD,FIASPCD,DRYBIO,&
       &GRNBIO,MRULEFLG,MERRULES,CULLMSTOP)

  !conVert Char(*) back to CHAR256 for return to C++ wrapper including
  !a null terminator required by C strings.  Tack on the null terminator
  call CREATE_C_STRING(FORSTC, FORST)
  call CREATE_C_STRING(VOLEQC, VOLEQ)
  call CREATE_C_STRING(HTTYPEC, HTTYPE)
  call CREATE_C_STRING(CONSPECC, CONSPEC)
  call CREATE_C_STRING(PRODC, PROD)
  call CREATE_C_STRING(LIVEC, LIVE)
  call CREATE_C_STRING(CTYPEC, CTYPE)

  !copy the logvol and logdia data back into the subroutine paramater for
  !return to c++
  LOGVOLC = RESHAPE(LOGVOL, SHAPE(LOGVOL))
  LOGDIAC = RESHAPE(LOGDIA, SHAPE(LOGDIA))

end
