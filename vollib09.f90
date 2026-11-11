!==============================!
!   VolumeLibrary : vollib09   !
!==============================!
subroutine VOLLIB09(REGN, FORSTC, VOLEQC, MTOPP, MTOPS, STUMP&
     &,   DBHOB,&
     &DRCOB, HTTYPEC, HTTOT, HTLOG, HT1PRD, HT2PRD, UPSHT1,&
     &UPSHT2, UPSD1,&
     &UPSD2, HTREF, AVGZ1, AVGZ2, FCLASS, DBTBH, BTR, I3, I7, I15,&
     &I20, I21,&
     &VOL,LOGVOLC,LOGDIAC,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
     &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPECC,PRODC,HTTFLL,LIVEC,&
     &BA,SI,CTYPEC,ERRFLAG, INDEB)

  !... Main entry point into dll

  use CHARMOD
  use DEBUG_MOD

  implicit none

  !... Expose subroutine VOLLIB09 to users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::VOLLIB09
  !      !DEC$ ATTRIBUTES NOMIXED_STR_LEN_ARG :: VOLLIB09

  !... Compiler adds and underscore to the beginning and @#bytes
  !... to each external call...must create alias to "see" it
      !DEC$ ATTRIBUTES ALIAS:'_VOLLIB09@196' :: VOLLIB09

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
  real    :: VOL(I15),LOGVOLC(I20,I7)
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
  integer :: IDIST
  !     Local variables
  character(FORSTC%LENGTH) :: FORST
  character(VOLEQC%LENGTH) :: VOLEQ
  character(HTTYPEC%LENGTH) :: HTTYPE
  character(CONSPECC%LENGTH) :: CONSPEC
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

  !     MERCH VARIABLES
  real    :: THT1,MAXLEN
  !      INTEGER         ISFIRSTIME

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
  else
     ANY_DEBUG = .false.
     DEBUG%VOLEQ = .false.
     DEBUG%MODEL = .false.
  end if

  if (ANY_DEBUG) then
     open (UNIT = LUDBG, FILE = 'Debug.txt', STATUS = 'UNKNOWN')
     write (LUDBG,5)'Debugging NVEL'
     5 format(A)
  end if

  if (DEBUG%MODEL) then
     write  (LUDBG, 10) ' -->Enter VOLLIB09'
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
  CONSPEC = CONSPECC%STR(1:CONSPECC%LENGTH)
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

  call VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST)

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
     write  (LUDBG, 30) ' <--Exit VOLLIB09'
     30 format (A)
  end if

  if (ANY_DEBUG) then
     close(LUDBG)
  end if

  return
  end subroutine VOLLIB09

subroutine CREATE_C_STRING(C256STR, CHARSTR)

  !...  covnert Char(*) back to CHAR256 for return to C++ wrapper
  !...  including a null terminator required by C strings.  Tack on the
  !...  null terminator

  use CHARMOD

  implicit none

  !...  Created TDH 03/02/09
  !...  Revised iii

  !...  Parameters
  TYPE(CHAR256)::           C256STR
  ! Variables
  character(C256STR%LENGTH) :: CHARSTR

  C256STR%STR(1:(C256STR%LENGTH+1)) = CHARSTR//CHAR(0)

  return
  end subroutine CREATE_C_STRING
