!===============================!
!   VolumeLibrary : volumelib   !
!===============================!
subroutine VOLUMELIB(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
     &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
     &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I11,I20,I21,&
     &VOL11,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
     &BFPFLG,CUPFLG,CDPFLG,CUSFLG,CDSFLG,CONSPEC,PROD,HTTFLL,LIVE,&
     &BA,SI,CTYPE,ERRFLAG)

  !== last modified  02-26-2004     Main call into volume dll
  !    VOLUMELIB      - subroutine

  implicit none

  ! Expose subroutine VOLUMELIB to users of this DLL

  !  !DEC$ ATTRIBUTES DLLEXPORT::VOLUMELIB

  !   THERE ARE CALLS TO THE FOLLOWING SUBROUTINES
  !     PROFILE - EXTERNAL
  !     DVE     - EXTERNAL
  !     R4VOL  - EXTERNAL
  !     R6VOL  - EXTERNAL
  !     BLMVOL - EXTERNAL
  !     R8VOL  - EXTERNAL
  !     R10VOL - EXTERNAL
  !     R12VOL - EXTERNAL
  !*****DECLARE VARIABLES

  ! Variables
  character(len=*) :: HTTYPE
  character(len=*) :: LIVE
  character(len=*) :: CTYPE
  character(len=*) :: FORST
  character(len=*) :: PROD
  character(len=*) :: conspec
  character(len=*) :: VOLEQ
  character(len=3) :: MDL
  character(len=2) :: DIST
  integer :: IDIST
  !     MERCH VARIABLES
  integer :: REGN,HTTFLL,BA,SI
  real    :: STUMP,MTOPP,MTOPS,THT1
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,CUSFLG,CDSFLG,ERRFLAG,SPFLG

  !     TREE VARIABLES
  real    :: HTTOT,HT1PRD,HT2PRD
  real    :: DBHOB,DRCOB,DBTBH,BTR
  integer :: FCLASS,HTLOG

  !    3RD POINT VARIABLES
  real    :: UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2
  integer :: HTREF

  !     OUTPUTS
  real    :: NOLOGP,NOLOGS
  integer :: TLOGS

  !     ARRAYS
  integer :: I11,I15,I21,I20,I7,I3,I,J
  real    :: VOL11(I11),LOGVOL(I7,I20)
  real    :: LOGDIA(I21,I3),LOGLEN(I20),BOLHT(I21)
  real    :: VOL(15)

  call VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST)
  4000 return
  end subroutine VOLUMELIB
