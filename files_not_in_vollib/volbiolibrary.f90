!===================================!
!   VolumeLibrary : volbiolibrary   !
!===================================!
subroutine VOLBIOLIB(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
     &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
     &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
     &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
     &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
     &BA,SI,CTYPE,ERRFLAG,IDIST,BIOEQ,SPEC,CR,BIOGRN,BIODRY)

  ! Expose subroutine VOLBIOLIBRARY to users of this DLL

      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLBIOLIB
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLBIOLIB
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLUMELIB'::VOLBIOLIB

  use CHARMOD
  use DEBUG_MOD

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
  real    :: WF(3),BMS(8),CR,MC,BIOGRN(8),BIODRY(8),SG(11),BIOMS(8)
  real    :: RATIO,STMDRYWT,STMGRNWT
  integer :: SPEC
  character(len=12) :: BIOEQ
  character(len=40) :: REF(8)

  call VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
       &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
       &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
       &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
       &BA,SI,CTYPE,ERRFLAG,IDIST)

  !    The volume has been calculated, now calculate biomass with Cruising biomass
  !      IF(ERRFLAG.EQ.0)THEN
  !        IF(LEN_TRIM(BIOEQ).EQ.12) THEN
  !          READ (BIOEQ(4:6),*,IOSTAT=STAT) SPCD
  !          IF(STAT.NE.0) READ(VOLEQ(8:10),*,IOSTAT=STAT) SPCD
  !        ENDIF

  if (SPEC.eq.0) then
     if (LEN_TRIM(BIOEQ).eq.12) then
        read (BIOEQ(4:6),'(I3)') SPEC
     else
        read(VOLEQ(8:10),'(I3)') SPEC
     end if
  end if

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
  call CRZSPDFT(REGN,FORST,SPEC,WF,BIOEQ,REF)

  !     Calculate merch stem green weight using cubic feet volume and weight factor
  STMGRNWT = WF(1)*(VOL(4)+VOL(7))

  !     Get the moisture content from Miles $ Smith 2009
  if (WF(3).eq.0) then
     call MILSDATA(SPEC,SG)
     WF(3) = (SG(9)-SG(10))/SG(10)*100
  end if
  MC = WF(3)/100

  STMDRYWT = STMGRNWT/(1+MC)

  !     GET the ratio for stem calculated from weight factor and Jenkins
  if (BIOMS(8).gt.0) then
     RATIO = STMDRYWT/BIOMS(8)
  else
     RATIO = 1
  end if

  !     Apply the ratio to biomass calculated from Jenkins and also add MC to get green weight
  BIODRY = BIOMS*RATIO
  BIOGRN = BIODRY*(1+MC)

  return
end
