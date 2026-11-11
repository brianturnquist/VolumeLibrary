!===============================!
!   VolumeLibrary : vollibfia   !
!===============================!
subroutine vollibfia(regn, iforst, voleqi, dbhob, httot,&
     &ht1prd,ht2prd,stems,si,ba,QUAL,&
     &ht_break,upsd1,upsht1,dia_centroid,&
     &ht_centroid,ht_dbh,mtops,stump,&
     &totcu, sawcu, mcu4, scrbbf, intlbf, errflag)

  ! regn     National forest admin region number
  ! iforst   National forest admin forest number
  ! voleqi   NVEL volume equation number (10 characters string)
  ! dbhob    Diameter at breast height (or DRC when ht_dbh < 4.5)
  ! httot    tree total height from ground to tip
  ! ht1prd   Sawtimber height
  ! ht2prd   bolht (height to 4 inch top)
  ! stems    woodland species stem count
  ! si       site index
  ! ba       stand basal area
  ! QUAL     First log merchanability (formcl in FIA tree table)
  ! ht_break Height to top break
  ! upsd1    Upper stem diameter
  ! upsht1   Upper stem height
  ! dia_centroid Centroid diameter
  ! ht_centroid  Height to centroid diameter
  ! ht_dbh   Height to dbhob
  ! mtops    Merch top diameter (4 inch)
  ! stump    Stump height

  ! 11/07/2017 YW
  ! Currently there is no variable for QUAL and ht_break
  ! Need to create new variable for them!!!!!!

  implicit none
  ! Variables
  character(len=*) :: voleqi
  real    :: totcu,sawcu,mcu4,scrbbf,intlbf
  !     variables from VOLINIT

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
  real    :: VOL(15),LOGVOL(7,20)
  real    :: LOGDIA(21,3),LOGLEN(20),BOLHT(21)
  !  variables for stump dia and vol
  integer :: SPN
  real    :: STUMPDIB,STUMPDOB,VOLIB,VOLOB
  real    :: DIB,DOB,HTUP,MHT

  !  test biomass calc variable
  real    :: WF(3),BMS(8)
  integer :: SPCD,FOREST
  !  FIA input variables
  real    :: ht_break,dia_centroid,ht_centroid,ht_dbh
  integer :: QUAL,stems

  VOLEQ = VOLEQI(1:10)
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
  !      HT1PRD=0.0
  !      HT2PRD=0.0
  FCLASS = 0
  !  NVEL use variable FCLASS to save woodland species stem count
  if (stems.gt.1.and.ht_dbh.lt.4.5) then
     FCLASS = stems
  end if

  DBTBH = 0.0
  BTR = 0.0
  PROD = '01'
  HTTYPE = 'F'
  HTLOG = 0
  !      STUMP=0.0
  !      UPSHT1=0.0
  !      UPSD1=0.0
  AVGZ1 = 0.0
  HTREF = 0
  UPSHT2 = 0.0
  UPSD2 = 0.0
  AVGZ2 = 0.0
  CONSPEC = '    '
  DRCOB = 0.0
  if (ht_dbh.lt.4.5) DRCOB = dbhob
  HTTFLL = 0
  !      BA=0
  !      SI=0
  CTYPE = 'F'
  CUTFLG = 1
  CUPFLG = 1
  SPFLG = 1
  BFPFLG = 1
  MTOPP = 0.0
  !      MTOPS=0.0
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
  if (ERRFLAG.eq.0) then
     totcu = VOL(1)
     sawcu = VOL(4)
     mcu4 = VOL(7)+VOL(4)
     scrbbf = VOL(2)
     intlbf = VOL(10)
  else
     totcu = 0.0
     sawcu = 0.0
     mcu4 = 0.0
     scrbbf = 0.0
     intlbf = 0.0
  end if
  return
end

subroutine vollibfia2(regn, iforst, voleqi, dbhob, httot,&
     &totcu, sawcu, mcu4, scrbbf, intlbf, errflag)

  implicit none
  ! Variables
  character(len=*) :: voleqi
  real    :: totcu,sawcu,mcu4,scrbbf,intlbf
  !     variables from VOLINIT

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
  real    :: VOL(15),LOGVOL(7,20)
  real    :: LOGDIA(21,3),LOGLEN(20),BOLHT(21)
  !  variables for stump dia and vol
  integer :: SPN
  real    :: STUMPDIB,STUMPDOB,VOLIB,VOLOB
  real    :: DIB,DOB,HTUP,MHT

  !  test biomass calc variable
  real    :: WF(3),BMS(8)
  integer :: SPCD,FOREST

  VOLEQ = VOLEQI(1:10)
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
  MTOPP = 0.0
  MTOPS = 0.0
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
  if (ERRFLAG.eq.0) then
     totcu = VOL(1)
     sawcu = VOL(4)
     mcu4 = VOL(7)+VOL(4)
     scrbbf = VOL(2)
     intlbf = VOL(10)
  else
     totcu = 0.0
     sawcu = 0.0
     mcu4 = 0.0
     scrbbf = 0.0
     intlbf = 0.0
  end if
  return
end

subroutine vollibfia3(regn, iforst, voleqi, dbhob, httot,&
     &mtopp, mtops, prodi,totcu,primcu,mcu,scrbbf,intlbf,errflag)

  ! This rubroutine is added for Android AddVol app to calculate volume
  ! It needs input for prod, mtopp, mtops.
  ! YW 2019/02/26
  implicit none
  ! Variables
  character(len=*) :: voleqi
  character(len=*) :: prodi
  real    :: totcu,primcu,mcu,scrbbf,intlbf
  !     variables from VOLINIT

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
  real    :: VOL(15),LOGVOL(7,20)
  real    :: LOGDIA(21,3),LOGLEN(20),BOLHT(21)
  !  variables for stump dia and vol
  integer :: SPN
  real    :: STUMPDIB,STUMPDOB,VOLIB,VOLOB
  real    :: DIB,DOB,HTUP,MHT

  VOLEQ = VOLEQI(1:10)
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
  PROD = prodi(1:2)
  if (PROD(2:2).lt.'0') then
     PROD(2:2) = PROD(1:1)
     PROD(1:1) = '0'
     if (PROD(2:2).lt.'0') PROD(2:2) = '1'
  end if
  HT1PRD = 0.0
  HT2PRD = 0.0
  FCLASS = 0
  DBTBH = 0.0
  BTR = 0.0
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
  if (MTOPP.lt.0.1) MTOPP = 0.0
  if (MTOPS.lt.0.1) MTOPS = 0.0
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
  if (ERRFLAG.eq.0) then
     totcu = VOL(1)
     primcu = VOL(4)
     mcu = VOL(7)+VOL(4)
     scrbbf = VOL(2)
     intlbf = VOL(10)
  else
     totcu = 0.0
     primcu = 0.0
     mcu = 0.0
     scrbbf = 0.0
     intlbf = 0.0
  end if
  return
end

subroutine vollibfsveg(regn,iforst,idist,voleqi,dbhob,httot,&
     &mtopp,stems,dbtbh,stump,ba,si,tcu, mcu, bdf, errflag)

  !    This subroutine is created for FSVeg to call the library
  !  YW 20210719 Set FCLASS initial to 0 in order to pick the species default form class
  !  YW 20210727 Added DBTBH as an input variable to the subroutine
  !  YW 20200811 Added stump, ba and si as input variables
  !  YW 20230605  Set the default region and forest for R1 BEH equation and default stump to 1.0
  !  YW 20241127 Added call NVBC for NVB equation.
  implicit none
  ! Variables
  character(len=*) :: voleqi
  real    :: tcu,mcu,bdf
  integer :: stems
  !     variables from VOLINIT

  character(len=1) :: HTTYPE
  character(len=1) :: LIVE
  character(len=1) :: CTYPE
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=4) :: CONSPEC
  character(len=10) :: VOLEQ
  character(len=11) :: VOLEQ11
  character(len=2) :: DIST
  character(len=2) :: VAR

  !   MERCH VARIABLES
  integer :: REGN,HTTFLL,BA,SI
  real    :: STUMP,MTOPP,MTOPS,THT1,MAXLEN
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG

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
  real    :: VOL(15),LOGVOL(7,20)
  real    :: LOGDIA(21,3),LOGLEN(20),BOLHT(21)
  !  variables for call NVB
  real    :: CULL,BRKHT,BRKHTD,CULLMSTOP,DRYBIO(15),GRNBIO(15)
  integer :: DECAYCD,FIASPCD

  CR = 0
  CULL = 0
  BRKHT = 0
  BRKHTD = 0
  CULLMSTOP = 0
  DRYBIO = 0
  GRNBIO = 0

  VOLEQ = VOLEQI(1:10)
  VOLEQ11 = VOLEQ
  !     Set default value for unused variables
  if (IFORST.gt.99) then
     FORST = '01'
  else
     write (FORST, '(I2)') IFORST
  end if
  !     Set the default region and forest for R1 BEH equation (2023/06/05)
  if (REGN.eq.1.and.VOLEQ(4:6).eq.'BEH') then
     REGN = 6
     FORST = '05'
  end if
  if (FORST(2:2) .lt. '0') then
     FORST(2:2) = FORST(1:1)
     FORST(1:1) = '0'
     if (FORST(2:2) .lt. '0') FORST(2:2) = '0'
  end if
  HT1PRD = 0.0
  HT2PRD = 0.0
  FCLASS = 0
  !     FOR WOODLAND SPECIES, NO-OF-STEMS IS PASSED IN USING FCLASS
  if (VOLEQ(4:6).eq.'DVE'.and.&
       &(VOLEQ(1:1).eq.'2'.or.VOLEQ(1:1).eq.'3'.or.&
       &VOLEQ(1:1).eq.'4')) then
     FCLASS = 1
     if (STEMS.gt.1) FCLASS = STEMS
  end if
  !      DBTBH=0.0
  BTR = 0.0
  PROD = '01'
  HTTYPE = 'F'
  HTLOG = 0
  !      STUMP=0.0
  if (STUMP.le.0.and.REGN.eq.6) STUMP = 1
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
  !      BA=0
  !      SI=0
  CTYPE = 'F'
  LIVE = 'L'
  CUTFLG = 1
  CUPFLG = 1
  SPFLG = 1
  BFPFLG = 1
  !      MTOPP=0.0
  MTOPS = 0.0
  I3 = 3
  I7 = 7
  I15 = 15
  I20 = 20
  I21 = 21
  read (VOLEQ(8:10), '(I3)') FIASPCD

  if (VOLEQ(1:3).eq.'NVB') then
     call NVBC(REGN,FORST,DIST,VOLEQ11,DBHOB,HTTOT,MTOPP,MTOPS,&
          &HT1PRD,HT2PRD,STUMP,PROD,BRKHT,BRKHTD,LIVE,CR,CULL,DECAYCD,&
          &LOGLEN,LOGDIA,LOGVOL,BOLHT,&
          &TLOGS,NOLOGP,NOLOGS,VOL,DRYBIO,GRNBIO,ERRFLAG,FIASPCD,CTYPE)
  else
     call VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
          &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
          &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
          &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
          &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
          &BA,SI,CTYPE,ERRFLAG,IDIST)
  end if
  if (ERRFLAG.eq.0) then
     tcu = VOL(1)
     ! R8 old clark equation does not have tatal cubic volume.
     ! Use volume to 4" top as total cubic volume
     if (tcu.eq.0.0.and.REGN.eq.8.and.&
          &VOLEQ(4:6).eq.'CLK'.and.VOLEQ(3:3).ne.'1') then
        tcu = VOL(4)+VOL(7)
     end if
     mcu = VOL(4)
     bdf = VOL(2)
  else
     tcu = 0.0
     mcu = 0.0
     bdf = 0.0
  end if
  return
end
