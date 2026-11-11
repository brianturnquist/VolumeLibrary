!================================!
!   VolumeLibrary : pmtprofile   !
!================================!
  !  DIB Calculation for the profile model tutorial

  !  Modified version of profile.f used by the profile model tutorial
  !  for calculating dib at at a given height.  NOTE: height where dib
  !  is requested is passed in the DRCOB field.  DIB is returned in DIBO

  !  created TDH 10/30/10

  !  revised 06/23/11
  !  Cleaned up code and added comments

  !_______________________________________________________________________

subroutine PMTPROFILE (FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,DIBO,&
     &HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,&
     &AVGZ1,AVGZ2,HTREF,DBTBH,BTR,VOL,&
     &CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,DRCOB,&
     &CTYPE,FCLASS,PROD,ERRFLAG)
  !_______________________________________________________________________

       !DEC$ ATTRIBUTES DLLEXPORT::PMTPROFILE

  use DEBUG_MOD

  !     MERCH VARIABLES
  ! Variables
  character(len=1) :: COR
  character(len=1) :: HTTYPE
  character(len=1) :: CTYPE
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=10) :: VOLEQ
  integer :: EVOD,OPT,REGN,HTFLG,FCLASS
  integer :: CUTFLG,BFPFLG,CUPFLG,SPFLG,ERRFLAG,CDPFLG
  real    :: LENGTH,DRCOB,MINBFD,MHT,slope,TOPD
  real    :: MAXLEN,MINLEN,minlent,MERCHL,MTOPP,MTOPS,STUMP,TRIM,TRM
  !     TREE VARIABLES
  real    :: DBHOB,HTTOT,DBTBH,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
  real    :: BTR,LMERCH,LENMS,HT1PRD,HT2PRD,TOP6
  integer :: HTREF,HTLOG,I,J

  !     OUTPUT VARIABLES
  real    :: VOL(15),LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3)
  real    :: BOLHT(21),NOLOGP,NOLOGS,LOGVOL32(20),LVOL32(2,20)
  real    :: LLEN32(20),LDIA32(21,2)
  integer :: NLCNT,LCNT

  !     PROGRAM VARIABLES
  real    :: LOGV,LHT,DIBL,DIBS,HT2,CUVOL,DOB
  integer :: LOGST,NUMSEG,NEXTRA,MFLAG,MFLG,TLOGS
  real    :: DIB,LOGLENG(20),BFINT,TOPV16,BOTV16,R,DIBO

  !       Variables to hold flewellings coefficients
  integer :: SETOPT(6),JSP
  real    :: RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
  real    :: HEX(2),dex(2),ZEX(2)
  !       Temp variables for Region 1

  real    :: UHT,VMER,VMER1,VMER2,TEMPVOL,TCVOL,TCVOL1,TCVOL2

  !zero out values
  MFLAG = 0
  NOLOGP = 0.0
  NOLOGS = 0.0
  NUMSEG = 0
  JSP = 0
  ERRFLAG = 0
  HTFLG = 0
  LMERCH = 0

  do 102,I = 1,20
  do 103, J = 1,7
  LOGVOL(J,I) = 0.0
  103 continue
  LOGDIA(I,1) = 0.0
  LOGDIA(I,2) = 0.0
  LOGDIA(I,3) = 0.0
  102 continue

  do 104, I = 1,15
  VOL(I) = 0.0
  104 continue

  !--   IF DBHOB OR HTTOT EQUALS ZERO THEN DON'T CALCULATE THE VOLUME
  if (DBHOB.lt.1.0.or. (HTTOT.lt.5.and. HT1PRD.le.0)) then
     ERRFLAG = 3
     if (HTTOT.gt.0)ERRFLAG = 0
     go to 1000
  end if
  ! test 1/29/14
  if (VOLEQ(4:6).eq.'BEH' .or. VOLEQ(4:6).eq.'beh') then
     !     added DIB calculation for Behr equation
     call BEHTAP(VOLEQ,DBHOB,HTTOT,LMERCH,DRCOB,FCLASS,MTOPP,DIBO)
     return
  end if
  ! end test 1/29/14
  ! test r8 Clark
  if (VOLEQ(1:1).eq.'8'.and.VOLEQ(3:3).ne.'1'.and.&
       &(VOLEQ(4:6).eq.'CLK' .or. VOLEQ(4:6).eq.'clk')) then
     call R8CLKDIB(VOLEQ,FORST,DBHOB,HTTOT,UPSHT1,DRCOB,DIBO,&
          &ERRFLAG)
     return
  end if

  if (VOLEQ(4:4).eq.'F' .or. VOLEQ(4:4).eq.'f' .or.&
       &VOLEQ(4:6).eq.'DEM' .or. VOLEQ(4:6).eq.'dem' .or.&
       &VOLEQ(4:6).eq.'CUR' .or. VOLEQ(4:6).eq.'cur') then

     TOPD = MTOPP

     if (VOLEQ(4:4).eq.'F' .or. VOLEQ(4:4).eq.'f') then
        !--   Initialize Flewelling model for this tree

        call FWINIT(VOLEQ,DBHOB,HTTOT,MHT,TOPD,UPSHT1,UPSHT2,UPSD1,&
             &UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,JSP,RHFW,RFLW,&
             &TAPCOE,F,SETOPT,NEXTRA,HEX,DEX,ZEX,PINV_Z,FMOD,btr,FCLASS,&
             &ERRFLAG)

        AVGZ1 = ZEX(1)
        AVGZ2 = ZEX(2)

        if (ERRFLAG .eq.1.or. errflag.eq.6)  go to 1000

     else
        !          region 10 call to determine total height or merch height
        call R10HTS(VOLEQ,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,MTOPP,&
             &LMERCH)
     end if

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
     !       CHECK FOR TOTAL TREE HEIGHT
     if (HTTOT.le.4.5) then
        ERRFLAG = 4
        goto 1000
     end if

  end if

  !DRCOB is passed in and is where the use whats DIB
  HT2 = DRCOB

  call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
       &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,&
       &HT2,MTOPP,MFLG,CUVOL,DIB,DOB,errflag)

  DIBO = DIB

  1000 continue

  return
end

  !##################################################################
