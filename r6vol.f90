!===========================!
!   VolumeLibrary : r6vol   !
!===========================!
  !== last modified  01-18-2013
  ! 01/18/2013 added calculation for stump vol(14 and tip VOL(15)
  ! 04/13/2017 removed stump and tip vol calc to voinit
subroutine R6VOL(VOLEQ,FORST,DBHOB,BTR,FCLASS,MTOPP,HT1,HTTYPE,&
     &VOL,LOGVOL,NOLOGP,LOGDIA,SCALEN,DBTBH,HT1PRD,CTYPE,&
     &ERRFLAG)

  !  CREATED:   06/19/90
  !  PURPOSE:   THIS ROUTINE IS THE DRIVER PROGRAM FOR CALCULATING
  !             R6 VOLUMES.
  !  DECLARE VARIABLES

  ! Variables
  character(len=1) :: HTTYPE
  character(len=1) :: CTYPE
  character(len=2) :: FORST
  !      CHARACTER*89 INREC
  character(len=10) :: VOLEQ

  integer :: FCLASS,ZONE,ERRFLAG
  integer :: LOGS,I,J
  integer :: lggrd(20),IXXX
  real    :: DBHIB,DBTBH,FC_HT,INTBF(20),D17,HT1PRD
  real    :: DBHOB,BTR,MTOPP,HT1,TLH,TTH,TAPCOF
  real    :: SCALEN(20),XLEN(20),LOGVOL(7,20),LOGV
  real    :: VDEF(20),VOL(15),NOLOGP,LOGDIA(21,3),T,TDI1,TDI2

  ! FLAG FOR CRUISING METHODS, FLAGS WRITING TO THE LOG STOCK FILE
  !      INITIALIZE VARIABLES

  do I = 1,20
     VDEF(I) = 0.0
     lggrd(i) = 0.0
     !        IF (I.LE.11) VOL(I)=0
     30 continue
  end do

  do 35, I = 1,7
  do 33, J = 1,20
  LOGVOL(I,J) = 0.0
  33 continue
  35 continue

  do 37, I = 1,15
  VOL(I) = 0.0
  37 continue

  LOGS = 0
  ERRFLAG = 0

  if (DBHOB.lt.1.0) then
     ERRFLAG = 3
     return
  end if
  ! ADD DECIMAL IF HT IS IN LOGS
  if (HTTYPE.eq.'L' .or. HTTYPE.eq.'l') then
     HT1 = HT1/10
     if (HT1.gt.20) then
        ERRFLAG = 7
        return
     end if
  end if

  if (FCLASS .le.0) then
     if (CTYPE .eq. 'F') then
        !  GET FORMCLASS
        call GETFCLASS(VOLEQ,FORST,DBHOB,FCLASS)
     else
        ERRFLAG = 2
        return
     end if
  end if
  !  STANDARD TAPER COEFFICIENT

  TAPCOF = 0.62

  ! DETERMINE THE ZONE

  if (VOLEQ(1:3).eq.'616'.or. voleq(1:3).eq.'628') then
     ZONE = 1
     FC_HT = 17.3
  else if (VOLEQ(1:3).eq.'632') then
     ZONE = 2
     FC_HT = 33.6
  else
     ERRFLAG = 1
     goto 1000
  end if

  ! DETERMINE IF TREE HT IS EXPRESSED AS NUMBER OF LOGS OR
  ! HT IN FT FROM STUMP.
  if (HTTYPE.eq.'L' .or. HTTYPE.eq.'l') then
     TLH = HT1
     TTH = 0
  else
     TLH = 0
     TTH = HT1
  end if

  !  SMALL TREE CHECKS FOR HEIGHT IN FEET
  DBHIB = DBHOB-DBTBH
  if (TTH.gt.0) then
     D17 = FCLASS/100.0*DBHOB
     if (TTH.le.FC_HT) then
        NOLOGP = 0
        !              use smailians to find total cubic volume
        VOL(1) = 0.00272708*(DBHIB*DBHIB)*TTH
        go to 1000
     else if (DBHIB .lt. MTOPP) then
        !            only total cubic volume possible
        call R6VOL3(DBHOB,DBTBH,FCLASS,TTH,ZONE,VOL)
        go to 1000
     else
        !            call the total cubic routines
        call R6VOL3(DBHOB,DBTBH,FCLASS,TTH,ZONE,VOL)
     end if
  end if
  !  one foot stump should be deducted from TTH, so it need the following line (YW 2017/06/09)
  !     TTH = TTH -1.0
  call R6DIBS(ZONE,DBHOB,BTR,FCLASS,MTOPP,TLH,TTH,NOLOGP,&
       &LOGDIA,SCALEN,XLEN,TAPCOF)

  if (TTH.gt.0) then
     HT1PRD = 1.0
     do 65, I = 1,20
     HT1PRD = HT1PRD + XLEN(I)
     65 continue
  end if

  if (ZONE.eq.2.and. TTH.gt.0.and. LOGDIA(1,1).gt.DBHOB) then
     !          SMALL TREE CORRECTION FOR WESTSIDE TREES
     LOGDIA(1,2) = LOGDIA(2,2) + 1.0
     LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
     if (TTH.lt.40.0) then
        LOGDIA(2,2) = 0.0
        LOGDIA(2,1) = 0.0
        SCALEN(2) = 0.0
        NOLOGP = 1.0
        LOGDIA(1,2) = MTOPP
        LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
     end if
  end if

  call R6VOL1(ZONE,DBHOB,FCLASS,NOLOGP,LOGDIA,LOGVOL,INTBF)

  !     PNW appraisal fix for Zone 2 bdft volume
  if (VOLEQ(1:3).eq.'628') then
     call R6FIX(DBHOB,FCLASS,TLH,TTH,HTTYPE,LOGVOL)
  end if

  !  DETERMINE LOG-LOOP PARAMETER AND IF TOP 8 FT SEGMENT PRESENT
  !  ON EASTSIDE.  LOGS VARIABLE = NO. OF OF 16 FT LOGS
  LOGS = INT(NOLOGP)
  T = NOLOGP-LOGS
  if (T.gt.0.0) LOGS = LOGS+1
  !  REMOVE THE 0.3 FEET OF TRIM ON ALL THE 16 FOOT LOGS
  do 100, I = 1,LOGS
  IXXX = INT(SCALEN(I))
  SCALEN(I) = IXXX
  100 continue

  ! IF WESTSIDE - CONVERT NOLOGP VARIABLE TO NO. OF 32'S

  if (ZONE.eq.2) NOLOGP = NOLOGP/2.0

  ! ACCUMULATE LOG VOLUMES FOR THE TOTAL TREE VOLUME

  do I = 1,LOGS

     LOGV = ANINT(LOGVOL(1,I))
     LOGVOL(1,I) = LOGV
     LOGV = ANINT(LOGVOL(4,I)*10.0)/10.0
     LOGVOL(4,I) = LOGV

     !        VOL(1) = VOL(1) + LOGVOL(4,I)
     VOL(2) = VOL(2) + LOGVOL(1,I)
     VOL(4) = VOL(4) + LOGVOL(4,I)
     VOL(10) = VOL(10) + anint(INTBF(I))

     !        logvol(2,i)=LOGVOL(1,I)
     !        logvol(5,i)=LOGVOL(4,I)
     LOGVOL(7,I) = INTBF(I)

     500 continue
  end do
  NOLOGP = logs
  do 600, I = LOGS,1,-1
  TDI1 = LOGDIA(I,1)
  LOGDIA(I+1,1) = TDI1
  TDI2 = LOGDIA(I,2)
  LOGDIA(I+1,2) = TDI2
  600 continue
  !     calculate stump vol as 1 foot cylindar
  !      IF(VOL(14).LT.0.01) VOL(14)=0.005454154*LOGDIA(1,2)**2

  LOGDIA(1,1) = 0
  LOGDIA(1,2) = 0
  !     calculate stem tip volume
  !      IF(VOL(4).GT.0.0) VOL(15)=VOL(1)-VOL(4)-VOL(14)
  !      VOL(15)=0.002727*LOGDIA(LOGS+1,2)**2*(TTH-HT1PRD)
  !      IF(VOL(15).LT.0.0) VOL(15)=0.0
  1000 return
end

subroutine GETFCLASS(VOLEQ,FORST,DBHOB,FCLASS)

  ! Variables
  character(len=2) :: FORST
  character(len=3) :: SPEC
  character(len=10) :: VOLEQ
  real    :: DBHOB
  integer :: IFORST,FCLASS

  SPEC = VOLEQ(8:10)
  if (SPEC .eq. '000') then
     FCLASS = 80
     return
  end if

  if (FORST(2:2) .lt. '0') then
     FORST(2:2) = FORST(1:1)
     FORST(1:1) = '0'
     if (FORST(2:2) .lt. '0') FORST(2:2) = '0'
  end if

  read(FORST,'(i2)') IFORST

  if (IFORST.eq.4.or. IFORST.eq.7.or. IFORST.eq.14.or.&
       &IFORST.eq.16) then
     call FORMCL_BM(SPEC,IFORST,DBHOB,FCLASS)
  else if (IFORST.eq.10.or. IFORST.eq.11) then
     call FORMCL_CA(SPEC,IFORST,DBHOB,FCLASS)
  else if (IFORST.eq.8.or. IFORST.eq.17 ) then
     call FORMCL_EC (SPEC,IFORST,DBHOB,FCLASS)
  else if (IFORST.eq.21) then
     call FORMCL_NI (SPEC,IFORST,DBHOB,FCLASS)
  else if (IFORST.eq.12.or. IFORST.eq.9) then
     call FORMCL_PN (SPEC,IFORST,DBHOB,FCLASS)
  else if (IFORST.eq.1.or. IFORST.eq.2.or. IFORST.eq.20) then
     call FORMCL_SO (SPEC,IFORST,DBHOB,FCLASS)
  else if (IFORST.eq.6.or. IFORST.eq.15.or. IFORST.eq.18.or.&
       &IFORST.eq.3.or. IFORST.eq.5) then
     call FORMCL_WC (SPEC,IFORST,DBHOB,FCLASS)
  else
     FCLASS = 80.
  end if

  return
end

  ! VARIABLES DEFINED IN THE CALL LIST
  !      VOLEQ - Volume equation number (10 digits)
  !      DBHOB - Diameter at Breast Height Outside Bark
  !      BTR  -  Bark Thickness ratio (optional)
  !      FCLASS - Girard's Form Class
  !      MTOPP - Merchantable Top Diameter for Main Stem
  !      HT1 - Height to a merchantable height (in feet or number of logs) (optional)
  !      HTTYPE - Character flag describes HT1 variable:
  !                  F or blank = HT1 is in feet
  !                  L = HT1 is number of logs
  !      VOL(15) - Tree Volumes
  !           VOL(1) - Total volume for the tree in cubic feet, ground to tip.
  !              Does not include the limbs and roots.
  !           VOL(2) - Gross amount of board foot as primary product - Scribner.
  !           VOL(3) - Net amount of board foot as primary product - Scribner.
  !           VOL(4) - Gross amount of cubic foot as primary product - Smalian.
  !           VOL(5) - Net amount of cubic foot as primary product - Smalian.
  !           VOL(6) - Amount of cord wood as primary product.
  !           VOL(7) - Gross amount of cubic foot as secondary product - Smalian.
  !           VOL(8) - Net amount of cubic foot as secondary product - Smalian.
  !           VOL(9) - Amount of cord wood as secondary product.
  !           VOL(10) - Gross amount of board foot as primary product - INT'L 1/4"
  !           VOL(11) - Net amount of board foot as primary product - INT'L 1/4"
  !           VOL(12) - GROSS AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
  !           VOL(13) - NET AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
  !      LOGVOL(7,20) - Volumes for individual logs (X = log number)
  !           LOGVOL(1,X) - GROSS BOARD FT LOG VOLUME (20 LOGS)
  !           LOGVOL(2,X) - GROSS REMOVED BOARD FT LOG VOLUME (20 LOGS)
  !           LOGVOL(3,X) - NET BOARD FT LOG VOLUME (20 LOGS)
  !           LOGVOL(4,X) - GROSS CUBIC FT LOG VOLUME (20 LOGS)
  !           LOGVOL(5,X) - GROSS REMOVED CUBIC FT LOG VOLUME (20 LOGS)
  !           LOGVOL(6,X) - NET CUBIC FT LOG VOLUME (20 LOGS)
  !           LOGVOL(7,X) - GROSS INT'L 1/4 VOLUME (20 LOGS)
  !      NOLOGP - Number of logs in the main stem
  !      LOGDIA(20,3) - Log end diameters
  !           LOGDIA(x,1) = scaling dib (used to compute the volumes)
  !           LOGDIA(x,2) = actual dib
  !           LOGDIA(x,3) = actual dob
  !      SCALEN - Scale length or log lengths
  !      DBTBH  -  Double Bark Thickness at Breast Height (optional)
  !      ERRFLAG - Error flag.
