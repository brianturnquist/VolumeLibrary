!=============================!
!   VolumeLibrary : r10vol1   !
!=============================!
  !== last modified  03-20-2006
subroutine R10VOL1(EQNUM,MTOPP,MTOPS,HTTOT,HT1PRD,DBHOB,&
     &HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,LOGVOL,&
     &BFPFLG,CUPFLG,SPFLG,ERRFLAG)

  !*                 LOCAL VARIABLES                                   *

  ! Variables
  character(len=10) :: EQNUM
  character(len=1) :: COR
  character(len=1) :: HTTYPE

  integer :: TLOGS,EVOD,NUMSEG,OPT,PIEST,I,ITMP
  integer :: NSEG16
  integer :: CUPFLG,BFPFLG,SPFLG,ERRFLAG,ILOG

  real    :: HTX
  real    :: TOP,NOLOGS,MERCHL
  real    :: DBHOB,HTTOT,HT1PRD,DIB,LENGTH,HMERCH
  real    :: LMERCH,LOGDIA(21,3),LOGLEN(20),PIELEN(20),LOGV
  real    :: LOGLENG(20),CUBVOL,PIEDIB(21,3),PIEVOL(20)
  real    :: MAXLEN,MINLEN,NOLOGP,DUMVOL,PCT,DIBL,DIBS
  real    :: STUMP,MTOPP,MTOPS,TRIM,VOL(15),LOGVOL(7,20)

  logical :: FLAG32

  !--   SET ALL POTENTIAL VOLUMES AND AVG NUM OF LOGS TO ZERO
  !      LOGDATA=.FALSE.

  do I = 1,15
     VOL(I) = 0.0
     10 continue
  end do

  do I = 1, 21
     PIEDIB(I,1) = 0.0
     PIEDIB(I,2) = 0.0
     15 continue
  end do

  do I = 1,20
     LOGLEN(I) = 0.0
     PIELEN(I) = 0.0
     PIEVOL(I) = 0.0
     20 continue
  end do

  TLOGS = 0
  NOLOGP = 0
  NOLOGS = 0
  NUMSEG = 0
  NSEG16 = 0
  LOGV = 0.0

  !--   IF DBHOB OR HTTOT AND HT1PRD EQUALS ZERO THEN DON'T CALCULATE THE VOLUME
  ERRFLAG = 0
  if (DBHOB.lt.1.0) then
     ERRFLAG = 3
     goto 1000
  end if
  if (HTTOT.le.0.0.and. HT1PRD.le.0.0) then
     ERRFLAG = 4
     goto 1000
  end if

  !           BOARD FOOT CACULATIONS  (DBHOB GE 9.0)              *

  !--   DETERMINE IF A BOARD FOOT PRODUCT REPRESTNTATION IS NEEDED

  if (BFPFLG.eq.1.or. CUPFLG.eq.1) then
     !--*************** USE STEM PROFILE MODEL TO DETERMINE VOLUME
     if (DBHOB .le. MTOPP) then
        ERRFLAG = 11
        goto 750
     end if

     !   --------- FIND LOG LENGTHS

     COR = 'Y'
     EVOD = 2
     OPT = 23
     STUMP = 1.0
     TOP = MTOPP

     if (EQNUM(1:3).eq.'A16') then
        MINLEN = 4.0
        MAXLEN = 16.0
        MERCHL = 8.0
        TRIM = .3
        FLAG32 = .false.
     else
        MINLEN = 4.0
        MAXLEN = 32.0
        MERCHL = 16.0
        TRIM = .6
        FLAG32 = .true.
     end if

     !--   SUBROUTINE "R10_HTS" IS INTERNAL AND USES PROFILE MODEL

     !--           WILL FIND LMERCH OR HTTOT DEPENDING ON ENTERED HEIGHT
     !--           LMERCH IS THE MERCHANTABLE LENGTH FROM STUMP TO
     !--           SPECIFIED TOP, INCLUDING TIP

     call R10_HTS(EQNUM,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,TOP,LMERCH)

     !--  CHECK FOR A MINIMUM MERCH STEM LENGTH

     if (HT1PRD.le.0) HT1PRD = LMERCH + STUMP

     if (LMERCH.ge.MERCHL) then

        !--  SUBROUTINE "NUMLOG" WILL DETERMINE THE NUMBER OF
        !--  MERCHANTABLE SEGMENTS IN A GIVEN MERCHANTABLE LENGTH
        !--  OF TREE STEM, ACCORDING TO ONE OF THE DEFINED SEGMENTATION
        !--  RULES IN THE VOLUME ESTIMATOR HANDBOOK FSH ???.

        if (HTTYPE.eq.'L' .or. HTTYPE.eq.'l') then
           NUMSEG = INT(HT1PRD)
           NOLOGP = HT1PRD
           if (NUMSEG.gt.20) then
              ERRFLAG = 12
              return
           end if
           do I = 1, NUMSEG
              LOGLEN(I) = MAXLEN
              35 continue
           end do
           if (FLAG32) then
              NSEG16 = INT(HT1PRD*2.0)
              do I = 1,NSEG16
                 PIELEN(I) = 16.0
              end do
           end if
           itmp = INT(ANINT((ht1prd-int(ht1prd))*10))
           if (itmp .eq.5) then
              NUMSEG = NUMSEG+1
              LOGLEN(NUMSEG) = MAXLEN/2.0
           end if
        else
           !Added max number of logs (20) check (20250626)
           if ((LMERCH/(MAXLEN+TRIM)).gt.20) then
              ERRFLAG = 12
              return
           end if
           call NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,&
                &TRIM,NUMSEG)
           HMERCH = LMERCH

           !--  SUBROUTINE "SEGMNT" WILL DETERMINE THE LENGTH OF EACH
           !--  SEGMENT, GIVEN A MERCHANTABLE LENGTH OF TREE STEM AND
           !--  THE NUMBER OF SEGMENTS IN IT (DETERMINED IN SUBROUTINE
           !--  "NUMLOG").  SEGMENT LENGTHS ARE DETERMINED ACCORDING TO
           !--  ONE OF THE DEFINED SEGMENTATION RULES IN THE VOLUME
           !--  ESTIMATOR HANDBOOK FSH ???.
           !--  NOTE - THE VARIABLE LMERCH WHEN PASSED IS THE TOTAL
           !--           MERCH LENGTH, BUT IS RETURNED AS THE MERCH LENGTH
           !--           WITH TRIM AND UNUSABLE PORTION OF THE TOP SUBTRACTED.

           call SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,&
                &TRIM,NUMSEG,LOGLENG)

           HTX = 0.0
           do I = 1,NUMSEG
              LOGLEN(I) = LOGLENG(I)
              HTX = HTX+LOGLENG(I)
              30 continue
           end do

           if (FLAG32) then
              NSEG16 = INT(HTX/16.0)
              if (NSEG16.lt.HTX/16.0) NSEG16 = NSEG16+1
              call SEGMNT(OPT,EVOD,HMERCH,16.0,4.0,&
                   &0.3,NSEG16,LOGLENG)
              do I = 1,NSEG16
                 PIELEN(I) = LOGLENG(I)
                 31 continue
              end do
           end if

           !--  SET NOLOGP TO THE NUMBER OF LOGS FOOT SEGMENTS
           NOLOGP = NUMSEG
           !               NOLOGP = LMERCH/16.0
        end if

        !--   SUBROUTINE "R10GDIB" IS INTERNAL AND USES PROFILE MODEL

        call R10GDIB(EQNUM,TOP,HTTOT,DBHOB,STUMP,TRIM,TLOGS,&
             &NUMSEG,LOGLEN,LOGDIA)

        if (FLAG32) then
           call R10GDIB(EQNUM,TOP,HTTOT,DBHOB,STUMP,.3,PIEST,&
                &NSEG16,PIELEN,PIEDIB)
        end if

        !      GET SCRIBNER VOLUME FOR THE PIECES                   *

        if (BFPFLG.eq.1.and. DBHOB .ge.9.0) then
           do I = 1,NUMSEG
              DIB = LOGDIA(I+1,1)
              LENGTH = LOGLEN(I)
              if (DIB.lt.6) then
                 LOGV = 0.0
              else if ((DIB.eq.6).and.(LENGTH.gt.3)&
                   &.and.(LENGTH.lt.10)) then
                 LOGV = .5
              else if ((DIB.eq.7).and.(LENGTH.gt.3)&
                   &.and.(LENGTH.lt.7)) then
                 LOGV = .5
              else if ((DIB.eq.8).and.(LENGTH.lt.5)&
                   &.and.(LENGTH.gt.2)) then
                 LOGV = .5
              else
                 call SCRIB (DIB,LENGTH,COR,LOGV)
              end if

              VOL(2) = VOL(2)+LOGV
              LOGVOL(1,I) = LOGV*10
              !              call international
              call INTL14(DIB,LENGTH,LOGV)
              VOL(10) = VOL(10) + LOGV
              250 continue
           end do

           ! IF 32FT EQ THEN NEED TO BREAK LOGS INTO 16FT PIECES  *

           if (FLAG32) then
              do I = 1,NSEG16
                 DIB = PIEDIB(I+1,1)
                 LENGTH = PIELEN(I)
                 if (DIB.lt.6) then
                    LOGV = 0.0
                 else if ((DIB.eq.6).and.(LENGTH.gt.3)&
                      &.and.(LENGTH.lt.10)) then
                    LOGV = .5
                 else if ((DIB.eq.7).and.(LENGTH.gt.3)&
                      &.and.(LENGTH.lt.7)) then
                    LOGV = .5
                 else if ((DIB.eq.8).and.(LENGTH.lt.5)&
                      &.and.(LENGTH.gt.2)) then
                    LOGV = .5
                 else
                    call SCRIB (DIB,LENGTH,COR,LOGV)
                 end if

                 PIEVOL(I) = LOGV*10

                 251 continue
              end do

              ! PRO-RATE LOG VOLUMES TO PRODUCE PIECE VOLUMES *

              ILOG = 1
              do I = 1,NSEG16,2
                 if (I.gt.20) goto 253
                 !                       IF (PIELEN(I+1).GT.0.0) THEN  ! REPLACED 1/10/2003 WITH PIEVOL CHECK
                 if (PIEVOL(I+1).gt.0.0) then
                    DUMVOL = PIEVOL(I)+PIEVOL(I+1)

                    PCT = PIEVOL(I)/DUMVOL
                    PIEVOL(I) = LOGVOL(1,ILOG)*PCT

                    PCT = PIEVOL(I+1)/DUMVOL
                    PIEVOL(I+1) = LOGVOL(1,ILOG)*PCT
                 else
                    PIEVOL(I) = LOGVOL(1,ILOG)
                    goto 253
                 end if
                 ILOG = ILOG+1
                 252 continue
              end do

              253 do I = 1,NSEG16
                 LOGVOL(1,I) = PIEVOL(I)
              end do
           end if

           !--    CONVERT FROM SCRIBNER DECIMAL C TO SCRIBNER
           VOL(2) = VOL(2)*10.0

           !--    (ENDIF FOR BOARD FOOT)
        end if
        if (VOL(2) .lt.0.0) then
           VOL(2) = 0.0
        end if

        !      GET CUBIC VOLUME FOR THE PIECES SMALIAN FORMULA     *

        !    USE DIB AT DBHOB FOR LARGE END BUTT LOG
        if (CUPFLG.eq.1) then
           DIBL = LOGDIA(1,1)
           do I = 1,NUMSEG
              DIBS = LOGDIA(I+1,1)
              LENGTH = LOGLEN(I)
              LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH

              LOGVOL(4,I) = ANINT(LOGV*10.0)/10.0

              VOL(4) = VOL(4)+LOGVOL(4,I)

              DIBL = DIBS
              350 continue
           end do

           !              (ENDIF FOR CUBIC VOLUMES)
        end if
        !           END IF FOR MERCH LENGTH
     end if

     if (VOL(4) .lt.0.0) then
        VOL(4) = 0.0
     end if

     !--   (ENDIF FOR BOARD AND CUBIC FOOT EQUATIONS)
  end if

  !--    DETERMINE TOTAL CUBIC FIBER CONTENT (ALL TREES)       *

  !--   ENTER LOGIC FOR TOTAL CUBIC VOLUME HERE
  750 if ((DBHOB.ge.9.0.and. HTTOT.gt.40.0) .and. CUPFLG.eq.1) then
     call R10TC(EQNUM,DBHOB,HTTOT,CUBVOL)
     VOL(1) = CUBVOL
  end if

  !*      CORD WOOD MAIN STEM EQUATIONS  ALL TREES             *

  !--   PUT LOGIC HERE FOR CORDS

  !--   SET TLOGS TO NUMBER OF SEGMENTS MAIN STEM

  !--   IF MORE THAN ONE STEM PROFILE EQUATION IS SPECIFIED FOR BDFT,
  !--   CUFT, OR CORDS, THEN THEY MUST BE THE SAME.  OTHERWISE
  !--   THE DATA IS INVALID AND THE VARIABLES ARE ZEROED OUT.  THE SAME
  !--   APPLIES TO THE SPECIFIED TOP DIAMETERS.

  !  IF 32FT LOGS THEN FILL LOG ARRARYS WITH THE PIECE INFO   *

  if (FLAG32) then
     do I = 1,NSEG16
        LOGDIA(I,1) = PIEDIB(I,1)
        LOGLEN(I) = PIELEN(I)
        399 continue
     end do
     LOGDIA(NSEG16+1,1) = PIEDIB(NSEG16+1,1)
     TLOGS = NSEG16
     NUMSEG = NSEG16
  end if

  TLOGS = NUMSEG

  !--  DETERMINE TOP WOOD CUBIC FOOT PRODUCTS (ALL TREES)

  1000 continue

  return
end

  !#############################################################
  !#############################################################
  !   THE LOGIC BETWEEN THE DOUBLE ROWS OF "#" MUST BE DUPLICATED
  !   IN EACH REGIONAL VOLUME ROUTINE WITH THE CALLS TO THE STEM
  !   PROFILE MODELS MODIFIED TO POINT TO THE APPROPRIATE PROFILE
  !   MODEL.  ALL CALLS ARE PRECEDED BY A SHORT ROW OF "#".
  !     THERE ARE THREE INTERNAL SUBROUTINES IN THIS SECTION.
  !     THESE INTERNAL SUBROUTINES CONTAIN ALL OF THE CALLS TO
  !     STEM PROFILE MODELS.
  !     SUBROUTINE "FASTGRO" WILL FIND SMALL TREE CUBIC VOLUMES
  !     SUBROUTINE "SECGRO" WILL FIND SMALL TREE CUBIC VOLUMES
  !     SUBROUTINE "R10MLEN" WILL FIND THE MERCHANTABLE LENGTH TO
  !     A SPECIFIED TOPWOOD TOP DIAMETER.
  !     SUBROUTINE "R10GDIA" WILL RETURN THE END DIAMETERS OF LOGS
  !     ONCE SEGMENTATION HAS OCCURED.
  !     SUBROUTINE "R10_HTS" WILL RETURN EITHER MERCHANTABLE LENGTH
  !     OR TOTAL TREE HEIGHT DEPENDING ON WHAT HEIGHT IS GIVEN

subroutine R10MLEN(EQNUM,HTTOT,DBHOB,STUMP,TOP,LMERCH)

  !--   SUBROUTINE WILL DETERMINE WHAT THE MERCHANTABLE LENGTH OF
  !--   STEM FROM THE STUMP TO A SPECIFIED TOP, INCLUDING TRIM.
  !--   NO ROUNDING IS USED WHEN FINDING THE MINIMUM TOP DIAMETER.
  !--   IF THE SPECIFIED MINIMUM TOP IS SET TO 6" THEN 6" DIB
  !--   WITHOUT ROUNDING IS USED AS THE CUT-OFF POINT.
  !--   USED ONLY FOR TOPWOOD VOLUME DETERMINATION

  ! Variables
  character(len=10) :: EQNUM
  integer :: I,FIRST,HALF,LAST,TOPLOP
  real    :: DBHOB,D2,HTTOT,HTUP,LMERCH,STUMP,TOP,TOP1

  !--   SET TOP1 EQUAL TO DESIRED TOP TIMES 10 TRUNCATED
  TOP1 = INT(TOP*10.0)

  FIRST = 1
  !--   FIND DESIRED TOP TO THE NEAREST TENTH OF A FOOT
  LAST = INT(HTTOT+.5)*10
  TOPLOP = LAST

  do I = 1,TOPLOP
     if (FIRST .eq. LAST) then
        go to 100
     end if
     HALF = (FIRST + LAST + 1 ) / 2
     HTUP = FLOAT(HALF)/10.0
     !###########
     call R10TAP(EQNUM,DBHOB,HTTOT,HTUP,D2)

     !--     CONVERT TOP DIAMETER TO TENTH INCH TRUNCATED.
     !--     BEFORE TRUNCATION ADD .005 TO ROUND UP THE DIAMETERS
     !--     THAT ARE CLOSE - 5.995 AND ABOVE WILL BE CONVERTED TO 6

     D2 = INT((D2 + 0.005)*10.0)
     if (TOP1.le. D2) then
        !--        MOVE UP STEM
        FIRST = HALF
     else
        !--        MOVE DOWN THE STEM
        LAST = HALF - 1
     end if
     10 continue
  end do
  100 continue
  LMERCH = FLOAT(FIRST)/10.0 - STUMP
  if (LMERCH .lt.0.0) LMERCH = 0.0
  return
end

subroutine R10GDIB(EQNUM,TOP,HTTOT,DBHOB,STUMP,TRIM,TLOGS,&
     &NUMSEG,LOGLEN,LOGDIA)

  !--   ALL LOG DIAMETERS ARE ROUNDED TO THE NEAREST 1 INCH
  !--   CLASS (I.E. 8" CLASS = 7.6 - 8.5)

  ! Variables
  character(len=10) :: EQNUM
  integer :: NUMSEG,TLOGS,I

  real    :: TOP,DBHOB,D2,HTTOT,HTUP,LOGLEN(20)
  real    :: LOGDIA(21,3),STUMP,TRIM,XXX,DIBCLS

  do I = 1+TLOGS,21
     LOGDIA(I,2) = 0.0
     10 continue
  end do

  if (NUMSEG .eq.0) go to 1001
  if (STUMP .gt.4.5 ) then
     !--     USE FOR TOP WOOD
     !##########A#
     call R10TAP(EQNUM,DBHOB,HTTOT,STUMP,D2)
     LOGDIA(1+TLOGS,2) = D2
  else

     !--     USE FOR MAIN STEM
     !###########
     call R10TAP(EQNUM,DBHOB,HTTOT,4.5,D2)
     LOGDIA(1,2) = D2
  end if
  HTUP = STUMP
  do I = 1+TLOGS,NUMSEG+TLOGS
     HTUP = HTUP+TRIM+LOGLEN(I)
     !###########
     call R10TAP(EQNUM,DBHOB,HTTOT,HTUP,D2)
     LOGDIA(I+1,2) = D2
     20 continue
  end do
  !--   NEED TO PUT LOG DIB IN PROPER DIAMETER CLASS
  do I = 1+TLOGS,NUMSEG+TLOGS+1
     DIBCLS = INT(LOGDIA(I,2))
     XXX = LOGDIA(I,2)-DIBCLS
     if ( XXX .gt.0.55) DIBCLS = DIBCLS+1.0
     LOGDIA(I,1) = DIBCLS
     30 continue
  end do
  !--  SMALL END DIAMETER OF TOP LOG MIGHT BE LESS THAN
  !--  THE MINIMUM SPECIFIED TOP DUE TO ROUNDING OF LOG LENGTHS
  !--  IF THIS IS TRUE, THEN FORCE TOP DIAMETER EQUAL TO
  !--  MINIMUM SPECIFIED TOP.
  if ( LOGDIA(NUMSEG+TLOGS+1,1) .lt. TOP) then
     LOGDIA(NUMSEG + TLOGS + 1,1) = TOP
  end if
  1001 continue
  return
end

subroutine R10_HTS(EQNUM,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,TOP,&
     &LMERCH)

  !     TAPEQU IS THE PROFILE EQUATION NUMBER THAT WILL BE USED IN THE
  !             CALCULATIONS
  !             TAPEQU 1051 IS SPRUCE-HEMLOCK - 16 FOOT LOGS
  !             TAPEQU 1052 IS SPRUCE-HEMLOCK - 32 FOOT LOGS
  !             TAPEQU 1053 IS WESTERN REDCEDAR - 16 FOOT LOGS
  !             TAPEQU 1054 IS WESTERN REDCEDAR - 32 FOOT LOGS
  !             TAPEQU 1055 IS ALASKA CEDAR - 16 FOOT LOGS
  !             TAPEQU 1056 IS ALASKA CEDAR - 32 FOOT LOGS
  !     HTTOT    IS THE TOTAL HEIGHT OF TREE FROM GROUND TO TIP
  !     DBHOB    IS DIAMETER BREAST HEIGHT OUTSIDE BARK
  !     STUMP  IS THE HEIGHT OF THE STUMP
  !     TOP    IS SPECIFIED TOP DIAMETER
  !     LMERCH IS THE MERCHANTABLE LENTH FROM STUMP TO SPECIFIED
  !             TOP DIAMETER, INCLUDING TRIM.

  ! Variables
  real    :: DST(2),DSLO(2),DSHI(2),XLL(20),DS(20),HH(20)

  !     LIST REAL VARIABLES

  character(len=1) :: HTTYPE
  character(len=10) :: EQNUM
  character(len=2) :: ISP
  character(len=2) :: IAC
  character(len=2) :: IRC
  character(len=2) :: ISS
  character(len=2) :: IRA
  real    :: LIMD,LOWD,LUPD,LMERCH,TAPER,HITOP,RH,DSX,BKWRC,DSI
  real    :: HTTOT,HT1PRD,DBHOB,STUMP,TOP,RXL,DXL,HTEST
  real    :: BKAYC,H,HEST,D,C,HTD,BK,RH32,RH40
  real    :: BKAC,DVA,BKWR,DVR,BB,DD2MI,DVREDA
  integer :: I,K,KNOL,IMO

  !     SPRUCE-HEMLOCK TAPER EQUATION
  !     PROFILE EQUATION USED FOR DIAMETER CALCULATIONS

  DD2MI(RH,RH32,D,H) = RH**1.5 + (-.0052554*H + 0.000034947*H**2&
       &+ 0.104477*H/D) * (RH**1.5 - RH**3) + (7.76807/D**2&
       &- 0.0000094852*H**2 - 0.011351*H/D) * (RH**1.5-RH32**32)

  !     BARK THICKNESS EQUATION

  BB(D,H) = (0.8467 + 0.0009144*D + 0.0003568*H)

  !     THE FOLLOWING TAPER EQUATION IS FOR WESTERN RED CEDAR
  !         BASED ON DEPENDENT VARIABLE  (DIBI/DBHIB)**2

  DVR(RH,RH32,H,DBHOB) = RH**1.5 + ((RH**1.5 - RH**3) *&
       &(5.17703194/DBHOB**2 - 0.12516819*DBHOB + 0.02537037*H&
       &- 0.00004193*H**2 + 0.00155481 * DBHOB**2)) + ((RH**1.5&
       &- RH32**32) * (-0.00002070*H**2 + 0.24125235/DBHOB**2))

  !     BARK THICKNESS EQUATION FOR WESTERN RED CEDAR

  BKWR(DBHOB,H) = 0.86031485 + 0.00059638*H - 0.18335961/DBHOB

  !     THE FOLLOWING TAPER EQUATION IS FOR ALASKA YELLOW CEDAR
  !         BASED ON DEPENDENT VARIABLE  (DIBI/DBHIB)

  DVA(RH,RH32,H,DBHOB) = RH**1.5+((RH**1.5-RH**3)*(-0.02834001*DBHOB&
       &+ 0.00007123*H**2 + 0.06709114*H/DBHOB))&
       &+ ((RH**1.5 - RH32**32) * (.00282021*DBHOB - 0.00002277*H**2&
       &+ 1.06064717/DBHOB**2 - 0.00528349*H/DBHOB))

  !     BARK THICKNESS EQUATION FOR ALASKA YELLOW CEDAR

  BKAC(DBHOB,H) = 0.95866817 + 0.00064402*DBHOB - 3.1299972/H

  !     THE FOLLOWING TAPER EQUATION IS FOR RED ALDER
  !         BASED ON DEPENDENT VARIABLE  (DIBI/DBHIB)

  DVREDA(RH,RH32,RH40,H,DBHOB) = 0.91274*(RH**1.5)&
       &-(1.9758*(RH**1.5-RH**3.)*(DBHOB*10.**(-2.)))&
       &+ (8.2375*(RH**1.5 - RH**3.)*(H)*(10.**(-3.)))&
       &- (4.964*(RH**1.5 - RH32**32.)*(H*DBHOB)*(10.**(-5.)))&
       &+ (3.773*(RH**1.5 - RH32**32.)*(H**0.5)*(10.**(-3.)))&
       &- (7.417*(RH**1.5 - RH40**40.)*(H**2.)*(10.**(-6.)))

  !     BARK THICKNESS EQUATION FOR RED ALDER

  H = HTTOT
  HEST = 0.0
  D = DBHOB
  DBHOB = D

  ! SET HTD TO MERCHANTABLE HEIGHT, CONVERTING LOGS TO FEET IF NECESSARY

  if (HTTYPE.eq.'L' .or. HTTYPE.eq.'l') then
     if (EQNUM(1:3).eq.'A16') then
        LMERCH = HT1PRD*16.3
        HTD = LMERCH +STUMP
     else if (EQNUM(1:3).eq.'A32') then
        LMERCH = HT1PRD*32.6
        HTD = LMERCH + STUMP
     end if
  else
     LMERCH = HT1PRD
     HTD = LMERCH + STUMP
  end if

  if (EQNUM(8:10).eq.'042') ISP = 'AC'
  if (EQNUM(8:10).eq.'242') ISP = 'RC'
  if (EQNUM(8:10).eq.'098') ISP = 'SS'
  if (EQNUM(8:10).eq.'351') ISP = 'RA'
  IAC = 'AC'
  IRC = 'RC'
  ISS = 'SS'
  IRA = 'RA'

  LIMD = TOP
  LUPD = LIMD + 0.001
  LOWD = LIMD - 0.001

  !     ZERO OUT ARRAYS

  do I = 1,20
     DS(I) = 0.0
     XLL(I) = 0.0
     15 continue
  end do

  I = 1
  if (LMERCH.gt.0.01) then
     H = 0.0
     HH(I) = HTD
  end if

  if (ISP.eq.ISS .or. ISP.eq.IRA .or. (ISP.eq.IAC .and. D.gt.38.01)&
       &.or. (ISP.eq.IRC .and. D.gt.56.01)) then
     !///////////////////////////////////////////////////////////////////////
     !     THE FOLLOWING ESTIMATES HI AT TOP DIAMETER OR TOTAL TREE HEIGHT
     !       DEPENDING ON WHICH VARIABLE IS MISSING (FOR SPRUCE-HEMLOCK EQ.)
     !       SPRUCE-HEMLOCK, RED ALDER, AND LARGE ALASKA AND WESTERN RED CEDAR
     !///////////////////////////////////////////////////////////////////////
     I = 1

     if (H.ge.0.01) then
        if (ISP.eq.IRA) then
           BK = 0
        else
           BK = BB(D,H)
        end if
        !            FV = 0.005454154*D**2*(H - 4.5)*BK
     end if

     XLL(I) = 1.0 - (2./3.)*(LIMD/D)

     if (H.lt.0.01)  HEST = HH(I)/XLL(I)

     if (HEST.gt.0.01) BK = BB(D,HEST)

     if (ISP.eq.IRA) then
        DST(I) = (LIMD**2)/(D**2)
     else
        DST(I) = (LIMD**2)/(BK*D**2)
     end if

     DSLO(I) = DST(I) - 0.0001
     DSHI(I) = DST(I) + 0.0001

     if (H.le.0.01) then
        RH = (HEST - HH(I))/(HEST - 4.5)

        !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
        !      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
        !      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.

        if (RH.le.0.0) then
           RH = 0
           RH32 = 0
           RH40 = 0
        else if (RH .lt.0.078) then
           RH40 = 0.15
           RH32 = 0.078
        else if (RH .lt.0.15) then
           RH40 = 0.15
           RH32 = RH
        else
           RH40 = RH
           RH32 = RH
        end if

        if (ISP.eq.IRA) then
           DS(I) = DVREDA(RH,RH32,RH40,HEST,D)
        else
           DS(I) = DD2MI(RH,RH32,D,HEST)
        end if
        if (ds(i).lt.0.0) ds(i) = 0.0

        RXL = 0.9*RH
        if (RXL.le.0.0) then
           RH32 = 0
           RH40 = 0
        else if (RXL .lt.0.078) then
           RH40 = 0.15
           RH32 = 0.078
        else if (RXL .lt.0.15) then
           RH40 = 0.15
           RH32 = RXL
        else
           RH40 = RXL
           RH32 = RXL
        end if
        if (ISP.eq.IRA) then
           DXL = DVREDA(RXL,RH32,RH40,HEST,D)
        else
           DXL = DD2MI(RXL,RH32,D,HEST)
        end if
        if (dxl.lt.0.0) dxl = 0.0
        TAPER = (DS(I) - DXL)/(.1*RH)
     else
        HH(I) = H*XLL(I)
        RH = (H - HH(I))/(H - 4.5)
        if (RH.le.0.0) then
           RH = 0
           RH32 = 0
           RH40 = 0
        else if (RH .lt.0.078) then
           RH40 = 0.15
           RH32 = 0.078
        else if (RH .lt.0.15) then
           RH40 = 0.13
           RH32 = RH
        else
           RH40 = RH
           RH32 = RH
        end if
        if (ISP.eq.IRA) then
           DS(I) = DVREDA(RH,RH32,RH40,H,D)
        else
           DS(I) = DD2MI(RH,RH32,D,H)
        end if
        if (ds(i).lt.0.0) ds(i) = 0.0

        RXL = 0.9*RH
        if (RXL.le.0.0) then
           RXL = 0
           RH32 = 0
           RH40 = 0
        else if (RXL .lt.0.078) then
           RH40 = 0.15
           RH32 = 0.078
        else if (RXL .lt.0.15) then
           RH40 = 0.15
           RH32 = RXL
        else
           RH40 = RXL
           RH32 = RXL
        end if
        if (ISP.eq.IRA) then
           DXL = DVREDA(RXL,RH32,RH40,H,D)
        else
           DXL = DD2MI(RXL,RH32,D,H)
        end if

        if (dxl.lt.0.0) dxl = 0.0
        TAPER = (DS(I) - DXL)/(.1*RH)
     end if

     do 245, K = 1,10
     if (DS(I).gt.DSLO(I) .and. DS(I).lt.DSHI(I))  go to 40
     if (TAPER.ne.0.0) then
        RXL = RH + (DST(I) - DS(I))/TAPER
     else
        RXL = RH
     end if
     RH = RXL
     if (RH.le.0.0) then
        RH = 0
        RH32 = 0
        RH40 = 0
     else if (RH .lt.0.078) then
        RH40 = 0.15
        RH32 = 0.078
     else if (RH .lt.0.15) then
        RH40 = 0.15
        RH32 = RH
     else
        RH40 = RH
        RH32 = RH
     end if
     if (H.le.0.01) then
        HEST = (HH(I) - 4.5*RH)/(1.0 - RH)
        if (ISP.eq.IRA) then
           DS(I) = DVREDA(RH,RH32,RH40,HEST,D)
        else
           DS(I) = DD2MI(RH,RH32,D,HEST)
        end if
        if (ds(i).lt.0.0) ds(i) = 0.0
     else
        if (ISP.eq.IRA) then
           DS(I) = DVREDA(RH,RH32,RH40,H,D)
        else
           DS(I) = DD2MI(RH,RH32,D,H)
        end if
        if (ds(i).lt.0.0) ds(i) = 0.0
     end if
     245 continue

     40 continue
     if (HTD.gt.0.01.and. H.lt.0.01)  go to 27
     if (H.lt.0.01)  go to 27

     HH(I) = H - (RH*(H - 4.5))
     LMERCH = HH(I) - STUMP

     27 continue

     if (H.lt.0.01)  H = HEST
     HTTOT = H

  else
     !////////////////////////////////////////////////////////////////////////////
     !     THE FOLLOWING ROUTINE IS DESIGNED TO ESTIMATE HT AT TOP DIAMETER
     !       OR TOTAL TREE HEIGHT DEPENDING ON WHICH VARIABLE IS MISSING
     !       SMALL ALASKA AND WESTERN RED CEDAR
     !////////////////////////////////////////////////////////////////////////////
     I = 1
     if (H.gt.0.001) then

        ! THIS PART OF THE PROGRAM ESTIMATES HT TO MERCHANTABLE TOP DIAMETER

        KNOL = 0
        HITOP = STUMP

        4541 continue

        HITOP = HITOP + 16.3
        if (HITOP.le.H) then

           KNOL = KNOL + 1
           RH = (H - HITOP)/(H - 4.5)
           if (RH.le.0.0) then
              RH = 0
              RH32 = 0
           else if (RH .lt.0.078) then
              RH32 = 0.078
           else
              RH32 = RH
           end if
           !               HX(KNOL) = HITOP

           !  WESTERN REDCEDAR MERCHANTABLE LENGTH
           if (ISP.eq.IRC) then
              DSX = DVR(RH,RH32,H,DBHOB)
              BKWRC = BKWR(DBHOB,H)
              if (dsx.lt.0) dsx = 0
              DSI = (DSX*BKWRC)**.5 * DBHOB

              !  ALASKA CEDAR MERCHANTABLE LENGTH
           else if (ISP.eq.IAC) then
              DSX = DVA(RH,RH32,H,DBHOB)
              BKAYC = BKAC(DBHOB,H)
              if (dsx.lt.0) dsx = 0
              DSI = (DSX*BKAYC)**.5 * DBHOB
           else
              go to 900
           end if

           DS(KNOL) = DSI

           if (DS(KNOL).ge.LIMD)  go to 4541
        end if

        if (DS(KNOL).lt.LIMD)  KNOL = KNOL - 1
        HITOP = HITOP - 16.3
        !            HX(KNOL+2) = H
        DS(KNOL+2) = 0.0
        IMO = KNOL + 1
        C = 1.0

        5566 continue
        HITOP = HITOP + C

        RH = (H - HITOP)/(H - 4.5)
        if (RH.le.0.0) then
           RH = 0
           RH32 = 0
        else if (RH .lt.0.078) then
           RH32 = 0.078
        else
           RH32 = RH
        end if

        !  WESTERN RED CEDAR TAPER EQUATION

        if (ISP.eq.IRC) then
           DSX = DVR(RH,RH32,H,DBHOB)
           BKWRC = BKWR(DBHOB,H)
           if (dsx.lt.0) dsx = 0
           DSI = (DSX*BKWRC)**.5 * DBHOB

           !  ALASKA YELLOW CEDAR TAPER EQUATION
        else
           continue
           DSX = DVA(RH,RH32,H,DBHOB)
           BKAYC = BKAC(DBHOB,H)
           if (dsx.lt.0) dsx = 0
           DSI = (DSX*BKAYC)**.5 * DBHOB
        end if

        if (DSI.gt.LOWD.and.DSI.lt.LUPD)  go to 5543

        if (DSI.gt.LIMD)  go to 5566

        HITOP = HITOP - C
        C = C * 0.1

        go to 5566

        5543 continue

        DS(IMO) = DSI
        HH(1) = HITOP
        LMERCH = HITOP - STUMP
        !            HX(IMO) = HITOP

        !     THIS PART OF THE PROGRAM ESTIMATES THE TOTAL HEIGHT OF THE TREE
        !     THIS PART USES ALASKA CEDAR OR WESTERN REDCEDAR EQUATIONS

     else

        HITOP = STUMP + LMERCH
        HTEST = HITOP
        C = 1.0

        7566 continue

        HTEST = HTEST + C
        H = HTEST
        RH = (H - HITOP)/(H - 4.5)
        if (RH.le.0.0) then
           RH = 0
           RH32 = 0
        else if (RH .lt.0.078) then
           RH32 = 0.078
        else
           RH32 = RH
        end if

        !   WESTERN RED CEDAR TAPER EQUATION
        if (ISP.eq.IRC) then
           DSX = DVR(RH,RH32,H,DBHOB)
           BKWRC = BKWR(DBHOB,H)
           if (dsx.lt.0) dsx = 0
           DSI = (DSX*BKWRC)**.5 * DBHOB

           !  ALASKA YELLOW CEDAR TAPER EQUATION
        else
           DSX = DVA(RH,RH32,H,DBHOB)
           BKAYC = BKAC(DBHOB,H)
           if (dsx.lt.0) dsx = 0
           DSI = (DSX*BKAYC)**.5 * DBHOB
        end if

        if (DSI.gt.LOWD.and.DSI.lt.LUPD)  go to 7543
        if (DSI.lt.LIMD)  go to 7566

        HTEST = HTEST - C
        C = C * 0.1

        go to 7566

        7543 continue
        H = HTEST
        HTTOT = H

     end if

  end if

  900 return
end

subroutine R10TC(VOLEQ,DBHOB,HTTOT,TCVOL)

  !--   DETERMINE TOTAL CUBIC FIBER CONTENT.  THE VOLUME WILL BE
  !--   DETERMINED BY USING THE VOLUME OF A CYLINDER WITH A DIAMETER
  !--   OF DIB AT 1 FOOT FOR THE STUMP VOLUME AND USING THE SMALIAN
  !--   FORMULA TO COMPUTE VOLUMES FOR 4 FOOT SECTIONS FROM THE STUMP
  !--   TO THE TIP.

  !--   VARIABLES OF INTEREST ARE:
  !--   totht - REAL - **TOTAL TREE HT INCLUDING THE STUMP.**
  !--   TCVOL - REAL - TOTAL VOLUME FOR THE TREE IN CUBIC FOOT,
  !--                  DOES NOT INCLUDE THE LIMBS OR ROOTS.

  ! Variables
  character(len=10) :: VOLEQ
  integer :: HTLOOP,I
  real    :: D2,D2OLD,HT2,TCVOL,VOLTMP
  real    :: DBHOB,HTTOT,R

  HTLOOP = INT((HTTOT - 1.0)/4.0)
  HT2 = 1.0
  !##########
  call R10TAP(VOLEQ,DBHOB,HTTOT,HT2,D2)
  !--   STUMP VOLUME IS VOLUME FOR A 1 FT HIGH CYLINDER
  R = D2/2.0
  TCVOL = (3.1416*R*R)/144.0
  do I = 1,HTLOOP
     D2OLD = D2
     HT2 = HT2+4.0
     !##########
     call R10TAP(VOLEQ,DBHOB,HTTOT,HT2,D2)
     !--     USE SMALIAN FOR NON STUMP SEGMENTS
     VOLTMP = .00272708*(D2OLD*D2OLD+D2*D2)*4.0
     TCVOL = TCVOL + VOLTMP
     10 continue
  end do
  !--   USE SMALIAN FOR TIP, WITH A TIP DIAMETER OF 0.0
  if ( (HTTOT-HT2) .gt.0.0 ) then
     VOLTMP = .00272708*(D2*D2)*(HTTOT-HT2)
     TCVOL = TCVOL + VOLTMP
  end if
  return
end

  !--   THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE

  !--   USING REGION 10 VOLUME DETERMINATION ROUTINES.
  !--   THERE ARE CALLS TO THE FOLLOWING SUBROUTINES
  !--     R10MERLEN - INTERNAL - R10TAP
  !--     R10GETDIB - INTERNAL - R10TAP
  !--     R10_HTS   - INTERNAL - NO CALLS
  !--     SECGRO    - INTERNAL - NO CALLS
  !--     FASTGRO   - INTERNAL - NO CALLS
  !--     R10TAP    - EXTERNAL - NO CALLS
  !--     NUMLOG    - EXTERNAL - NO CALLS
  !--     SCRIB     - EXTERNAL - NO CALLS
  !--     SEGMNT    - EXTERNAL - NO CALLS

  !--  VARIABLES OF INTEREST ARE:

  !--  COR - CHARACTER - FLAG TO USE SCRIBNER OR SCRIBNER
  !--                    FACTORS TO GENERATE SCRIBNER PRODUCT
  !--  DBHOB - REAL - DIAMETER BREAST HEIGHT
  !--  EQNUM - INTEGER - EQUATION NUMBER TO USE
  !--    EQNUM(1) - BOARD FOOT MAIN STEM
  !--    EQNUM(2) - CUBIC FOOT MAIN STEM
  !--    EQNUM(3) - CORD WOOD MAIN STEM
  !--    EQNUM(4) - CUBIC FOOT TOP WOOD
  !--    EQNUM(5) - CORD WOOD TOP WOOD

  !--  TOP DIB TO USE
  !--    MTOPP - MAIN STEM
  !--    MTOPS - TOP WOOD

  !********BOARD FOOT VOLUME EQUATION NUMBERS********

  !--    THE FOLLOWING ARE REGION 10 STEM PROFILE MODELS
  !--    1051 = SPRUCE - HEMLOCK - 16 FOOT LOG LENGTH
  !--    1052 = SPRUCE - HEMLOCK - 32 FOOT LOG LENGTH
  !--    1053 = WESTERN RED CEDAR - 16 FOOT LOG LENGTH
  !--    1054 = WESTERN RED CEDAR - 32 FOOT LOG LENGTH
  !--    1055 = ALASKA CEDAR - 16 FOOT LOG LENGTH
  !--    1056 = ALASKA CEDAR - 32 FOOT LOG LENGTH

  !********CUBIC FOOT VOLUME EQUATION NUMBERS********

  !--    THE FOLLOWING ARE REGION 10 STEM PROFILE MODELS
  !--    1051 = SPRUCE - HEMLOCK - 16 FOOT LOG LENGTH
  !--    1052 = SPRUCE - HEMLOCK - 32 FOOT LOG LENGTH
  !--    1053 = WESTERN RED CEDAR - 16 FOOT LOG LENGTH
  !--    1054 = WESTERN RED CEDAR - 32 FOOT LOG LENGTH
  !--    1055 = ALASKA CEDAR - 16 FOOT LOG LENGTH
  !--    1056 = ALASKA CEDAR - 32 FOOT LOG LENGTH

  !--  EVOD - INTEGER - EVEN OR ODD LENGTH SEGMENTS ALLOWED
  !--         SEGMENTATION OPTIONS 11-14 ALLOW ODD LENGTHS BY
  !--         DEFINITION
  !--        1 = ODD SEGMENTS ALLOWED
  !--        2 = ONLY EVEN SEGMENTS ALLOWED

  !--  HTTOT - REAL - TREE HEIGHT FROM GROUND TO TIP
  !--  HT1PRD - REAL - TREE MERCHANTABLE HEIGHT IN FEET OR LOGS
  !--  LENMS - REAL - LENGTH OF MAIN STEM IF A TOP WOOD PRODUCT
  !--                 IS DESIRED
  !--  LMERCH - REAL - GIVEN MERCHANTABLE LENGTH OF STEM EITHER INPUT
  !--             IN LOGS AS HT1PRD FROM A6 RECORD OR RETURNED
  !--             FROM SUBROUTINE "R10_HTS" AND THE MERCHANTABLE
  !--             LENGTH OF STEM WITH TRIM REMOVED RETURNED FROM
  !--             SUBROUTINE "SEGMNT"
  !--  LOGLEN - REAL(20) - LOG SEGMENT LENGTHS COMPUTED BY
  !--           SUBROUTINE "SEGMNT". DOES NOT INCLUDE TRIM.
  !--  LOGV - REAL - THE PRODUCT VOLUME OF A LOG
  !--  MAXLEN - REAL - MAXIMUM SEGMENT LENGTH
  !--  MINLEN - REAL - MINIMUM SEGMENT LENGTH
  !--  MERCHL - INTEGER - MINIMUM PRODUCT LENGTH FOR A MERCH TREE
  !--  NOLOGP - REAL - AVERAGE NUMBER OF 16 FOOT LOGS IN MAIN STEM
  !--  NOLOGS - REAL - AVERAGE NUMBER OF 16 FOOT LOGS IN TOP WOOD
  !--  NUMSEG - INTEGER - THE COMPUTED NUMBER OF SEGMENTS FROM
  !--           SUBROUTINE "NUMSEG"
  !--  OPT - INTEGER - SPECIFIED SEGMENTATION OPTION
  !--        OPTION CODES ARE AS FOLLOWS:
  !--        11 = 16 FT LOG SCALE (FSH 2409.11)
  !--        12 = 20 FT LOG SCALE (FSH 2409.11)
  !--        13 = 32 FT LOG SCALE
  !--        14 = 40 FT LOG SCALE
  !--        21 = NOMINAL LOG LENGTH (NLL), IF TOP LESS THAN HALF
  !--             OF NLL IT IS COMBINED WITH NEXT LOWEST LOG AND
  !--             SEGMENTED ACORDING TO RULES FOR NLL MAX LENGTH.
  !--             IF SEGMENT IS HALF OR MORE OF NLL THEN SEGMENT
  !--             STANDS ON ITS' OWN.
  !--        22 = NOMINAL LOG LENGTH, TOP IS PLACED WITH NEXT LOWEST
  !--             LOG AND SEGMENTED ACORDING TO RULES FOR NLL MAX
  !--             LENGTH.
  !--        23 = NOMINAL LOG LENGTH, TOP SEGMENT STANDS ON ITS' OWN.
  !--        24 = NOMINAL LOG LENGTH, TOP SEGMENT LESS THAN 1/4 OF
  !--             NNL THEN SEGMENT IS DROPED, IF SEGMENT IS
  !--             1/4 TO 3/4 THEN SEGMENT IS = 1/2 OF NNL,
  !--             IF SEGMENT IS GREATER THAN 3/4 OF NNL THEN
  !--             SEGMENT IS = NNL.

  !--  PCTDF1 - REAL - THE INTEGER VALUE REPRESENTING THE % OF DEFECT
  !--          DUE TO CULL AND BREAKAGE.IN THE MAIN STEM PORTION OF TREE
  !--          THIS DEFECT % WILL BE APPLIED TO BOTH BDFT AND CUFT PROD/

  !--  PCTDF2 - REAL - THE INTEGER VALUE REPRESENTING THE % OF HIDDEN DEF
  !--  LOGDEF() - REAL - THE VALUE REPRESENTING THE % OF SEEN DEF BY LOG

  !--  STUMP - REAL - HEIGHT OF STUMP
  !--  TAPEQU - REAL - TAPER EQUATION TO USE - FROM EQNUM(?)
  !--  TRIM - REAL - TRIM LENGTH FOR EACH SEGMENT
  !--  VOL - REAL
  !--    VOL(1) -  TOTAL VOLUME FOR THE TREE IN CUBIC FOOT, DOES NOT
  !--              INCLUDE LIMBS AND ROOTS.
  !--    VOL(2) - GROSS AMOUNT OF BOARD FOOT PRODUCT IN MAIN STEM.
  !--    VOL(3) - NET AMOUNT OF BOARD FOOT PRODUCT IN MAIN STEM.
  !--    VOL(4) - GROSS AMOUNT OF CUBIC FOOT PRODUCT IN MAIN STEM.
  !--    VOL(5) - NET AMOUNT OF CUBIC FOOT PRODUCT IN MAIN STEM.
  !--    VOL(6) - AMOUNT OF CORD WOOD PRODUCT IN MAIN STEM
  !--    VOL(7) - GROSS AMOUNT OF CUBIC PRODUCT IN TOP WOOD.
  !--    VOL(8) - NET AMOUNT OF CUBIC PRODUCT IN TOP WOOD.
  !--    VOL(9) - AMOUNT OF CORD WOOD PRODUCT IN TOP WOOD.
  !--    VOL(10) - GROSS AMOUNT OF INT'L 1/4" BOARD FOOT PRODUCT IN MAIN STEM.
  !--    VOL(11) - NET AMOUNT OF INT'L 1/4" BOARD FOOT PRODUCT IN MAIN STEM.
  !--    VOL(12) - GROSS AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
  !--    VOL(13) - NET AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.

  !--    LOGVOL(1,X) - GROSS BOARD FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(2,X) - GROSS REMOVED BOARD FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(3,X) - NET BOARD FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(4,X) - GROSS CUBIC FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(5,X) - GROSS REMOVED CUBIC FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(6,X) - NET CUBIC FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(7,X) - GROSS INT'L 1/4 VOLUME (20 LOGS)
