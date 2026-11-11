!=============================!
!   VolumeLibrary : profile   !
!=============================!
subroutine PROFILE (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
     &HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,&
     &AVGZ1,AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,&
     &LOGST,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,DRCOB,&
     &CTYPE,FCLASS,PROD,ERRFLAG)

  !--   This subroutine determines the cubic and board foot volume of a tree
  !--     using species and area specific profile models developed by
  !--     Jim Flewelling, Ray Czaplewski, Wensel and Olson, and John Byrne.
  !--     All merchandising logic was developed by the Forest Management
  !--     Service Center in accordance with existing FS Handbook direction.

  !--   THERE ARE CALLS TO THE FOLLOWING SUBROUTINES
  !--     MRULES - EXTERNAL - No calls
  !--     NUMLOG - EXTERNAL - No calls
  !--     SCRIB -  EXTERNAL - No calls
  !--     INTL14 - EXTERNAL - No calls (part of scrib.for)
  !--     SEGMNT - EXTERNAL - No calls

  !--     GETDIB   - INTERNAL - CALLS TAPERMODEL
  !--     MERLEN   - INTERNAL - CALLS TAPERMODEL
  !--     TCUBIC   - INTERNAL - CALLS TAPERMODEL
  !--     TOP6LEN  - INTERNAL - CALLS TAPERMODEL
  !--     VOLRATIO - INTERNAL - CALLS R1TAP
  !--     TAPERMODEL - INTERNAL - CALLS PROFILE EQUATION ROUTINES
  !--     FWSMALL  - INTERNAL - CALLS SF_DS, BRK_UP

  use DEBUG_MOD
  !REV  Revised TDH 03/08/10
  !REV  See notes in GETDIB subroutine below
  ! YW 11/06/2012 Changed the errflag to 12 for NUMSEG > 20
  ! YW 01/18/2013 Added calculation for stump VOL(14) and tip VOL(15)
  ! YW 03/25/2014 added PROD to call MRULES
  ! YW 06/20/2014 added logic for region 10 32-foot log equation to reset MINLEN
  !               to correct the missing 18, 20, and 22 foot logs
  ! YW 07/02/2014 added errflag (13) for top diameter greater than DBH in VOLINTRP routine
  ! YW 08/19/2015 Added VOL to SUBROUTINE TCUBIC and modified TCUBIC to calculate stump and tip vol
  !               and save value in VOL(14) and VOL(15)
  ! YW 10/15/2015 Modified TAPERMODEL for call R1tap to calc vol.
  ! YW 06/16/2017 Added merch height to MTOPS to HT2PRD
  ! YW 12/20/2019 Added check REGN=3 for FIREWOOD in two places with REGN 5
  ! YW 02/19/2020 Changed variable MINLEN/MINLENT for call NUMLOG and SEGMNT
  ! YW 08/19/2021 Changed stump for Demar model DBH > 36

  !     MERCH VARIABLES
  ! Variables
  character(len=1) :: COR
  character(len=1) :: HTTYPE
  character(len=1) :: CTYPE
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=10) :: VOLEQ
  integer :: EVOD,OPT,REGN,HTFLG,FCLASS,N16SEG
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
  real    :: DIB,LOGLENG(20),BFINT,TOPV16,BOTV16,R

  !       Variables to hold flewellings coefficients
  integer :: SETOPT(6),JSP
  real    :: RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
  real    :: HEX(2),dex(2),ZEX(2)
  !       Temp variables for Region 1

  real    :: UHT,VMER,VMER1,VMER2,TEMPVOL,TCVOL,TCVOL1,TCVOL2
  !     Temp variable for BEH equation
  integer :: ZONE
  real    :: FC_HT,D17,TTH,DBHIB

  !  DW 08/22 initializations of variables otherwise uninitialized prior to MRULES call
  OPT = 0
  EVOD = 0
  MINLENT = 0
  MHT = 0

  !      LOGST = 0      ! commented out for variable log cruising
  !     changed to set LOGST = 0 for NOT variable log cruising (CTYPE = V)
  !     YW 05/10/2016
  if (CTYPE.ne.'V') LOGST = 0
  ! calc secondary product
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
     !Return different error flag for DBHOB and HTTOT 12/28/2022 (YW)
     if (DBHOB.lt.1.0) ERRFLAG = 3
     !IF(HTTOT.GT.0)ERRFLAG = 0
     if (HTTOT.lt.5.and. HT1PRD.le.0) ERRFLAG = 4
     go to 1000
  end if

  !     MRULES IS EXTERNAL AND CONTAINS THE MERCHANDIZING RULES SETTINGS
  call MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,MINLEN,&
       &MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,&
       &PROD)

  !     Add 1.5 to HTTOT to make it same as it in blmvol (YW 09/17/13)
  if (VOLEQ(1:1).eq.'B' .or. VOLEQ(1:1).eq.'b') then
     HTTOT = HTTOT+1.5
  else if (VOLEQ(1:1).eq.'6') then
     TTH = HTTOT
     HTTOT = HTTOT+1.0
  end if

  if (FCLASS .le.0.and. VOLEQ(4:6).eq.'BEH') then
     ERRFLAG = 2
     return
  end if

  if (VOLEQ(4:4).eq.'F' .or. VOLEQ(4:4).eq.'f' .or.&
       &VOLEQ(4:6).eq.'DEM' .or. VOLEQ(4:6).eq.'dem' .or.&
       &VOLEQ(4:6).eq.'CUR' .or. VOLEQ(4:6).eq.'cur') then

     TOPD = MTOPP
     if (HTTOT.le.0) then
        ! no total height
        HTFLG = 1
        !do not calc secondary product
        if (HT2PRD.le.0) MFLAG = 1
        if (HTTYPE.eq.'l' .or. HTTYPE.eq.'L') then
           !--           CHECK FOR VARIABLE LOG CRUISE
           if (CTYPE.eq.'V' .and. LOGST.gt.0) then
              ! SUM UP LOG LENGTHS
              MHT = 0
              do 120, I = 1,LOGST
              MHT = MHT + LOGLEN(I)
              if (LOGLEN(I) .ge.12) MHT = MHT + 0.5
              120 continue
           else
              !logs are in 10s of logs
              LHT = HT1PRD/10.0
              if (LHT.lt.1.0.and. LHT.ne.0.5) then
                 ERRFLAG = 7
                 goto 1000
              end if
              if (HTLOG .le.0) then
                 if (VOLEQ(1:3).eq.'A32' .or. VOLEQ(1:3).eq.'a32') then
                    HTLOG = 32
                 else
                    HTLOG = 16
                 end if
              end if
              if (HTLOG .ge.32) then
                 TRM = TRIM * 2
              else
                 TRM = TRIM
              end if
              MHT = (LHT * (HTLOG + TRM)) + STUMP
           end if

        else
           if (ht2prd .gt.0) then
              mht = ht2prd + stump
              topd = mtops
              if (ht1prd .gt.0) then
                 UPSHT1 = ht1prd + stump
                 UPSD1 = MTOPP
              else
                 UPSHT1 = 0
              end if

           else
              MHT = HT1PRD + STUMP
              topd = mtopp
           end if
        end if
     end if

     if (VOLEQ(4:4).eq.'F' .or. VOLEQ(4:4).eq.'f') then
        !--   Initialize Flewelling model for this tree

        call FWINIT(VOLEQ,DBHOB,HTTOT,MHT,TOPD,UPSHT1,UPSHT2,UPSD1,&
             &UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,JSP,RHFW,RFLW,&
             &TAPCOE,F,SETOPT,NEXTRA,HEX,DEX,ZEX,PINV_Z,FMOD,btr,FCLASS,&
             &ERRFLAG)

        AVGZ1 = ZEX(1)
        AVGZ2 = ZEX(2)

        if (ERRFLAG .eq.1.or. errflag.eq.6)  go to 1000

        if (ERRFLAG .eq.11) then
           !         USE ITERPOLATION ROUTINE 11/02
           call VOLINTRP(REGN,VOLEQ,DBHOB,LHT,MHT,MTOPP,HTTYPE,DBTBH,&
                &LOGVOL,LOGDIA,HTLOG,LOGLEN,LOGST,NOLOGP,VOL,CTYPE,PROD,&
                &ERRFLAG)
           go to 1000
        end if
     else
        !          region 10 call to determine total height or merch height
        call R10HTS(VOLEQ,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,MTOPP,&
             &LMERCH)

        !           IF(HT1PRD.LE.0) HT1PRD = LMERCH + STUMP

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

  !--    DETERMINE TOTAL CUBIC FIBER CONTENT (ALL TREES)       *

  !--   SUBROUTINE "TCUBIC" IS INTERNAL AND USES PROFILE MODEL
  if (CUTFLG .eq.1) then
     !     For Behre equation 616BEH***, 628BEH***, 632BEH*** use R6VOL3 to calculate total cubic
     if (VOLEQ(1:1).eq.'6') then
        if (VOLEQ(1:3).eq.'632') then
           ZONE = 2
           FC_HT = 33.6
        else
           ZONE = 1
           FC_HT = 16.3
        end if
        DBHIB = DBHOB-DBTBH
        if (TTH.gt.0) then
           D17 = FCLASS/100.0*DBHOB
           if (TTH.le.FC_HT) then
              !                use smailians to find total cubic volume
              VOL(1) = 0.00272708*(DBHIB*DBHIB)*TTH
           else
              !                call the total cubic routines
              call R6VOL3(DBHOB,DBTBH,FCLASS,TTH,ZONE,VOL)
           end if
        end if
     else

        call TCUBIC (VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,DBTBH,&
             &MTOPP,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,&
             &TCVOL,slope,errflag,VOL,MTOPS)
        VOL(1) = NINT(TCVOL * 10.0) * 1E-1
        if (drcob.le.0.and. ctype.eq.'F') then
           drcob = dex(2)
        end if
     end if
  end if

  if (DEBUG%MODEL) then
     write  (LUDBG, 125)'Profile - VOL(1)=', VOL(1)
     125 format (A, 2x, F8.4)
  end if

  if (BFPFLG.eq.1.or. CUPFLG.eq.1) then

     !--   MERCHANDISE THE TREE BOLE FOR PRIMARY PRODUCT
     !--      CHECK FOR VARIABLE LOG CRUISE
     if (CTYPE .eq. 'V') then
        NUMSEG = LOGST
        LOGST = 0
        NOLOGP = NUMSEG
     else

        do 150, i = 1,20
        loglen(i) = 0
        150 continue
        if (HTTYPE.eq.'L' .or. HTTYPE.eq.'l') then
           LMERCH = MHT - STUMP
        else if (LMERCH .le.0) then
           LOGST = 0
           !MERCH HEIGHT ENTERED
           if (HT1PRD.gt.0) then
              LMERCH = HT1PRD - STUMP
              ! TOTAL HEIGHT ENTERED, FIND TOP DIAMETER
              if (HTFLG.eq.0) then
                 call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,&
                      &HTTOT,DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,&
                      &PINV_Z,TOP6,HT1PRD,MTOPP,MFLG,CUVOL,DIB,DOB,errflag)

                 MTOPP = DIB
              end if
              !NO MERCH HT, TOTAL HEIGHT
           else if (HT1PRD.eq.0.and. HTFLG.eq.0) then
              !--           SUBROUTINE "MERLEN" IS INTERNAL AND USES PROFILE MODEL
              !--           LMERCH IS THE MERCHANTABLE LENGTH FROM STUMP TO SPECIFIED TOP, INCLUDING TRIM.
              call MERLEN(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
                   &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,&
                   &STUMP,MTOPP,LMERCH,errflag)

              HT1PRD = LMERCH + STUMP
           end if
        end if

        !--   CHECK FOR REGION 5 FIREWOOD LOGIC
        if ((REGN.eq.5.or. REGN.eq.3) .and. PROD.eq.'07') then
           call FIREWOOD (VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
                &LMERCH,DBTBH,MTOPP,STUMP,HEX,DEX,ZEX,RHFW,RFLW,&
                &TAPCOE,F,FMOD,PINV_Z,TOP6,TCVOL,slope,errflag)
           VOL(4) = TCVOL
           !--   Added cordwood calculation (YW 2019/12/20)
           if (CDPFLG .eq.1) then
              VOL(6) = ANINT((VOL(4)/90.0)*10)/10.0
           end if
           go to 500
        end if

        !--    CHECK FOR A MINIMUM MERCH STEM LENGTH - IF NOT MERCH DO NOT
        !--            CACULATE PRODUCT VOLUMES
        if (LMERCH.lt.MERCHL) then
           go to 500
        end if

        !--   For Region 10 32 foot log equation, it needs to reset MINLEN in some
        !--   situation in order to NOT miss 18, 20, 22 foot logs. (YW 06/20/2014)
        if ((voleq(4:5).eq.'f3' .or. voleq(4:5).eq.'F3' .or.&
             &voleq(2:3).eq.'61' .or. voleq(2:3).eq.'62'.or.&
             &voleq(2:3).eq.'32') .and. CTYPE.ne.'V') then
           N16SEG = INT(LMERCH/(MAXLEN+TRIM))
           if (MOD(N16SEG,2).eq.1) MINLEN = 2
        end if

        !--         SUBROUTINE "NUMLOG" WILL DETERMINE THE NUMBER OF
        !--         MERCHANTABLE SEGMENTS IN A GIVEN MERCHANTABLE LENGTH
        !--         OF TREE STEM, ACCORDING TO ONE OF THE DEFINED SEGMENTATION
        !--         RULES IN THE VOLUME ESTIMATOR HANDBOOK FSH ???.
        !Added max number of logs (20) check (20250626)
        if ((LMERCH/(MAXLEN+TRIM)).gt.20) then
           ERRFLAG = 12
           return
        end if
        call NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG)

        if (NUMSEG .gt.20) then
           !               ERRFLAG = 4
           !    Changed ERRFLAG to 12 (YW 11/06/2012)
           ERRFLAG = 12
           do 151, I = 1,15
           VOL(I) = 0.0
           151 continue
           return
        end if
        !--         SUBROUTINE "SEGMNT" WILL DETERMINE THE LENGTH OF EACH
        !--         SEGMENT, GIVEN A MERCHANTABLE LENGTH OF TREE STEM AND
        !--         THE NUMBER OF SEGMENTS IN IT (DETERMINED IN SUBROUTINE
        !--         "NUMLOG").  SEGMENT LENGTHS ARE DETERMINED ACCORDING TO
        !--         ONE OF THE DEFINED SEGMENTATION RULES IN THE VOLUME
        !--         ESTIMATOR HANDBOOK FSH ???.
        !--         NOTE - THE VARIABLE LMERCH WHEN PASSED IS THE TOTAL
        !--           MERCH LENGTH, BUT IS RETURNED AS THE MERCH LENGTH
        !--           WITH TRIM AND UNUSABLE PORTION OF THE TOP SUBTRACTED.

        call SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG,&
             &LOGLEN)
        ! 2020/02/19 changed to use the variable MINLENT instead of MINLEN
        ! The change made on 2020/02/19 has problem. Reverse change
        !           CALL SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLENT,TRIM,NUMSEG,
        !     >               LOGLEN)

        !--         SET NOLOGP TO THE NUMBER OF 16 FOOT SEGMENTS
        !           NOLOGP = LMERCH/16.0
        NOLOGP = NUMSEG
        ! ENDIF FOR VARIABLE LOG LENGTH CHECK
     end if

     !--     SUBROUTINE "GETDIB" IS INTERNAL AND USES PROFILE MODEL
     call GETDIB(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,MTOPP,&
          &STUMP,TRIM,DBTBH,LOGST,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,&
          &PINV_Z,TOP6,NUMSEG,LOGLEN,LOGDIA,BOLHT,errflag)

     !--     TREE BOLE HAS BEEN MERCHANDISED FOR PRIMARY PRODUCT
  end if

  !     GET BOARD FOOT PRODUCT VOLUMES          *

  !--   DETERMINE IF A BOARD FOOT PRODUCT REPRESTNTATION IS NEEDED

  if (BFPFLG.eq.1.and. DBHOB.ge.MINBFD) then

     do I = 1,NUMSEG
        DIB = LOGDIA(I+1,1)
        LENGTH = LOGLEN(I)
        ! removed scribner check for small diameter logs Jan, 98
        call SCRIB (DIB,LENGTH,COR,LOGV)

        if (cor.eq.'Y') then
           LOGVOL(1,I) = LOGV * 10
        else
           LOGVOL(1,I) = ANINT(LOGV)
        end if
        VOL(2) = VOL(2) + LOGVOL(1,I)

        !          WRITE(*,*)" 16FT ",NUMSEG,I,LOGV
        !-- GET INTERNATIONAL 1/4 VOLUME FOR THE PIECES

        call INTL14(DIB,LENGTH,BFINT)
        VOL(10) = VOL(10) + BFINT
        LOGVOL(7,I) = BFINT

        !-- END OF INTERNATIONAL 1/4 SECTION

        250 continue
     end do

     !-- CONVERT FROM SCRIBNER DECIMAL C TO SCRIBNER
     !      if 32 foot volume is asked for do the following

     !     find volume for 32 foot logs (Flewelling & Demars equation only)

     if ((voleq(4:5).eq.'f3' .or. voleq(4:5).eq.'F3' .or.&
          &voleq(2:3).eq.'61' .or. voleq(2:3).eq.'62'.or.&
          &voleq(2:3).eq.'32') .and. CTYPE.ne.'V') then
        LCNT = 0
        do I = 2, NUMSEG,2
           !           truncate diameter for 32foot board 6/2001
           LCNT = LCNT + 1
           DIB = INT (LOGDIA(I+1,2))
           LENGTH = LOGLEN(I) + loglen(I-1)
           call SCRIB (DIB,LENGTH,COR,LOGV)
           LOGVOL32(LCNT) = LOGV
           LVOL32(1,LCNT) = LOGV*10
           LDIA32(LCNT,1) = DIB
           LDIA32(LCNT,2) = LOGDIA(I+1,2)
           LLEN32(LCNT) = LENGTH
           260 continue
        end do
        !    check for a 16 foot top log
        if (((NUMSEG+1)/2) .gt. (NUMSEG/2)) then
           LCNT = LCNT + 1
           DIB = INT(LOGDIA(NUMSEG+1,2))
           LENGTH = LOGLEN(NUMSEG)
           if (REGN.eq.7.and. LENGTH.lt.16) then
              LOGV = 0
           else
              call SCRIB (DIB,LENGTH,COR,LOGV)
           end if
           LOGVOL32(LCNT) = LOGV
           LVOL32(1,LCNT) = LOGV*10
           LDIA32(LCNT,1) = DIB
           LDIA32(LCNT,2) = LOGDIA(NUMSEG+1,2)
           LLEN32(LCNT) = LENGTH
        end if

        if (REGN.eq.10.and. HTLOG.eq.32) then
           do I = 1,NUMSEG
              LOGVOL(1,I) = 0
              263 continue
           end do
           NLCNT = (NUMSEG + 1) / 2
           do I = 1,NLCNT
              LOGVOL(1,I) = LVOL32(1,I)
              264 continue
           end do
        else
           !    prorate 32 volumes into 16 foot pieces
           LCNT = 0
           do 261, i = 2,numseg,2
           LCNT = LCNT + 1
           if (regn .eq.7) then
              logvol(1,I-1) = ANINT(logvol32(lcnt)/2.0)
              logvol(1,I) = logvol32(lcnt) - logvol(1,i-1)
           else
              topv16 = logvol(1,i)
              botv16 = logvol(1,i-1)
              R = TOPV16 / (TOPV16 + BOTV16)
              if (R .eq.0.5) then
                 logvol(1,I) = INT(logvol32(LCNT) * R)
              else
                 logvol(1,I) = ANINT(logvol32(LCNT) * R)
              end if
              if (LOGVOL(1,I) .le.0) LOGVOL(1,I) = 1
              logvol(1,I-1) = ANINT(logvol32(LCNT) - logvol(1,i))
              if (cor.eq.'Y') then
                 logvol(1,I-1) = logvol(1,I-1) * 10
                 logvol(1,I) = logvol(1,I) * 10
              end if
           end if
           261 continue
           !     check for top log
           if (((NUMSEG+1)/2) .gt. (NUMSEG/2)) then
              LCNT = LCNT + 1
              LOGVOL(1,NUMSEG) = LOGVOL32(LCNT)
              if (cor.eq.'Y') LOGVOL(1,NUMSEG) = LOGVOL(1,NUMSEG)*10
           end if
        end if
        VOL(2) = 0
        do 265, i = 1,numseg
        VOL(2) = VOL(2) + LOGVOL(1,I)
        !            WRITE(*,*)" 32FT ",NUMSEG,I,LOGVOL(1,I)
        265 continue
     end if
     !************** 32 foot logs endif ******************************

     !----- REGION 1 BYRNE BOARD FOOT LOGIC (bdft to cuft rato)
     if (VOLEQ(4:4) .eq. 'J' .or. VOLEQ(4:4) .eq. 'j') then
        if (VOLEQ(8:10) .ne. '108') then
           call VOLRATIO (VOLEQ,FORST,DBHOB,HTTOT,MTOPP,LOGDIA,&
                &LOGLEN,NUMSEG,TCVOL,VMER)
           TEMPVOL = VOL(2)
           VOL(2) = VMER*(TEMPVOL/TCVOL)
           TEMPVOL = VOL(10)
           VOL(10) = VMER*(TEMPVOL/TCVOL)
        end if
     end if

     if (VOL(10) .lt.0.0) then
        VOL(10) = 0.0
     end if
     if (VOL(2) .lt.0.0) then
        VOL(2) = 0.0
     end if
     !--   (ENDIF FOR BOARD FOOT EQUATIONS)
  end if
  !-------END OF BOARD FOOT SECTION

  !*      CUBIC FOOT MAIN STEM EQUATIONS  ALL TREES             *

  !--   DETERMINE IF A CUBIC FOOT PRODUCT REPRESTNTATION IS NEEDED
  if (CUPFLG .eq.1 ) then

     !--   USE DIB AT DBHOB FOR LARGE END BUTT LOG
     DIBL = LOGDIA(1,1)
     do I = 1,NUMSEG
        DIBS = LOGDIA(I+1,1)
        LENGTH = LOGLEN(I)
        LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH

        LOGVOL(4,I) = ANINT(LOGV*10.0)/10.0

        VOL(4) = VOL(4) + LOGVOL(4,I)

        DIBL = DIBS
        350 continue
     end do

     !     CHECK FOR 32 FOOT LOGS
     if (REGN.eq.10.and. HTLOG.eq.32.and. (voleq(4:5).eq.'f3' .or.&
          &voleq(4:5).eq.'F3' .or. voleq(2:3).eq.'61' .or.&
          &voleq(2:3).eq.'62' .or. voleq(2:3).eq.'32')) then
        LCNT = 0
        do I = 2, NUMSEG,2
           LCNT = LCNT + 1
           LVOL32(2,LCNT) = LOGVOL(4,I) + LOGVOL(4,I-1)
           !           if boardfoot flag is zero, need to compute the following
           if (BFPFLG.eq.0) then
              DIB = INT (LOGDIA(I+1,2))
              LENGTH = LOGLEN(I) + loglen(I-1)
              LDIA32(LCNT,1) = DIB
              LDIA32(LCNT,2) = LOGDIA(I+1,2)
              LLEN32(LCNT) = LENGTH
           end if
           351 continue
        end do
        !    check for a 16 foot top log
        if (((NUMSEG+1)/2) .gt. (NUMSEG/2)) then
           LCNT = LCNT + 1
           LVOL32(2,LCNT) = LOGVOL(4,NUMSEG)
           if (BFPFLG.eq.0) then
              LDIA32(LCNT,1) = INT(LOGDIA(NUMSEG+1,1))
              LDIA32(LCNT,2) = LOGDIA(NUMSEG+1,2)
              LLEN32(LCNT) = LOGLEN(NUMSEG)
           end if
        end if
     end if

     !----- REGION 1 BYRNE CUBIC FOOT LOGIC
     if (VOLEQ(4:4) .eq. 'J' .or. VOLEQ(4:4) .eq. 'j') then
        if (VOLEQ(8:10) .ne. '108') then
           MFLG = 1
           call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
                &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,&
                &HT2,MTOPP,MFLG,CUVOL,DIB,DOB,errflag)

           VOL(4) = CUVOL
        end if
     end if
     if (VOL(4) .lt.0.0) then
        VOL(4) = 0.0
     end if
     !--  (ENDIF FOR CUBIC FOOT MAIN STEM EQUATIONS)
  end if

  !     RETURN 32 FOOT LOGS
  if (REGN.eq.10.and. HTLOG.eq.32.and. (voleq(4:5).eq.'f3' .or.&
       &voleq(4:5).eq.'F3' .or. voleq(2:3).eq.'61' .or.&
       &voleq(2:3).eq.'62' .or. voleq(2:3).eq.'32')) then
     do I = 1,NUMSEG
        LOGVOL(1,I) = 0
        LOGVOL(4,I) = 0
        LOGDIA(I+1,1) = 0
        LOGDIA(I+1,2) = 0
        LOGDIA(I+1,3) = 0
        LOGLEN(I) = 0
        352 continue
     end do
     NLCNT = (NUMSEG + 1) / 2
     do J = 1,NLCNT
        LOGVOL(1,J) = LVOL32(1,J)
        LOGVOL(4,J) = LVOL32(2,J)
        LOGLEN(J) = LLEN32(J)
        LOGDIA(J+1,1) = LDIA32(J,1)
        LOGDIA(J+1,2) = LDIA32(J,2)
        353 continue
     end do
     NUMSEG = NLCNT
     NOLOGP = NLCNT
  end if

  !*      CORD WOOD MAIN STEM EQUATIONS  ALL TREES             *

  if (CDPFLG .eq.1) then
     !--   PUT LOGIC HERE FOR CORDS  (SOURCE:  DON MARTINEZ)
     VOL(6) = ANINT((VOL(4)/90.0)*10)/10.0
  end if

  !--   SET LOGST TO NUMBER OF SEGMENTS MAIN STEM

  LOGST = NUMSEG

  !--   SET COMMON VARIABLE LOGS TO LOGST TO BE USED IN LOG STOCK FILE
  !      LOGS = LOGST

  !--  DETERMINE SECONDARY PRODUCTS (TOP WOOD) ALL TREES          *

  !--   MERCHANDISE THE UPPER TREE BOLE FOR SECONDARY PRODUCTS

  500 if (SPFLG.eq.1.and. MFLAG.eq.0) then

     !--     LENMS IS THE LENGTH OF MAIN STEM FOR WHICH A
     !--        PRODUCT WAS DETERMINED.
     if ((REGN.eq.5.or.REGN.eq.3) .and. PROD.eq.'07') then
        LENMS = LMERCH
     else
        LENMS = 0.0
        do I = 1,NUMSEG
           !32 FOOT LOGS CONTAIN TWICE THE TRIM
           if (LOGLEN(I) .ge.32) then
              LENMS = LENMS + TRIM + TRIM + LOGLEN(I)
           else
              LENMS = LENMS+TRIM+LOGLEN(I)
           end if
           400 continue
        end do
     end if

     if (HT2PRD.gt.0) then
        if (HT2PRD .lt. LENMS) then
           VOL(7) = 0
           goto 1000
        end if
        if (HTFLG.eq.0) then
           call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,&
                &HTTOT,DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,&
                &PINV_Z,TOP6,HT2PRD,MTOPS,MFLG,CUVOL,DIB,DOB,errflag)

           MTOPS = DIB
        end if
        LMERCH = HT2PRD - LENMS
     else
        !--     SUBROUTINE "MERLEN" IS INTERNAL AND USES PROFILE MODEL
        call MERLEN(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,DBTBH,&
             &HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,STUMP,&
             &MTOPS,LMERCH,errflag)

        !--     LMERCH IS CHANGED TO BE THE MERCH LENGTH OF THE
        !--       TOP WOOD PORTION ONLY.  THERE MIGHT BE A SMALL
        !--       DIFFERENCE IN TOP WOOD LENGTH DEPENDING ON THE
        !--       SEGMENTATION LOGIC USED FOR THE MAIN STEM.
        !--   CHECK FOR REGION 5 FIREWOOD LOGIC
        !       Set the merch height to HT2PRD
        HT2PRD = LMERCH + STUMP

        LMERCH = LMERCH - LENMS
        !--  SEE DEFINITION OF SUBROUTINE "NUMLOG" UNDER BDFT CACULATIONS
     end if

     if ((REGN.eq.5.or.REGN.eq.3) .and. PROD.eq.'07') then
        call FIREWOOD (VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
             &LMERCH,DBTBH,MTOPS,LENMS,HEX,DEX,ZEX,RHFW,RFLW,&
             &TAPCOE,F,FMOD,PINV_Z,TOP6,TCVOL,slope,errflag)
        VOL(7) = TCVOL
        !--   Added cordwood calculation (YW 2019/12/20)
        if (CDPFLG .eq.1) then
           VOL(9) = ANINT((VOL(7)/90.0)*10)/10.0
        end if
        go to 1000
     end if

     !       check for mainstem piece, if not, topwood must meet minimum merch length
     if (LENMS.le.0) then
        if (LMERCH.lt.MERCHL) then
           go to 1000
        end if
     end if

     !        CALL NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLENT,TRIM,NUMSEG)
     !  2020/02/19 changed to use the variable MINLEN instead of MINLENT
     !  Reversed the change on 2020/02/19. Using MINLEN instead of MINLENT is not correct for topwood (20260303)
     !Added max number of logs (20) check (20250626)
     if ((LMERCH/(MAXLEN+TRIM)).gt.20) then
        ERRFLAG = 12
        return
     end if
     call NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLENT,TRIM,NUMSEG)

     if ((NUMSEG + LOGST) .gt.20) then
        do 401, I = 1,15
        VOL(I) = 0.0
        401 continue
        !           ERRFLAG = 4
        ! changed errflag to 12 (YW 11/06/2012)
        ERRFLAG = 12
        return
     end if

     !--  SEE DEFINITION OF SUBROUTINE "SEGMNT" UNDER BDFT CACULATIONS

     call SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLENT,TRIM,NUMSEG,LOGLENG)
     do I = 1, NUMSEG
        LOGLEN(I+LOGST) = LOGLENG (I)
        405 continue
     end do

     !--  SET NOLOGS TO NUMBER OF 16 FOOT PIECES

     NOLOGS = LMERCH/16.0

     !--   SUBROUTINE "GETDIB" IS INTERNAL AND USES PROFILE MODEL

     LENMS = LENMS + STUMP
     call GETDIB(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,MTOPS,&
          &LENMS,TRIM,DBTBH,LOGST,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,&
          &PINV_Z,TOP6,NUMSEG,LOGLEN,LOGDIA,BOLHT,errflag)

     !--  UPPER TREE BOLE HAS BE MERCHANDISED FOR SECONDARY PRODUCT

     !       GET CUBIC VOLUME FOR THE PIECES SMALIAN FORMULA     *

     DIBL = LOGDIA(1 + LOGST,1)

     do I = 1 + LOGST, NUMSEG + LOGST
        DIBS = LOGDIA(I+1,1)
        LENGTH = LOGLEN(I)
        if (CUPFLG.eq.1) then
           LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH

           LOGVOL(4,I) = ANINT(LOGV*10.0)/10.0

           VOL(7) = VOL(7)+LOGVOL(4,I)
        end if
        if (BFPFLG.eq.1) then
           call SCRIB (DIBS,LENGTH,COR,LOGV)
           if (cor.eq.'Y') then
              LOGVOL(1,I) = LOGV * 10
           else
              LOGVOL(1,I) = ANINT(LOGV)
           end if
           VOL(12) = VOL(12) + LOGVOL(1,I)
        end if
        !           LOGGRS(I,3) = LOGV
        DIBL = DIBS
        450 continue
     end do
     !----- REGION 1 BYRNE CUBIC FOOT LOGIC
     if (VOLEQ(4:4).eq.'J' .or. VOLEQ(4:4).eq.'j') then
        if (CUPFLG.eq.1) then
           if (VOLEQ(8:10) .ne. '108') then
              MFLG = 1
              call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,&
                   &HTTOT,DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,&
                   &PINV_Z,TOP6,HT2,MTOPS,MFLG,CUVOL,DIB,DOB,errflag)

              VOL(7) = CUVOL - VOL(4)
           end if
        end if
        if (BFPFLG.eq.1) then
           !--            REGION 1 BYRNE BOARD FOOT LOGIC (bdft to cuft rato)
           if (VOLEQ(8:10) .ne. '108') then
              call VOLRATIO (VOLEQ,FORST,DBHOB,HTTOT,MTOPP,LOGDIA,&
                   &LOGLEN,LOGST,TCVOL1,VMER1)
              TLOGS = LOGST + NUMSEG
              call VOLRATIO (VOLEQ,FORST,DBHOB,HTTOT,MTOPS,LOGDIA,&
                   &LOGLEN,TLOGS,TCVOL2,VMER2)
              VMER = VMER2 - VMER1
              TCVOL = TCVOL2 - TCVOL1
              TEMPVOL = VOL(12)
              VOL(12) = VMER*(TEMPVOL/TCVOL)
           end if
        end if
     end if

     if (VOL(7) .lt.0.0) VOL(7) = 0.0
     if (VOL(12).lt.0.0) VOL(12) = 0.0
     !--   PUT LOGIC HERE FOR CORDS  (SOURCE:  DON MARTINEZ)
     if (CDPFLG.eq.1.and. CUPFLG.eq.1) then
        VOL(9) = ANINT((VOL(7)/90.0)*10)/10.0
     end if
     !     UPDATE TOTAL LOGS
     LOGST = LOGST + NUMSEG
     !ENDIF FOR TOP WOOD EQUATIONS
  end if

  1000 continue

  return
end

  !##################################################################
  !##################################################################
  !   THE LOGIC BETWEEN THE DOUBLE ROWS OF "#" MUST BE DUPLICATED
  !   IN EACH REGIONAL VOLUME ROUTINE THAT USES FLEWELLING'S PROFILE
  !   MODELS.  ALL CALLS ARE PRECEDED BY A SHORT ROW OF "#".
  !     THERE ARE FOUR INTERNAL SUBROUTINES IN THIS SECTION.
  !     THESE INTERNAL SUBROUTINES CONTAIN ALL OF THE CALLS TO
  !     FLEWELLINGS STEM PROFILE ROUTINES.
  !     SUBROUTINE "TCUBIC" WILL COMPUTE TOTAL CUBIC VOLUME.
  !     SUBROUTINE "MERLEN" WILL FIND THE MERCHANTABLE LENGTH TO
  !         A SPECIFIED TOP.
  !     SUBROUTINE "GETDIB" WILL RETURN THE END DIAMETERS OF LOGS
  !         ONCE SEGMENTATION HAS OCCURED.

subroutine TCUBIC(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,DBTBH,&
     &MTOPP,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,TCVOL,&
     &slope,errflag, VOL, MTOPS)

  !--   DETERMINE TOTAL CUBIC FIBER CONTENT.  THE VOLUME WILL BE
  !--   DETERMINED BY USING THE VOLUME OF A CYLINDER WITH A DIAMETER
  !--   OF DIB AT 1 FOOT FOR THE STUMP VOLUME AND USING THE SMALIAN
  !--   FORMULA TO COMPUTE VOLUMES FOR 4 FOOT SECTIONS FROM THE STUMP
  !--   TO THE TIP.

  !--   VARIABLES OF INTEREST ARE:
  !--   HTTOT - REAL - **TOTAL TREE HT INCLUDING THE STUMP.**
  !--   TCVOL - REAL - TOTAL VOLUME FOR THE TREE IN CUBIC FOOT,
  !--                  DOES NOT INCLUDE THE LIMBS OR ROOTS.

  ! Variables
  character(len=2) :: FORST
  character(len=10) :: VOLEQ
  integer :: HTLOOP,JSP,NEXTRA,SETOPT(6),errflag,I,MFLG
  real    :: D2OLD,HTTOT,HT2,TCVOL,VOLTMP,MTOPP,DBTBH
  real    :: RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
  real    :: DBHOB,HEX(2),ZEX(2),TOP6,DEX(2),slope
  real    :: DIB,DOB,R,CUVOL
  real    :: TIPV,TIPL,TIPTMP,VOL(15)
  real    :: MTOPS,TIPD

  VOL = 0.

  TIPD = MTOPS
  if (TIPD.gt.4.0.or. TIPD.le.0.0) TIPD = 4.0
  TIPV = 0.0
  TIPTMP = 0.0
  !      geosub=voleq(2:3)
  HTLOOP = INT((HTTOT + .5 - 1.0)/4.0)
  HT2 = 1.0
  !##########
  MFLG = 0
  if (JSP.gt.0.and. HTTOT.le.15) then
     !                  SMALL TREE FIX FOR FLEWELLING STUMP DIAMETERS
     call FWSMALL(JSP,NEXTRA,SETOPT,slope,DBHOB,HTTOT,DBTBH,&
          &HEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,DIB,errflag)
  else

     call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
          &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,&
          &MTOPP,MFLG,CUVOL,DIB,DOB,errflag)

     if (VOLEQ(4:6).eq.'JB2' .or. VOLEQ(4:6).eq.'jb2') then
        if (VOLEQ(8:10) .eq. '108') then
           TCVOL = 0.005454154 * CUVOL * DBHOB * DBHOB * HTTOT
        else
           TCVOL = CUVOL
        end if
        return
     end if
  end if
  !--   STUMP VOLUME IS VOLUME FOR A 1 FT HIGH CYLINDER
  R = DIB/2.0
  TCVOL = (3.1416*R*R)/144.0
  !     Save stump vol to VOL(14) (YW 2015/08/19)
  !      VOL(14) = TCVOL
  dex(2) = dib
  do I = 1,HTLOOP
     D2OLD = DIB
     HT2 = HT2+4.0
     !##########
     call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
          &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,&
          &MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
     !       WRITE(*,*) DBHOB, HTTOT, HT2, DIB

     !--     USE SMALIAN FOR NON STUMP SEGMENTS
     VOLTMP = .00272708*(D2OLD*D2OLD+DIB*DIB)*4.0
     !       Check if DIB < 4 for tip volume
     if (D2OLD.gt.TIPD .and. DIB.lt.TIPD) then
        TIPL = 4.0-(D2OLD*D2OLD-TIPD*TIPD)/(D2OLD*D2OLD-DIB*DIB)*4.0
        TIPTMP = .00272708*(TIPD*TIPD+DIB*DIB)*TIPL
     else if (D2OLD.le.TIPD) then
        TIPTMP = VOLTMP
     end if
     TCVOL = TCVOL + VOLTMP
     TIPV = TIPV + TIPTMP
     10 continue
  end do
  !--   USE SMALIAN FOR TIP, WITH A TIP DIAMETER OF 0.0
  if ( (HTTOT-HT2) .gt.0.0 ) then
     VOLTMP = .00272708*(DIB*DIB)*(HTTOT-HT2)
     TCVOL = TCVOL + VOLTMP
     TIPV = TIPV + VOLTMP
  end if
  !      VOL(15) = TIPV
  return
end

subroutine MERLEN(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,DBTBH,&
     &HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,&
     &STUMP,TOP,LMERCH,errflag)

  !--   SUBROUTINE WILL DETERMINE WHAT THE MERCHANTABLE LENGTH OF
  !--   STEM FROM THE STUMP TO A SPECIFIED TOP, INCLUDING TRIM.
  !--   NO ROUNDING IS USED WHEN FINDING THE MINIMUM TOP DIAMETER.
  !--   IF THE SPECIFIED MINIMUM TOP IS SET TO 6" THEN 6" DIB
  !--   WITHOUT ROUNDING IS USED AS THE CUT-OFF POINT.

  ! Variables
  character(len=2) :: geosub
  character(len=2) :: FORST
  character(len=10) :: VOLEQ
  integer :: JSP,NEXTRA,SETOPT(6),half,first,errflag
  integer :: LAST,TOPLOP,I,MFLG
  real    :: LMERCH,STUMP,TOP,DBHOB,HTTOT,DBTBH,F
  real    :: DS,HS,DEX(2),TOP6,HT2,CVOL
  real    :: HEX(2),ZEX(2),RHFW(4),RFLW(6),TAPCOE(12),FMOD(3),PINV_Z(2)
  real    :: TOP1,CUVOL,DIB,DOB
  geosub = voleq(2:3)

  !--   SET TOP1 EQUAL TO DESIRED TOP
  DS = TOP
  HS = 0
  if (VOLEQ(4:4).eq.'F' .or. VOLEQ(4:4).eq.'f') then
     call SF_HS(JSP,geosub,NEXTRA,SETOPT,DBHOB,HTTOT,DBTBH,HEX,&
          &ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,DS,HS)
     LMERCH = HS - STUMP

  else if (VOLEQ(4:4).eq.'J' .or. VOLEQ(4:4).eq.'j') then
     call R1TAP('100JB2W108',FORST,DBHOB,HTTOT,TOP,HS,1,CVOL,DS)
     LMERCH = HS - STUMP

  else
     !--   SET TOP1 EQUAL TO DESIRED TOP TIMES 10 TRUNCATED
     TOP1 = ANINT((DS +.005)*10.00)

     FIRST = 1
     !--   FIND DESIRED TOP TO THE NEAREST TENTH OF A FOOT
     LAST = INT(HTTOT+.5)*10
     TOPLOP = LAST

     do I = 1,TOPLOP
        if (FIRST .eq. LAST) then
           go to 100
        end if
        HALF = (FIRST + LAST + 1 ) / 2
        HT2 = FLOAT(HALF)/10.0

        !##########
        call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
             &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,&
             &TOP,MFLG,CUVOL,DIB,DOB,errflag)

        !--     CONVERT TOP DIAMETER TO TENTH INCH TRUNCATED.
        !--     BEFORE TRUNCATION ADD .005 TO ROUND UP THE DIAMETERS
        !--     THAT ARE CLOSE - 5.995 AND ABOVE WILL BE CONVERTED TO 6

        DIB = INT((DIB+.005)*10.0)
        if (TOP1.le. DIB) then
           !--          MOVE UP STEM
           FIRST = HALF
        else
           !--        MOVE DOWN THE STEM
           LAST = HALF - 1
        end if
        10 continue
     end do

     100 continue

     LMERCH = FLOAT(FIRST)/10.0 - STUMP

  end if

  if (LMERCH .lt.0.0) LMERCH = 0.0

  return
end

subroutine GETDIB(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,MTOPP,&
     &STUMP,TRIM,DBTBH,LOGST,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,&
     &PINV_Z,TOP6,NUMSEG,LOGLEN,LOGDIA,BOLHT,errflag)

  !--   ALL LOG DIAMETERS ARE ROUNDED TO THE NEAREST 1 INCH
  !--   CLASS (I.E. 8" CLASS = 7.6 - 8.5)
  !--   LOGDIA(*,1) - REAL - SCALED DIAMETER
  !--   LOGDIA(*,2) - REAL - DIAMETER INSIDE BARK
  !--   LOGDIA(*,3) - REAL - DIAMETER OUTSIDE BARK
  !--   BOLHT(*)   - REAL - HEIGHT UP STEM WHERE LOGDIA WAS PREDICTED

  use DEBUG_MOD

  ! Variables
  character(len=2) :: FORST
  character(len=10) :: VOLEQ
  integer :: NUMSEG,LOGST,JSP,NEXTRA,SETOPT(6)
  integer :: errflag,MFLG,I,DIBCLS
  real    :: MTOPP,HT2,LOGLEN(20),BOLHT(21)
  real    :: LOGDIA(21,3),STUMP,TRIM,CUVOL
  real    :: DBHOB,HEX(2),DEX(2),ZEX(2),PINV_Z(2),FMOD(3),F,TOP6
  real    :: RHFW(4),RFLW(6),TAPCOE(12)
  real    :: HTTOT,DBTBH,DIB,DOB,XXX
  !      geosub=voleq(2:3)

  !debug info
  if (DEBUG%MODEL) then
     write  (LUDBG, 2) ' -->Enter GETDIB (profile.f)'
     2 format (A)

     WRITE  (LUDBG, 5)'DBHOB HTTOT  MTOPP  STUMP  NUMSEG            &
          &    TRIM  LOGST'
     5 format (A)
     write  (LUDBG, 66)DBHOB,HTTOT,MTOPP,STUMP,NUMSEG,TRIM,&
          &LOGST
     66 format(F5.1, 1X, F5.1, 2X, F5.1, 2X, F5.1, 2X, I4, 4X,&
          &F5.2, 4X, I6)

     write  (LUDBG, 76)'LOGDIA(2,2) ', LOGDIA(2,2)
     76 format (A, 2x, F8.4)
  end if

  MFLG = 2
  if (NUMSEG .eq.0 ) go to 1000

  if (STUMP .gt.4.5 ) then
     !--     USE FOR TOP WOOD
     !###########
     !  TDH commented out this code on 3/8/2010 as it was causing the
     !  following problem - when topwood is calcuated the large end of
     !  the 1st topwood log is the same as the small end of the last
     !  main stem log.  The code below would reset this value and the
     !  rounding check (if less than MTOPP) would not apply.

     !          CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     !     >      DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,STUMP,
     !     >      MTOPP,MFLG,CUVOL,DIB,DOB,errflag)

     !         LOGDIA(1+LOGST,2)= DIB
     !         LOGDIA(1+LOGST,3)= DOB
     !         BOLHT(1+LOGST) = STUMP

     if (DEBUG%MODEL) then

        write  (LUDBG, 96)'LOGDIA(5,2) ', LOGDIA(5,2)
        96 format (A, 2x, F8.4)

     end if
  else

     !--     USE FOR MAIN STEM
     !###########
     ht2 = 4.5
     !     find DBHOB inside bark
     call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
          &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,ht2,&
          &MTOPP,MFLG,CUVOL,DIB,DOB,errflag)

     LOGDIA(1,2) = DIB
     LOGDIA(1,3) = DOB
     BOLHT(1) = 4.5
  end if
  !     Demar stump fix for DBH>36
  if ((VOLEQ(4:6).eq.'DEM'.or.VOLEQ(4:6).eq.'CUR').and.&
       &DBHOB.gt.36) then
     STUMP = DBHOB/36
  end if
  HT2 = STUMP
  do I = 1+LOGST,NUMSEG+LOGST
     HT2 = HT2+TRIM+LOGLEN(I)
     call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
          &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,&
          &MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
     LOGDIA(I+1,2) = DIB
     LOGDIA(I+1,3) = DOB
     BOLHT(I+1) = HT2
     20 continue
  end do

  !--  SMALL END DIAMETER OF TOP LOG MIGHT BE LESS THAN
  !--  THE MINIMUM SPECIFIED TOP DUE TO ROUNDING OF LOG LENGTHS
  !--  IF THIS IS TRUE, THEN FORCE TOP DIAMETER EQUAL TO
  !--  MINIMUM SPECIFIED TOP.

  if (DEBUG%MODEL) then

     write  (LUDBG, 25)'LOGDIA(NUMSEG+LOGST+1,2)  MTOPP '
     25 format (A)

     write  (LUDBG, 26)LOGDIA(NUMSEG+LOGST+1,2), MTOPP
     26 format(F5.1, 1X, F5.1)

  end if

  if ( LOGDIA(NUMSEG+LOGST+1,2) .lt. MTOPP) then
     LOGDIA(NUMSEG + LOGST + 1,2) = MTOPP
  end if
  !--   NEED TO PUT LOG DIB IN PROPER DIAMETER CLASS
  do I = 1+LOGST,NUMSEG+LOGST+1
     DIBCLS = INT(LOGDIA(I,2))
     XXX = LOGDIA(I,2)-DIBCLS
     !          IF( XXX .GT. 0.55) DIBCLS=DIBCLS+1
     if ( XXX .gt.0.501) DIBCLS = DIBCLS+1
     LOGDIA(I,1) = DIBCLS
     30 continue
  end do

  !--  SMALL END DIAMETER OF TOP LOG MIGHT BE LESS THAN
  !--  THE MINIMUM SPECIFIED TOP DUE TO ROUNDING OF LOG LENGTHS
  !--  IF THIS IS TRUE, THEN FORCE TOP DIAMETER EQUAL TO
  !--  MINIMUM SPECIFIED TOP.

  !  TDH - why is this code here and also above???
  if ( LOGDIA(NUMSEG+LOGST+1,2) .lt. MTOPP) then
     LOGDIA(NUMSEG + LOGST + 1,2) = MTOPP
  end if

  !--   NEED TO CHECK TOP DIB
  DIBCLS = INT(LOGDIA(NUMSEG+LOGST+1,2))
  XXX = LOGDIA(NUMSEG+LOGST+1,2)-DIBCLS
  !       IF( XXX .GT. 0.55) DIBCLS=DIBCLS+1
  if ( XXX .gt.0.501) DIBCLS = DIBCLS+1
  LOGDIA(NUMSEG+LOGST+1,1) = DIBCLS

  if (DEBUG%MODEL) then

     write  (LUDBG, 600)'LOGDIA(5,2) ', LOGDIA(5,2)
     600 format (A, 2x, F8.4)

  end if

  1000 continue

  !debug info
  if (DEBUG%MODEL) then
     write  (LUDBG, 1100) ' <--Exit GETDIB (profile.f)'
     1100 format (A)
  end if

  return
end

subroutine TOP6LEN(VOLEQ,HTTOT,DBHOB,DEX,HEX,STUMP,TOP,TOP6,&
     &DBTBH,errflag)

  !--   FIX FOR CZAPLEWSKI 3 POINT MODEL.  DETERMINES THE HEIGHT TO A
  !--   6-INCH TOP.

  !      CHARACTER*3 SPEC
  ! Variables
  integer :: I,FIRST,HALF,LAST,TOPLOP
  real    :: HTTOT,STUMP,TOP
  character(len=2) :: FORST
  character(len=10) :: VOLEQ
  integer :: JSP,NEXTRA,SETOPT(6)
  integer :: errflag,MFLG
  real    :: DIB,DOB,HT2,DBTBH
  real    :: CUVOL,TOP1
  real    :: DBHOB,HEX(2),DEX(2),ZEX(2),PINV_Z(2),FMOD(3),F,TOP6
  real    :: RHFW(4),RFLW(6),TAPCOE(12)
  !      SPEC = VOLEQ(8:10)

  TOP6 = 0.0

  if (DEX(1).lt.6.0) then
     TOP6 = HEX(1)
     go to 500
  end if

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
     HT2 = FLOAT(HALF)/10.0

     !###########
     call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
          &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,&
          &TOP,MFLG,CUVOL,DIB,DOB,errflag)

     !--     CONVERT TOP DIAMETER TO TENTH INCH TRUNCATED.
     !--     BEFORE TRUNCATION ADD .005 TO ROUND UP THE DIAMETERS
     !--     THAT ARE CLOSE - 5.995 AND ABOVE WILL BE CONVERTED TO 6

     DIB = INT((DIB+.005)*10.0)
     if (TOP1.le. DIB) then
        !--        MOVE UP STEM
        FIRST = HALF
     else
        !--        MOVE DOWN THE STEM
        LAST = HALF - 1
     end if
     10 continue
  end do

  100 continue

  TOP6 = FLOAT(FIRST)/10.0 - STUMP
  500 if (TOP6.lt.0.0) TOP6 = 0.0

  return
end

subroutine VOLRATIO(VOLEQ,FORST,DBHOB,HTTOT,TOPD,LOGDIB,LOGLEN,&
     &NUMSEG,CUBV,VMER)

  !--   SUBROUTINE WILL DETERMINE CALCULATE SMALIANS CUBIC VOLUME
  !--   FOR LODGEPOLE AND THE CUBIC VOLUME FOR THE SPECIES OF
  !--   INTEREST.  BOARD FOOT FOR SPECIES OF INTEREST IS CALCULATED
  !--   USING A BDFT TO CUBIC FOOT RATIO BASED ON LODGEPOLE.
  ! Variables
  character(len=2) :: FORST
  character(len=10) :: VOLEQ
  integer :: NUMSEG,I
  real    :: DBHOB,HTTOT,LOGV,HTUP,TOPD,LENGTH
  real    :: LOGDIB(21,3),LOGLEN(20),CUBV,VMER,CVOL,DIB,DIBL,DIBS

  CUBV = 0.0
  HTUP = 0.0

  call R1TAP(VOLEQ,FORST,DBHOB,HTTOT,TOPD,HTUP,1,CVOL,DIB)

  VMER = CVOL

  DIBL = LOGDIB(1,1)

  do I = 1,NUMSEG
     DIBS = LOGDIB(I+1,1)
     LENGTH = LOGLEN(I)
     LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH
     CUBV = CUBV + LOGV
     DIBL = DIBS
     10 continue
  end do

  return
end

subroutine TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
     &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,&
     &MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
  ! calls taper equations or profile models.
  use VOLINPUT_MOD
  implicit none

  ! Variables
  character(len=2) :: geosub
  character(len=2) :: FORST
  character(len=10) :: VOLEQ
  integer :: JSP,NEXTRA,SETOPT(6)
  integer :: ineedsl,errflag,MFLG,FCLASS
  real    :: DIB,DOB,HT2,DBTBH,DBT,HTTOT,TLH
  real    :: slope,CUVOL,MTOPP,TOP
  real    :: DBHOB,HEX(2),DEX(2),ZEX(2),PINV_Z(2),FMOD(3),F,TOP6
  real    :: RHFW(4),RFLW(6),TAPCOE(12)
  !Extra variables for R4MATTAPER
  real    :: STUMPD,BUTTCF,CF0,B

  FCLASS = FORMCLASS
  ineedsl = 0
  DOB = 0.0
  geosub = voleq(2:3)
  !###########
  if (VOLEQ(4:4).eq.'F' .or. VOLEQ(4:4).eq.'f') then
     call SF_DS(JSP,GEOSUB,NEXTRA,SETOPT,ineedsl,slope,DBHOB,&
          &HTTOT,HT2,HEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,DIB)
     !         IF(JSP.GE.22 .AND. JSP.LE.30) THEN
     call BRK_UP(JSP,geosub,DBHOB,HTTOT,DBTBH,HT2,DIB,DOB,DBT)
     !         ENDIF
  else if (VOLEQ(4:6).eq.'CZ2' .or. VOLEQ(4:6).eq.'cz2' .or.&
       &VOLEQ(4:6).eq.'CZ3' .or. VOLEQ(4:6).eq.'cz3') then
     call R2TAP(VOLEQ,DBHOB,DEX(1),HEX(1),TOP6,HTTOT,HT2,DIB,DBTBH,&
          &errflag)

  else if (VOLEQ(4:6).eq.'WO2' .or. VOLEQ(4:6).eq.'wo2') then
     call R5TAP(VOLEQ,DBHOB,HTTOT,HT2,DIB,errflag)

  else if (VOLEQ(4:6).eq.'DEM' .or. VOLEQ(4:6).eq.'dem') then
     call R10TAP(VOLEQ,DBHOB,HTTOT,HT2,DIB)

  else if (VOLEQ(4:6).eq.'CUR' .or. VOLEQ(4:6).eq.'cur') then
     call R10TAP(VOLEQ,DBHOB,HTTOT,HT2,DIB)

  else if (VOLEQ(4:6).eq.'JB2' .or. VOLEQ(4:6).eq.'jb2') then
     if (MFLG .eq.0) then
        TOP = 0.0
        HT2 = 0.0
        !   The following function should be called for volume calc.(10/15/2015)!
        call R1TAP(VOLEQ,FORST,DBHOB,HTTOT,TOP,HT2,MFLG,&
             &CUVOL,DIB)

     else if (MFLG .eq.1) then
        call R1TAP(VOLEQ,FORST,DBHOB,HTTOT,MTOPP,HT2,MFLG,&
             &CUVOL,DIB)
     else
        call R1TAP('100JB2W108',FORST,DBHOB,HTTOT,MTOPP,HT2,MFLG,&
             &CUVOL,DIB)
     end if
     ! Added to test for BEH equation in profile(09/12/13 YW)
  else if (VOLEQ(4:6).eq.'BEH' .or. VOLEQ(4:6).eq.'beh') then
     TLH = 0.0
     call BEHTAP(VOLEQ,DBHOB,HTTOT,TLH,HT2,FCLASS,MTOPP,DIB)
     ! Added call R4MATTAPER to enable R4 Forest 17 uses R5 merch rules (20251216)
  else if (VOLEQ(4:6).eq.'MAT' .or. VOLEQ(4:6).eq.'mat') then
     call R4MATTAPER(VOLEQ,DBHOB,HTTOT,STUMPD,BUTTCF,CF0,B,&
          &HT2,DIB,ERRFLAG )
  end if

  return
end

subroutine FWSMALL(JSP,NEXTRA,SETOPT,slope,DBHOB,HTTOT,&
     &DBTBH,HEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,DIB1,errflag)

  !     FIND TOTAL CUBIC VOLUME OF TREES BETWEEN 5 AND 15 FEET TALL
  ! Variables
  character(len=2) :: geosub
  integer :: JSP,NEXTRA,SETOPT(6)
  integer :: ineedsl,errflag
  real    :: HTTOT,DBTBH,SLOPE,DIB,DOB,DBT
  real    :: RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
  real    :: DBHOB,HEX(2),ZEX(2)

  real    :: H1,H5,H15,HR_5,HR_15LO,HR_15HI,HRATIO,HR_MIN,HR_MAX
  real    :: DR_MIN,DR,DIB1
  !              set the parameters of the special modifiers
  errflag = 0
  ineedsl = 0
  H1 = 1.0
  H5 = 5.0
  H15 = 15.0
  HR_5 = .18
  HR_15LO = .11
  HR_15HI = .25
  !           Ponderosa Pine has slightly higher values
  if (JSP .eq.14) then
     HR_5 = .25
     HR_15LO = .21
     HR_15HI = .27
  end if
  !                For the original eqns (published hemlock) only 2 limits allowed
  call SF_DS(JSP,GEOSUB,NEXTRA,SETOPT,ineedsl,slope,DBHOB,&
       &HTTOT,H1,HEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,DIB)
  if (JSP.ge.22.and. JSP.le.30) then
     call BRK_UP(JSP,geosub,DBHOB,HTTOT,DBTBH,H1,DIB,DOB,DBT)
  end if
  !                    Compute and possible modify ratio DIB(1 ft) /(TOTALH -1)
  HRATIO = DIB / ( HTTOT-H1 )
  HR_MIN = HR_5 + (HTTOT - H5)/(H15-H5)* (HR_15LO - HR_5)
  HR_MAX = HR_5 + (HTTOT - H5)/(H15-H5)* (HR_15HI - HR_5)

  if (HRATIO .lt. HR_MIN) HRATIO = HR_MIN
  if (HRATIO .gt. HR_MAX) HRATIO = HR_MAX

  DIB1 = HRATIO *(HTTOT - H1)
  !                 Impose a limit of DOB(1 ft)/DBHob > DR_MIN
  !                      where DR_MIN=1 at totalH=15   , 1.3 at TOTALH = 5

  DR_MIN = 1.0
  if (HTTOT .lt. H15) DR_MIN = DR_MIN + 0.3*(H15-HTTOT)/(H15 - H5)
  DR = DIB1/(DBHOB - DBTBH)

  if (DR .lt. DR_MIN) DIB1 = DR_MIN*(DBHOB - DBTBH)

  return
end

subroutine VOLINTRP(REGN,VOLEQ,DBH,LHT,MHT,MTOPP,HTTYPE,DBTBH,&
     &LOGVOL,LOGDIA,HTLOG,LOGLEN,LOGST,NOLOGP,VOL,CTYPE,PROD,&
     &ERRFLAG)
  !     total height could not be predicted, interpolate the diameters for logs
  !     in the merch piece.
  ! Variables
  character(len=1) :: HTTYPE
  character(len=1) :: COR
  character(len=1) :: CTYPE
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=10) :: VOLEQ
  integer :: LOGST,HTLOG,REGN,EVOD,OPT,I,LCNT,ERRFLAG
  real    :: DBH,LHT,MTOPP,DBTBH,VOL(15),NOLOGP,MHT,HTUP
  real    :: LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3),D2,LEN,MINBFD
  real    :: DBHIB,MAXLEN,MINLEN,MERCHL,MINLENT,MTOPS,STUMP,TRIM,BTR
  real    :: DIBL,DIBS,LENGTH,LOGV,LOGVOL32(20),TOPV16,BOTV16,R,LMERCH

  !  DW 08/22 initializations of variables otherwise uninitialized prior to MRULES call
  OPT = 0
  EVOD = 0
  MINLENT = 0
  BTR = 0

  DBHIB = DBH-DBTBH
  !     check top diameter not greater than DBH inside bark. (YW 2014/07/02)
  if (MTOPP.ge.DBHIB) then
     ERRFLAG = 13
     return
  end if
  !       MRULES IS EXTERNAL AND CONTAINS THE MERCHANDIZING RULES SETTINGS
  call MRULES(REGN,FORST,VOLEQ,DBH,COR,EVOD,OPT,MAXLEN,MINLEN,&
       &MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,&
       &PROD)
  if (HTTYPE.eq.'L' .or. HTTYPE.eq.'l') then
     LOGDIA(1,1) = ANINT(DBHIB)
     LOGDIA(1,2) = DBHIB
     !         IF ONE LOG, SET DIAMETER AND LENGTH
     if (LHT.eq.1) then
        LOGST = 1
        NOLOGP = 1
        LOGDIA(2,1) = ANINT(MTOPP)
        LOGDIA(2,2) = MTOPP
        LOGLEN(1) = HTLOG
     else
        !         IF MULTIPLE LOGS, LOOP THROUGH, FIND DIAMETERS AND LENGTHS
        LOGST = INT (LHT + 0.5)
        NOLOGP = LHT
        LEN = MHT - 4.5
        HTUP = 1.0
        do 100, I = 1,INT(LHT)-1
        LOGLEN(I) = HTLOG
        HTUP = HTUP + HTLOG
        D2 = DBHIB*DBHIB - (DBHIB*DBHIB-MTOPP*MTOPP)*&
             &((HTUP-4.5)/LEN)
        LOGDIA(I+1,1) = ANINT(SQRT(D2))
        LOGDIA(I+1,2) = SQRT(D2)
        100 continue
        HTUP = HTUP + LOGLEN(INT(LHT))
        LOGDIA(INT(LHT)+1,1) = ANINT(MTOPP)
        LOGDIA(INT(LHT)+1,2) = MTOPP
     end if
  else
     LMERCH = MHT - STUMP
     if (CTYPE.ne.'V') then
        !Added max number of logs (20) check (20250626)
        if ((LMERCH/(MAXLEN+TRIM)).gt.20) then
           ERRFLAG = 12
           return
        end if
        call NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,LOGST)
        call SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,LOGST,LOGLEN)
        !--     SET NOLOGP TO THE NUMBER OF 16 FOOT SEGMENTS
        NOLOGP = LOGST
        !           NOLOGP = LMERCH/16.0
     else
        NOLOGP = LOGST
     end if
     LOGDIA(1,1) = ANINT(DBHIB)
     LOGDIA(1,2) = DBHIB
     !        CHECK FOR A ONE LOG TREE, SET DIAMETER AND LENGTH
     if (LOGST.eq.1) then
        LOGDIA(2,1) = ANINT(MTOPP)
        LOGDIA(2,2) = MTOPP
        !        IF MULTIPLE SEGMENTS, LOOP THROUGH, FIND DIAMETERS AND LENGTHS
     else
        LEN = MHT - 4.5
        HTUP = 1.0
        do 200, I = 1,LOGST-1
        HTUP = HTUP + LOGLEN(I)
        D2 = DBHIB*DBHIB - (DBHIB*DBHIB-MTOPP*MTOPP)*&
             &((HTUP-4.5)/LEN)
        LOGDIA(I+1,1) = ANINT(SQRT(D2))
        LOGDIA(I+1,2) = SQRT(D2)
        200 continue
        HTUP = HTUP + LOGLEN(LOGST)
        LOGDIA(LOGST+1,1) = ANINT(MTOPP)
        LOGDIA(LOGST+1,2) = MTOPP
     end if
  end if
  !     CALL THE SCRIB ROUTINE
  DIBL = LOGDIA(1,1)
  do I = 1,LOGST
     DIBS = LOGDIA(I+1,1)
     LENGTH = LOGLEN(I)
     call SCRIB (DIBS,LENGTH,COR,LOGV)
     if (cor.eq.'Y') then
        LOGVOL(1,I) = LOGV * 10
     else
        LOGVOL(1,I) = ANINT(LOGV)
     end if
     VOL(2) = VOL(2) + LOGVOL(1,I)
     !     FIND SMAILIAN'S VOLUME
     LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH
     LOGVOL(4,I) = ANINT(LOGV*10.0)/10.0
     VOL(4) = VOL(4) + LOGVOL(4,I)
     DIBL = DIBS
     250 continue
  end do
  !     find volume for 32 foot logs (Flewelling & Demars equation only)
  if ((VOLEQ(4:5).eq.'f3' .or. VOLEQ(4:5).eq.'F3' .or.&
       &VOLEQ(2:3).eq.'61') .and. LOGST.gt.1) then
     LCNT = 0
     do I = 2, LOGST,2
        LCNT = LCNT + 1
        !           truncate diameter for 32foot board 6/2001
        DIBS = INT (LOGDIA(I+1,2))
        LENGTH = LOGLEN(I) + loglen(I-1)
        call SCRIB (DIBS,LENGTH,COR,LOGV)
        LOGVOL32(LCNT) = LOGV
        260 continue
     end do
     !    check for a 16 foot top log
     if (((LOGST+1)/2) .gt. (LOGST/2)) then
        LCNT = LCNT + 1
        DIBS = INT(LOGDIA(LOGST+1,2))
        LENGTH = LOGLEN(LOGST)
        call SCRIB (DIBS,LENGTH,COR,LOGV)
        LOGVOL32(LCNT) = LOGV
     end if
     !    prorate 32 volumes into 16 foot pieces
     LCNT = 0
     do 261, i = 2,LOGST,2
     LCNT = LCNT + 1
     topv16 = logvol(1,i)
     botv16 = logvol(1,i-1)
     R = TOPV16 / (TOPV16 + BOTV16)
     logvol(1,I) = ANINT(logvol32(LCNT) * R)
     logvol(1,I-1) = ANINT(logvol32(LCNT) - logvol(1,i))
     if (cor.eq.'Y') then
        logvol(1,I-1) = logvol(1,I-1) * 10
        logvol(1,I) = logvol(1,I) * 10
     end if
     261 continue
     !     check for top log
     if (((LOGST+1)/2) .gt. (LOGST/2)) then
        LCNT = LCNT + 1
        LOGVOL(1,LOGST) = LOGVOL32(LCNT)
        if (cor.eq.'Y') LOGVOL(1,LOGST) = LOGVOL(1,LOGST)*10
     end if

     VOL(2) = 0
     do 265, i = 1,LOGST
     VOL(2) = VOL(2) + LOGVOL(1,I)
     !            WRITE(*,*)" 32FT ",NUMSEG,I,LOGVOL(1,I)
     265 continue
  end if
  !************** 32 foot logs endif ******************************
  !     calculate stump and stem tip volume
  !      IF(STUMP.LT.0.01) STUMP=1.0
  !      VOL(14)=0.005454154*LOGDIA(1,2)*LOGDIA(1,2)*STUMP
  !      IF(VOL(4).GT.0.0) VOL(15)=VOL(1)-VOL(14)-VOL(4)-VOL(7)
  !      IF(VOL(15).LT.0.01) VOL(15)=0.0

  return
end

subroutine R10HTS(EQNUM,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,TOP,&
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
     !logs are in 10s of logs. So added /10 to get number of logs. YW 9/9/13
     if (EQNUM(1:3).eq.'A16') then
        LMERCH = HT1PRD/10*16.3
        HTD = LMERCH +STUMP
     else if (EQNUM(1:3).eq.'A32') then
        LMERCH = HT1PRD/10*32.6
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

subroutine FIREWOOD (VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
     &LMERCH,DBTBH,MTOPP,STUMP,HEX,DEX,ZEX,RHFW,RFLW,&
     &TAPCOE,F,FMOD,PINV_Z,TOP6,TCVOL,slope,errflag)

  !--   DETERMINE CUBIC FIBER CONTENT TO TOP DIAMETER.  THE VOLUME WILL BE
  !--   DETERMINED BY USING THE SMALIAN FORMULA TO COMPUTE VOLUMES FOR 4
  !--   FOOT SECTIONS FROM THE STUMP TO THE TOP DIAMETER, NO TRIM.

  !--   VARIABLES OF INTEREST ARE:
  !--   TCVOL - REAL - VOLUME FOR THE TREE IN CUBIC FOOT,
  !--                  DOES NOT INCLUDE THE LIMBS OR ROOTS.

  ! Variables
  character(len=2) :: FORST
  character(len=10) :: VOLEQ
  integer :: HTLOOP,JSP,NEXTRA,SETOPT(6),errflag,I,MFLG
  real    :: D2OLD,HTTOT,HT2,TCVOL,VOLTMP,MTOPP,DBTBH
  real    :: RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
  real    :: DBHOB,HEX(2),ZEX(2),TOP6,DEX(2),slope
  real    :: DIB,DOB,CUVOL,LMERCH,MERCHHT,STUMP

  !     GET MERCH HEIGHT

  HTLOOP = INT((LMERCH + .5 - 1.0)/4.0)
  MERCHHT = LMERCH + STUMP

  HT2 = STUMP
  !##########
  if (JSP.gt.0.and. HTTOT.le.15) then
     !                  SMALL TREE FIX FOR FLEWELLING STUMP DIAMETERS
     call FWSMALL(JSP,NEXTRA,SETOPT,slope,DBHOB,HTTOT,DBTBH,&
          &HEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,DIB,errflag)
  else

     call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
          &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,&
          &MTOPP,MFLG,CUVOL,DIB,DOB,errflag)

     !        IF(VOLEQ(4:6).EQ.'JB2' .OR. VOLEQ(4:6).EQ.'jb2') THEN
     !          IF(VOLEQ(8:10) .EQ. '108') THEN
     !             TCVOL = 0.005454154 * CUVOL * DBHOB * DBHOB * HTTOT
     !          ELSE
     !             TCVOL = CUVOL
     !          ENDIF
     !          RETURN
     !        ENDIF
  end if

  TCVOL = 0
  do I = 1,HTLOOP
     D2OLD = DIB
     HT2 = HT2+4.0
     !##########
     call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
          &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HT2,&
          &MTOPP,MFLG,CUVOL,DIB,DOB,errflag)

     !--     USE SMALIAN FOR NON STUMP SEGMENTS
     VOLTMP = .00272708*(D2OLD*D2OLD+DIB*DIB)*4.0
     TCVOL = TCVOL + VOLTMP
     10 continue
  end do
  !--   USE SMALIAN FOR TIP, WITH A TIP DIAMETER OF MTOPP
  if ((MERCHHT-HT2) .gt.0.0 ) then
     VOLTMP = .00272708*(DIB*DIB)*(MERCHHT - HT2)
     TCVOL = TCVOL + VOLTMP
  end if
  TCVOL = NINT(TCVOL * 10.0) * 1E-1
  return
end

  !--  VARIABLES OF INTEREST ARE:

  !--  Following variables have been be passed to the model *******
  !--            optional variables are identified

  !--  MERCHANDIZING VARIABLES

  !--  REGION - INTEGER - Region number used to set Regional Merchandizing Rules
  !--  INTEGERS - Indicating which volumes are desired
  !--    BFPFLG - Board foot primary product
  !--    CUPFLG - Cubic foot primary product
  !--    CDPFLG - Cord wood primary product
  !--    SPFLG - Secondary product

  !****** End of Merchandising parameters.

  !    TREE CHARACTERISTICS

  !--  VOLEQ - INTEGER - The volume equation number for this tree

  !--  DBHOB - REAL - Diameter Breast Height Outside Bark
  !--  DRCOB - REAL - Diameter Root Collar Outside Bark

  !--  HTTYPE - CHAR - Tree height measured in feet (F) or logs (L)
  !--  HTTOT - REAL - Tree height from the ground to the tip
  !--  LOGHT - REAL - Tree height in number of logs to MTOPP
  !--  HT1PRD - REAL - Tree height in feet to MTOPP
  !--  HT2PRD - REAL - Tree height in feet to MTOPS

  !--  Note measurements of bark and form are optional entries
  !--  DBTBH - REAL - Double bark thickness at breast height
  !--  FCLASS - INTEGER - Girard form class
  !--  BTR - INTEGER - Bark thickness ratio
  !--  UPSHT1 - REAL - The height of the first upper stem form measurement
  !--  UPSHT2 - REAL - The height of the second upper stem form measurement
  !--  UPSD1 - REAL - The measured diameter outisde bark at point UPSHT1.
  !--  UPSD2 - REAL - The measured diameter outisde bark at point UPSHT2 .
  !--  HTREF - REAL - The percent reference height for AVGZ1
  !--  AVGZ1 - REAL - An average Z correction factor to use for point UPSHT1.
  !--  AVGZ2 - REAL - An average Z correction factor to use for point UPSHT2.

  !--  PCTDF - REAL - THE INTEGER VALUE REPRESENTING THE % OF DEFECT
  !--          IN THE MAIN STEM PORTION OF A TREE - THIS DEFECT %
  !--          WILL BE APPLIED TO BOTH BDFT AND CUFT PRODUCTS.
  !--  PCTDF2 - REAL - THE INTEGER VALUE REPRESENTING THE % OF DEFECT
  !--          IN THE TOPWOOD PORTION OF A TREE - THIS DEFECT %
  !--          WILL BE APPLIED TO CUFT PRODUCTS.

  !--  RETURNED VARIABLES

  !--  Following variables will be returned to the calling routine ******
  !--             when requested
  !--  VOL - REAL
  !--    VOL(1) - Total volume for the tree in cubic feet, ground to tip.
  !--              Does not include the limbs and roots.
  !--    VOL(2) - Gross amount of board foot as primary product - Scribner.
  !--    VOL(3) - Net amount of board foot as primary product - Scribner.
  !--    VOL(4) - Gross amount of cubic foot as primary product - Smalian.
  !--    VOL(5) - Net amount of cubic foot as primary product - Smalian.
  !--    VOL(6) - Amount of cord wood as primary product.
  !--    VOL(7) - Gross amount of cubic foot as secondary product - Smalian.
  !--    VOL(8) - Net amount of cubic foot as secondary product - Smalian.
  !--    VOL(9) - Amount of cord wood as secondary product.
  !--    VOL(10) - Gross amount of board foot as primary product - INT'L 1/4"
  !--    VOL(11) - Net amount of board foot as primary product - INT'L 1/4"
  !--    VOL(12) - GROSS AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
  !--    VOL(13) - NET AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
  !--    VOL(14) - Stump inside bark cubic foot volume
  !--    VOL(15) - Stump outside bark cubic foot volume

  !--  LOGLEN - REAL(20) - Log segment lengths computed by
  !--                      subroutine "SEGMENT".  Does not include trim.
  !--  LOGVOL(7,20) - REAL - The product volume of a log
  !--    LOGVOL(1,X) - GROSS BOARD FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(2,X) - GROSS REMOVED BOARD FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(3,X) - NET BOARD FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(4,X) - GROSS CUBIC FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(5,X) - GROSS REMOVED CUBIC FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(6,X) - NET CUBIC FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(7,X) - GROSS INT'L 1/4 VOLUME (20 LOGS)

  !--  LOGDIA - REAL(21,3) - Log end diameters
  !       LOGDIA(x,1) = scaling dib
  !       LOGDIA(x,2) = actual dib
  !       LOGDIA(x,3) = actual dob

  !--  BOLHT - REAL(21) - point on the bole where diameters were estimated
  !--  *** Note LOGDIA and BOLHT are dimensioned 1 larger then the number of
  !--     logs.  This is to hold the values for the small end of the top log.

  !--  NOLOGP - REAL - Average number of 16 ft logs, primary product.
  !--  NOLOGS - REAL - Average number of 16 ft logs, secondary product.

  !--  VARIABLES OF INTEREST USED WITHIN THIS ROUTINE ARE:

  !--  LENMS - REAL - Length of primary product if a secondary product
  !--                 is desired.
  !--  LMERCH - REAL - Total merchantable length of stem returned
  !--             from subroutine "MERLEN" and the merchantable
  !--             length of stem with trim and unusable top
  !--             removed returned from subroutine "SEGMNT"
  !--  NUMSEG - INTEGER - The computed number of segments from
  !--           subroutine "NUMSEG"
