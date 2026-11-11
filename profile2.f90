!==============================!
!   VolumeLibrary : profile2   !
!==============================!
  !     Subroutine to determine tree profile for APSS

  !     Created ??? ??/??/??
  !     Revised TDH 06/18/09 - created profile2 for APSS
  !     YW 06/20/2014 Add logic to reset MINLEN for region 10 32 foot log equation
  !     YW 07/02/2014 Added errflag to call VOLINTRP routine
  !     YW 03/17/2017 Modified PROFILE2 to call PROFILE to avoid duplicated codes
subroutine PROFILE2 (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
     &HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,&
     &AVGZ1,AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,&
     &LOGST,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,DRCOB,&
     &CTYPE,FCLASS,PROD,ERRFLAG, MERRULES)

  use MRULES_MOD
  use DEBUG_MOD
  use VOLINPUT_MOD

  !...  This is a minor modification to profile.f for use with the APSS
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
  TYPE(MERCHRULES)::MERRULES

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

  if (DEBUG%MODEL) then
     write  (LUDBG, 2) ' -->Enter PROFILE2'
     2 format (A)
  end if

  MRULEMOD = 'Y'
  NEWCOR = MERRULES%COR
  NEWEVOD = MERRULES%EVOD
  NEWOPT = MERRULES%OPT
  NEWMAXLEN = MERRULES%MAXLEN
  NEWMINLEN = MERRULES%MINLEN
  NEWMERCHL = MERRULES%MERCHL
  NEWMINLENT = MERRULES%MINLENT
  NEWMTOPP = MERRULES%MTOPP
  NEWMTOPS = MERRULES%MTOPS
  NEWSTUMP = MERRULES%STUMP
  NEWTRIM = MERRULES%TRIM
  NEWBTR = MERRULES%BTR
  NEWDBTBH = MERRULES%DBTBH
  NEWMINBFD = MERRULES%MINBFD
  call PROFILE (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,HTTYPE,&
       &HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,&
       &AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,&
       &TLOGS, NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,&
       &DRCOB,CTYPE,FCLASS,PROD,ERRFLAG)

  goto 1000
  ! The following codes are duplicated from PROFILE subroutine and will be deleted!

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
     ERRFLAG = 3
     if (HTTOT.gt.0)ERRFLAG = 0
     go to 1000
  end if

  !...  don't need to call mrules for apss, it is passed in as a
  !...  defined type and initialized in the calling app.

  !     MRULES IS EXTERNAL AND CONTAINS THE MERCHANDIZING RULES SETTINGS
  !      CALL MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,MINLEN,
  !     >         MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD)

  !  this was for testing apss i think
  !      WRITE  (LUDBG, 106)'  COR EVOD OPT MAXLEN MINLEN MERCHL'
  !  106    FORMAT (A)
  !         WRITE  (LUDBG, 107) MERRULES%COR(1:1), MERRULES%EVOD,
  !     +   MERRULES%OPT, MERRULES%MAXLEN, MERRULES%MINLEN, MERRULES%MERCHL
  !  107    FORMAT (2X, A, 3X, I2, 3X, I3,3X, F7.1, F7.1, F7.1)

  COR = MERRULES%COR
  EVOD = MERRULES%EVOD
  OPT = MERRULES%OPT
  MAXLEN = MERRULES%MAXLEN
  MINLEN = MERRULES%MINLEN
  MERCHL = MERRULES%MERCHL
  MINLENT = MERRULES%MINLENT
  MTOPP = MERRULES%MTOPP
  MTOPS = MERRULES%MTOPS
  STUMP = MERRULES%STUMP
  TRIM = MERRULES%TRIM
  BTR = MERRULES%BTR
  DBTBH = MERRULES%DBTBH
  MINBFD = MERRULES%MINBFD
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

  if (DEBUG%MODEL) then
     write  (LUDBG, 108)'  COR EVOD OPT MAXLEN MINLEN MERCHL'
     108 format (A)
     write  (LUDBG, 110) COR, EVOD, OPT, MAXLEN, MINLEN, MERCHL
     110 format (2X, A, 3X, I2, 3X, I3,3X, F7.1, F7.1, F7.1)
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
           VOL(4) = NINT(TCVOL * 10.0) * 1E-1
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
     if (REGN.eq.5.and. PROD.eq.'07') then
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

        LMERCH = LMERCH - LENMS
        !--  SEE DEFINITION OF SUBROUTINE "NUMLOG" UNDER BDFT CACULATIONS

     end if

     if (REGN.eq.5.and. PROD.eq.'07') then
        call FIREWOOD (VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
             &LMERCH,DBTBH,MTOPS,LENMS,HEX,DEX,ZEX,RHFW,RFLW,&
             &TAPCOE,F,FMOD,PINV_Z,TOP6,TCVOL,slope,errflag)
        VOL(7) = NINT(TCVOL * 10.0) * 1E-1
        go to 1000
     end if

     !       check for mainstem piece, if not, topwood must meet minimum merch length
     if (LENMS.le.0) then
        if (LMERCH.lt.MERCHL) then
           go to 1000
        end if
     end if
     !Added max number of logs (20) check (20250626)
     if ((LMERCH/(MAXLEN+TRIM)).gt.20) then
        ERRFLAG = 12
        return
     end if
     call NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLENT,TRIM,NUMSEG)

     !--  SEE DEFINITION OF SUBROUTINE "SEGMNT" UNDER BDFT CACULATIONS

     call SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLENT,TRIM,NUMSEG,LOGLENG)
     do I = 1, NUMSEG
        LOGLEN(I+LOGST) = LOGLENG (I)
        50 continue
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
              TCVOL = NINT(TCVOL * 10.0) * 1E-1
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

  if (DEBUG%MODEL) then
     write  (LUDBG, 20) ' <--Exit PROFILE2'
     20 format (A)
  end if

  MERRULES%MTOPS = MTOPS

  return
  end subroutine PROFILE2

  !##################################################################
