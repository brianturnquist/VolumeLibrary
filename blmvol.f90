!============================!
!   VolumeLibrary : blmvol   !
!============================!
  !== last modified  09-18-2013
  ! 01/18/2013 Added calculation for stump VOL(14) and tip VOL(15)
  ! 09/18/2013 Correct XINT to sum all logs
  ! 03/26/2015 Modified SUBROUTINE DOUBLE_BARK to include TAPEQN 14 and also catch error for no bark equation (YW)
  ! 04/13/2017 Removed stump and tip vol calc. Now they are calced in voinit.
subroutine BLMVOL(VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,FCLASS,&
     &VOL,LOGDIA,LOGLEN,LOGVOL,TLOGS,NOLOGP,NOLOGS,BFPFLG,&
     &CUPFLG,ERRFLAG)

  !   THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE
  !   USING BLM, OREGON, VOLUME DETERMINATION ROUTINES.

  !   THERE ARE CALLS TO THE FOLLOWING SUBROUTINES
  !     BLMGDIB - INTERNAL - CALLS BLMTAP
  !     BLMMLEN - INTERNAL - CALLS BLMTAP
  !     BLMTCUB - INTERNAL - CALLS BLMTAP
  !     NUMLOG  - EXTERNAL - NO CALLS
  !     BLMOLDV - EXTERNAL - NO CALLS
  !     SCRIB   - EXTERNAL - NO CALLS
  !     SEGMNT  - EXTERNAL - NO CALLS

  use DEBUG_MOD

  ! Variables
  character(len=10) :: VOLEQ
  character(len=1) :: COR
  character(len=1) :: HTTYPE

  integer :: EVOD,NUMSEG,OPT,TAPEQU,FCLASS,ERRFLAG,I,J,lcnt
  integer :: PROFILE,bfpflg,cupflg,tlogs,ITAPER

  real    :: DBHOB,DIB,DBHIB,LOGVOL(7,20),MTOPP,D2
  real    :: HTTOT,TTH,D17,TLH,LENGTH,MERCHL,HALFLOG,DIBL,DIBS
  real    :: LMERCH,LOGV,HGT2,HT1PRD,MAXLEN,MINLEN,NOLOGP,NOLOGS
  real    :: STUMP,TRIM,VOL(15),LOGDIA(21,3),LOGLEN(20),HTLOG
  real    :: BLMBUTT(3,9),LOGVOL32(20),TOTCUB,SMD_17,TCVOL
  real    :: BFINT
  !     COEFFICIENTS FOR THE BUTT LOGS (B1,B2,B3)

  !     DOUGLAS FIR, COAST
  data ((BLMBUTT(I,J),I = 1,3),J = 1,9)&
       &/ 0.8936,  0.2810,  -0.0407,&
       !     DOUGLAS FIR, CASCADE
       &0.8737,  0.3359,  -0.0581,&
       !     DOUGLAS FIR, SOUTHWEST,EAST,CALIFORNIA & W LARCH
       &0.7111,  0.3433,  -0.0339,&
       !     PONDEROSA, SUGAR, JEFFREY & W WHITE PINE
       &0.6232,  0.3388,   0.0,&
       !     GRAND, WHITE FIR, SPRUCES EAST & WEST SIDE
       &0.7082,  0.3490,  -0.0238,&
       !     SHASTA, SILVER, & NOBLE FIR
       &0.8304,  0.1901,   0.0,&
       !     HEMLOCK
       &0.5393,  0.4447,   0.0,&
       !     CEDARS
       &0.9305,  0.2003,   0.0,&
       !     ALL OTHER SPECIES
       &0.7150,  0.294,    0.0  /

  if (DEBUG%MODEL) then
     write  (LUDBG, 90) ' -->Enter BLMVOL'
     90 format (A)

     WRITE  (LUDBG, 100)'  VOLEQ     MTOPP HTTOT HT1PRD DBHOB       &
          &    HTTYPE FCLASS'
     100 format (A)
     write  (LUDBG, 104)VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,&
          &FCLASS
     104 format(A, 1X, F5.1, 1X, F5.1, 2X, F5.1, 2X, F5.1, 5X, A,&
          &1X, I5)

     WRITE  (LUDBG, 300)'TLOGS NOLOGP NOLOGS BFPFLG CUPFLG          &
          &                        ERRFLAG'
     300 format (A)
     write  (LUDBG, 320)TLOGS,NOLOGP,NOLOGS,BFPFLG,CUPFLG,&
          &ERRFLAG
     320 format(1X, I2, 2X, F5.1, 2X, F5.1, 2X, A, 1X, I5, I5, 1X,I5)
  end if

  !   SET ALL POTENTIAL VOLUMES AND AVG NUM OF LOGS TO ZERO
  do I = 1, 15
     VOL(I) = 0.0
     10 continue
  end do

  do I = 1, 21
     LOGDIA(I,1) = 0.0
     LOGDIA(I,2) = 0.0
     LOGDIA(I,3) = 0.0
     15 continue
  end do

  do 21, I = 1,7
  do J = 1, 20
     logvol(I,J) = 0.0
     20 continue
  end do
  21 continue

  TLOGS = 0
  STUMP = 0.0
  NOLOGP = 0.0
  NOLOGS = 0.0
  NUMSEG = 0
  ERRFLAG = 0

  !   IF DBHOB OR HT EQUALS ZERO THEN DON'T CALCULATE THE VOLUME

  if (DBHOB.le.0.0) then
     ERRFLAG = 3
  end if

  if (FCLASS .le.0) then
     ERRFLAG = 2
     go to 1000
  end if

  if (HTTYPE.eq.'L'.or.HTTYPE.eq.'l') then
     if (HT1PRD.lt.10.or. HT1PRD.gt.200) then
        ERRFLAG = 7
        go to 1000
     end if

     TLH = HT1PRD/10.0 ! logs are in 10s of logs, 11/2000
     TTH = 0.0
     ! find number of 16 foot logs
     !         IF(MODEL.EQ.'B32' .OR. MODEL.EQ.'b32') TLH = TLH * 2

  else
     if (HTTOT .le.0) then
        ERRFLAG = 4
        go to 1000
     end if
     TTH = HTTOT + 1.5
     TLH = 0
  end if

  !      Set the Taper Equation Indicator for the current tree.
  !      First, place the Taper Equation number, in <EQNUM(1)>, in the
  !      integer scalar variable <TAPEQU>.
  !      For the BLM, the taper equation number is the two-digit Species Code
  !      plus 700 AND is the same for Scribner and CUBIC volume computations.

  if (BFPFLG.eq.1.or. CUPFLG.eq.1) then
     call BLMTAPEQ(VOLEQ,PROFILE,TAPEQU)
     !      The following codes are moved to SUBROUTINE BLMTAPEQ
     !         itaper = 0
     !         IF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B01'
     !     >               .AND. ITAPER .GT. 0 .AND. ITAPER .LT.4) THEN
     !            TAPEQU = ITAPER
     !          PROFILE = 1
     !         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B01'
     !     >               .AND. ITAPER .EQ. 0) THEN
     !            TAPEQU = 1
     !          PROFILE = 1
     !         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B02'
     !     >               .AND. ITAPER .GT. 0 .AND. ITAPER .LT.4) THEN
     !            TAPEQU = ITAPER
     !          PROFILE = 2
     !         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B02'
     !     >               .AND. ITAPER .EQ. 0) THEN
     !            TAPEQU = 2
     !          PROFILE = 2
     !         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B03'
     !     >                .AND. ITAPER .GT. 0 .AND. ITAPER .LT.4) THEN
     !            TAPEQU = ITAPER
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B03'
     !     >               .AND. ITAPER .EQ. 0) THEN
     !            TAPEQU = 3
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B04')THEN
     !            TAPEQU = 4
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'211')THEN
     !            TAPEQU = 5
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'202' .AND. VOLEQ(1:3).EQ.'B05')THEN
     !            TAPEQU = 6
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'B01')THEN
     !            TAPEQU = 10
     !          PROFILE = 3
     !         ELSEIF(VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'B00')THEN
     !            TAPEQU = 11
     !          PROFILE = 3
     !         ELSEIF(VOLEQ(8:10).EQ.'116')THEN
     !            TAPEQU = 12
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'117')THEN
     !            TAPEQU = 13
     !          PROFILE = 4
     !         ELSEIF(VOLEQ(8:10).EQ.'119')THEN
     !            TAPEQU = 14
     !          PROFILE = 5
     !         ELSEIF(VOLEQ(8:10).EQ.'108')THEN
     !            TAPEQU = 15
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'231')THEN
     !            TAPEQU = 20
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'631')THEN
     !            TAPEQU = 21
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'351')THEN
     !            TAPEQU = 22
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'998')THEN
     !            TAPEQU = 23
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'312')THEN
     !            TAPEQU = 24
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'361')THEN
     !            TAPEQU = 25
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'431')THEN
     !            TAPEQU = 26
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'542')THEN
     !            TAPEQU = 27
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'747')THEN
     !            TAPEQU = 28
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'800')THEN
     !            TAPEQU = 29
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'015' .AND. VOLEQ(1:3).EQ.'B01')THEN
     !            TAPEQU = 30
     !          PROFILE = 6
     !         ELSEIF(VOLEQ(8:10).EQ.'015' .AND. (VOLEQ(1:3).EQ.'B00' .OR.
     !     >                  VOLEQ(1:3).EQ.'B02'))THEN
     !            TAPEQU = 31
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'021')THEN
     !            TAPEQU = 32
     !          PROFILE = 7
     !         ELSEIF(VOLEQ(8:10).EQ.'017')THEN
     !            TAPEQU = 33
     !          PROFILE = 6
     !         ELSEIF(VOLEQ(8:10).EQ.'011')THEN
     !            TAPEQU = 34
     !          PROFILE = 7
     !         ELSEIF(VOLEQ(8:10).EQ.'022')THEN
     !            TAPEQU = 35
     !          PROFILE = 7
     !         ELSEIF(VOLEQ(8:10).EQ.'093')THEN
     !            TAPEQU = 41
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'098')THEN
     !            TAPEQU = 42
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'260' .OR. VOLEQ(8:10).EQ.'263')THEN
     !            TAPEQU = 48
     !          PROFILE = 8
     !         ELSEIF(VOLEQ(8:10).EQ.'081')THEN
     !            TAPEQU = 51
     !          PROFILE = 9
     !         ELSEIF(VOLEQ(8:10).EQ.'042')THEN
     !            TAPEQU = 52
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'041')THEN
     !            TAPEQU = 53
     !          PROFILE = 10
     !         ELSEIF(VOLEQ(8:10).EQ.'242')THEN
     !            TAPEQU = 54
     !          PROFILE = 9
     !         ELSEIF(VOLEQ(8:10).EQ.'073')THEN
     !            TAPEQU = 55
     !          PROFILE = 9
     !         ELSE
     !            TAPEQU = 56
     !          PROFILE = 10
     !         ENDIF
  end if

  !     Second, set the taper equation array subscript, <PROFILE>, according to
  !     the (Species Code plus 700), provided in the VOLUME EQUATIONs given
  !     in the Key File, which, for the BLM, is the Equation Number,
  !     and is now the value of <TAPEQU>:

  !      IF (TAPEQU.EQ.1) THEN
  !        PROFILE = 1
  !      ELSEIF (TAPEQU.EQ.2) THEN
  !        PROFILE = 2
  !      ELSEIF (TAPEQU.EQ.3.OR.TAPEQU.EQ.4.OR.
  !     >        TAPEQU.EQ.6.OR.TAPEQU.EQ.55) THEN
  !        PROFILE = 3
  !      ELSEIF (TAPEQU.GE.10.AND.TAPEQU.LE.14) THEN
  !        PROFILE = 4
  !      ELSEIF (TAPEQU.EQ.30.OR.TAPEQU.EQ.31.OR.
  !     >        TAPEQU.EQ.33.OR.TAPEQU.EQ.41.OR.
  !     >        TAPEQU .EQ. 42) THEN
  !        PROFILE = 5
  !      ELSEIF (TAPEQU.EQ.32.OR.TAPEQU.EQ.34.OR.
  !     >        TAPEQU.EQ.35) THEN
  !        PROFILE = 6
  !      ELSEIF (TAPEQU.EQ.48) THEN
  !        PROFILE = 7
  !      ELSEIF (TAPEQU.GE.51.AND.TAPEQU.LE.54) THEN
  !        PROFILE = 8
  !      ELSE
  !        PROFILE = 9
  !      ENDIF

  call DOUBLE_BARK (TAPEQU, DBHOB, DBHIB)
  !     Added arror catch for equations without Double bark equation (03/26/2015)
  if (DBHIB.le.0.0001) then
     ERRFLAG = 14
     return
  end if
  if (MTOPP.le.0) then
     MTOPP = anint((0.184*DBHOB)+2.24)
  end if

  !     small tree fixes for FVS
  !     Tree must be measured in feet.
  if (tth.gt.0.0) then

     if (TTH.le.17.8) then
        TOTCUB = 0.00272708*(DBHIB*DBHIB)*TTH
        VOL(1) = TOTCUB
        goto 1000
     else

        SMD_17 = AINT(SQRT(DBHIB*DBHIB-(DBHIB*DBHIB)*17.3/TTH)+0.5)

        if (SMD_17.lt.MTOPP) then
           TOTCUB = 0.00272708*(DBHIB*DBHIB)*TTH
           VOL(1) = TOTCUB
           goto 1000
        end if
     end if
  end if

  !      if(VOLEQ(4:6).eq.'b32' .or. VOLEQ(4:6).EQ.'B32') THEN
  !          HTLOG = 32.6
  !      ELSE
  HTLOG = 16.3
  !      ENDIF

  !      total cubic volume calculations

  if (cupflg.eq.1) then
     D17 = ANINT((DBHOB*FCLASS) / 100.0)
     call BLMTCUB (PROFILE, DBHOB, TTH, TLH, D17, MTOPP, HTLOG,&
          &TCVOL)
     vol(1) = tcvol
  end if

  !           BOARD FOOT CACULATIONS  (DBHOB GE 7.0)              *

  !   DETERMINE IF A BOARD FOOT PRODUCT REPRESENTATION IS NEEDED

  if (BFPFLG.eq.1.or. CUPFLG.eq.1) then
     ! *************** USE STEM PROFILE MODEL TO DETERMINE VOLUME
     COR = 'N'
     EVOD = 2
     MAXLEN = 16.0
     !              MAXLEN = 32.0
     MINLEN = 8.0
     OPT = 23 !changed from 21, 10/11/2001 klc
     STUMP = 1.0

     TRIM = 0.3
     MERCHL = 8
     !         ROUNDING IN ACCORDANCE WITH BLM STANDARDS.
     D17 = ANINT ((DBHOB*FCLASS) / 100)

     if (HTTYPE.eq.'L'.or.HTTYPE.eq.'l') then
        ! check for half logs
        NUMSEG = INT(TLH)
        HALFLOG = TLH - NUMSEG
        LOGDIA(1,2) = 0.0
        do 102, I = 1, NUMSEG
        HGT2 = FLOAT(I)

        call BLMTAP (DBHOB,TTH,TLH,HGT2,D17,MTOPP,HTLOG,D2,&
             &PROFILE)

        if (DEBUG%MODEL) then
           write  (LUDBG, 400)'  DBHOB   TTH    TLH     HGT2   D17'&
                &//'   MTOPP  HTLOG  D2   PROFILE'
           400 format (A)
           write  (LUDBG, 420)DBHOB,TTH,TLH,HGT2,D17,MTOPP,HTLOG,&
                &D2, PROFILE
           420 format(9F7.1)
        end if

        !               PUTS DIAMETERS IN PROPER DIAMETER CLASS
        !               ROUNDING CONDITION USED WAS .49 WHICH IS IN
        !               ACCORDANCE WITH BLM'S STANDARDS.
        LOGDIA(I+1,2) = D2
        LOGLEN(I) = 16.0
        102 continue
        if (HALFLOG.gt.0.0) then
           HGT2 = FLOAT(NUMSEG) + HALFLOG
           call BLMTAP (DBHOB,TTH,TLH,HGT2,D17,MTOPP,HTLOG,D2,&
                &PROFILE)

           NUMSEG = NUMSEG + 1
           LOGDIA(NUMSEG+1,2) = D2
           if (VOLEQ(4:6).eq.'b32' .or. VOLEQ(4:6).eq.'B32') then
              LOGLEN(NUMSEG) = 16.0
           else
              LOGLEN(NUMSEG) = 8.0
           end if
        end if
        NOLOGP = NUMSEG
     else

        !             LMERCH IS THE MERCHANTABLE LENGTH FROM STUMP TO
        !             SPECIFIED TOP, INCLUDING TRIM.
        call BLMMLEN (PROFILE,TTH,TLH,DBHOB,D17,STUMP,MTOPP,&
             &HTLOG,LMERCH)
        HT1PRD = LMERCH + STUMP
        !             CHECK FOR A MINIMUM MERCH STEM LENGTH
        if (LMERCH.lt.MERCHL) go to 1000

        !             SUBROUTINE "NUMLOG" WILL DETERMINE THE NUMBER OF
        !             MERCHANTABLE SEGMENTS IN A GIVEN MERCHANTABLE LENGTH
        !             OF TREE STEM, ACCORDING TO ONE OF THE DEFINED SEGMENTATION
        !             RULES IN THE VOLUME ESTIMATOR HANDBOOK FSH ???.
        !Added max number of logs (20) check (20250626)
        if ((LMERCH/(MAXLEN+TRIM)).gt.20) then
           ERRFLAG = 12
           return
        end if
        call NUMLOG (OPT, EVOD, LMERCH, MAXLEN, MINLEN,&
             &TRIM,NUMSEG)

        !             SUBROUTINE "SEGMNT" WILL DETERMINE THE LENGTH OF EACH
        !             SEGMENT, GIVEN A MERCHANTABLE LENGTH OF TREE STEM AND
        !             THE NUMBER OF SEGMENTS IN IT (DETERMINED IN SUBROUTINE
        !             "NUMLOG").  SEGMENT LENGTHS ARE DETERMINED ACCORDING TO
        !             ONE OF THE DEFINED SEGMENTATION RULES IN THE VOLUME
        !             ESTIMATOR HANDBOOK FSH ???.
        !             NOTE - THE VARIABLE LMERCH WHEN PASSED IS THE TOTAL
        !                    MERCH LENGTH, BUT IS RETURNED AS THE MERCH LENGTH
        !                    WITH TRIM AND UNUSABLE PORTION OF THE TOP SUBTRACTED.

        call SEGMNT (OPT, EVOD, LMERCH, MAXLEN, MINLEN,&
             &TRIM, NUMSEG, LOGLEN)

        !           NOLOGP = LMERCH / 16.0
        NOLOGP = NUMSEG

        !             SUBROUTINE "BLMGDIB" IS INTERNAL AND USES PROFILE MODEL

        call BLMGDIB (PROFILE,MTOPP,TTH,TLH,DBHOB,DBHIB,D17,STUMP,&
             &TRIM, NUMSEG,HTLOG,LOGLEN, LOGDIA)

     end if

     !      GET SCRIBNER VOLUME FOR THE PIECES                   *

     if (BFPFLG.eq.1) then

        do I = 1, NUMSEG
           ! Ref:         DIB = LOGDIA(I+1,2)
           DIB = ANINT (LOGDIA(I+1,2))
           LOGDIA(I+1,1) = DIB
           LENGTH = LOGLEN(I)
           !   Move to Scribner Factor volumes       11/2000 klc
           !               IF (DIB.EQ.5.0)  THEN
           !                 LOGV = 16.0 * (LENGTH/16.0)
           !               ELSEIF (DIB.EQ.6.0)  THEN
           !                 LOGV = 21.0 * (LENGTH/16.0)
           !               ELSEIF (DIB.EQ.7.0)  THEN
           !                 LOGV = 25.0 * (LENGTH/16.0)
           !               ELSE
           !                 LOGV = (0.79*(DIB**2)-2*DIB-4.0)*(LENGTH/16.0)
           !               ENDIF
           call SCRIB (DIB,LENGTH,COR,LOGV)

           LOGVOL(1,I) = ANINT(LOGV) !ROUND EACH LOG VOLUME
           !               write(LUDBG, '(F8.1)')LOGVOL(1,I)
           !               VOL(2) = VOL(2) + LOGVOL(1,I)

           !  international 1/4
           call INTL14(DIB,LENGTH,BFINT)
           vol(10) = bfint + VOL(10) !Added +VOL(10) YW 09/18/13
           250 continue
        end do
        !      if 32 foot volume is asked for do the following

        !     find volume for 32 foot logs
        if ((voleq(4:6).eq.'b32' .or.&
             &voleq(4:6).eq.'B32')) then

           do I = 2, NUMSEG,2
              ! Ref:            DIB = LOGDIA(I+1,2)
              DIB = INT (LOGDIA(I+1,2))
              LENGTH = LOGLEN(I) + loglen(I-1)

              !                  IF (DIB.EQ.5.0)  THEN
              !                      LOGV = 16.0 * (LENGTH/16.0)
              !                  ELSEIF (DIB.EQ.6.0)  THEN
              !                      LOGV = 21.0 * (LENGTH/16.0)
              !                  ELSEIF (DIB.EQ.7.0)  THEN
              !                      LOGV = 25.0 * (LENGTH/16.0)
              !                  ELSE
              !                      LOGV = (0.79*(DIB**2)-2*DIB-4.0)*(LENGTH/16.0)
              !                  ENDIF
              call SCRIB (DIB,LENGTH,COR,LOGV)
              LOGVOL32(I) = ANINT(LOGV)
              260 continue
           end do
           !    DIVIDE 32 volumes into 16 foot pieces

           do 261, i = 2,numseg,2
           !                  topv16 = logvol(1,i)
           !                  botv16 = logvol(1,i-1)
           !                   R = TOPV16 / (TOPV16 + BOTV16)
           logvol(1,I-1) = ANINT(logvol32(i)/2.0)
           logvol(1,I) = logvol32(i) - logvol(1,i-1)
           261 continue
        end if
        ! ************** 32 foot logs endif ******************************

        !  ROUND LOG VOLUMES BEFORE SUMMING THE TREE
        do 265, i = 1,numseg
        LOGV = ANINT(LOGVOL(1,I))
        LOGVOL(1,I) = LOGV
        VOL(2) = VOL(2) + LOGVOL(1,I)
        265 continue
        !                         !  MINIMUM STEM LENGTH
        !                         !  EQNUM(1) EQUAL TO A VALID EQUATION
     end if

     if (VOL(2).lt.0.0) then
        VOL(2) = 0.0
     end if

     !      GET CUBIC VOLUME FOR THE PIECES SMALIAN FORMULA      *

     if (CUPFLG.eq.1) then
        !    USE DIB AT DBHOB FOR LARGE END BUTT LOG
        LOGDIA(1,2) = DBHIB
        ! Ref:      DIBL = LOGDIB(1)
        DIBL = ANINT (LOGDIA(1,2))
        LOGDIA(1,1) = DIBL
        do I = 1,NUMSEG
           !           truncate diameter for 32foot board 6/2001
           DIBS = ANINT (LOGDIA(I+1,2))
           LOGDIA(I+1,1) = DIBS
           LENGTH = LOGLEN(I)
           !             IF (I.EQ.1) THEN

           !     Removed  20 Sep 93, RJM, OSO 931.5, as requested by J. Alegria.
           ! **            LOGVOL = 0.005454 * (16.0 *
           ! ** >                   (BLMBUTT(1,PROFILE) * D17**2 +
           ! ** >                    BLMBUTT(2,PROFILE) * (DBHOB**2)) *
           ! ** >                   DBHOB**BLMBUTT(3,PROFILE))

           !     Included 20 Sep 93, RJM, OSO 931.5, as requested by J. Alegria.:

           !               LOGVOL = 0.00272708 * (DBHIB**2 + D17**2  ) * LENGTH
           !             ELSE
           LOGV = .00272708 * (DIBL*DIBL+DIBS*DIBS) * LENGTH

           !             ENDIF
           LOGVOL(4,I) = (ANINT(LOGV*10.0)/10.0)
           VOL(4) = VOL(4) + LOGVOL(4,I)
           DIBL = DIBS
           350 continue
        end do
     end if
     !--       (ENDIF FOR VALID EQUATION)
     if (VOL(4).lt.0.0) then
        VOL(4) = 0.0
     end if

  end if

  !--   SET TLOGS TO NUMBER OF SEGMENTS MAIN STEM

  TLOGS = NUMSEG

  !--   NUMSEG IS ZEROED OUT FOR NOW UNTIL TOPWOOD LOGIC IS IMPLEMENTED
  NUMSEG = 0

  !--    DETERMINE TOP WOOD CUBIC FOOT PRODUCTS (ALL TREES)    *

  !--     (ENDIF FOR CUBIC FOOT  TOP WOOD EQUATIONS)
  !       ENDIF

  !      DO 630 NNN = TLOGS + 1, TLOGS + NUMSEG
  !          VOL(8) = VOL(8)+LOGVOL(6,NNN)
  ! 630  CONTINUE

  TLOGS = TLOGS + NUMSEG

  1000 continue

  if (DEBUG%MODEL) then
     write  (LUDBG, '(A)')'<--Exit BLMVOL'
  end if
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

  !     SUBROUTINE "BLMTCUB" WILL COMPUTE TOTAL CUBIC VOLUME.

  !     SUBROUTINE "BLMMLEN" WILL FIND THE MERCHANTABLE LENGTH TO
  !     A SPECIFIED TOP.

  !     SUBROUTINE "LOGDIA" WILL RETURN THE END DIAMETERS OF LOGS
  !     ONCE SEGMENTATION HAS OCCURED.

  !##############################################################
  !##############################################################

subroutine BLMTCUB (PROFILE, DBHOB, TTH, TLH, D17, TOP, HTLOG,&
     &TCVOL)

  !--   DETERMINE TOTAL CUBIC FIBER CONTENT.  THE VOLUME WILL BE
  !--   DETERMINED BY USING THE VOLUME OF A CYLINDER WITH A DIAMETER
  !--   OF DIB AT 1 FOOT FOR THE STUMP VOLUME AND USING THE SMALIAN
  !--   FORMULA TO COMPUTE VOLUMES FOR 4 FOOT SECTIONS FROM THE STUMP
  !--   TO THE TIP.

  !--   VARIABLES OF INTEREST ARE:
  !--   DBHOB - REAL - DIAMETER BREAST HEIGHT
  !--   TTH - REAL - **TOTAL TREE HT INCLUDING THE STUMP.**
  !--   TLH - REAL - **NUMBER OF 16 FOOT LOGS IN TREE.**
  !--   PROFILE - INTEGER - EQUATION NUMBER TO USE
  !--   TCVOL - REAL - TOTAL VOLUME FOR THE TREE IN CUBIC FOOT,
  !--                  DOES NOT INCLUDE THE LIMBS OR ROOTS.

  ! Variables
  integer :: HTLOOP,PROFILE,I
  real    :: DBHOB,D2,D2OLD,TTH,HGT2,TCVOL,VOL,STUMPDIB
  real    :: TLH,TLH1,D17,TOP,HTLOG,R

  if (TTH.gt.0.0) then

     HTLOOP = INT ((TTH + .5 - 1.0) / 4.0)
     HGT2 = 1.0
     TLH1 = 0.0

     !##########
     call BLMTAP (DBHOB,TTH,TLH1,HGT2,D17,TOP,HTLOG,D2,PROFILE)
     ! Ref:STUMPDIB = D2
     STUMPDIB = ANINT ( D2 )

     !--   STUMP VOLUME IS VOLUME FOR A 1 FT HIGH CYLINDER
     R = D2 / 2.0
     TCVOL = (3.1416 * R*R) / 144.0

     do I = 1,HTLOOP
        D2OLD = D2
        HGT2 = HGT2 + 4.0

        !##########
        call BLMTAP (DBHOB,TTH,TLH1,HGT2,D17,TOP,HTLOG,D2,PROFILE)

        !--     USE SMALIAN FOR NON STUMP SEGMENTS
        VOL = .00272708 * (D2OLD*D2OLD + D2*D2) * 4.0
        TCVOL = TCVOL + VOL

        10 continue
     end do

     !--   USE SMALIAN FOR TIP, WITH A TIP DIAMETER OF 0.0
     if ((TTH - HGT2).gt.0.0) then
        VOL = .00272708 * (D2*D2) * (TTH - HGT2)
        TCVOL = TCVOL + VOL
     end if
  end if
  return
end

subroutine BLMMLEN (PROFILE, TTH, TLH, DBHOB, D17, STUMP, TOP,&
     &HTLOG, LMERCH)

  !--   SUBROUTINE WILL DETERMINE WHAT THE MERCHANTABLE LENGTH OF
  !--   STEM FROM THE STUMP TO A SPECIFIED TOP, INCLUDING TRIM.
  !--   NO ROUNDING IS USED WHEN FINDING THE MINIMUM TOP DIAMETER.
  !--   IF THE SPECIFIED MINIMUM TOP IS SET TO 6" THEN 6" DIB
  !--   WITHOUT ROUNDING IS USED AS THE CUT-OFF POINT.

  ! Variables
  integer :: I,FIRST,HALF,LAST,PROFILE,TOPLOP,TOP1
  real    :: DBHOB,D2,TTH,HGT2,LMERCH,STUMP,TOP,D17,TLH
  real    :: HTLOG

  !--   SET TOP1 EQUAL TO DESIRED TOP TIMES 10 TRUNCATED
  TOP1 = AINT (TOP*10.0)

  FIRST = 1
  !--   FIND DESIRED TOP TO THE NEAREST TENTH OF A FOOT
  LAST = INT (TTH + 0.5) * 10
  TOPLOP = LAST

  do I = 1,TOPLOP
     if (FIRST.eq.LAST) go to 100
     HALF = (FIRST + LAST + 1) / 2
     HGT2 = FLOAT(HALF) / 10.0

     !###########
     call BLMTAP (DBHOB,TTH,TLH,HGT2,D17,TOP,HTLOG,D2,PROFILE)

     !--     CONVERT TOP DIAMETER TO TENTH INCH TRUNCATED.
     !--     BEFORE TRUNCATION ADD .005 TO ROUND UP THE DIAMETERS
     !--     THAT ARE CLOSE - 5.995 AND ABOVE WILL BE CONVERTED TO 6

     D2 = AINT ((D2 + 0.005) * 10.0)
     if (TOP1.le.D2) then
        !--        MOVE UP STEM
        FIRST = HALF
     else
        !--        MOVE DOWN THE STEM
        LAST = HALF - 1
     end if
     10 continue
  end do

  100 continue

  LMERCH = FLOAT (FIRST) / 10.0 - STUMP
  if (LMERCH.lt.0.0) LMERCH = 0.0

  return
end

subroutine BLMGDIB (PROFILE,TOP,TTH,TLH,DBHOB,DBHIB,D17,STUMP,&
     &TRIM,NUMSEG,HTLOG,LOGLEN,LOGDIA)

  !--   ALL LOG DIAMETERS ARE ROUNDED TO THE NEAREST 1 INCH
  !--   CLASS (I.E. 8" CLASS = 7.6 - 8.5)

  ! Variables
  integer :: NUMSEG,PROFILE,I

  real    :: TOP,DBHOB,D2,TTH,HGT2,LOGLEN(20),DBHIB
  real    :: LOGDIA(21,3),STUMP,TRIM,D17,TLH,HTLOG

  do I = 1, 21
     LOGDIA(I,2) = 0.0
     10 continue
  end do

  if (NUMSEG.eq.0) go to 1000

  if (STUMP.gt.4.5) then

     !--     USE FOR TOP WOOD

     !###########
     call BLMTAP (DBHOB,TTH,TLH,STUMP,D17,TOP,HTLOG,D2,PROFILE)
     LOGDIA(1,2) = D2
  else

     !--     USE FOR MAIN STEM

     !###########
     !        CALL BLMTAP (DBHOB,TTH,TLH,4.5,D17,TOP,HTLOG,D2,PROFILE)
     LOGDIA(1,2) = DBHIB
  end if

  HGT2 = STUMP
  do I = 1, NUMSEG
     HGT2 = HGT2 + TRIM + LOGLEN(I)

     !###########
     call BLMTAP (DBHOB,TTH,TLH,HGT2,D17,TOP,HTLOG,D2,PROFILE)
     LOGDIA(I+1,2) = D2
     20 continue
  end do

  !--   NEED TO PUT LOG DIB IN PROPER DIAMETER CLASS
  !--   ROUNDING CONDITION USED WAS .49 WHICH IS IN ACCORDANCE WITH
  !--   BLM'S STANDARDS.
  ! Ref:DO 30 I = 1, NUMSEG+1
  ! Ref:    DIBCLS = INT (LOGDIB(I))
  ! Ref:    XXX = LOGDIB(I) - DIBCLS
  ! Ref:    IF (XXX .GT. 0.49) DIBCLS = DIBCLS + 1.0
  ! Ref:    LOGDIB(I) = DIBCLS
  ! Ref:   30 CONTINUE

  !--  SMALL END DIAMETER OF TOP LOG MIGHT BE LESS THAN
  !--  THE MINIMUM SPECIFIED TOP DUE TO ROUNDING OF LOG LENGTHS
  !--  IF THIS IS TRUE, THEN FORCE TOP DIAMETER EQUAL TO
  !--  MINIMUM SPECIFIED TOP.

  if (LOGDIA(NUMSEG+1,2).lt.TOP) then
     LOGDIA(NUMSEG+1,2) = TOP
  end if

  1000 continue
  return
end

subroutine DOUBLE_BARK (TAPEQU, DBHOB, DBHIB)

  ! Variables
  integer :: TAPEQU
  real    :: DBHOB,DBHIB

  !     THIS IS INTENDED TO BE IN (used by {RJM}) THE BLMVOL.F77 SUBROUTINE
  !     Purpose:        Compute the Double Bark Thickness, <DBHIB>, according
  !                     to the given species code (= 700 - <TapEqu>) & DBHOB.

  !     Input:  <TapEqu>: Taper Equation Number
  !     Input:  <DBHOB>:    Diameter @ Breast Height
  !     Output: <DBHIB:>  The Large End Diameter OF A BUTT LOG!

  !     by      J. Alegria & R. Miller, OSO 930, 20 Sep 93

  !     Note:   All of the Bark Thickness Equations are explicitly coded
  !             for ease of reference.

  !     Usage:
  !             Prepare the Taper Equation Number
  !             Call Double_Bark (TapEqu, DBHIB)
  !             Use <DBHIB> as the large diameter in computing the CUBIC
  !             Volume of the BUTT LOG by means of the Smalian Formula.

  if (TAPEQU.eq.1.or.TAPEQU.eq.2.or.TAPEQU.eq.3.or.&
       &TAPEQU.eq.5.or.TAPEQU.eq.35) then
     !     DOUGLAS-FIR (LARSEN & HANN, 1985)
     DBHIB = 0.903563 * DBHOB**0.989388

  else if (TAPEQU .eq.11.or. TAPEQU .eq.12) then
     !     PONDEROSA AND JEFFREY PINE (LARSEN & HANN, 1985)
     DBHIB = 0.809427 * DBHOB**1.016866

  else if (TAPEQU .eq.13.or. TAPEQU .eq.14) then
     !     Added TAPEQN 14 for white pine per BLM 3/26/2015 (Kristen Thompson, state cruiser)
     !     SUGAR PINE (LARSEN & HANN, 1985)
     ! => ?  DBHIB = 0.859045 * DBHOB**1.000000
     DBHIB = 0.859045 * DBHOB

  else if (TAPEQU .eq.15) then
     !     LODGEPOLE PINE (SPADA, 1960)
     DBHIB = DBHOB - (0.3147 + 0.0274 * DBHOB)

  else if (TAPEQU.eq.20.or.TAPEQU.eq.25) then
     !     PACIFIC YEW AND PACIFIC MADRONE
     DBHIB = -0.03425 + 0.98155 * DBHOB

  else if (TAPEQU.eq.21) then
     !     TAN OAK (PILLSBURY, 1984)
     DBHIB = -4.36852 + 0.95354 * DBHOB + 0.18307 * 4.5

  else if (TAPEQU.eq.22.or.TAPEQU.eq.23.or.&
       &TAPEQU.eq.24.or.TAPEQU.eq.26.or.&
       &TAPEQU.eq.27) then
     !     RED ALDER, OREGON MYRTLE, BIG LEAF MAPLE
     !     GOLDERN CHIQUAPIN AND OREGON ASH (PILLSBURY, 1984)
     DBHIB = 0.39534 + 0.90182 * DBHOB

  else if (TAPEQU.eq.28.or.TAPEQU.eq.29) then
     !     BLACK COTTONWOOD AND OAK SPECIES (OREGON WHITE: PILLSBURY, 1984)
     DBHIB = -0.78034 + 0.95956 * DBHOB

  else if (TAPEQU.eq.31.or.TAPEQU.eq.33) then
     !     WHITE AND GRAND FIR (LARSEN & HANN, 1985)
     ! => ?  DBHIB = 0.904973 * DBHOB**1.000000
     DBHIB = 0.904973 * DBHOB

  else if (TAPEQU.eq.32.or.TAPEQU.eq.34) then
     !     RED FIR & PACIFIC SILVER FIR (DOLPH, 1989)
     DBHIB = 0.86951 * DBHOB**1.00983

  else if (TAPEQU.eq.41.or.TAPEQU.eq.42) then
     !     ENGELMANN AND SITKA SPRUCE (SPADA, 1960)
     DBHIB = DBHOB - (0.2113 + 0.0445 * DBHOB)

  else if (TAPEQU.eq.48.or.TAPEQU.eq.56) then
     !     HEMLOCK AND MISCELLANEOUS (FINCH, 1948)
     DBHIB = DBHOB / 1.071

  else if (TAPEQU.eq.52.or.TAPEQU.eq.54) then
     !     ALASKA YELLOW CEDAR AND WESTERN RED CEDAR (FINCH, 1948)
     DBHIB = DBHOB / 1.053

  else if (TAPEQU.eq.51.or.TAPEQU.eq.53) then
     !     INCENSE AND PORT ORFORD CEDAR (LARSEN & HANN, 1985)
     ! => ?  DBHIB = 0.837291 * DBHOB**1.000000
     DBHIB = 0.837291 * DBHOB

  else if (TAPEQU.eq.55) then
     !     WESTERN LARCH
     DBHIB = DBHOB - (0.1231 + 0.1306 * DBHOB)

  end if

  return
end

subroutine BLMTAPEQ(VOLEQ,PROFILE,TAPEQU)
  ! Variables
  character(len=10) :: VOLEQ
  integer :: PROFILE,TAPEQU,ITAPER
  itaper = 0
  if (VOLEQ(8:10).eq.'202' .and. VOLEQ(1:3).eq.'B01'&
       &.and. ITAPER .gt.0.and. ITAPER .lt.4) then
     TAPEQU = ITAPER
     PROFILE = 1
  else if (VOLEQ(8:10).eq.'202' .and. VOLEQ(1:3).eq.'B01'&
       &.and. ITAPER .eq.0) then
     TAPEQU = 1
     PROFILE = 1
  else if (VOLEQ(8:10).eq.'202' .and. VOLEQ(1:3).eq.'B02'&
       &.and. ITAPER .gt.0.and. ITAPER .lt.4) then
     TAPEQU = ITAPER
     PROFILE = 2
  else if (VOLEQ(8:10).eq.'202' .and. VOLEQ(1:3).eq.'B02'&
       &.and. ITAPER .eq.0) then
     TAPEQU = 2
     PROFILE = 2
  else if (VOLEQ(8:10).eq.'202' .and. VOLEQ(1:3).eq.'B03'&
       &.and. ITAPER .gt.0.and. ITAPER .lt.4) then
     TAPEQU = ITAPER
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'202' .and. VOLEQ(1:3).eq.'B03'&
       &.and. ITAPER .eq.0) then
     TAPEQU = 3
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'202' .and. VOLEQ(1:3).eq.'B04') then
     TAPEQU = 4
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'211') then
     TAPEQU = 5
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'202' .and. VOLEQ(1:3).eq.'B05') then
     TAPEQU = 6
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'122' .and. VOLEQ(1:3).eq.'B01') then
     TAPEQU = 10
     PROFILE = 3
  else if (VOLEQ(8:10).eq.'122' .and. VOLEQ(1:3).eq.'B00') then
     TAPEQU = 11
     PROFILE = 3
  else if (VOLEQ(8:10).eq.'116') then
     TAPEQU = 12
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'117') then
     TAPEQU = 13
     PROFILE = 4
  else if (VOLEQ(8:10).eq.'119') then
     TAPEQU = 14
     PROFILE = 5
  else if (VOLEQ(8:10).eq.'108') then
     TAPEQU = 15
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'231') then
     TAPEQU = 20
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'631') then
     TAPEQU = 21
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'351') then
     TAPEQU = 22
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'998') then
     TAPEQU = 23
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'312') then
     TAPEQU = 24
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'361') then
     TAPEQU = 25
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'431') then
     TAPEQU = 26
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'542') then
     TAPEQU = 27
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'747') then
     TAPEQU = 28
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'800') then
     TAPEQU = 29
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'015' .and. VOLEQ(1:3).eq.'B01') then
     TAPEQU = 30
     PROFILE = 6
  else if (VOLEQ(8:10).eq.'015' .and. (VOLEQ(1:3).eq.'B00' .or.&
       &VOLEQ(1:3).eq.'B02')) then
     TAPEQU = 31
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'021') then
     TAPEQU = 32
     PROFILE = 7
  else if (VOLEQ(8:10).eq.'017') then
     TAPEQU = 33
     PROFILE = 6
  else if (VOLEQ(8:10).eq.'011') then
     TAPEQU = 34
     PROFILE = 7
  else if (VOLEQ(8:10).eq.'022') then
     TAPEQU = 35
     PROFILE = 7
  else if (VOLEQ(8:10).eq.'093') then
     TAPEQU = 41
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'098') then
     TAPEQU = 42
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'260' .or. VOLEQ(8:10).eq.'263') then
     TAPEQU = 48
     PROFILE = 8
  else if (VOLEQ(8:10).eq.'081') then
     TAPEQU = 51
     PROFILE = 9
  else if (VOLEQ(8:10).eq.'042') then
     TAPEQU = 52
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'041') then
     TAPEQU = 53
     PROFILE = 10
  else if (VOLEQ(8:10).eq.'242') then
     TAPEQU = 54
     PROFILE = 9
  else if (VOLEQ(8:10).eq.'073') then
     TAPEQU = 55
     PROFILE = 9
  else
     TAPEQU = 56
     PROFILE = 10
  end if
  return
end

  !       As of December 14, 1993, all diameters are kept as computed
  !       (UN-rounded).  Diameters are ROUNDED only for the purpose of
  !       computing a segment volume.

  !       As agreed by W. Abbey, J. Alegria, and G. Willits, on December
  !       13th, 1993.  RJM. OSO 931.5.

  ! --  VARIABLES OF INTEREST ARE:

  ! --  COR    - CHARACTER - FLAG TO USE SCRIBNER OR SCRIBNER
  ! --                       FACTORS TO GENERATE SCRIBNER PRODUCT
  ! --  DBHOB    - REAL      - DIAMETER BREAST HEIGHT
  ! --  DBHIB  - REAL      - DIAMETER-INSIDE-BARK @ BREAST HEIGHT
  ! --  EVOD    - INTEGER - EVEN OR ODD LENGTH SEGMENTS ALLOWED
  ! --            SEGMENTATION OPTIONS 11-14 ALLOW ODD LENGTHS BY
  ! --            DEFINITION
  ! --        1 = ODD SEGMENTS ALLOWED
  ! --        2 = ONLY EVEN SEGMENTS ALLOWED

  ! --  HT      - REAL -  **TREE HEIGHT IN FT. FROM A 1 FOOT STUMP OR
  ! --                      TREE HEIGHT IN NUMBER OF 16 FOOT LOGS PER TREE**
  ! --  TTH     - REAL - TREE HEIGHT FROM GROUND TO TIP
  ! --  TLH     - REAL - TREE HEIGHT IN LOGS PER TREE
  ! --  LENMS   - REAL - LENGTH OF MAIN STEM IF A TOP WOOD PRODUCT
  ! --                   IS DESIRED
  ! --  LMERCH  - REAL - GIVEN MERCHANTABLE LENGTH OF STEM RETURNED
  ! --                   FROM SUBROUTINE "BLMMLEN" AND THE MERCHANTABLE
  ! --                   LENGTH OF STEM WITH TRIM REMOVED RETURNED FROM
  ! --                   SUBROUTINE "SEGMNT"

  ! --  LOGLEN - REAL(20) - LOG SEGMENT LENGTHS COMPUTED BY
  ! --           SUBROUTINE "SEGMNT". DOES NOT INCLUDE TRIM.

  ! --  LOGVOL - REAL  - THE PRODUCT VOLUME OF A LOG

  ! --  MAXLEN - REAL  - MAXIMUM SEGMENT LENGTH
  ! --  MINLEN - REAL  - MINIMUM SEGMENT LENGTH
  ! --  MERCHL - INTEGER - MINIMUM PRODUCT LENGTH FOR A MERCH TREE

  ! --  NOLOGP - REAL  - AVERAGE NUMBER OF 16 FOOT LOGS IN MAIN STEM
  ! --  NOLOGS - REAL  - AVERAGE NUMBER OF 16 FOOT LOGS IN TOP WOOD
  ! --  NUMSEG - INTEGER - THE COMPUTED NUMBER OF SEGMENTS FROM
  ! --           SUBROUTINE "NUMSEG"
  ! --  STUMP  - REAL  - HEIGHT OF STUMP
  ! --  TAPEQU - Integer - TAPER EQUATION TO USE - FROM EQNUM(1)
  ! --  TRIM   - REAL  - TRIM LENGTH FOR EACH SEGMENT

  ! --  VOL    - REAL
  ! --    VOL(1)  - TOTAL VOLUME FOR THE TREE IN CUBIC FOOT, DOES NOT
  ! --              INCLUDE LIMBS AND ROOTS.
  ! --    VOL(2)  - GROSS AMOUNT OF BOARD FOOT PRODUCT IN MAIN STEM.
  ! --    VOL(3)  - NET AMOUNT OF BOARD FOOT PRODUCT IN MAIN STEM.
  ! --    VOL(4)  - GROSS AMOUNT OF CUBIC FOOT PRODUCT IN MAIN STEM.
  ! --    VOL(5)  - NET AMOUNT OF CUBIC FOOT PRODUCT IN MAIN STEM.
  ! --    VOL(6)  - AMOUNT OF CORD WOOD PRODUCT IN MAIN STEM
  ! --    VOL(7)  - GROSS AMOUNT OF CUBIC PRODUCT IN TOP WOOD.
  ! --    VOL(8)  - NET AMOUNT OF CUBIC PRODUCT IN TOP WOOD.
  ! --    VOL(9)  - AMOUNT OF CORD WOOD PRODUCT IN TOP WOOD.
  ! --    VOL(10) - NOT USED
