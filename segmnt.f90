!============================!
!   VolumeLibrary : segmnt   !
!============================!
  !== last modified 03-25-2014
subroutine SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,&
     &TRIM,NUMSEG,LOGLEN)
  !--  THIS SUBROUTINE WILL DETERMINE THE LENGTH OF EACH
  !--  SEGMENTS GIVEN MERCHANTABLE LENGTH OF TREE STEM AND
  !--  THE NUMBER OF SEGMENTS IN IT (DETERMINED IN SUBROUTINE
  !--  NUMLOG).  SEGMENT LENGTHS ARE DETERMINED ACCORDING TO
  !--  ONE OF THE DEFINED SEGMENTATION RULES IN THE VOLUME
  !--  ESTIMATOR HANDBOOK FSH ???.

  !--  THE LOGIC WAS DEVELOPED BY JIM BRICKELL, TCFPM R-1, AND
  !--  IMPLEMENTED BY WO-TM.
  !--  VARIABLES OF INTEREST LISTED AT END OF ROUTINE

  ! Variables
  integer :: EVOD,NUMSEG,OPT,I,AVLEN,KNTIT
  real    :: LEFTOV,LMERCH,LOGLEN(20),MAXLEN,MINLEN,TRIM

  !      INCLUDE 'COMM0'
  !      INCLUDE 'COMM6'

  do I = 1,20
     LOGLEN(I) = 0.0
     5 continue
  end do

  if (NUMSEG .eq.0) then
     LMERCH = 0.0
     return
  end if

  !--  DETERMINE MERCHANTABLE LENGTH MINUS TRIM AND ROUND
  !--  TO THE NEAREST FOOT OR NEAREST EVEN FOOT

  LMERCH = LMERCH-(FLOAT(NUMSEG)*TRIM)
  if (EVOD .eq.1) then
     LMERCH = INT(LMERCH+0.5)
  else
     LMERCH = INT((LMERCH+1.0)/2.0)*2.0
     !      ELSE
     !          WRITE (REPOUT,10)
     !   10     FORMAT(' ERROR FROM SEGMNT - LMERCH NOT ROUNDED')
  end if

  !--   BECAUSE OF ROUNDING LMERCH MIGHT BE LONGER THAN THE
  !--   THE SUM OF THE PIECES BUT NOT LONG ENOUGH TO MAKE A NEW
  !--   SEGMENT IF TRIM IS CONSIDERED.  IF THIS IS THE CASE FORCE
  !--   LMERCH TO BE EQUAL TO THE SUM OF THE MAXIMUN SEGMENT LENGTHS

  if (LMERCH .gt. (FLOAT(NUMSEG)*MAXLEN)) then
     LMERCH = FLOAT(NUMSEG)*MAXLEN
  end if

  !--   CHECK FOR ONE LOG TREE

  if (NUMSEG .eq.1) then
     if (OPT .eq.24) then

        !--       OPTION 24 ROUNDS TO HALF LOG
        if (LMERCH .lt. (MAXLEN*.25)) then
           LOGLEN(NUMSEG) = 0.0
        else if (LMERCH .ge. (MAXLEN*.25) .and.&
             &LMERCH .le. (MAXLEN*.75)) then
           LOGLEN(NUMSEG) = MAXLEN/2.0
        else
           LOGLEN(NUMSEG) = MAXLEN
        end if

     else if (LMERCH .ge. MINLEN) then
        if (LMERCH .gt. MAXLEN) LMERCH = MAXLEN
        LOGLEN(1) = LMERCH
     else
        LOGLEN(1) = 0
     end if
  else

     !--   MORE THAN ONE LOG TREES

     if (OPT .lt.20) then

        !--        DEAL WITH ALL SEGMENTS - ODD LENGTHS OK

        AVLEN = INT(LMERCH/FLOAT(NUMSEG))
        LEFTOV = LMERCH-(AVLEN*FLOAT(NUMSEG))

        !--        SET ALL LENGTHS EQUAL TO THE AVGLEN.

        do I = 1,NUMSEG
           LOGLEN(I) = AVLEN
           15 continue
        end do

        !--        IF AVLEN IS AN ODD NUMBER ADD ONE FOOT TO BOTTOM
        !--        LOG, NEXT LOWEST LOG, ECT., WHILE AT THE SAME TIME
        !--        SUBTRACTING ONE FOOT FROM THE TOP LOG, THE NEXT
        !--        TO THE TOP LOG, ECT.

        if (AVLEN .gt. INT(AVLEN/2.0)*2.0) then
           do I = 1,NUMSEG
              if ((NUMSEG-2*I+1) .ge.1) then
                 LOGLEN(I) = LOGLEN(I)+1.0
                 LOGLEN(NUMSEG-I+1) = LOGLEN(NUMSEG-I+1)-1.0
              end if
              20 continue
           end do
        end if

        if ( LEFTOV .gt.0) then

           !--           THERE WAS A LEFTOVER AMOUNT, LMERCH WAS NOT EVENLY
           !--           DIVISIBLE BY NUMSEG.

           if (NUMSEG .gt. INT(FLOAT(NUMSEG)/2.0)*2.0) then

              !--           DISTRIBUTE LEFTOV LENGTH AMONGST THE ODD
              !--           LENGTH SEGMENTS, IF THERE ARE ANY.  THERE WILL
              !--           ONLY BE ODD LENGTHS IF NUMSEG IS ODD.

              do I = 1,NUMSEG
                 if (LEFTOV .gt.0) then
                    if (LOGLEN(I) .gt. INT(LOGLEN(I)/2.0)*2) then
                       LOGLEN(I) = LOGLEN(I)+1.0
                       LEFTOV = LEFTOV-1.0
                    end if
                 end if
                 30 continue
              end do
           end if

           !--           DISTRIBUTE THE REST OF LEFTOV, AT THIS POINT
           !--           ALL LOG LENGTHS ARE ASSUMED TO BE EVEN IF LEFTOV
           !--           IS GREATER THAN ZERO

           if (LEFTOV .gt.0) then
              KNTIT = 0
              40 do I = 1,NUMSEG
                 if (LEFTOV.gt.0.and. LOGLEN(I).lt.MAXLEN) then

                    !--                    MAKE SURE 1 OR 2 FOOT SECTION IS ADDED
                    !--                    TO THE LOWEST AND SHORTEST PIECE.

                    if (LEFTOV .ge.2) then
                       if (LOGLEN(I).eq.LOGLEN(NUMSEG)) then
                          LOGLEN(I) = LOGLEN(I)+2.0
                          LEFTOV = LEFTOV-2
                       else if (LOGLEN(I) .gt. LOGLEN(I+1)) then
                          LOGLEN(I+1) = LOGLEN(I+1)+2.0
                          LEFTOV = LEFTOV-2
                       end if
                    else
                       if (LOGLEN(I).eq.LOGLEN(NUMSEG)) then
                          LOGLEN(I) = LOGLEN(I)+1.0
                          LEFTOV = LEFTOV-1
                       else if (LOGLEN(I) .gt. LOGLEN(I+1)) then
                          LOGLEN(I+1) = LOGLEN(I+1)+1.0
                          LEFTOV = LEFTOV-1
                       end if
                    end if
                 end if
                 50 continue
              end do

              KNTIT = KNTIT+1
              if (KNTIT .gt.500) then
                 !                    WRITE(*,60)
                 !   60               FORMAT(' PRINTED IN SEGMNT STATEMENT',
                 !     >              ' NUMBER 60 - ERROR IN PARAMETERS')
                 return
              end if
              if (LEFTOV .gt.0) goto 40
           end if
        end if

     else

        !--     NOMINAL LOG SECTION - STACK BOTTOM LOGS THEN DEAL WITH
        !--     THE TOP ONE OR TWO LOGS

        LEFTOV = LMERCH-(INT(MAXLEN)*(NUMSEG-1))

        !--            SET ALL LOGS TO THE NOMINAL LOG LENGTH

        do I = 1,NUMSEG
           LOGLEN(I) = MAXLEN
           100 continue
        end do

        if (OPT .eq.21) then
           if (LEFTOV .ge. (MAXLEN/2.0)) then
              LOGLEN(NUMSEG) = LEFTOV
           else
              LOGLEN(NUMSEG) = INT((MAXLEN+LEFTOV)/2.0)
              LOGLEN(NUMSEG-1) = MAXLEN+LEFTOV-LOGLEN(NUMSEG)
              !--              IF BOTH SEGMENTS ARE ODD LENGTHS ADD ONE
              !--              TO LOWER SEGMENT AND SUBTRACT ONE FROM UPPER

              if (LOGLEN(NUMSEG) .eq. LOGLEN(NUMSEG-1) .and.&
                   &LOGLEN(NUMSEG).gt.INT(LOGLEN(NUMSEG)/2.0)*2.0) then

                 LOGLEN(NUMSEG) = LOGLEN(NUMSEG)-1.0
                 LOGLEN(NUMSEG-1) = LOGLEN(NUMSEG-1)+1.0
              end if
           end if
        else if (OPT .eq.22) then
           LOGLEN(NUMSEG) = INT((MAXLEN+LEFTOV)/2.0)
           LOGLEN(NUMSEG-1) = MAXLEN+LEFTOV-LOGLEN(NUMSEG)
           !             Check min log len for last log (3/25/14)
           if (LOGLEN(NUMSEG) .lt. MINLEN) then
              LOGLEN(NUMSEG) = 0
              LOGLEN(NUMSEG-1) = MAXLEN
              !             Reset the NUMSEG (YW 2015/10/08)
              NUMSEG = NUMSEG - 1
           else
              !              ENDIF

              !--              IF BOTH SEGMENTS ARE ODD LENGTHS ADD ONE
              !--              TO LOWER SEGMENT AND SUBTRACT ONE FROM UPPER

              if (LOGLEN(NUMSEG) .eq. LOGLEN(NUMSEG-1) .and.&
                   &LOGLEN(NUMSEG).gt.INT(LOGLEN(NUMSEG)/2.0)*2.0) then

                 LOGLEN(NUMSEG) = LOGLEN(NUMSEG)-1.0
                 LOGLEN(NUMSEG-1) = LOGLEN(NUMSEG-1)+1.0
              end if
           end if
        else if (OPT .eq.23) then
           if (LEFTOV .ge. MINLEN) then
              LOGLEN(NUMSEG) = LEFTOV
           else
              LOGLEN(NUMSEG) = 0
              !             Reset the NUMSEG (YW 2015/10/08)
              NUMSEG = NUMSEG - 1
           end if
        else if (OPT .eq.24) then
           if (LEFTOV .lt. (MAXLEN*.25)) then
              LOGLEN(NUMSEG) = 0
              !             Reset the NUMSEG (YW 2015/10/08)
              NUMSEG = NUMSEG - 1
           else if (LEFTOV .ge. (MAXLEN*.25) .and.&
                &LEFTOV .le. (MAXLEN*.75)) then
              LOGLEN(NUMSEG) = INT((MAXLEN*.5)+.5)
           else
              LOGLEN(NUMSEG) = MAXLEN
           end if
        end if
     end if
  end if

  return
end

  !--  EVOD - INTEGER - EVEN OR ODD LENGTH SEGMENTS ALLOWED
  !--         SEGMENTATION OPTIONS 11-14 ALLOW ODD LENGTHS BY
  !--         DEFINITION
  !--        1 = ODD SEGMENTS ALLOWED
  !--        2 = ONLY EVEN SEGMENTS ALLOWED

  !--  LOGLEN - REAL(20) - SEGMENT LENGTHS COMPUTED BY THIS
  !--                         PROGRAM DO NOT INCLUDE TRIM

  !--  NUMSEG - INTEGER - THE COMPUTED NUMBER OF SEGMENTS

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

  !--  LEFTOV - REAL - LEFTOVER OR FRACTIONAL PORTION OF A SEGMENT
  !--  LMERCH - REAL - GIVEN MERCHANTABLE LENGTH OF STEM AND
  !--                  MERCHANTABLE LENGTH OF STEM WITH TRIM REMOVED
  !--  MAXLEN - REAL - MAXIMUM SEGMENT LENGTH
  !--  MINLEN - REAL - MINIMUM SEGMENT LENGTH
  !--  TRIM - REAL - TRIM LENGTH FOR EACH SEGMENT
