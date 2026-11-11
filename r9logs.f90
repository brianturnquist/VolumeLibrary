!============================!
!   VolumeLibrary : r9logs   !
!============================!
  !  subroutine for calculating region log volumes

  !  Created TDH 11/20/09

  !  Revised TDH 07/26/10
  !  Added a check to makes sure pulp height > 0 and > sawht
  !  was causing problems in the loglen variable and subsequent
  !  boardfoot calcs

  !  Revised TDH 03/03/2011
  !  Made changes to how R9LOGLEN was calculating top piece(LEFTOV)
  !  Made it so you get volume for a leftover piece of secondary
  !  change made on line 115

  !  Revised YW 12/08/2011
  !  Added check number of logs not greater than 20 in r9logs and write round
  !  DBH to logdia(1,1).

  !  Revised YW 08/21/2012
  !  Added ERRFLG to R9LOGS, R9LOGDIB and R9LOGLEN subroutines.
  !  YW 04/13/2017 Modified R9LOGS to also calculate logs for topwood for sawtimber
  !  YW 01/18/2018 Removed the changes made on 4/13/2017.
  !  YW 05/30/2019 R9 do want logs calculation for the topwood of the sawtimber.
  !                So I put the change back again.
  !  YW 03/11/2021 When topwood not long for a min log, set NOLOGS to 0 and continue
  !  YW 2023/08/18 Corrected topwood log calculation for only LEFTOV > MINLEN
  !_______________________________________________________________________

subroutine R9LOGS(SAWHT, PLPHT, STUMP, MINLEN, MAXLEN, TRIM,&
     &LOGLEN,LOGDIA,NOLOGP,NOLOGS,TLOGS,COEFFS,ERRFLG,BOLHT)
  !_______________________________________________________________________

  use DEBUG_MOD
  use CLKCOEF_MOD

  implicit none

  !...  Parameters
  ! Variables
  real    :: SAWHT,PLPHT,STUMP,MINLEN,MAXLEN,TRIM
  real    :: LOGLEN(20),LOGDIA(21,3),BOLHT(21)
  integer :: NOLOGP,NOLOGS,TLOGS,NUMSEG,ERRFLG
  TYPE(CLKCOEF):: COEFFS

  !...  Local Variables
  integer :: ILOG,JLOG,I
  real    :: LMERCH,LEFTOV
  ERRFLG = 0

  if (DEBUG%MODEL) then
     write  (LUDBG, 100) ' -->Enter R9LOGS'
     100 format (A)
     write  (LUDBG, 125)' SAWHT  PLPHT MINLEN MAXLEN TRIM'
     125 format (A)
     write  (LUDBG, 150)SAWHT, PLPHT, MINLEN, MAXLEN, TRIM
     150 format (5F6.1)
  end if
  !     Check number of logs
  if (SAWHT .gt.0) then
     LMERCH = SAWHT-STUMP
     NOLOGP = INT(LMERCH/(MAXLEN+TRIM))
     NUMSEG = NOLOGP
  else
     LMERCH = PLPHT-STUMP
     NOLOGS = INT(LMERCH/(MAXLEN+TRIM))
     NUMSEG = NOLOGS
  end if
  if (NUMSEG .gt.20) then
     ERRFLG = 12
     return
  end if
  !------ Sawtimber segmentation----------------------------------------
  if (SAWHT .gt.0) then

     !...saw portion

     LMERCH = SAWHT-STUMP

     NOLOGP = INT(LMERCH/(MAXLEN+TRIM))

     LEFTOV = LMERCH-((MAXLEN+TRIM)*FLOAT(NOLOGP))-TRIM

     !check for saw logs
     if (.not.(LMERCH.lt. (MINLEN+TRIM)& !.OR. NOLOGP.LE.0
          &.or. (NOLOGP .eq.0.and. LEFTOV.lt.(MINLEN+TRIM)))) then

        ILOG = 1
        JLOG = NOLOGP
        call R9LOGLEN(ILOG, JLOG, NOLOGP, MINLEN, MAXLEN, TRIM,&
             &LOGLEN, LEFTOV, ERRFLG)
        if (ERRFLG .ne.0) return
        if (DEBUG%MODEL) then
           write  (LUDBG, 200)'LMERCH  NOLOGP LLEN(1) LLEN(TOP)'&
                &//' LEFTOV'
           200 format (A)
           write  (LUDBG, 220)LMERCH, NOLOGP, LOGLEN(1),&
                &LOGLEN(NOLOGP), LEFTOV
           220 format (F6.1,4X I2, 3F7.1)
        end if
     end if
     !...TOP WOOD PORTION
     !first check to see if there's top wood
     !make sure there's a plpht value and it > sawht
     !        ELSE IF (PLPHT > 0 .AND. PLPHT .GT. SAWHT) THEN
     if (PLPHT > 0.and. PLPHT .gt. SAWHT) then
        SAWHT = STUMP
        if (NOLOGP.gt.0) then
           do 250, I = 1,NOLOGP
           SAWHT = SAWHT + LOGLEN(I) + TRIM
           250 continue
        end if
        LMERCH = PLPHT - SAWHT
        NOLOGS = INT(LMERCH/(MAXLEN+TRIM))
        !         The LEFTOVER need to be calculate here (5/22/2017)
        LEFTOV = LMERCH-((MAXLEN+TRIM)*FLOAT(NOLOGS))-TRIM

        !do we need a check for nologs >0???
        if (LMERCH.lt. (MINLEN+TRIM)& !.OR. NOLOGS.LE.0
             &.or. (NOLOGS.eq.0.and. LEFTOV.lt.(MINLEN+TRIM)) ) then
           !RETURN
           NOLOGS = 0
        end if
        !Also check LEFTOV with MINLEN 2023/08/16
        if (NOLOGS.gt.0.or.LEFTOV.gt.(MINLEN+TRIM)) then
           ILOG = NOLOGP + 1
           JLOG = ILOG + NOLOGS - 1
           if (JLOG .gt.20) then
              ERRFLG = 12
              return
           end if
           call R9LOGLEN(ILOG, JLOG, NOLOGS, MINLEN, MAXLEN, TRIM,&
                &LOGLEN, LEFTOV, ERRFLG)
           if (ERRFLG .ne.0) return
        end if
        if (DEBUG%MODEL) then
           write  (LUDBG, 300)'LMERCH  NOLOGS ilog  jlog'&
                &//' LEFTOV'
           300 format (A)
           write  (LUDBG, 320)LMERCH, NOLOGS, ilog,&
                &jlog,LEFTOV
           320 format (F6.1, 4X, I2, 2I4, 2X, F7.1)
        end if
     end if

     !------ non-saw timber segmentation----------------------------------

  else !...sawht <= 0

     LMERCH = PLPHT-STUMP
     NOLOGP = 0
     NOLOGS = INT(LMERCH/(MAXLEN+TRIM))

     LEFTOV = LMERCH-((MAXLEN+TRIM)*FLOAT(NOLOGS))-TRIM

     !IF(LEFTOV .GT. (MINLEN+TRIM) .AND. NOLOGS .EQ. 0) NOLOGS = 1

     if (LMERCH.lt. (MINLEN+TRIM)& !.OR. NOLOGS.LE.0
          &.or. (NOLOGS.eq.0.and. LEFTOV.lt.(MINLEN+TRIM)) ) then
        !  RETURN
        NOLOGS = 0
     end if
     if (NOLOGS.gt.0) then
        ILOG = 1
        JLOG = NOLOGS
        call R9LOGLEN (ILOG, JLOG, NOLOGS, MINLEN, MAXLEN, TRIM,&
             &LOGLEN, LEFTOV, ERRFLG)
        if (ERRFLG .ne.0) return
     end if
  end if

  TLOGS = INT(NOLOGP + NOLOGS)

  call R9LOGDIB(TLOGS, TRIM, STUMP, LOGLEN, LOGDIA, COEFFS,BOLHT)

  !...      WRITE OUT ALL LOGLEN/LOGDIA TO DEBUG FILE
  if (DEBUG%MODEL) then
     do I = 1,TLOGS
        write  (LUDBG, 600)'LOGDIA ', I, LOGDIA(I,1),'LOGLEN ', I,&
             &LOGLEN(I)
        600 format (A, I2, F6.1, 2X, A,I2, F6.1)
        650 continue
     end do
  end if

  if (DEBUG%MODEL) then
     write  (LUDBG, 1000) ' <--EXIT R9LOGS'
     1000 format (A)
  end if

  end subroutine R9LOGS

  !  subroutine for calculating region 9 log lenghts

  ! Created TDH 11/20/09

  !  Revised TDH 12/21/09
  !  Fixed bug that was incorrectly calculating num segs.

  !_______________________________________________________________________

subroutine R9LOGLEN(ILOG, JLOG, NUMSEG, MINLEN, MAXLEN, TRIM,&
     &LOGLEN, LEFTOV,ERRFLG)
  !_______________________________________________________________________

  use DEBUG_MOD

  implicit none

  !...  Parameters
  ! Variables
  integer :: ILOG,JLOG,NUMSEG,ERRFLG
  real    :: MINLEN,MAXLEN,TRIM
  real    :: LOGLEN(20),LEFTOV

  !...  Local variables
  integer :: I
  real    :: HT

  HT = 0.

  if (DEBUG%MODEL) then
     write  (LUDBG, 100) ' -->Enter R9LOGLEN'
     100 format (A)
  end if

  if (DEBUG%MODEL) then
     write  (LUDBG, 120)'ILOG  JLOG numseg MAXLEN TRIM  LEFTOV'
     120 format (A)
     write  (LUDBG, 130)ILOG, JLOG, numseg, MAXLEN,TRIM,LEFTOV
     130 format (3I5,F6.1, F7.1, 2X, F7.1)
  end if

  if (JLOG .gt.0) then
     if (JLOG .gt.20) then
        ERRFLG = 12
        return
     end if
     do I = ILOG,JLOG
        LOGLEN(I) = MAXLEN
        ht = ht+trim+logLen(i)
        200 continue
     end do
  end if
  if (DEBUG%MODEL) then
     write (LUDBG, 600)'HT ', HT
     600 format (A,F7.1)
  end if

  if (LEFTOV.ge.(MINLEN+TRIM)) then
     NUMSEG = NUMSEG+1
     JLOG = JLOG+1
     if (JLOG .gt.20) then
        ERRFLG = 12
        return
     end if
     LOGLEN(JLOG) = LEFTOV !OR IS IT MINLEN????
  end if

  !..   SAWLOG TREES MAY GET TOP TWO LOGS RESEGMENTED (WHOLE FEET)
  !Added to make sure EVEN log lengths (2025/03/07)
  if (NUMSEG.eq.1) then
     LOGLEN(ILOG) = INT(LOGLEN(ILOG))
     LOGLEN(ILOG) = INT(LOGLEN(ILOG)/2)*2
  else if (LEFTOV.lt.MINLEN) then
     LOGLEN(JLOG) = INT(LOGLEN(JLOG))
     LOGLEN(JLOG) = INT(LOGLEN(JLOG)/2)*2
  else
     LOGLEN(JLOG) = INT((MAXLEN+LEFTOV)/2)
     LOGLEN(JLOG) = INT(LOGLEN(JLOG)/2)*2
     LOGLEN(JLOG-1) = INT(MAXLEN+LEFTOV-LOGLEN(JLOG))
     LOGLEN(JLOG-1) = INT(LOGLEN(JLOG-1)/2)*2
  end if

  if (DEBUG%MODEL) then
     write  (LUDBG, 1000) ' <--Exit R9LOGLEN'
     1000 format (A)
  end if

  end subroutine R9LOGLEN

  !  subroutine for calculating r9 log dib's

  !  Created TDH 11/20/09

  !  Revised ... ../../..

  !_______________________________________________________________________

subroutine R9LOGDIB(NUMSEG, TRIM, STUMP, LOGLEN, LOGDIA,&
     &COEFFS,BOLHT)
  !_______________________________________________________________________

  use CLKCOEF_MOD

  implicit none

  !...  Parameters
  ! Variables
  integer :: NUMSEG,I
  real    :: TRIM,STUMP,LOGLEN(20),LOGDIA(21,3)
  real    :: DIB,BOLHT(21)
  TYPE(CLKCOEF):: COEFFS

  !...  Local variables
  real    :: HT

  !-----Get diameters at log ends
  if (NUMSEG.gt.0) then
     !--     Get DIB at 4.5'
     HT = 4.5
     call R9DIB(DIB,HT,COEFFS)
     LOGDIA(1,2) = DIB
     LOGDIA(1,1) = INT(DIB+0.499)
     !        LOGDIA(1,3)= 0.0
     BOLHT(1) = HT
     !--     Get DIB at all log ends
     HT = STUMP
     do I = 1,NUMSEG
        HT = HT+TRIM+LOGLEN(I)
        BOLHT(I+1) = HT

        !..       don't need below for logs methinks
        !--       Never let height get above sawtimber height
        !...         if(ht.gt.sawHt) then
        !...         logLen(i)=logLen(i)-(ht-sawHt)
        !...         ht=sawHt
        !...          endif

        call R9DIB(DIB,HT,COEFFS)
        LOGDIA(I+1,2) = DIB
        LOGDIA(I+1,1) = INT(DIB+0.499)
        !...          LOGDIA(I+1,3)= 0.0

        850 continue
     end do
  end if

  end subroutine R9LOGDIB

  !  subroutine for calculating weighted r9 log volumes

  !  Created TDH 11/24/09

  !  Revised ... ../../..

  !_______________________________________________________________________

subroutine R9LGCFT(TLOGS, LOGLEN, LOGDIA, LOGVOL,TLOGVOL,TCFVOL)
  !_______________________________________________________________________

  use DEBUG_MOD

  implicit none

  !..   Parameters
  ! Variables
  integer :: TLOGS
  real    :: LOGLEN(20),LOGDIA(21,3),LOGVOL(7,20),TLOGVOL,TCFVOL

  !...  Local variables
  integer :: I

  TLOGVOL = 0.0

  if (DEBUG%MODEL) then
     write  (LUDBG, 100) ' -->Enter R9LOGCUFT'
     100 format (A)
     write  (LUDBG, 125)' TLOGS TLOGVOL TCFVOL'
     125 format (A)
     write  (LUDBG, 150)TLOGS, TLOGVOL, TCFVOL
     150 format (I4, 2F8.1)
  end if

  do I = 1,TLOGS
     LOGVOL(4,I) = 0.00272708*(LOGDIA(I,2)**2 + LOGDIA(I+1,2)**2)&
          &* LOGLEN(I)

     if (DEBUG%MODEL) then
        write  (LUDBG, 175)'LOGDIA I', LOGDIA(I,2),' LOGDIA I+1',&
             &LOGDIA(I+1,2)
        175 format (A,F6.1,A,F6.1)
     end if

     TLOGVOL = TLOGVOL + LOGVOL(4,I)
     200 continue
  end do

  !..   Calculate percentage of total volume in each log and scale back
  !..   to total integrated cubic volume so they match
  do I = 1,TLOGS
     LOGVOL(4,I) = (LOGVOL(4,I)/TLOGVOL) * TCFVOL
     250 continue
  end do

  TLOGVOL = 0.0
  !..   Now sum the "weighted" log volumes to get total log volume
  do I = 1,TLOGS
     TLOGVOL = TLOGVOL + LOGVOL(4,I)
     350 continue
  end do

  if (DEBUG%MODEL) then
     write  (LUDBG, 425)' TLOGS TLOGVOL TCFVOL'
     425 format (A)
     write  (LUDBG, 450)TLOGS, TLOGVOL, TCFVOL
     450 format (I4, 2F8.1)
     write  (LUDBG, 500) ' -->Exit R9LOGCUFT'
     500 format (A)
  end if

  end subroutine R9LGCFT
