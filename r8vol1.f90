!============================!
!   VolumeLibrary : r8vol1   !
!============================!
  !== last modified  10-26-2005
subroutine R8VOL1(VOLEQ,DBHOB,HTONE,HTTWO,PROD,VOL,ERRFLAG)

  !  CREATED:   05-28-91
  !  PURPOSE:   THIS ROUTINE IS FOR CALCULATING REGION 8 VOLUMES FOR
  !             EQUATION NUMBERS 1 THROUGH 799.

  implicit none

  !  DECLARE VARIABLES

  ! Variables
  character(len=2) :: PROD
  !      CHARACTER*4 SPC
  !     CHARACTER*8 READIT
  character(len=10) :: VOLEQ
  !character*19 atmp1(2592)

  integer :: ERRFLAG,DONEFLAG,LAST,HALF,LASTFLAG
  integer :: GC1
  integer :: SPEC,VSPEC,FIRST
  integer :: COEFEQ(4)

  real    :: DBHOB,HTONE,HTTWO,VEQ
  real    :: VOL(15)
  !COFARR(162,10)

  ! Common blocks
  include 'r8clist.inc'
  include 'r8vlist.inc' !'R8VLIST.inc'

  !      INCLUDE 'COMM0'

  !      DATA READIT / ' ' /

  !      IF (READIT .NE. 'COEFREAD') THEN
  !        tmp1 = 10
  !        OPEN (TMP1,FILE='R8CLIST')
  !       OPEN (TMP1,FILE='R8CLIST',PAD='YES')

  !        DO 10 I = 1,162
  !          READ (TMP1,*) (COFARR(I,J),J=1,10)
  ! 10     CONTINUE

  !        CLOSE (TMP1)

  !        READIT = 'COEFREAD'

  !        OPEN (TMP1,FILE='R8VLIST',ACCESS='DIRECT',STATUS='OLD',
  !     >        FORM='FORMATTED',RECL=21)

  !      ENDIF

  read(VOLEQ(8:10),'(I3)') VSPEC

  if (vspec.eq.268) vspec = 261

  if (VOLEQ(4:6).eq.'DVE' .or. VOLEQ(4:6).eq.'dve') then
     read (VOLEQ(2:3),'(I2)')GC1
  else
     GC1 = 0
  end if

  DONEFLAG = 0
  LASTFLAG = 0
  if (GC1.eq.0.or.GC1.ge.80) go to 998

  FIRST = ((GC1-1)*81)+2
  LAST = FIRST + 80
  do 5, while (DONEFLAG.eq.0)
  !DID NOT FIND A MATCH
  if (FIRST.eq.LAST)LASTFLAG = 1
  !DETERMINE WHERE TO CHECK
  HALF = ((LAST-FIRST+1)/2) + FIRST
  read (ATMP1(HALF),'(I3,4I4)') SPEC,&
       &(COEFEQ(J),J = 1,4)
  !FOUND THE COEFFECIENTS
  if (VSPEC.eq.SPEC) then
     DONEFLAG = 1
     !MOVE DOWN THE LIST
  else if (VSPEC.gt.SPEC) then
     FIRST = HALF
     !MOVE UP THE LIST
  else if (VSPEC.lt.SPEC) then
     LAST = HALF - 1
  end if
  !DID NOT FIND A MATCH
  if (LASTFLAG.eq.1.and. DONEFLAG.eq.0) go to 998
  5 continue

  !     END BINARY SEARCH
  if (PROD.eq.'01') then
     VOL(2) = VEQ (DBHOB,HTONE,COFARR(COEFEQ(1),1),&
          &COFARR(COEFEQ(1),2),COFARR(COEFEQ(1),3),&
          &COFARR(COEFEQ(1),4),COFARR(COEFEQ(1),5),&
          &COFARR(COEFEQ(1),6),COFARR(COEFEQ(1),7),&
          &COFARR(COEFEQ(1),8),COFARR(COEFEQ(1),9),&
          &COFARR(COEFEQ(1),10),ERRFLAG)
     if (VOL(2).lt.10.0) VOL(2) = 10.0
     VOL(3) = VOL(2)

     VOL(4) = VEQ (DBHOB,HTONE,COFARR(COEFEQ(2),1),&
          &COFARR(COEFEQ(2),2),COFARR(COEFEQ(2),3),&
          &COFARR(COEFEQ(2),4),COFARR(COEFEQ(2),5),&
          &COFARR(COEFEQ(2),6),COFARR(COEFEQ(2),7),&
          &COFARR(COEFEQ(2),8),COFARR(COEFEQ(2),9),&
          &COFARR(COEFEQ(2),10),ERRFLAG)
     VOL(5) = VOL (4)

     VOL(7) = VOL(4) *(VEQ (DBHOB,HTONE,COFARR(COEFEQ(4),1),&
          &COFARR(COEFEQ(4),2),COFARR(COEFEQ(4),3),&
          &COFARR(COEFEQ(4),4),COFARR(COEFEQ(4),5),&
          &COFARR(COEFEQ(4),6),COFARR(COEFEQ(4),7),&
          &COFARR(COEFEQ(4),8),COFARR(COEFEQ(4),9),&
          &COFARR(COEFEQ(4),10),ERRFLAG) - 1.0)
     VOL(8) = VOL (7)

  else if (PROD.eq.'02') then

     VOL(4) = VEQ (DBHOB,HTTWO, COFARR(COEFEQ(3),1),&
          &COFARR(COEFEQ(3),2),COFARR(COEFEQ(3),3),&
          &COFARR(COEFEQ(3),4),COFARR(COEFEQ(3),5),&
          &COFARR(COEFEQ(3),6),COFARR(COEFEQ(3),7),&
          &COFARR(COEFEQ(3),8),COFARR(COEFEQ(3),9),&
          &COFARR(COEFEQ(3),10),ERRFLAG)
     VOL(5) = VOL (4)
     go to 999
  else
     go to 998
  end if

  go to 999

  998 ERRFLAG = 6

  999 return
end

function VEQ (DBHOB,HT,MODEL,B0,B1,B2,B3,B4,B5,B6,B7,B8,ERRFLAG)

  !      CHARACTER*2 ST
  ! Variables
  integer :: ERRFLAG
  real    :: D,DBHOB,HT,D2H,VTEMP,VEQ
  real    :: MODEL,B0,B1,B2,B3,B4,B5,B6,B7,B8

  !      INCLUDE 'COMM0'

  VEQ = 0.0

  D2H = DBHOB * DBHOB * HT
  if (D2H.le.0) then
     ERRFLAG = 5
     go to 999
  end if

  VTEMP = 0.0
  D = DBHOB

  if (MODEL.eq.1) VTEMP = B0 + B1*D2H
  if (MODEL.eq.2) VTEMP = B0 + B1/D + B2*D*HT +&
       &B3*D2H
  if (MODEL.eq.3) VTEMP = B0 + B1*D*D + B2*D*HT*HT +&
       &B3*D2H + B4*HT*HT*HT
  if (MODEL.eq.5) VTEMP = B0 + B1*D + B2*D*D + B3*HT*HT +&
       &B4*D2H + B5*D*D2H
  if (MODEL.eq.6) VTEMP = B0 + B1*D*D +&
       &B2*HT + B3*D2H
  if (MODEL.eq.7) VTEMP = EXP (B0 * (HT ** B1) *&
       &((1.0 - (4.0 / (0.78 * D)) ** 2.0) ** 2.0) ** B2)
  if (MODEL.eq.9) VTEMP = B0 + B1*D2H/1000.0 +&
       &B2*D*HT*HT/1000.0 + B3*D*D + B4*HT*HT*HT/10000.0 +&
       &B5*D*D/HT + B6*D*D*HT*HT/100000.0 +&
       &B7*D2H*HT*HT/100000000 + B8*D*HT/100.0

  VEQ = VTEMP

  999 return
end
