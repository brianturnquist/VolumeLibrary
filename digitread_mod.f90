!===================================!
!   VolumeLibrary : digitread_mod   !
!===================================!
MODULE DIGITREAD_MOD

  !     Fast conversion between digit fields and integers.

  !     A formatted internal READ such as READ(VOLEQ(8:10),'(I3)') costs
  !     far more than the arithmetic it replaces, and the NSVB equations
  !     repeat several such reads for every tree. Such a READ uses the
  !     default BLANK='NULL' mode: blanks anywhere in the field are
  !     ignored, and an all-blank field reads as zero. DIGITREAD applies
  !     that rule to fields made only of the digits 0-9 and blanks, as in
  !     FIA codes stored as '202 ' or districts written as ' 1'. Every
  !     other field, including signs, letters and empty fields, is left to
  !     the caller's original READ statement. That READ keeps the original
  !     error messages and stop behaviour:

  !        IF (.NOT.DIGITREAD(VOLEQ(8:10),SPCD)) READ(VOLEQ(8:10),'(I3)') SPCD

  !     FIELD must be exactly the characters the fallback READ consumes, so
  !     LEN(FIELD) equals that READ's field width w; for an I3 READ of a
  !     4-character variable, pass its first three characters. The READ must
  !     use the default blank mode (no BZ edit descriptor or BLANK='ZERO').
  !     Fields wider than nine characters are also left to the READ, so a
  !     default integer cannot overflow here.

  implicit none

contains

  ! Return .TRUE. and set VALUE when FIELD holds only digits and blanks;
  ! otherwise return .FALSE. and leave VALUE unchanged for the fallback READ.
  logical function DIGITREAD(FIELD,VALUE)
    ! Variables
    character(len=*), intent(in) :: FIELD
    integer, intent(inout) :: VALUE
    integer :: I,DIGIT,TOTAL

    DIGITREAD = .false.
    if (len(FIELD).lt.1.or.len(FIELD).gt.9) return
    TOTAL = 0
    do I = 1, len(FIELD)
      ! BLANK='NULL': a blank contributes nothing to the value.
      if (FIELD(I:I).eq.' ') cycle
      DIGIT = ichar(FIELD(I:I)) - ichar('0')
      if (DIGIT.lt.0.or.DIGIT.gt.9) return
      TOTAL = 10*TOTAL + DIGIT
    end do
    VALUE = TOTAL
    DIGITREAD = .true.
  end function DIGITREAD

  ! The output counterpart of DIGITREAD. An internal WRITE such as
  ! WRITE(CONSPEC,'(I3)') FIASPCD right-justifies the digits in the first W
  ! characters and blanks the rest of the record. DIGITWRITE does the same for
  ! 0 <= VALUE < 10**W with W of one to nine characters, no wider than RECORD.
  ! Negative and overflowing values ('***') are left to the caller's original
  ! WRITE, as is any record shorter than W:

  !        IF (.NOT.DIGITWRITE(FIASPCD,CONSPEC,3)) WRITE(CONSPEC,'(I3)') FIASPCD

  ! W must equal the w of that plain Iw edit descriptor (no Iw.m or sign mode).
  ! RECORD is unchanged when .FALSE. is returned.
  logical function DIGITWRITE(VALUE,RECORD,W)
    ! Variables
    integer, intent(in) :: VALUE,W
    character(len=*), intent(inout) :: RECORD
    integer :: I,REST

    DIGITWRITE = .false.
    if (W.lt.1.or.W.gt.9.or.W.gt.len(RECORD)) return
    if (VALUE.lt.0.or.VALUE.ge.10**W) return
    RECORD = ' '
    REST = VALUE
    do I = W, 1, -1
      RECORD(I:I) = achar(ichar('0') + mod(REST,10))
      REST = REST/10
      if (REST.eq.0) exit
    end do
    DIGITWRITE = .true.
  end function DIGITWRITE

end MODULE DIGITREAD_MOD
