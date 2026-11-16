!===================================!
!   VolumeLibrary : digitread_mod   !
!===================================!
MODULE DIGITREAD_MOD

  !     Fast conversion of an all-digit field to an integer.

  !     A formatted internal READ such as READ(VOLEQ(8:10),'(I3)') costs
  !     far more than the arithmetic it replaces, and the NSVB equations
  !     repeat several such reads for every tree. DIGITREAD converts only
  !     fields made entirely of the digits 0-9, where the result of an
  !     Iw read is simply the decimal value. Every other field, including
  !     blanks, signs, letters and empty fields, is left to the caller's
  !     original READ statement, which keeps the original blank handling,
  !     error messages and stop behaviour:

  !        IF (.NOT.DIGITREAD(VOLEQ(8:10),SPCD)) READ(VOLEQ(8:10),'(I3)') SPCD

  !     FIELD must be exactly the substring the fallback READ consumes, so
  !     LEN(FIELD) equals that READ's field width w. Fields wider than nine
  !     characters are also left to the READ, so a default integer cannot
  !     overflow here.

  implicit none

contains

  ! Return .TRUE. and set VALUE when FIELD holds only digits; otherwise
  ! return .FALSE. and leave VALUE unchanged for the fallback READ.
  logical function DIGITREAD(FIELD,VALUE)
    ! Variables
    character(len=*), intent(in) :: FIELD
    integer, intent(inout) :: VALUE
    integer :: I,DIGIT,TOTAL

    DIGITREAD = .false.
    if (len(FIELD).lt.1.or.len(FIELD).gt.9) return
    TOTAL = 0
    do I = 1, len(FIELD)
      DIGIT = ichar(FIELD(I:I)) - ichar('0')
      if (DIGIT.lt.0.or.DIGIT.gt.9) return
      TOTAL = 10*TOTAL + DIGIT
    end do
    VALUE = TOTAL
    DIGITREAD = .true.
  end function DIGITREAD

end MODULE DIGITREAD_MOD
