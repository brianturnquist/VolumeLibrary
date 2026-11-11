!=============================!
!   VolumeLibrary : charmod   !
!=============================!
MODULE CHARMOD

  !     Module to store utility type definitions

  !     Created TEH 02/24/09
  !     Revised TDH 02/27/09 .

  !     Class for receiving strings in calls from C++
  TYPE CHAR256
     SEQUENCE
     ! Variables
     integer :: LENGTH
     character(256) :: STR
  end TYPE CHAR256

  end MODULE CHARMOD
