!================================!
!   VolumeLibrary : mrules_mod   !
!================================!
MODULE MRULES_MOD

  !     Module to store merch rules

  !     Created TDH 06/17/09
  !     Revised ... DD/MM/YY

  !     Class for passing/receiving merch rules in calls from C++
  TYPE MERCHRULES
     SEQUENCE
     ! Variables
     integer :: EVOD
     integer :: OPT

     real    :: MAXLEN
     real    :: MINLEN
     real    :: MINLENT
     real    :: MERCHL
     real    :: MTOPP
     real    :: MTOPS

     real    :: STUMP
     real    :: TRIM
     real    :: BTR
     real    :: DBTBH
     real    :: MINBFD

     character(len=1) :: COR

  end TYPE MERCHRULES

  end MODULE MRULES_MOD
