!==================================!
!   VolumeLibrary : volinput_mod   !
!==================================!
MODULE VOLINPUT_MOD
  implicit  none

  ! Variables
  integer :: FORMCLASS
  character(len=1) :: MRULEMOD = 'N'
  integer :: NEWEVOD
  integer :: NEWOPT
  real    :: NEWMAXLEN,NEWMINLEN,NEWMINLENT,NEWMERCHL,NEWMTOPP
  real    :: NEWMTOPS,NEWSTUMP,NEWTRIM,NEWBTR,NEWDBTBH,NEWMINBFD
  character(len=1) :: NEWCOR = 'Y'

  end MODULE VOLINPUT_MOD
