!=================================!
!   VolumeLibrary : regndft_mod   !
!=================================!
MODULE REGNDFT_MOD

  !     Module to store Regional species default variable

  !     Created YW 06/1/15

  TYPE REGNDFT
     SEQUENCE
     ! Variables
     integer :: REGN
     integer :: FORST
     integer :: SPCD
     real    :: WF1
     real    :: WF2
     real    :: MC

     character(len=12) :: AGT
     character(len=12) :: LBR
     character(len=12) :: DBR
     character(len=12) :: FOL
     character(len=12) :: TIP

  end TYPE REGNDFT

  end MODULE REGNDFT_MOD
