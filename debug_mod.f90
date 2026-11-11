!===============================!
!   VolumeLibrary : debug_mod   !
!===============================!
MODULE DEBUG_MOD

  !...  Module to store NVEL debug data

  implicit  none

  !CREV  Created TDH 04/01/09
  !CREV  Revised TDH 11/18/09
  !CREV  added VEDBG unit number for debugging the getvoleq subroutine

  TYPE :: DEBUG_INDICATORS
     ! Variables
     logical :: INPUT ! LBP input debug
     logical :: DLL ! LBP DLL debug
     logical :: MODEL ! LBP model debug
     logical :: ANALYSIS ! LBP analysis debug
     logical :: external ! Ext subroutine call debug
     logical :: VOLEQ ! LBP design debug
  end TYPE DEBUG_INDICATORS

  !      CHARACTER DEBUG_FILE_NAME*160
  !      CHARACTER SA_DEBUG_FILE_NAME*160
  !      LOGICAL   DEBUG_FILE_OPEN    / .FALSE. /
  logical :: ANY_DEBUG = .false.

  integer :: LUDBG = 1 ! Debug report unit number
  integer :: VEDBG = 3 ! Vol Eq report unit number
  integer :: LUOUT = 2 ! Ouput report unit number
  TYPE(DEBUG_INDICATORS) DEBUG

  end MODULE DEBUG_MOD
