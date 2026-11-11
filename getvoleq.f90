!==============================!
!   VolumeLibrary : getvoleq   !
!==============================!
  ! last modified on 01-18-2013
  ! 01/18/2013 added GETFIAVOLEQ
subroutine GETVOLEQ2(REGN,FORSTC,DISTC,SPEC,PRODC,VOLEQC,ERRFLAG)
      !DEC$ ATTRIBUTES DLLEXPORT::GETVOLEQ2
  !      !DEC$ ATTRIBUTES ALIAS:'_GETVOLEQ2@28' :: GETVOLEQ2

  !REV  Created TDH 04/01/09
  !REV  Revised iii ../../..

  use CHARMOD
  use DEBUG_MOD

  implicit none

  ! Variables
  integer :: REGN
  TYPE(CHAR256):: FORSTC
  TYPE(CHAR256):: DISTC
  TYPE(CHAR256):: PRODC
  integer :: SPEC
  TYPE(CHAR256):: VOLEQC
  integer :: ERRFLAG

  !     Local Variables
  character(len=2) :: VAR
  character(FORSTC%LENGTH) :: FORST
  character(DISTC%LENGTH) :: DIST
  character(VOLEQC%LENGTH) :: VOLEQ
  character(PRODC%LENGTH) :: PROD

  !      IF (DEBUG%VOLEQ) THEN
  !         OPEN (UNIT=VEDBG, FILE='VEDebug.txt', STATUS='UNKNOWN')
  !           WRITE (VEDBG,1)'Debugging NVEL'
  !   1     FORMAT(A)
  !         WRITE  (VEDBG, 15) ' -->Enter GETVOLEQ2'
  !   15    FORMAT (A)
  !        END IF

  FORST = FORSTC%STR(1:FORSTC%LENGTH)
  DIST = DISTC%STR(1:DISTC%LENGTH)
  PROD = PRODC%STR(1:PRODC%LENGTH)
  VOLEQ = VOLEQC%STR(1:VOLEQC%LENGTH)

  VAR = '  '
  call VOLEQDEF(VAR,REGN,FORST,&
       &DIST,SPEC,PROD,VOLEQ,ERRFLAG)

  !      WRITE  (VEDBG, 16)var, forst, dist, prod, spec, voleq
  !   16 FORMAT (4A, 2X, I4, A)

  !conVert Char(*) back to CHAR256 for return to C++ wrapper including
  !a null terminator required by C strings.  Tack on the null terminator
  call CREATE_C_STRING(FORSTC, FORST)
  call CREATE_C_STRING(DISTC, DIST)
  call CREATE_C_STRING(VOLEQC, VOLEQ)
  call CREATE_C_STRING(PRODC, PROD)

  if (LEN_TRIM(VOLEQC%STR) .lt.10) then
     ERRFLAG = 1
     return
  end if

  !      IF (DEBUG%VOLEQ) THEN
  !         WRITE  (VEDBG, 25) ' <--Exit GETVOLEQ2'
  !   25    FORMAT (A)

  !         CLOSE(VEDBG)
  !        END IF

  return
  end subroutine GETVOLEQ2

  !    GETVOLEQ     - subroutine

subroutine GETVOLEQ(REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG)
  ! Expose subroutine VERNUM to users of this DLL

      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::GETVOLEQ
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG ::GETVOLEQ
  !      !DEC$ ATTRIBUTES ALIAS:'_GETVOLEQ@44' :: GETVOLEQ
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'GETVOLEQ'::GETVOLEQ

  !     CHARACTER*(1) FORST,PROD,DIST
  ! Variables
  character*(*) :: FORST,PROD,DIST

  character*(*):: VOLEQ
  character(len=2) :: VAR
  character(len=10) :: nvoleq
  integer :: SPEC,ERRFLAG,REGN

  !        print *, '--> enter getvoleq'
  !        print *, '    regn = ',regn, 'forst = ', forst
  !        print *, '    dist = ', dist
  !        print *, '*****************************'
  !        print *, '    spec = ', spec,'prod = ', prod, 'voleq = ', voleq
  VAR = '  '
  call VOLEQDEF(VAR,REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG)

  return
  end subroutine GETVOLEQ

  !    GETVOLEQ     - subroutine

subroutine GETVOLEQ3(REGN,FORSTI,DISTI,SPEC,PRODI,VOLEQI,ERRFLAG)
  ! Expose subroutine VERNUM to users of this DLL

      !DEC$ ATTRIBUTES DLLEXPORT::GETVOLEQ3
  !      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG ::GETVOLEQ
  !      !DEC$ ATTRIBUTES ALIAS:'_GETVOLEQ@44' :: GETVOLEQ

  implicit none

  ! Variables
  character*256,INTENT(INOUT):: FORSTI,PRODI,DISTI,VOLEQI
  character*(2) :: FORST,PROD,DIST

  character*(10):: VOLEQ
  character(len=2) :: VAR
  integer :: SPEC,ERRFLAG,REGN

  FORST = FORSTI(1:3)
  DIST = DISTI(1:3)
  PROD = PRODI(1:3)
  VOLEQ = VOLEQI(1:11)

  VAR = '  '
  call VOLEQDEF(VAR,REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG)

  FORSTI = FORST // char(0)
  DISTI = DIST // char(0)
  PRODI = PROD // char(0)
  VOLEQI = VOLEQ // char(0)

  return
  end subroutine GETVOLEQ3

  ! Added this routine to be used by biomass library c#
subroutine GETVARIANTCS(REGN,FORSTI,DISTI,VARI)
      !DEC$ ATTRIBUTES DLLEXPORT::GETVARIANTCS
  implicit none

  ! Variables
  character*256,INTENT(INOUT):: FORSTI,DISTI,VARI
  character*(2) :: FORST,DIST

  character(len=2) :: VAR
  integer :: REGN

  FORST = FORSTI(1:3)
  DIST = DISTI(1:3)
  call GETVARIANT(REGN,FORST,DIST,VAR)
  FORSTI = FORST // char(0)
  DISTI = DIST // char(0)
  VARI = VAR // char(0)
  return
  end subroutine GETVARIANTCS

subroutine GETFIAVOLEQ(REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
  ! The routine used by Excel function to get FIA volume equation for PNW
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::GETFIAVOLEQ
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG ::GETFIAVOLEQ
  !      !DEC$ ATTRIBUTES ALIAS:'_GETVOLEQ@44' :: GETVOLEQ
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'GETFIAVOLEQ'::GETFIAVOLEQ

  ! Variables
  character*(*) :: FORST,DIST

  character*(*):: VOLEQ
  character(len=2) :: VAR
  !      CHARACTER*10 nvoleq
  integer :: SPEC,ERRFLAG,REGN

  !        print *, '--> enter getFIAvoleq'
  !        print *, '    regn = ',regn, 'forst = ', forst
  !        print *, '    dist = ', dist
  !        print *, '*****************************'
  !        print *, '    spec = ', spec,'prod = ', prod, 'voleq = ', voleq
  VAR = '  '
  call FIAVOLEQDEF(VAR,REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)

  return
  end subroutine GETFIAVOLEQ

subroutine getvoleq_r(REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
  ! This subroutine is for R user to get default equation from vollib      !
  ! YW 02/10/2017
      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::getvoleq_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'getvoleq_r_'::getvoleq_r

  ! Variables
  character*(*) :: FORST,DIST

  character*(*):: VOLEQ
  character(len=2) :: VAR
  character(len=2) :: PROD
  integer :: SPEC,ERRFLAG,REGN

  VAR = '  '
  call VOLEQDEF(VAR,REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG)

  return
  end subroutine getvoleq_r

subroutine GETNVBEQ(REGN,FORSTI,DISTI,SPEC,VOLEQI,ERRFLAG)
  ! Expose subroutine GETNVBEQ to users of this DLL
  ! This is for C# application
      !DEC$ ATTRIBUTES DLLEXPORT::GETNVBEQ
  implicit none
  ! Variables
  character*256,INTENT(INOUT):: FORSTI,DISTI,VOLEQI
  character*(2) :: FORST,DIST
  character*(10):: VOLEQ
  integer :: SPEC,ERRFLAG,REGN
  FORST = FORSTI(1:3)
  DIST = DISTI(1:3)
  VOLEQ = VOLEQI(1:11)
  call NVB_DefaultEq(REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
  FORSTI = FORST // char(0)
  DISTI = DIST // char(0)
  VOLEQI = VOLEQ // char(0)
  return
  end subroutine GETNVBEQ

subroutine GETNVBEQ2(REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
  ! Expose subroutine GETNVBEQ2 to users of this DLL
  ! This is for Excel VBA application
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::GETNVBEQ2
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG ::GETNVBEQ2
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'GETNVBEQ2'::GETNVBEQ2
  ! Variables
  character*(*) :: FORST,DIST
  character*(*):: VOLEQ
  integer :: SPEC,ERRFLAG,REGN

  call NVB_DefaultEq(REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
  return
  end subroutine GETNVBEQ2

subroutine getnvbeq_r(REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
  ! This subroutine is for R user to get default NSVB equation from vollib      !
  ! YW 02/16/2023
      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::getnvbeq_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'getnvbeq_r_'::getnvbeq_r
  ! Variables
  character*(*) :: FORST,DIST
  character*(*):: VOLEQ
  integer :: SPEC,ERRFLAG,REGN
  call NVB_DefaultEq(REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
  return
  end subroutine getnvbeq_r
