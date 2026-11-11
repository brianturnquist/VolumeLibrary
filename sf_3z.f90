!===========================!
!   VolumeLibrary : sf_3z   !
!===========================!
  !== last modified  10-30-2003
subroutine SF_3Z(JSP,GEOSUB,SETOPT,NEXTRA,DBH,TOTALH,HEX,ZEX,&
     &RFLW,RHFW,TAPCOE,F,PINV_Z,FMOD)
  !     given standardized errors at 1 or 2 heights besides BH, perform
  !     necessary calculations prior to estimating all conditional diameters
  !     This routine assumes that NEXTRA, hextra(*) and dobex(*) are filled
  !           in with correct values.

  !     NEXTRA    Input   I       Number of extra measurement points (besides HT, DBH)
  !                            (currently must = 1 or 2)
  !     HEX    Input   R*4()   Vector of extra measurement heights
  !     ZEX    Input   R*4()   Standardized errors at those heights.

  !      LOGICAL METRIC
  ! Variables
  character(len=2) :: GEOSUB
  integer :: SETOPT(6),JSP,NEXTRA,INEEDSL,JX
  real*4  :: DBH,TOTALH,BH,SLOPE

  real*4  :: HEX(2),ZEX(2),RH,DIBMOD,SF_YHAT,F,DIBACT,ABS_CHNG,SF_CORR
  real*8  :: P12,P12SQ
  real*8  :: P_INV(2,2)
  real*4  :: RFLW(6),RHFW(4),TAPCOE(12)

  real    :: FMODMAX,HFIRSTUP,FMODMAXU,FMOD(3)
  real    :: HEXTRA(2),Z(2)
  real    :: pinv_z(2)
  !      Logical ivstat_2, IVSTAT_S

  INEEDSL = 0
  if (NEXTRA.lt.1.or. NEXTRA.gt.2) then
     !            write(*,1) NEXTRA
     !1           format(' Subroutine SF_3z given invalid NEXTRA value =',i4/
     !     1             ' Please correct the calling program. STOP. ')
  end if
  BH = 4.5
  if ( SETOPT(1).eq.1) then
     fmodmax = .15
     HFIRSTUP = 0.0
  end if

  do JX = 1,NEXTRA
     HEXTRA(jx) = HEX(jx)
     Z(jx) = ZEX(jx)
     if (setopt(1) .eq.1) then
        RH = HEXTRA(JX)/TOTALH
        DIBmod = SF_YHAT(JSP,RH,TOTALH,ineedsl,slope,RHFW,RFLW,&
             &TAPCOE,F)
        call SF_DFZ(JSP,GEOSUB,DBH,TOTALH,HEXTRA(JX),DIBmod,&
             &Z(JX),DIBact)
        ABS_CHNG = ABS(DIBmod - DIBact)
        FMODMAX = MAX(FMODMAX, ABS_CHNG / DIBmod)
        if (HEX(jx).gt. bh ) then
           if (hfirstup .eq.0.0.or. HEX(jx).lt. hfirstup) then
              fmodmaxu = min(dibmod, 2.0*abs_chng)/dibmod
              Hfirstup = HEX(jx)
           end if
        end if
        FMOD(1) = FMODMAX
        FMOD(2) = FMODMAXU
        FMOD(3) = HFIRSTUP
     end if
     100 continue
  end do

  if (NEXTRA.eq.2) then
     P12 = SF_corr(JSP,GEOSUB,TOTALH, HEXTRA(1), HEXTRA(2))
     P12SQ = P12*P12
     if (p12.ne.0.) then
        P_INV(1,1) = 1.0d0/(1.0d0-P12SQ)
        P_INV(2,2) = 1.0d0/(1.0d0-P12SQ)
        P_INV(1,2) = -P12/ (1.0d0-P12SQ)
        P_INV(2,1) = -P12/ (1.0d0-P12SQ)
     else
        P_INV(1,1) = 1.
        P_INV(2,2) = 1.
        P_INV(1,2) = 0.
        P_INV(2,1) = 0.
     end if
     !                              multiply inverse corr matrix by (z1 z2)'
     PINV_Z(1) = p_inv(1,1)*z(1) + p_inv(1,2)*z(2)
     PINV_Z(2) = p_inv(2,1)*z(1) + p_inv(2,2)*z(2)
  end if
  !        IVSTAT_S = .false.
  return
end
