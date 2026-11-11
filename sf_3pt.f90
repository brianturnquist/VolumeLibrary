!============================!
!   VolumeLibrary : sf_3pt   !
!============================!
  !== last modified  04-02-2014
subroutine SF_3pt(JSP,geosub,setopt,DBH,TOTALH,DBT_BH,NEXTRA,HEX,&
     &DEX,ZEX,RHFW,RFLW,TAPCOE,F,PINV_Z,FMOD)
  !     given meausremts at 1 or 2 heights besides BH, perform
  !     necessary calculations prior to estimating all conditional diameters
  !     This routine assumes that NEXTRA, hextra(*) and dobex(*) are filled
  !           in with correct values.

  !     NEXTRA    Input   I       Number of extra measurement points (besides HT, DBH)
  !                            (currently must = 1 or 2)
  !     HEX    Input   R*4()   Vector of extra measurement heights
  !     DEX    Input   R*4()   Vector of measured diameters at stated heights.
  !     IDTYPE Input  I       1: DEX's are outside bark; 2: DEX's are inside bark.
  !     ZEX    Output  R*4()   Standardized errors at those heights.
  !                                (These are Never adjusted for white noise).
  !  04/02/2014 YW Comment out a piece code which does not doing anything but causes problem for JSP > 25.
  ! Variables
  integer :: NSP
  character(len=2) :: GEOSUB
  integer :: SETOPT(6),JSP,NEXTRA,IDTYPE,ineedsl,JX

  real    :: DBH,TOTALH,DBT_BH,BH,slope
  real*4  :: RFLW(6),RHFW(4),TAPCOE(12),ABS_CHNG
  real*4  :: HEX(2),DEX(2),ZEX(2),X,F,DIB,DBT,DIBMOD,DIBACT,SE_LNX
  real    :: SF_YHAT,BRK_UPB2,BRK_UPA2,BRK_WS,SF_CORR
  real*8  :: p_inv(2,2)
  real    :: FMODMAX,HFIRSTUP,FMODMAXU,PINV_Z(2),FMOD(3)
  real    :: hextra(2),dhatex(2),bark_r(2),dobex(2),z(2)

  real*8  :: P12,P12sq

  parameter (NSP = 25)
  real*4  :: whitenoise(NSP) = (/  1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
       &1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0 /)

  BH = 4.5
  IDTYPE = 1
  INEEDSL = 0
  if (NEXTRA.lt.1.or. NEXTRA.gt.2) then
     !            write(*,1) NEXTRA
     !1           format(' Subroutine SF_3pt given invalid NEXTRA value =',i4/
     !     1             ' Please correct the calling program. STOP. ')
     return
  end if
  if (setopt(1).eq.1) then
     !            FMODMAX=SETC(1)
     FMODMAX = .15
     HFIRSTUP = 0.0
  end if

  do JX = 1,NEXTRA
     hextra(jx) = HEX(jx)
     X = hextra(jx)/TOTALH

     dhatex(JX) = sf_yhat(JSP,X,TOTALH,ineedsl,slope,RHFW,RFLW,&
          &TAPCOE,F)
     if (JSP.ge.11.and. JSP.le.21) then
        !                           Inside bark model, use upper stem inside bark
        idtype = 2
        if (IDTYPE.eq.1) then
           dobex(jx) = DEX(jx)
           !                           INGY bark routines in file brk_up.for
           bark_r(jx) = brk_upb2(JSP,DBH,TOTALH,DBT_BH,&
                &hextra(jx), dobex(jx))
           dbt = bark_r(jx)* dobex(jx)
           dib = dobex(jx) - dbt
        else
           dib = DEX(jx)
           !              bark_r(jx)=brk_upa2(JSP,DBH,TOTALH,DBT_BH,
           !     >                                         hextra(jx), dib)
           !              dbt = dib * (bark_r(jx) / (1.0-bark_r(jx)))
           !              dobex(jx) = dib + dbt
           !              write(*,15)jsp,dbh,totalh,hextra(1),dib,dobex(1)
           !   15         format(i3,5f6.1)
        end if
     else if (JSP.ge.3.and. JSP.le.5) then
        !                           Inside bark model
        !            idtype = 2
        if (IDTYPE.eq.1) then
           dobex(jx) = DEX(jx)
           !                westside bark routines in file brk_up.for
           bark_r(jx) = brk_ws(JSP,DBH,TOTALH,DBT_BH,hextra(jx))
           dbt = bark_r(jx)* dobex(jx)
           dib = dobex(jx) - dbt
        else
           dib = DEX(jx)
           !                westside bark routines in file brk_up.for
           bark_r(jx) = brk_ws(JSP,DBH,TOTALH,DBT_BH,hextra(jx))
           dbt = dib * (bark_r(jx) / (1.0-bark_r(jx)))
           dobex(jx) = dib + dbt
           !              write(*,15)jsp,dbh,totalh,hextra(1),dib,dobex(1)
           !   15         format(i3,5f6.1)
        end if
        !          ELSEIF(JSP.GE.31 .AND. JSP.LE.32)THEN

     else
        !                          Outside bark model
        DIB = DEX(JX)
     end if
     DIBmod = DHATEX(JX)
     DIBact = DIB

     if (JSP .ge.11.and. JSP .le.21) then
        call VAR_C2(JSP,geosub,DBH,TOTALH,hextra(jx),SE_LNX)
        Z(jx) = log(dibact/DIBmod) /SE_LNX

     else if (JSP.ge.3.and. JSP.le.5 ) then
        call VAR_C1(JSP,DBH,TOTALH,Hextra(jx),dibmod,dibact,Z(jx))

     else if (JSP.ge.23.and. JSP.le.30 ) then
        call VAR_OT(JSP,DBH,TOTALH,hextra(jx),SE_LNX)
        Z(jx) = log( dibact/DIBmod) /SE_LNX

     else if (JSP.ge.31.and. JSP.le.36 ) then
        call VAR_AK(JSP,DBH,TOTALH,hextra(jx),SE_LNX)
        Z(jx) = log( dibact/DIBmod) /SE_LNX

     else if (JSP.eq.22) then
        call VAR_BH(DBH,TOTALH,HEXTRA(JX),SE_LNX)
        Z(jx) = (DIBact - DIBmod) / SE_LNX
     end if
     ZEX(jx) = Z(jx)
     if (setopt(1).eq.1) then

        abs_chng = abs(dib-dhatex(jx))
        FMODMAX = max( fmodmax, abs_chng /dhatex(jx) )
        if (HEX(jx).gt. bh ) then
           if (hfirstup .eq.0.0.or. HEX(jx).lt. hfirstup) then
              fmodmaxu = min(dhatex(jx), 2.0*abs_chng)/dhatex(jx)
              Hfirstup = HEX(jx)
           end if
        end if
        FMOD(1) = FMODMAX
        FMOD(2) = FMODMAXU
        FMOD(3) = HFIRSTUP
     end if
     !              white noise adjustments (on Z in common, not ZEX argument)
     ! The following piece of code is comment out by YW on 04/02/2014. It is not doing anything.
     !          IF(setopt(4).eq.1  .and. whitenoise(jsp).lt. 1.0)
     !     1                            Z(jx) = Z(jx) * whitenoise(jsp)
     100 continue
  end do

  if (NEXTRA.eq.2) then
     P12 = SF_CORR(JSP,GEOSUB,TOTALH,hextra(1),hextra(2))
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
     !                              multiply inverse corr matrix by (Z1 Z2)'
     PINV_Z(1) = p_inv(1,1)*Z(1) + p_inv(1,2)*Z(2)
     PINV_Z(2) = p_inv(2,1)*Z(1) + p_inv(2,2)*Z(2)
  end if

  !      IVSTAT_S= .false.
  return
end
