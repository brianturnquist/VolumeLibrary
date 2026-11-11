!=============================!
!   VolumeLibrary : calcdia   !
!=============================!

  !== last modified  2019/05/09
  ! Added UPSHT! and UPSD1 to calcdib_r and calcdob_r (2019/05/08)
  ! Added calcdib_r subroutind for R program to use the CALCDIA (4/4/2017)
  ! Added DIB calculation for region 8 Clark equation (6/4/14)
  ! Added DIB calculation for Behr equation (1/28/2014)
  ! Added stump diameter (from ground to 4.5 ft) calculation for non profile model using Raile 1983 (YW)
  ! YW 2016/01/13 Added BTR default value for Region 3 Santa Fe forest DF and PP
  ! YW 2023/08/08 Corrected variable name error for DBHOB in CALCDIA2
  ! YW 2024/03/26 Added initial value 0 to DIB and DOB in CALCDIA2
  !  CalcDia.f90
  !  FUNCTIONS/SUBROUTINES exported from VOLLIB.dll:
  !    CALCDIA      - subroutine

subroutine CALCDIA(REGN,FORST,VOLEQ,STUMP,DBHOB,&
     &DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,&
     &AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)

  ! Expose subroutine CALCDIA to users of this DLL

  !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::CALCDIA
  !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: CALCDIA
  !DEC$ ATTRIBUTES DECORATE, ALIAS:'CALCDIA'::CALCDIA
  ! Variables
  character(len=*) :: FORST
  character(len=*) :: VOLEQ

  integer :: REGN,ERRFLAG,FCLASS,HTREF
  real    :: STUMP,DBHOB,DRCOB,HTTOT
  real    :: UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
  real    :: DBTBH,BTR,HTUP,DIB,DOB
  character(len=10) :: VOLEQI
  character(len=2) :: FORSTI

  FORSTI = FORST(1:2)
  VOLEQI = VOLEQ(1:10)
  call CALCDIA2(REGN,FORSTI,VOLEQI,STUMP,DBHOB,&
       &DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,&
       &AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)

  return
end

subroutine CALCDIA2(REGN,FORST,VOLEQ,STUMP,DBHOB,&
     &DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,&
     &AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)

  ! Variables
  character(len=10) :: VOLEQ
  character(len=2) :: FORST
  character(len=3) :: MDL

  !     MERCH VARIABLES
  integer :: REGN,BA,SI
  integer :: ERRFLAG

  !     TREE VARIABLES
  real    :: HTTOT,HTUP,MHT,MTOPP,UHT,CUVOL
  real    :: DBHOB,DRCOB,DBTBH,BTR,STUMP,TOP6
  integer :: FCLASS
  !      3RD POINT VARIABLES
  real    :: UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2
  integer :: HTREF
  !     OUTPUTS
  real    :: DIB,DOB,UPSHT
  !         Variables to hold flewellings coefficients
  integer :: SETOPT(6),JSP,MFLG,NEXTRA
  real    :: RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
  real    :: HEX(2),dex(2),ZEX(2)
  real    :: mTopS,ht1Prd,ht2Prd
  character(len=2) :: prod
  integer :: SPN
  real    :: TLH
  ! Variable for R4 taper
  real    :: STUMPD,BUTTCF,CF0,B
  character(len=11) :: VOLEQ11
  real    :: Vib
  real    :: WDSG
  integer :: SPGRPCD

  !Added initial value 0 to DIB and DOB (2024/03/26)
  DIB = 0.0
  DOB = 0.0
  TLH = 0.
  !     ARRAYS
  ! initialize profile model
  !C  heck for a DBH of less than 1.  Drop out of volume if true.  10/97
  !   Added check for height 08/2022 DW
  !   DBHOB and HTTOT use dofferent error code 12/28/2022
  if (dbhob.lt.1.or. HTTOT.lt.5) then
     if (DBHOB.lt.1.0) then
        errflag = 3
        goto 1000
     end if
     if (HTTOT.lt.5.0) ERRFLAG = 4
     goto 1000
  end if

  if (VOLEQ .eq. "") then
     ERRFLAG = 1
     goto 1000
  end if
  MFLG = 2
  MHT = 0
  MTOPP = 0
  MDL = VOLEQ(4:6)
  prod = '01'

  if (MDL.eq.'FW2' .or. MDL.eq.'fw2' .or. MDL.eq.'FW3' .or.&
       &MDL.eq.'fw3' .or. MDL.eq.'CZ2' .or. MDL.eq.'cz2' .or.&
       &MDL.eq.'CZ3' .or. MDL.eq.'cz3' .or. MDL.eq.'WO2' .or.&
       &MDL.eq.'wo2' .or. MDL.eq.'F32' .or. MDL.eq.'f32' .or.&
       &MDL.eq.'F33' .or. MDL.eq.'f33' .or. MDL.eq.'JB2' .or.&
       &MDL.eq.'jb2' .or. MDL.eq.'DEM' .or. MDL.eq.'CUR' .or.&
       &MDL.eq.'dem' .or. MDL.eq.'cur') then

     !    FLEWELLING MODELS  *
     !    REGION 2 MODELS    *
     !    REGION 5 MODELS    *

     if (VOLEQ(4:4).eq.'F' .or. VOLEQ(4:4).eq.'f') then
        !--   Initialize Flewelling model for this tree
        call FWINIT(VOLEQ,DBHOB,HTTOT,MHT,MTOPP,UPSHT1,UPSHT2,UPSD1,&
             &UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,JSP,RHFW,RFLW,&
             &TAPCOE,F,SETOPT,NEXTRA,HEX,DEX,ZEX,PINV_Z,FMOD,btr,FCLASS,&
             &ERRFLAG)

     else if (VOLEQ(4:6).eq.'CZ3' .or. VOLEQ(4:6).eq.'cz3') then
        !        initialize Czaplewski three point model
        if (HTTOT.le.4.5) then
           ERRFLAG = 4
           goto 1000
        end if
        UHT = HTTOT * 0.95
        if (UPSHT1.le.0.or. UPSD1.le.0) then
           ERRFLAG = 9
           go to 1000
        end if
        if (UPSHT1.le.4.5.or. UPSHT1.gt.UHT) then
           ERRFLAG = 10
           go to 1000
        end if
        HEX(1) = UPSHT1
        DEX(1) = UPSD1
        call TOP6LEN(VOLEQ,HTTOT,DBHOB,DEX,HEX,STUMP,6.0,&
             &TOP6,DBTBH,errflag)
     else
        !C       CHECK FOR TOTAL TREE HEIGHT
        if (HTTOT.le.4.5) then
           ERRFLAG = 4
           goto 1000
        end if
     end if
     ! GET THE DIAMETERS
     call TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
          &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HTUP,&
          &MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
     ! Added Clark profile model for region 9
  else if (MDL.eq.'CLK' .or. MDL.eq.'clk') then
     if (VOLEQ(1:1).eq.'9'.or.&
          &(VOLEQ(1:1).eq.'8'.and.VOLEQ(3:3).eq.'1')) then
        ht2Prd = UPSHT2
        ! reset UPSHT1 to 0 (yw 09/24/2012)
        UPSHT = 0
        if (UPSD1.eq.7.0.or.UPSD1.eq.9.0) then
           UPSHT = UPSHT1
        else
           ht1Prd = UPSHT1
        end if
        mTopP = 0.0 !UPSD1
        mTopS = 0.0 !UPSD2
        errFlag = 0
        call r9clarkdib (VOLEQ,STUMP,mTopP,mTopS,DBHOB,&
             &ht1Prd,ht2Prd,HTTOT,HTUP,DIB,prod,errFlag,&
             &UPSHT)
     else
        call R8CLKDIB(VOLEQ, FORST, DBHOB, HTTOT, UPSHT1,HTUP,DIB,&
             &ERRFLAG)
     end if
  else if (MDL.eq.'BEH' .or. MDL.eq.'beh') then
     !     added DIB calculation for Behr equation
     if (FCLASS.le.0) then
        call GETFCLASS(VOLEQ,FORST,DBHOB,FCLASS)
     end if
     call BEHTAP(VOLEQ,DBHOB,HTTOT,TLH,HTUP,FCLASS,MTOPP,DIB)
     !     Added the calculation for R4 taper equation (03/20/2017)
  else if (MDL.eq.'MAT' .or. MDL.eq.'mat') then
     DIB = 0.0
     call R4MATTAPER(VOLEQ,DBHOB,HTTOT,STUMPD,BUTTCF,CF0,B,&
          &HTUP,DIB,ERRFLAG)
     !     Added the calc for R2 small trees equation for Black Hills
  else if (VOLEQ.eq.'223DVEW122') then
     call GetHTDIB(DBHOB,HTTOT,HTUP,DIB)
     !     Added DIB calc for the NSVB equations
  else if (VOLEQ(1:3).eq.'NVB') then
     VOLEQ11 = VOLEQ
     call NVB_Vib(VOLEQ11,DBHOB,HTTOT,Vib,ERRFLAG,SPGRPCD,WDSG)
     call NVB_DibAtHT(VOLEQ11,DBHOB,HTTOT,Vib,HTUP,DIB,ERRFLAG,&
          &SPGRPCD,WDSG)
     ! calculation for diameter from ground to 4.5 ft heigh for non profile model
     ! added on 7/22/2012 YW
     ! using Raile 1982
  else
     if (HTUP .lt.4.5) then
        read (VOLEQ(8:10),'(I3)') SPN
        if (HTUP .lt.0.0001) HTUP = 1.0
        call STUMPDIA(SPN, DBHOB, HTUP, DIB, DOB)
        return
     end if
  end if

  1000 return
  end subroutine CALCDIA2

subroutine calcdib_r(VOLEQ,REGN,FORST,DBHOB_d,HTTOT_d,&
     &HTUP_d,DIB_d, ERRFLAG, UPSHT1_d,UPSD1_d)
  ! This subroutine is for R user to calculate DIB at given height      !
  ! YW 04/04/2017

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::calcdib_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'calcdib_r_'::calcdib_r

  implicit none

  double precision :: DBHOB_d,HTTOT_d,DIB_d,HTUP_d
  double precision :: UPSHT1_d,UPSD1_d
  ! Variables
  character(len=2) :: FORST
  character(len=10) :: VOLEQ
  integer :: REGN,ERRFLAG

  !   Tree variables
  real    :: HTTOT,HTUP,DIB,DOB
  real    :: DBHOB,DRCOB,DBTBH,BTR,STUMP
  integer :: FCLASS

  !    3RD POINT VARIABLES
  real    :: UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2
  integer :: HTREF

  DBHOB = real(DBHOB_d)
  HTTOT = real(HTTOT_d)
  HTUP = real(HTUP_d)
  UPSHT1 = real(UPSHT1_d)
  UPSD1 = real(UPSD1_d)
  !     Set the default value for other variable
  STUMP = 0.0
  DBTBH = 0.0
  BTR = 0.0
  AVGZ1 = 0.0
  HTREF = 0
  UPSHT2 = 0.0
  UPSD2 = 0.0
  AVGZ2 = 0.0
  DRCOB = 0.0
  FCLASS = 0

  call CALCDIA2(REGN,FORST,VOLEQ,STUMP,DBHOB,&
       &DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,&
       &AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)

  DIB_d = DBLE(DIB)

  continue
  return
  end subroutine calcdib_r

subroutine calcdob_r(VOLEQ,REGN,FORST,DBHOB_d,HTTOT_d,&
     &HTUP_d,DOB_d, ERRFLAG, UPSHT1_d,UPSD1_d)
  ! This subroutine is for R user to calculate DIB at given height      !
  ! YW 11/29/2018

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::calcdob_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'calcdob_r_'::calcdob_r

  implicit none

  double precision :: DBHOB_d,HTTOT_d,DOB_d,HTUP_d
  double precision :: UPSHT1_d,UPSD1_d
  ! Variables
  character(len=2) :: FORST
  character(len=10) :: VOLEQ
  integer :: REGN,ERRFLAG

  !   Tree variables
  real    :: HTTOT,HTUP,DIB,DOB
  real    :: DBHOB,DRCOB,DBTBH,BTR,STUMP
  integer :: FCLASS

  !    3RD POINT VARIABLES
  real    :: UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2
  integer :: HTREF

  DBHOB = real(DBHOB_d)
  HTTOT = real(HTTOT_d)
  HTUP = real(HTUP_d)
  UPSHT1 = real(UPSHT1_d)
  UPSD1 = real(UPSD1_d)
  !     Set the default value for other variable
  !      UPSHT1 = 0.0
  !      UPSD1 = 0.0
  STUMP = 0.0
  DBTBH = 0.0
  BTR = 0.0
  AVGZ1 = 0.0
  HTREF = 0
  UPSHT2 = 0.0
  UPSD2 = 0.0
  AVGZ2 = 0.0
  DRCOB = 0.0
  FCLASS = 0

  call CALCDIA2(REGN,FORST,VOLEQ,STUMP,DBHOB,&
       &DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,&
       &AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)

  DOB_d = DBLE(DOB)

  continue
  return
  end subroutine calcdob_r

subroutine fwdbt_r(VOLEQ,DBHOB_d,HTTOT_d,HTUP_d,DOB_d,DBT_d,&
     &ERRFLAG)
  ! This subroutine is for R user to calculate double bark thickness (DBT) at given height      !
  ! YW 02/19/2021

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::fwdbt_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'fwdbt_r_'::fwdbt_r

  implicit none

  double precision :: DBHOB_d,HTTOT_d,DOB_d,HTUP_d,DBT_d
  ! Variables
  character(len=10) :: VOLEQ
  real*4  :: RHFW(4),RFLW(6),TAPCOE(12),F
  real    :: DBHOB,HTTOT,DBTBH,BTR,HTUP,DOB,DBT,DIB
  character(len=1) :: GEOCODE
  character(len=2) :: GEOSUB
  character(len=3) :: SPEC
  integer :: JSP,SETOPT(6),ERRFLAG
  real    :: FDBT_C2,BRK_UPB2
  real    :: RATIO,BRK_UPA2,BRK_WS,BARK_R,fdbt_c1

  DBHOB = real(DBHOB_d)
  HTTOT = real(HTTOT_d)
  HTUP = real(HTUP_d)
  DOB = real(DOB_d)

  ! The following code is copied from fwinit.f
  SETOPT(1) = 1
  !                              Region effects on bark predictions
  SETOPT(2) = 1
  SETOPT(3) = 1

  !                          The following white noise option is ALWAYS set
  !                            to zero (off). For future development.
  SETOPT(4) = 0

  GEOCODE = VOLEQ(1:1)
  GEOSUB = VOLEQ(2:3)
  SPEC = VOLEQ(8:10)

  !       ERROR CHECK THE GEOSUB CODE
  if (GEOSUB.eq.'OO' .or. GEOSUB.eq.'oo') GEOSUB = '00'

  if (GEOCODE.eq.'I' .or. GEOCODE.eq.'i' .or. GEOCODE.eq.'1') then
     if (SPEC.eq.'202'.or.spec.eq.'205'.or.spec.eq.'204') then
        !     Douglas fir
        JSP = 11
     else if (SPEC.eq.'073'.or.SPEC.eq.'070') then
        !     Western Larch
        JSP = 12
     else if (SPEC.eq.'017') then
        !     Grand fir
        JSP = 13
     else if (SPEC.eq.'122') then
        !     Ponderosa pine
        JSP = 14
     else if (SPEC.eq.'108') then
        !     Lodgepole pine
        JSP = 15
     else if (SPEC.eq.'242'.or.SPEC.eq.'240') then
        !     Western Red Cedar
        JSP = 16
     else if (SPEC.eq.'260'.or.SPEC.eq.'263'.or.SPEC.eq.'264') then
        !     Mountain Hemlock
        JSP = 17
     else if (SPEC.eq.'119') then
        !     White pine
        JSP = 18
     else if (SPEC.eq.'093'.or.SPEC.eq.'090') then
        !     Engelmann Spruce
        JSP = 19
     else if (SPEC.eq.'019') then
        !     Subalpine fir
        JSP = 20
     else if (SPEC.eq.'012') then
        !     Balsam fir
        JSP = 21
     else
        ERRFLAG = 1
        return
     end if

     !                                WESTSIDE SPECIES
  else if (GEOCODE.eq.'F' .or. GEOCODE.eq.'f') then
     if (SPEC.eq.'202'.or.spec.eq.'205'.or.spec.eq.'204') then
        !     Douglas fir
        JSP = 3
     else if (SPEC.eq.'263') then
        !     Western Hemlock
        JSP = 4
     else if (SPEC.eq.'242') then
        !     Western Red Cedar
        JSP = 5
     else
        ERRFLAG = 1
        return
     end if

     !                                 REGION 2
  else if (GEOCODE.eq.'2') then
     if (SPEC.eq.'122') then
        !         Black Hills model
        if (GEOSUB.eq.'03') then
           JSP = 22
        else
           !         region wide and san juan
           JSP = 23
        end if
     else if (SPEC.eq.'108') then
        !         Lodgepole model
        JSP = 25
     else if (SPEC.eq.'202') then
        !         Douglas Fir model
        JSP = 26
     else if (SPEC.eq.'015') then
        !         White fir model
        JSP = 27
     else if (SPEC.eq.'746') then
        !         Aspen model
        JSP = 28
     else
        ERRFLAG = 1
        return
     end if
     !                                 REGION 4
  else if (GEOCODE.eq.'4') then
     if (GEOSUB.eq.'07') then
        if (SPEC.eq.'093') then
           !         Engelmann spruce model
           JSP = 24
        else if (SPEC.eq.'122') then
           !         R2 Ponderosa Pine model with R4 bark
           JSP = 23
        else
           ERRFLAG = 1
           return
        end if
     else
        ERRFLAG = 1
        return
     end if
     !                                 REGION 3
     !     REGION 3
  else if (GEOCODE.eq.'3') then
     if (GEOSUB.eq.'00') then
        if (SPEC.eq.'122') then
           JSP = 29
        else
           ERRFLAG = 1
           return
        end if
        !        GEOSUB 01 is for Santa Fe NF
     else if (GEOSUB.eq.'01') then
        if (SPEC.eq.'122') then
           JSP = 29
           if (BTR.le.0) BTR = 89.12
           !         SPECIES 202, 015, AND 108 USE REGION 2 PROFILE
        else if (SPEC.eq.'108') then
           JSP = 25
           if (BTR.le.0) BTR = 93.26
        else if (SPEC.eq.'202') then
           JSP = 26
           if (BTR.le.0) BTR = 89.72
        else if (SPEC.eq.'015') then
           JSP = 27
           if (BTR.le.0) BTR = 91.16
        else
           ERRFLAG = 1
           return
        end if
     else
        ERRFLAG = 1
        return
     end if
     !                                 REGION 10
  else if (GEOCODE.eq.'A') then
     if (SPEC.eq.'042') then
        !         Alaska yellow cedar
        JSP = 31
     else if (SPEC.eq.'242') then
        !         Western Red Cedar
        JSP = 32
     else if (SPEC.eq.'098') then
        !         Spruce
        if (GEOSUB.eq.'02') then
           JSP = 35
        else
           JSP = 33
        end if
     else if (SPEC.eq.'263'.or.SPEC.eq.'260'.or.SPEC.eq.'264') then
        !         Hemlock
        if (GEOSUB.eq.'02') then
           JSP = 36
        else
           JSP = 34
        end if
     else
        ERRFLAG = 1
        return
     end if
  else
     ERRFLAG = 1
     return
  end if

  !      IF(BTR.GT.0.0 .AND. DBTBH.LE.0) DBTBH = DBHOB-(DBHOB*BTR/100.0)
  !      CALL sf_2pt(JSP,GEOSUB,SETOPT,DBHOB,HTTOT,DBTBH,F,RHFW,
  !     >                                              RFLW,TAPCOE)
  ! End code from fwinit.f

  ! Code from Brk_up.f
  DIB = DOB
  DBT = 0.0
  DBTBH = 0.0
  if (JSP.ge.22.and. JSP.le.30) then
     call BRK_OT(JSP,geosub,DBHOB,DOB,HTUP,DBTBH,DIB,dbt)
  else if (jsp.ge.11.and. jsp.lt.22) then
     !          RATIO = BRK_UPA2(JSP,DBHOB,HTTOT,DBTBH,HTUP,dib)
     !          dbt = RATIO/(1.0-RATIO)*DIB
     DBTBH = FDBT_C2(JSP,GEOSUB,SETOPT,DBHOB,HTTOT)
     BTR = BRK_UPB2(JSP,DBHOB,HTTOT,DBTBH,HTUP,DOB)
     DBT = BTR*DOB
  else if (JSP.ge.3.and. JSP.le.5) then
     DBTBH = fdbt_c1(JSP,GEOSUB,DBHOB,HTTOT)
     BARK_R = BRK_WS(JSP,DBHOB,HTTOT,DBTBH,HTUP)
     dbt = dib * (bark_r / (1.0-bark_r))
     !         DBT=BARK_R*DOB
  end if

  DBT_d = DBLE(DBT)
  end subroutine fwdbt_r
