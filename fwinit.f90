!============================!
!   VolumeLibrary : fwinit   !
!============================!
  !== last modified  04-02-2014
subroutine FWINIT(VOLEQ,DBHOB,HTTOT,MHT,MTOPP,UPSHT1,UPSHT2,&
     &UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,JSP,RHFW,RFLW,&
     &TAPCOE,F,SETOPT,NEXTRA,HEX,DEX,ZEX,PINV_Z,FMOD,btr,FCLASS,&
     &ERRFLAG)

  !        Initialiazes SF Subroutines using Flewelling profile models
  !        Uses SF_TEST2 initializing code
  !                  J. Flewelling  July, 1996
  !                  Modified by K. Cormier Sept, 1996
  !  04/02/2014 YW Changed UPSHT1 to 33.6 for 32 foot log when using FCLASS
  !  07/12/2016 YW ADDED BTR VALUE FOR REGION 3 SANTA FE NF SPECIES 122,202,015,108
  !  07/13/2023 YW Changed BTR for 301FW2W equatin for species 122,202,015,108 with R3 combined validation data
  !     TREE VARIABLES
  ! Variables
  real    :: DBHOB,HTTOT,DBTBH,MHT,MTOPP,btr

  !     3PT VARIABLES
  real    :: UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
  real*4  :: HEX(2),DEX(2),ZEX(2),FMOD(3),PINV_Z(2)
  real*4  :: RHFW(4),RFLW(6),TAPCOE(12),F

  integer :: JSP,SETOPT(6),NEXTRA,htref,ERRFLAG,ICON,FCLASS,threeflag
  !      LOGICAL METRIC
  character(len=1) :: GEOCODE
  !     character*1 unit
  character(len=2) :: GEOSUB
  character(len=3) :: SPEC
  character(len=10) :: VOLEQ

  !    Units in english or metric (E=english)
  !      UNIT='E'
  !                              use default settings
  !    Apply constraints to 3-pt fit (1=yes/0=no)
  ICON = 1
  !    Stem form: user regin when available (1=yes/0=no)
  !         IREGIONU(1)=1
  !    Bark thickness: user regin when available (1=yes/0=no)
  !         IREGIONU(2)=1

  !    Upper stem measurement (1=DOB/2=DIB)
  !      IDTYPE=1
  !    Supplimental regression use intercept (1=yes/0=no)
  !      IMOD_USE=1
  !                Line from SF-SETERR
  !      IER_UNIT=6
  !                                                Other setup features
  !                                                Note: Call changed from 1994.
  !         FOLLOWING FROM SF_SET
  !      if (unit.eq.'E' .or. unit.eq.'e') then
  !          METRIC  = .false.
  !          BH=4.5
  !      else if  (unit.eq.'M' .or. unit.eq.'m') then
  !          METRIC=.TRUE.
  !          BH = 4.5 * 0.3048
  !      else
  !          IER=1
  !      endif

  !           set an upper bound on fractional changes;
  !           predictions can exceed this only if measured data also exceeds.
  SETOPT(1) = ICON
  !                              Region effects on bark predictions
  SETOPT(2) = 1
  SETOPT(3) = 1

  !                          The following white noise option is ALWAYS set
  !                            to zero (off). For future development.
  SETOPT(4) = 0
  !                           Numerical volume segments
  !     SETOPT(5)= NVSEGU
  !     SETOPT(6)= IVOLINT

  !                       The following is the maximum allowed 3-pt fractional
  !                         change in DBHOB. Unless a greater change is observed,
  !                         this will become FMODMAX
  !      SETC(1) = .15

  !                               Prepare for modifier regressions
  !      INT_USE = IMOD_USE
  !     CALL SF_MODR0
  !                                INGY
  ZEX(1) = 0
  ZEX(2) = 0
  HEX(1) = 0
  HEX(2) = 0
  DEX(1) = 0
  DEX(2) = 0
  NEXTRA = 0
  THREEFLAG = 0

  !     MOVED THE FOLLOWING LINE TO END OF THE SETUP PHASE(YW 07/12/2016)
  !      IF(BTR.GT.0.0 .AND. DBTBH.LE.0) DBTBH = DBHOB-(DBHOB*BTR/100.0)

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
        else if (SPEC.eq.'202') then
           JSP = 26
           if (BTR.le.0) BTR = 88.85 !the BTR for combined R3 DF data
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

  if (BTR.gt.0.0.and. DBTBH.le.0) DBTBH = DBHOB-(DBHOB*BTR/100.0)
  !-------------------------- end of set-up phase ------------------------------

  !      WRITE(*,*)'FWINIT',JSP

  !                 TWO POINT MODEL INITIALIZE,
  !    check for total height
  if (HTTOT.le.0.and. MHT.gt.0) then
     call SF_2PTH(JSP,GEOSUB,SETOPT,DBHOB,MHT,MTOPP,DBTBH,&
          &HTTOT,HEX,ZEX,F,RHFW,RFLW,TAPCOE)
     if (httot .le.0) then !added  10/4/2001
        errflag = 11
        return
     end if
     if (upsht1.gt.0) threeflag = 1
  else
     call sf_2pt(JSP,GEOSUB,SETOPT,DBHOB,HTTOT,DBTBH,F,RHFW,&
          &RFLW,TAPCOE)
  end if
  !          if (ierr.gt.0) then
  !            write(6,106)  IERR
  !106         format(' SF_2pt returned error code ', I6)
  !            GOTO 1000
  !          endif

  !                                                    THREE POINT MODEL INITIALIZE
  !                                                       Specify upper dob(s)
  if (VOLEQ(6:6).eq.'3'.or.THREEFLAG.eq.1) then
     if (htref.gt.0) then
        NEXTRA = 1
        upsht1 = float(htref)*HTTOT/100.0
     else if (UPSHT1.gt.0.0) then
        if (UPSHT2.gt.0.0) then
           NEXTRA = 2
        else
           NEXTRA = 1
        end if
     else if (FCLASS .gt.0) then
        NEXTRA = 1
        UPSHT1 = 17.5
        !          FCLASS for 32 foot log (YW 04/02/14)
        if (VOLEQ(5:5).eq.'3') UPSHT1 = 33.6
        UPSD1 = ANINT(DBHOB*FCLASS) / 100.0
     end if

     if (NEXTRA .gt.0) then
        HEX(1) = UPSHT1
        HEX(2) = UPSHT2
        !                                                      Specify an upper dib
        if (AVGZ1.ne.0.0) then
           ZEX(1) = AVGZ1
           ZEX(2) = AVGZ2

           call SF_3z(JSP,GEOSUB,SETOPT,NEXTRA,DBHOB,HTTOT,HEX,ZEX,&
                &RFLW,RHFW,TAPCOE,F,PINV_Z,FMOD)

        else if (UPSD1.gt.0.0) then
           DEX(1) = UPSD1
           DEX(2) = UPSD2

           call sf_3pt(JSP,GEOSUB,setopt,DBHOB,HTTOT,DBTBH,NEXTRA,&
                &HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,PINV_Z,FMOD)
           !                                                       Specify upper z(s)
        end if
     else
        ERRFLAG = 9
     end if
  end if
  !                    END OF INTIALIZATION ROUTINE
  return
end

  ! JSP VALUES

  !    WESTSIDE:              3 - 5
  !    INGY:                 11 - 21
  !      Douglas fir            11
  !      Western Larch          12
  !      Grand fir              13
  !      Ponderosa pine         14
  !      Lodgepole pine         15
  !      Western Red Cedar      16
  !      Mountain Hemlock       17
  !      White pine             18
  !      Engelmann Spruce       19
  !      Subalpine fir          20
  !      Balsam fir             21

  !    BLACK HILLS              22
  !    SAN JUAN & R2 PP         23
  !    DIXIE ES                 24
  !    R2 LODGEPOLE             25
  !    R2 DOUG FIR              26
  !    R2 WHITE FIR             27
  !    R10 ALASKA CEDAR         31
  !    R10 WESTERN RED CEDAR    32
