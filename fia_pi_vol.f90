!================================!
!   VolumeLibrary : fia_pi_vol   !
!================================!
  ! This is from NIMS_VOl_PACIFIC_ISLANDS package
  ! Created by YW 2018/08/28

  !--Cubic foot volume of main stem outside bark to a variable top and stump
  !--Developed by Olaf Kuegler for Pacific Island inventory
  !-- UPDIA = Centroid Diameter
  !-- UPHT = Centroid Height
  ! CU000150 FUNCTION CENTROID_CV
  ! NVEL EQUATION NUMBER:
  ! P03CEN0000
subroutine CENTROID_CV(DIA,THT,UPDIA,UPHT,MTOPP,STUMP,VOL,ERRFLG)
  ! Variables
  real    :: DIA,THT,UPD,UPHT,MTOPP,TOPD,STUMP,VOL(15),A,HT_STUMP
  integer :: ERRFLG
  CV(THT,UPDIA,UPHT,TOPD,STUMP,A) = &
       &3.14/4.0 *(UPDIA**2)*(A/(A+2.0))*((THT-UPHT)**(-2.0/A))*&
       &((THT-STUMP)**((A+2.0)/A) -(THT-UPHT)**((A+2.0)/A)*&
       &(TOPD/UPDIA)**(A+2.0))/144.0
  A = 2.0
  if (MTOPP.lt.0.1) MTOPP = 4.0
  if (STUMP.lt.0.1) STUMP = 1.0
  VOL = 0.0
  TOPD = 0.0
  HT_STUMP = 0.0
  if (UPDIA.lt.0.1.or.UPHT.lt.0.1) then
     ERRFLG = 9
     return
  end if
  VOL(1) = CV(THT,UPDIA,UPHT,TOPD,HT_STUMP,A)
  TOPD = MTOPP
  HT_STUMP = STUMP
  VOL(4) = CV(THT,UPDIA,UPHT,TOPD,HT_STUMP,A)
  if (VOL(1).lt.0.0) VOL(1) = 0.1
  if (VOL(4).lt.0.0) VOL(4) = 0.1
  return
  end subroutine CENTROID_CV

  !--Cubic foot volume of main stem outside bark from stump to tip
  !-- DIA_UPPER = Upper Stem Diameter
  !-- HT_UPPER  = Height to Upper Stem dia measurement
  ! CU000146 FRUSTRUM_CVTS(DBH, THT, DIA_UPPER, HT_UPPER, HT_DBH)
  ! CU000147 FRUSTRUM_CVTS_PHY(DBH, THT, HT_BREAK, DIA_UPPER, HT_UPPER, HT_DBH)
  ! CU000148 FRUSTRUM_CV4(DBH, THT, DIA_UPPER, HT_UPPER, HT_DBH)
  ! CU000149 FRUSTRUM_CV4_PHY(DBH, THT, DIA_UPPER, HT_UPPER, HT_DBH)
  ! NVEL EQUATION NUMBER:
  ! P03FRU0000
subroutine FRUSTRUM_VOL(DBHOB,DRCOB,HTTOT,UPSHT1,UPSD1,&
     &UPSHT2,UPSD2,MTOPP,STUMP,VOL,ERRFLG)
  ! UPSHT1 - HT_UPPER = Height to Upper Stem dia measurement
  ! UPSD1 - DIA_UPPER = Upper Stem Diameter
  ! UPSHT2 - HT_BREAK = Height to top break
  ! UPSD2 = Set to 0 when UPSHT2 is height to top break
  ! Variables
  real    :: DBHOB,DRCOB,HTTOT,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL(15)
  integer :: ERRFLG
  real    :: DBH,THT,HTDBH,UPDIA,UPHT,HT2BRK,MTOPP,TOPD,STUMP
  real    :: CVTS,CV4,DRC,TAPER_ANGEL,VOL_LOWER,VOL_MID,VOL_UPPER
  real    :: STUMPD,HTTOP,DIA_BREAK,TAPER_ANGEL2
  real    :: DIA_TOP_BREAK,HT_TOP_BREAK,STUMPV,D1,D2,LEN,PI
  FRUSTRUMV(LEN,D1,D2) = PI*LEN/3.0*(D1**2+D1*D2+D2**2)/24.0**2
  ERRFLG = 0
  VOL = 0.0
  PI = 3.1415927
  TAPER_ANGEL2 = 0.0
  if (DBHOB.gt.0.1) then
     DBH = DBHOB
     HTDBH = 4.5
  else if (DRCOB.gt.0.1) then
     DBH = DRCOB
     HTDBH = 0.0
  end if
  THT = HTTOT
  if (STUMP.lt.0.1) STUMP = 1.0
  TOPD = MTOPP
  if (TOPD.lt.0.1) TOPD = 4.0
  UPHT = UPSHT1
  UPDIA = UPSD1
  if (UPHT.le.HTDBH) then
     ERRFLG = 7
     return
  end if
  TAPER_ANGLE = ATAN(((DBH-UPDIA)/24.0)/(UPHT-HTDBH))
  DRC = DBH + 24.0*HTDBH*TAN(TAPER_ANGLE)
  STUMPD = DBH + 24.0*(HTDBH-1.0)*TAN(TAPER_ANGLE)

  LEN = HTDBH
  VOL_LOWER = FRUSTRUMV(LEN,DRC,DBH)
  LEN = UPHT-HTDBH
  VOL_MID = FRUSTRUMV(LEN,DBH,UPDIA)
  LEN = THT - UPHT
  VOL_UPPER = FRUSTRUMV(LEN,UPDIA,0.0)
  CVTS = VOL_LOWER+VOL_MID+VOL_UPPER
  !     CALC CV4
  if (UPDIA.le.TOPD) then
     LEN = UPHT-1.0
     VOL_LOWER = FRUSTRUMV(LEN,STUMPD,UPDIA)
     VOL_UPPER = 0.0
  else if (UPHT.eq.THT) then
     LEN = UPHT-1.0
     VOL_LOWER = FRUSTRUMV(LEN,STUMPD,UPDIA)
     VOL_UPPER = 0.0
  else if (UPDIA.gt.TOPD) then
     TAPER_ANGLE2 = ATAN((UPDIA/24.0)/(THT-UPHT))
     HTTOP = THT - (TOPD/24.0)/TAN(TAPER_ANGLE2)
     LEN = UPHT-1.0
     VOL_LOWER = FRUSTRUMV(LEN,STUMPD,UPDIA)
     LEN = HTTOP-UPHT
     VOL_UPPER = FRUSTRUMV(LEN,UPDIA,TOPD)
  else
     VOL_LOWER = 0.0
     VOL_UPPER = 0.0
  end if
  CV4 = VOL_LOWER+VOL_UPPER
  !--Cubic foot volume of main stem outside bark from ground to tip excluing broken top.
  if (UPSHT2.gt.4.5.and.UPSD2.eq.0.0) then
     HT2BRK = UPSHT2
     if (UPHT.ge.HT2BRK) then !-- Upper measurement at break
        LEN = UPHT
        VOL_LOWER = FRUSTRUMV(LEN,DRC,UPDIA)
        VOL_UPPER = 0.0
     else if (UPHT.eq.THT) then !-- Handel error when upper measurement same as height
        LEN = UPHT
        VOL_LOWER = FRUSTRUMV(LEN,DRC,UPDIA)
        VOL_UPPER = 0.0
     else if (UPHT.lt.HT2BRK) then !-- Upper measurement below break
        TAPER_ANGLE2 = ATAN((UPDIA/24.0)/(THT-UPHT))
        DIA_BREAK = UPDIA - 24.0*(HT2BRK-UPHT)*TAN(TAPER_ANGLE2)
        LEN = UPHT
        VOL_LOWER = FRUSTRUMV(LEN,DRC,UPDIA)
        LEN = HT2BRK-UPHT
        VOL_UPPER = FRUSTRUMV(LEN,UPDIA,DIA_BREAK)
     else
        VOL_LOWER = 0.0
        VOL_UPPER = 0.0
     end if
     CVTS = VOL_LOWER+VOL_UPPER
     !       CALC CV4
     if (THT.gt.UPHT) then
        TAPER_ANGEL2 = ATAN((UPDIA/24.0)/(THT-UPHT))
     else
        TAPER_ANGEL2 = TAPER_ANGEL
     end if
     !      --Find Diameter at break
     if (UPHT.lt.HT2BRK) then !-- Upper measurement below break
        DIA_BREAK = UPDIA - 24.0*(HT2BRK-UPHT)*TAN(TAPER_ANGLE2)
     else
        DIA_BREAK = DBH - 24.0*(UPHT-HTDBH)*TAN(TAPER_ANGLE2)
     end if
     !      --Find DIA and HT to lowest of break or merch top
     if ((TOPD.le.DIA_BREAK) .or. (THT.le.UPHT)) then
        DIA_TOP_BREAK = DIA_BREAK
        HT_TOP_BREAK = HT2BRK
     else
        DIA_TOP_BREAK = DIA_TOP
        if (TAPER_ANGLE2.eq.0.0) then
           return
        end if
        HT_TOP = THT - (TOPD/24.0)/TAN(TAPER_ANGLE2);
        HT_TOP_BREAK = HT_TOP
     end if
     if (UPDIA.le.DIA_TOP_BREAK) then
        LEN = UPHT-1.0
        VOL_LOWER = FRUSTRUMV(LEN,STUMPD,UPDIA)
        VOL_UPPER = 0.0
     else if (UPHT.eq.THT) then
        LEN = UPHT-1.0
        VOL_LOWER = FRUSTRUMV(LEN,STUMPD,UPDIA)
        VOL_UPPER = 0.0
     else if (UPDIA.gt.DIA_TOP_BREAK) then
        LEN = UPHT-1.0
        VOL_LOWER = FRUSTRUMV(LEN,STUMPD,UPDIA)
        LEN = HT_TOP_BREAK-UPHT
        VOL_UPPER = FRUSTRUMV(LEN,UPDIA,DIA_TOP_BREAK)
     else
        VOL_LOWER = 0.0
        VOL_UPPER = 0.0
     end if
     CV4 = VOL_LOWER+VOL_UPPER
  end if
  STUMPV = 0.002727*(DRC**2+STUMPD**2)
  VOL(14) = STUMPV
  VOL(1) = CVTS - STUMPV
  VOL(4) = CV4
  return
  end subroutine FRUSTRUM_VOL

  !--Function used to compute volume of tree ferns in Pacific Islands 2001 compilation
  !--Assumes cone from ground to tip
  !--Above ground stem volume outside bark.
subroutine CONE_VOL(DBHOB,DRCOB,HTTOT,UPSHT2,UPSD2,MTOPP,&
     &STUMP,VOL,ERRFLG)
  ! Variables
  real    :: DBHOB,DRCOB,HTTOT,UPSHT2,UPSD2,MTOPP,VOL(15),STUMP
  integer :: ERRFLG
  real    :: DBH,HT,HT_DBH,DRC,CVTS,TAPER_ANGLE,DIA_STUMP,HT_TOP
  real    :: HT_BREAK,HT_STUMP,DIA_TOP,STUMPV,PI,LEN,D1,D2
  FRUSTRUMV(LEN,D1,D2) = PI*LEN/3.0*(D1**2+D1*D2+D2**2)/24.0**2
  PI = 3.1415927
  if (DBHOB.gt.0.1) then
     DBH = DBHOB
     HT_DBH = 4.5
  else if (DRCOB.gt.0.1) then
     DBH = DRCOB
     HT_DBH = 0.0
  end if
  ERRFLG = 0
  VOL = 0.0
  CVTS = 0.0
  CV4 = 0.0
  HT = HTTOT
  if (STUMP.lt.0.1) THENSTUMP = 1.0
  HT_STUMP = STUMP
  if (MTOPP.lt.0.1) THENMTOPP = 4.0
  !      DIA_TOP = MTOPP
  DIA_TOP = 4.0
  if (HT.le.HT_DBH) then
     ERRFLG = 4
     return
  end if
  TAPER_ANGLE = ATAN((DBH/24.0)/(HT-HT_DBH))
  DRC = 24.0*HT*TAN(TAPER_ANGLE)
  CVTS = 3.14/3.0 * (DRC/24.0)**2 * HT
  DIA_STUMP = DBH + 24.0*(HT_DBH-1.0)*TAN(TAPER_ANGLE)
  HT_TOP = HT - (DIA_TOP/24.0)/TAN(TAPER_ANGLE)
  LEN = HT_TOP-1.0
  CV4 = FRUSTRUMV(LEN,DIA_STUMP,DIA_TOP)
  if (UPSHT2.gt.4.5.and.UPSD2.eq.0.0) then
     HT_BREAK = UPSHT2
     DRC = DBH + 24.0*HT_DBH*TAN(TAPER_ANGLE)
     DIA_BREAK = DBH - 24.0*(HT_BREAK-HT_DBH)*TAN(TAPER_ANGLE)
     if (HT.ge.HT_BREAK) then
        CVTS = 3.14/3.0 * (DRC/24.0)**2 * HT
     else
        CVTS = FRUSTRUMV(HT_BREAK,DRC,DIA_BREAK)
     end if;
     DIA_STUMP = 24.0*(HT-HT_STUMP)*TAN(TAPER_ANGLE)
     DIA_BREAK = 24.0*(HT-HT_BREAK)*TAN(TAPER_ANGLE)
     HT_TOP = HT - (DIA_TOP/24.0)/TAN(TAPER_ANGLE)
     if (HT_TOP.lt.HT_BREAK) then
        LEN = HT_TOP-HT_STUMP
        CV4 = FRUSTRUMV(LEN,DIA_STUMP,DIA_TOP)
     else
        LEN = HT_BREAK-HT_STUMP
        CV4 = FRUSTRUMV(LEN,DIA_STUMP,DIA_BREAK)
     end if
  end if
  STUMPV = 0.002727*(DRC**2+DIA_STUMP**2)
  VOL(14) = STUMPV
  VOL(1) = CVTS - STUMPV
  VOL(4) = CV4
  return
  end subroutine CONE_VOL

  !--Picks Pacific Island volume equations based on what tree measurements
  !--were taken and species.
  ! CU000140 PACIFIC_ISLAND_CVTS_OB(VOLSPN, DBH, THT, HT_BREAK, DIA_UPPER, HT_UPPER, DIA_CENTROID, HT_CENTROID, HT_DBH)
  ! CU000141 PACIFIC_ISLAND_CVTS_PHY_OB(VOLSPN, DBH, THT, HT_BREAK, DIA_UPPER, HT_UPPER, DIA_CENTROID, HT_CENTROID, HT_DBH)
  ! CU000142 PACIFIC_ISLAND_CVTS_NET_OB(VOLSPN, DBH, THT, HT_BREAK, DIA_UPPER, HT_UPPER, DIA_CENTROID, HT_CENTROID, HT_DBH)
  ! CU000143 PACIFIC_ISLAND_CV4_OB(VOLSPN, DBH, THT, HT_BREAK, DIA_UPPER, HT_UPPER, DIA_CENTROID, HT_CENTROID, HT_DBH)
  ! CU000144 PACIFIC_ISLAND_CV4_PHY_OB(VOLSPN, DBH, THT, HT_BREAK, DIA_UPPER, HT_UPPER, DIA_CENTROID, HT_CENTROID, HT_DBH)
  ! CU000145 PACIFIC_ISLAND_CV4_NET_OB(VOLSPN, DBH, THT, HT_BREAK, DIA_UPPER, HT_UPPER, DIA_CENTROID, HT_CENTROID, HT_DBH)
  ! NVEL VOLUME EQUATION NUMBER:
  ! P03ISL****
subroutine PACIFIC_ISLAND(VOLEQ,DBHOB,DRCOB,HTTOT,UPSHT1,UPSD1,&
     &UPSHT2,UPSD2,MTOPP,STUMP,VOL,ERRFLG)
  ! UPSHT1 - HT_UPPER = Height to Upper Stem dia measurement
  ! UPSD1 - DIA_UPPER = Upper Stem Diameter
  ! UPSHT2 - HT_BREAK = Height to top break
  ! UPSD2 - When UPSHT2 is height to top break, UPSD2 has to be 0 and THT needs to be 0
  ! OR
  ! CU000150 FUNCTION CENTROID_CV
  ! UPSHT2 - HT_CENTROID = The height (in feet) to stem centroid
  ! UPSD2 - DIA_CENTROID = Centroid diameter
  ! Variables
  character(len=10) :: VOLEQ
  real    :: DBHOB,DRCOB,HTTOT,UPSHT1,UPSD1,UPSHT2,UPSD2,MTOPP,STUMP
  real    :: VOL(15)
  integer :: ERRFLG,SPN
  real    :: DIA,HT,HT_BREAK,DIA_UPPER,HT_UPPER,DIA_CENTROID,HT_CENTROID
  real    :: HT_DBH,DIA_TOP,HT_STUMP,CVTS,CV4
  CYLINDER_CVTS(DIA,HT) = 3.14 * (DIA/24.0)**2 * HT
  if (MTOPP.lt.0.1) MTOPP = 4.0
  if (STUMP.lt.0.1) STUMP = 1.0
  read(VOLEQ(7:10),'(I4)')SPN
  if (DBHOB.gt.0.1) then
     DIA = DBHOB
  else if (DRCOB.gt.0.1) then
     DIA = DRCOB
  end if
  VOL = 0.0
  HT = HTTOT
  if (SPN.ge.6545.and.SPN.le.6549) then !SPN IN (6545, 6546, 6547, 6548, 6549)
     !       IF TREES HAVE BROKEN HEIGHT AND NO TOTAL HEIGHT
     !       THE BROKEN HEIGHT IS INPUTED AS UPSHT2 AND UPSD2 IS SET 0
     if (UPSHT2.gt.0.1.and.UPSD2.eq.0.0) HT = UPSHT2
     VOL(1) = CYLINDER_CVTS(DIA,HT)
     VOL(4) = VOL(1)
     !     TREES WITH UPPER STEM HEIGHT AND UPPER STEM DIAMETER MEASUREMENTS. THEY ARE INPUTED AS UPSHT1 AND UPSD1
     !     IF THE TREE ALSO HAS BROKEN TOP HEIGHT, THE BROKEN HEIGHT IS INPUTED AS UPSHT2 AND THE UPSD2 IS SET TO 0
  else if (UPSHT1.gt.0.1.and.UPSD1.gt.0.1) then
     !     &        UPSHT2.GT.0.1.AND.UPSD2.EQ.0.0)THEN
     call FRUSTRUM_VOL(DBHOB,DRCOB,HTTOT,UPSHT1,UPSD1,&
          &UPSHT2,UPSD2,MTOPP,STUMP,VOL,ERRFLG)
     !     WHEN CENTROID_HT AND CENTROID_DIA ARE MEASURED, THEY ARE INPUTED AS UPSHT2 AND UPSD2
  else if (UPSHT2.gt.0.1.and.UPSD2.gt.0.1) then
     call CENTROID_CV(DIA,HT,UPSD2,UPSHT2,MTOPP,STUMP,VOL,ERRFLG)
  else
     call CONE_VOL(DBHOB,DRCOB,HTTOT,UPSHT2,UPSD2,MTOPP,&
          &STUMP,VOL,ERRFLG)
  end if
  return
  end subroutine PACIFIC_ISLAND
