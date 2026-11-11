!============================!
!   VolumeLibrary : fia_nc   !
!============================!
subroutine FIA_NC(BEQ, DBHOB, HTTOT, VOL, BMS)
  ! Variables
  character(12) :: BEQ
  character(3) :: COMP
  real    :: DBHOB,HTTOT,BMS,DBH
  real    :: VOL(15)
  integer :: SPN,SPLIST(49),DONE,LAST,ERRFLG,STEMS,I
  real    :: BOLE_WD,BOLE_BK,ABV_GRD,ADJ
  real    :: BK_RAT(49),C1(49),C2(49),A1(49)
  integer :: WD_DWT(49),WD_GWT(49),BK_DWT(49),BK_GWT(49)
  real    :: WD_CFWT,BK_CFWT,BOLE,TOP_WDVOL,TOP_WD,TOP_BKVOL,TOP_BK
  real    :: TOP_LIMB,STUMP_WDVOL,STUMP_WD,STUMP_BKVOL,STUMP_BK,STUMP

  data (SPLIST(I),I = 1,49)/&
       &12, 68, 71, 94, 95,103,105,110,122,125,&
       &129,130,131,132,221,241,261,313,316,318,&
       &371,373,375,402,407,462,531,543,544,601,&
       &602,611,621,701,731,741,742,743,746,762,&
       &802,830,833,835,901,922,951,972,999/

  data (BK_RAT(I),I = 1,49)/&
       &0.15,0.12,0.13,0.10,0.14,0.15,0.17,0.15,0.15,0.16,&
       &0.16,0.13,0.15,0.15,0.13,0.12,0.21,0.15,0.12,0.12,&
       &0.12,0.16,0.16,0.13,0.13,0.15,0.07,0.14,0.16,0.15,&
       &0.15,0.15,0.15,0.15,0.08,0.18,0.15,0.18,0.18,0.10,&
       &0.18,0.15,0.14,0.18,0.15,0.16,0.16,0.14,0.15/

  data (WD_DWT(I),I = 1,49)/&
       &21,27,31,23,24,26,25,29,24,26,&
       &23,24,29,25,26,18,24,26,31,35,&
       &34,30,30,37,40,31,35,28,34,22,&
       &32,29,25,28,29,23,23,24,24,29,&
       &37,35,35,37,26,21,20,29,28/

  data (BK_DWT(I),I = 1,49)/&
       &24,25,19,18,24,26,21,20,21,15,&
       &31,28,20,25,25,19,25,26,32,34,&
       &35,32,32,37,37,31,35,28,21,25,&
       &17,29,25,28,28,31,27,31,31,30,&
       &33,41,41,33,26,27,28,17,28/

  data (WD_GWT(I),I = 1,49)/&
       &46,43,55,46,45,46,47,58,45,49,&
       &43,43,58,45,57,36,50,46,55,63,&
       &61,54,54,59,64,52,59,50,54,44,&
       &57,60,52,50,58,50,50,48,48,49,&
       &59,63,63,59,46,46,40,58,50/

  data (BK_GWT(I),I = 1,49)/&
       &53,40,34,36,46,47,40,40,40,29,&
       &59,50,50,45,55,38,53,47,58,61,&
       &63,58,58,59,59,53,60,50,34,50,&
       &31,61,53,50,56,68,59,62,62,51,&
       &53,74,74,53,47,59,56,34,50/

  data (C1(I),I = 1,49)/&
       &0.061,0.061,0.092,0.061,0.061,0.0,0.092,0.092,0.092,0.092,&
       &0.092,0.061,0.092,0.092,0.061,0.061,0.061,0.0,0.0,0.0,&
       &0,0,0,0,0,0,0,0,0,0,&
       &0,0,0,0,0,0,0,0.106,0.106,0,&
       &0,0,0,0,0,0,0,0,0/

  data (C2(I),I = 1,49)/&
       &0.659,0.659,1.628,0.659,0.659,0.471,1.628,1.628,1.628,1.628,&
       &1.628,0.659,1.628,1.628,0.659,0.659,0.659,0.471,0.471,0.471,&
       &0.471,0.471,0.471,0.471,0.471,0.471,0.471,0.471,0.471,0.471,&
       &0.471,0.471,0.471,0.471,0.471,0.471,0.471,0.832,0.832,0.471,&
       &0.471,0.471,0.471,0.471,0.471,0.471,0.471,0.471,0.471/

  data (A1(I),I = 1,49)/&
       &0.009967,0.008877,0.008877,0.010699,0.008877,&
       &0.00898, 0.007017,0.007176,0.007176,0.007176,&
       &0.008269,0.008877,0.007176,0.007176,0.008269,&
       &0.011946,0.008579,0.00898, 0.008476,0.008894,&
       &0.009968,0.00838, 0.00838, 0.00898, 0.00898,&
       &0.010422,0.010202,0.011016,0.008728,0.00898,&
       &0.00898,0.00898, 0.00898, 0.00898, 0.00898,&
       &0.006594,0.011145,0.006594,0.007369,0.00898,&
       &0.009727,0.00898, 0.008908,0.009727,0.00898,&
       &0.011145,0.009639,0.010422,0.00898/

  read(BEQ(4:6),'(I3)') SPN
  COMP = BEQ(7:9)
  LAST = 49
  DONE = 0
  DBH = DBHOB

  !     First check the species for in the SPLIST1
  call SEARCH(LAST,SPLIST,SPN,DONE,ERRFLG)
  if (DONE.gt.0) then
     if (BEQ(12:12).eq.'D') then
        WD_CFWT = WD_DWT(DONE)
        BK_CFWT = BK_DWT(DONE)
     else
        WD_CFWT = WD_GWT(DONE)
        BK_CFWT = BK_GWT(DONE)
     end if
     DBH = DBHOB
     if (DBHOB.lt.5) DBH = 5

     BOLE_WD = VOL(4)*WD_CFWT
     BOLE_BK = VOL(4)*BK_RAT(DONE)*BK_CFWT
     BOLE = BOLE_WD + BOLE_BK

     TOP_WDVOL = (C1(DONE) + (1.0/DBH**C2(DONE)))*VOL(4)
     TOP_WD = TOP_WDVOL*WD_CFWT
     TOP_BKVOL = TOP_WDVOL*BK_RAT(DONE)
     TOP_BK = TOP_BKVOL*BK_CFWT
     TOP_LIMB = TOP_WD + TOP_BK

     STUMP_WDVOL = A1(DONE)*DBH**2
     STUMP_WD = STUMP_WDVOL*WD_CFWT
     STUMP_BKVOL = STUMP_WDVOL*BK_RAT(DONE)
     STUMP_BK = STUMP_BKVOL*BK_CFWT
     STUMP = STUMP_WD + STUMP_BK
     ABV_GRD = BOLE + STUMP + TOP_LIMB

     if (DBHOB.lt.5) then
        ADJ = (DBHOB/DBH)**2.4323866
        ABV_GRD = ADJ*ABV_GRD
        TOP_LIMB = 0
        STUMP = 0
     end if

     !       Return the biomass for the BEQ
     !       Stem and branches wood and bark
     if (COMP.eq.'AWB') then
        BMS = ABV_GRD
        !       Merch stem total (wood and bark)
     else if (COMP.eq.'MST') then
        BMS = BOLE
        !       Merch stem wood only
     else if (COMP.eq.'MSW') then
        BMS = BOLE_WD
        !       Merch stem bark only
     else if (COMP.eq.'MSB') then
        BMS = BOLE_BK
     else if (COMP.eq.'AGW') then
        BMS = BOLE_WD + TOP_WD + STUMP_WD
     else if (COMP.eq.'AGB') then
        BMS = BOLE_BK + TOP_BK + STUMP_BK
     else if (COMP.eq.'SBT') then
        BMS = TOP_LIMB + STUMP
        !       Top limb (branched plus stem tip)
     else if (COMP.eq.'BTP') then
        BMS = TOP_LIMB
     end if
  end if
end
