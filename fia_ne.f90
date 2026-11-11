!============================!
!   VolumeLibrary : fia_ne   !
!============================!
subroutine FIA_NE(BEQ, DBHOB, HTTOT, VOL, BMS)
  ! Variables
  character(12) :: BEQ
  character(3) :: COMP
  real    :: DBHOB,HTTOT,BMS,DBH
  real    :: VOL(15)
  integer :: SPN,SPLIST(21),DONE,FIRST,LAST,ERRFLG,I
  real    :: Y1(21),Y2(21)
  integer :: MDL(21)
  real    :: ABVGRD_TOT,PCT_FOL,PCT_BRA,PCT_STP_RT,PCT_MST
  real    :: M3,PCT_STEM,PCT_STEM_BRA

  data (SPLIST(I),I = 1,21)/&
       &12, 94,129,131,132,241,261,310,372,746,&
       &400,421,611,802,832,833,835,837,318,531,999/

  data (Y1(I),I = 1,21)/&
       &0.5958,0.8079,0.4080,0.8162,0.7157,&
       &1.1182,0.6803,0.9392,1.1297,0.4689,&
       &1.9338,1.5779,2.5883,1.2892,2.1202,&
       &1.6891,2.6574,2.1457,5.2480,5.3373,3.2031/

  data (Y2(I),I = 1,21)/&
       &2.4017,2.3316,2.4490,2.2453,2.3865,&
       &1.9269,2.3617,2.3804,2.3376,2.6087,&
       &2.6209,2.5153,2.4253,2.7010,2.5344,&
       &2.6598,2.4395,2.5030,-0.3661,-0.3257,-0.2337/

  data (MDL(I),I = 1,21)/&
       &1,1,1,1,1,1,1,1,1,1,&
       &2,2,2,2,2,2,2,2,3,3,3/

  read(BEQ(4:6),'(I3)') SPN
  COMP = BEQ(7:9)
  FIRST = 1
  LAST = 21
  DONE = 0
  DBH = DBHOB

  !     First check the species for in the SPLIST
  !CALL SEARCH(LAST,SPLIST,SPN,DONE,ERRFLG)
  do I = FIRST, LAST
     if (SPN.eq.SPLIST(I)) then
        DONE = I
        exit
     end if
     10 continue
  end do
  if (DONE.gt.0) then
     !       First calculate the total above ground dry biomass including stump, branches and foliage
     !       Created from FCS_TREE_BIOM_NERS written by Carol Alerich
     if (MDL(DONE).eq.1) then
        ABVGRD_TOT = EXP(Y1(DONE) + Y2(DONE)*LOG(DBH))
     else if (MDL(DONE).eq.2) then
        ABVGRD_TOT = 10**(LOG10(Y1(DONE)) + Y2(DONE)*LOG10(DBH))
     else if (MDL(DONE).eq.3) then
        if (SPN.eq.318) then
           M3 = 0.0076
        else if (SPN.eq.531) then
           M3 = 0.0072
        else if (SPN.eq.999) then
           M3 = 0.0061
        end if
        ABVGRD_TOT = (Y1(DONE)+Y2(DONE)*DBH*25.4+M3*(DBH*25.4)**2)*2.2046
     end if

     !       Then calculate component ratio
     if (SPN.lt.300) then
        if (DBH.ge.5.and.DBH.lt.9) then
           PCT_BRA = 0.1402
           PCT_FOL = 0.1126
           PCT_STP_RT = 0.2552
           PCT_MST = 0.6503
        else if (DBH.ge.9.and.DBH.lt.15) then
           PCT_BRA = 0.141
           PCT_FOL = 0.096
           PCT_STP_RT = 0.2547
           PCT_MST = 0.7695
        else if (DBH.ge.15.and.DBH.lt.21) then
           PCT_BRA = 0.1427
           PCT_FOL = 0.086
           PCT_STP_RT = 0.255
           PCT_MST = 0.8053
        else if (DBH.ge.21) then
           PCT_BRA = 0.1457
           PCT_FOL = 0.0768
           PCT_STP_RT = 0.2563
           PCT_MST = 0.8136
        end if
     else
        if (DBH.ge.5.and.DBH.lt.11) then
           PCT_BRA = 0.1093
           PCT_FOL = 0.0348
           PCT_STP_RT = 0.2673
           PCT_MST = 0.7269
        else if (DBH.ge.11.and.DBH.lt.15) then
           PCT_BRA = 0.0991
           PCT_FOL = 0.0268
           PCT_STP_RT = 0.252
           PCT_MST = 0.7737
        else if (DBH.ge.15.and.DBH.lt.21) then
           PCT_BRA = 0.0932
           PCT_FOL = 0.0224
           PCT_STP_RT = 0.2412
           PCT_MST = 0.7852
        else if (DBH.ge.21) then
           PCT_BRA = 0.086
           PCT_FOL = 0.0175
           PCT_STP_RT = 0.231
           PCT_MST = 0.7884
        end if
     end if
     PCT_STEM = 1 - PCT_BRA - PCT_FOL
     PCT_STEM_BRA = 1 - PCT_FOL

     !       calculate component biomass with ratio

     !       Return the biomass for the BEQ
     !       Total stem
     if (COMP.eq.'STT') then
        BMS = ABVGRD_TOT*PCT_STEM
        if (DBH.lt.5) BMS = 0
        !       Merch stem total
     else if (COMP.eq.'MST') then
        BMS = ABVGRD_TOT*PCT_MST
        !       BRANCHES
     else if (COMP.eq.'BRT') then
        BMS = ABVGRD_TOT*PCT_BRA
        !       FOLIAGE
     else if (COMP.eq.'FOT') then
        BMS = ABVGRD_TOT*PCT_FOL
        !       STUMP+ROOT
     else if (COMP.eq.'SMR') then
        BMS = ABVGRD_TOT*PCT_STP_RT
        !       STEM+BRANCHES
     else if (COMP.eq.'AWB') then
        if (DBH.lt.5) then
           BMS = EXP(0.95595 + 2.4264*LOG(DBH))
        else
           BMS = ABVGRD_TOT*PCT_STEM_BRA
        end if
        !       ABOVE GROUND TOTAL
     else if (COMP.eq.'AGT') then
        BMS = ABVGRD_TOT
     end if
  else
     ERRFLG = 15
  end if
end
