!============================!
!   VolumeLibrary : fia_pi   !
!============================!
  ! Asner etal 2011 High-resolution carbon mapping on the million-hectare island of Hawaii.
  ! Frontiers in Ecology and the Environment 9:434-439.
  ! Above Ground Dry Biomass in pounds
subroutine ASNER_AGT(SPN, DBH, THT, AGT)
  ! Variables
  integer :: SPN
  real    :: DBH,THT,AGT,P,DBHCM,THTM,WDEN,SG(11)
  ! THE ORIGINAL EQUATION DBH IN CM, THT IN M AND AGT IN KG, p in g/cm3
  DBHCM = DBH*2.54
  THTM = THT*0.3048
  call MILESDATA(SPN, SG)
  P = SG(4)/62.4
  AGT = 3.14*(DBHCM/2.0)**2*THTM*P/10.0
  ! RETURN AGT IN LB
  AGT = AGT*2.2046
  return
end

subroutine CHAVE_AGT(SPN,BEQ,DBH,THT,AGT)
  ! Variables
  integer :: SPN
  real    :: DBH,THT,AGT
  character(len=12) :: BEQ
  real    :: DBHCM,THTM,P,SG(11)
  character(len=2) :: EQTYPE
  AGT = 0.0
  DBHCM = DBH*2.54
  THTM = THT*0.3084
  call MILESDATA(SPN, SG)
  P = SG(4)/62.4
  EQTYPE = BEQ(10:11)
  if (BEQ(1:3).eq.'CHV'.or.EQTYPE.eq.'04') then
     ! Chave J et al. 2014. Improved allometric models to estimate the aboveground
     ! biomass of tropical trees. Glob Change Biol, 20: 3177.3190.
     AGT = 0.0673*(P*DBHCM**2*THTM)**0.976
  else
     ! Chave J, Andalo C, Brown S, et al. 2005. Tree allometry and improved estimation
     ! of carbon stocks and balance in tropical forests. Oecologia 145: 87-99.
     ! the first digit number in the eqtype as: 1 = dry, 2 = moist, 3 = wet, 0 = all
     if (EQTYPE.eq.'01') then
        AGT = -2.801+2.115*LOG(DBHCM)+0.78*LOG(THTM)+0.809*LOG(P)
     else if (EQTYPE.eq.'11') then
        AGT = -2.68+1.805*LOG(DBHCM)+1.038*LOG(THTM)+0.377*LOG(P)
     else if (EQTYPE.eq.'21') then
        AGT = -2.994+2.135*LOG(DBHCM)+0.824*LOG(THTM)+0.809*LOG(P)
     else if (EQTYPE.eq.'31') then
        AGT = -2.408+2.04*LOG(DBHCM)+0.659*LOG(THTM)+0.746*LOG(P)
     else if (EQTYPE.eq.'02') then
        AGT = -2.922 + 0.99*LOG(DBHCM**2*THTM*P)
     else if (EQTYPE.eq.'12') then
        AGT = -2.235 + 0.916*LOG(DBHCM**2*THTM*P)
     else if (EQTYPE.eq.'22') then
        AGT = -3.08 + 1.007*LOG(DBHCM**2*THTM*P)
     else if (EQTYPE.eq.'32') then
        AGT = -2.605 + 0.94*LOG(DBHCM**2*THTM*P)
     else if (EQTYPE.eq.'03') then
        AGT = -2.994 + LOG(DBHCM**2*THTM*P)
     else if (EQTYPE.eq.'13') then
        AGT = -2.843 + LOG(DBHCM**2*THTM*P)
     else if (EQTYPE.eq.'23') then
        AGT = -3.027 + LOG(DBHCM**2*THTM*P)
     else if (EQTYPE.eq.'33') then
        AGT = -3.024 + LOG(DBHCM**2*THTM*P)
     end if
     AGT = EXP(AGT)
  end if
  ! CONVERT TO LB
  AGT = AGT*2.2046
  return
end

subroutine PI_AGT(SPN,BEQ,DBH,THT,AGT)
  ! Variables
  integer :: SPN,ERRFLG
  real    :: DBH,THT,AGT
  character(len=12) :: BEQ
  real    :: HT1PRD,HT2PRD,CR,TOPD,BIOMS
  CR = 0.5
  HT1PRD = 0.0
  HT2PRD = 0.0
  TOPD = 4.0
  ERRFLG = 0
  if (SPN.ge.6545.and.SPN.le.6549) then
     call ASNER_AGT(SPN, DBH, THT, AGT)
  else if ((SPN.eq.6001.or.SPN.eq.303).and.DBH.lt.12.0) then
     BEQ = "AS6006AGT01D"
     call BIOEQDB(BEQ,DBH,THT,HT1PRD,HT2PRD,CR,TOPD,&
          &BIOMS,ERRFLG)
     AGT = BIOMS
  else if ((SPN.eq.7782.or.SPN.eq.7783).and.DBH.lt.12.0) then
     BEQ = "AS7783AGT01D"
     call BIOEQDB(BEQ,DBH,THT,HT1PRD,HT2PRD,CR,TOPD,&
          &BIOMS,ERRFLG)
     AGT = BIOMS
  else if ((SPN.eq.8355).and.DBH.lt.8.0) then
     BEQ = "AS8355AGT01D"
     call BIOEQDB(BEQ,DBH,THT,HT1PRD,HT2PRD,CR,TOPD,&
          &BIOMS,ERRFLG)
     AGT = BIOMS
  else
     call CHAVE_AGT(SPN,BEQ,DBH,THT,AGT)
  end if
  return
end

subroutine PI_CRM_MSW(SPN,BEQ,DBH,THT,MSW)
  ! Variables
  integer :: SPN
  real    :: DBH,THT,MSW,AGT
  character(len=12) :: BEQ
  real    :: BIOMS(8),JK_WDRATIO
  MSW = 0.0
  call JENKINS(SPN, DBH, BIOMS)
  JK_WDRATIO = BIOMS(2)/BIOMS(1)
  if (DBH.gt.5.0) then
     call PI_AGT(SPN,BEQ,DBH,THT,AGT)
     MSW = AGT*JK_WDRATIO
  end if
  return
end

subroutine PI_BIOMASS(SPN,BEQ,DBH,THT,CV4,BIOMS,ERRFLG)
  ! Variables
  integer :: SPN,ERRFLG,REGN
  real    :: DBH,THT,CV4,BIOMS,CR
  character(len=12) :: BEQ
  character(len=10) :: VOLEQ
  real    :: TOPD,VOL(15),MHT,DBHOB,HTTOT,WDEN,BDEN,HT1PRD,HT2PRD
  CR = 0.5
  ERRFLG = 0
  HT1PRD = 0.0
  HT2PRD = 0.0
  if (BEQ.eq.'FPI999MST01D') then
     ! biomass equation num 000120
     if (CV4.le.0.0) then
        VOLEQ = 'CU000144'
        REGN = 5
        TOPD = 4.0
        VOL = 0.0
        call VOLLIBVB8INIT(VOLEQ, REGN,DBHOB, HTTOT, TOPD,&
             &VOL, MHT, ERRFLG)
        CV4 = VOL(4)
     end if
     call WOODDEN(SPN,WDEN,BDEN)
     BIOMS = CV4*WDEN
  else if (BEQ(1:9).eq.'FPI999AGT') then
     ! biomass equation num 000121, 000123
     call PI_AGT(SPN,BEQ,DBH,THT,BIOMS)
  else if (BEQ(1:9).eq.'FPI999MSW') then
     ! biomass equation num 000122
     call PI_CRM_MSW(SPN,BEQ,DBH,THT,BIOMS)
  else if (BEQ(1:3).eq.'CAB'.or.BEQ(1:3).eq.'CHV') then
     ! biomass equation num 000124, 000125
     call CHAVE_AGT(SPN,BEQ,DBH,THT,BIOMS)
  else if (BEQ(1:3).eq.'ASN') then
     ! biomass equation num 000126
     call ASNER_AGT(SPN, DBH, THT, BIOMS)
  else if (BEQ.eq.'AS6006AGT01D'.or.BEQ.eq.'AS7783AGT01D'&
       &.or.BEQ.eq.'AS8355AGT01D') then
     ! biomass equation num 000127, 000128, 000129
     call BIOEQDB(BEQ,DBH,THT,HT1PRD,HT2PRD,CR,TOPD,&
          &BIOMS,ERRFLG)
  end if
  return
end
