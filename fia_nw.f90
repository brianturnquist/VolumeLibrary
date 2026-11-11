!============================!
!   VolumeLibrary : fia_nw   !
!============================!
subroutine FIA_NW(BEQ, DBHOB, HTTOT, VOL, BMS)
  !      INCLUDE 'wdbkwtdata.inc'       !'WDBKWTDATA.INC'
  ! Variables
  character(12) :: BEQ
  character(12) :: BIOEQ
  character(3) :: COMP
  real    :: DBHOB,HTTOT,BMS,DBH,HT1PRD,HT2PRD,CR,TOPD
  real    :: VOL(15),BIOMS(8)
  integer :: SPN,SPLIST(123),DONE,LAST,ERRFLG,I,STEMS
  real    :: WD_DEN(123),SG(11),WDDEN,BKDEN
  real    :: STEM_WDTOT,STEM_WDMCH,BRCH_L,BRCH_D,STEM_BRCH
  real    :: STEM_BK,FOL,BRCH_TOT,ABVGRD_TOT,CRWN,BRATIO
  character(2) :: EQMDL

  data (SPLIST(I),I = 1,123)/&
       &1, 11, 14, 15, 17, 19, 20, 21, 22, 41,&
       &42, 50, 51, 52, 53, 54, 55, 56, 62, 64,&
       &65, 66, 72, 73, 81, 92, 93, 94, 95, 98,&
       &101,102,103,104,108,109,113,116,117,119,&
       &120,122,124,127,130,133,137,142,201,202,204,&
       &211,212,231,242,251,263,264,298,299,312,&
       &313,320,321,322,330,333,341,351,352,361,&
       &374,375,421,431,475,492,500,510,511,540,&
       &542,547,591,600,602,603,604,611,631,660,&
       &661,730,731,746,747,748,756,758,760,763,&
       &766,768,771,801,805,807,811,815,818,821,&
       &826,839,901,920,922,926,927,981,990,997,&
       &998,999/

  data (WD_DEN(I),I = 1,123)/&
       &28.08,24.96,22.46,23.09,21.84,19.34,22.46,22.46,23.09,24.34,&
       &26.21,41.81,25.58,41.81,25.58,41.81,41.81,41.81,33.70,33.70,&
       &33.70,33.70,29.95,29.95,21.84,21.84,20.59,20.59,20.59,23.09,&
       &23.09,23.09,23.09,23.09,23.71,26.83,23.09,23.71,21.22,21.84,&
       &26.83,23.71,21.84,26.83,26.83,23.09,23.09,23.09,28.70,28.70,28.7,&
       &21.22,23.71,41.81,19.34,31.82,26.21,26.21,25.58,25.58,27.46,&
       &19.34,23.71,23.71,29.33,23.71,23.71,18.72,23.09,23.09,43.06,&
       &18.72,18.72,24.96,29.95,43.68,43.68,25.58,49.92,49.92,31.20,&
       &31.2 ,31.2 ,25.58,31.82,31.82,31.82,31.82,31.82,36.19,36.19,&
       &36.19,28.7 ,28.7 ,21.84,19.34,19.34,43.68,43.68,29.32,29.32,&
       &29.33,29.32,29.32,49.92,49.92,37.44,37.44,37.44,34.94,37.44,&
       &37.44,49.92,41.18,22.46,22.46,22.46,22.46,36.82,31.82,32.45,&
       &31.82,28.08/

  read(BEQ(4:6),'(I3)') SPN
  COMP = BEQ(7:9)
  EQMDL = BEQ(10:11)
  LAST = 123
  DONE = 0
  DBH = DBHOB

  !     First check the species for in the SPLIST
  !CALL SEARCH(LAST,SPLIST,SPN,DONE,ERRFLG)
  !IF(DONE.GT.0) THEN
  !       First calculate the total stem wood and merch stem wood using cubic volume
  !  STEM_WDTOT = VOL(1)*WD_DEN(DONE)
  !  STEM_WDMCH = VOL(4)*WD_DEN(DONE)
  call WOODDEN(SPN, WDDEN, BKDEN)
  STEM_WDTOT = VOL(1)*WDDEN
  STEM_WDMCH = VOL(4)*WDDEN

  BIOEQ(4:6) = BEQ(4:6)
  BIOEQ(10:12) = '01D'

  if (COMP.eq.'AWB') then
     !         CALCULATE BRANCHES AND STEM BARK USING STANDISH, SHAW, OR GHOLZ EQUATION
     if (EQMDL.eq.'ST') then
        !           STANDISH EQUSTION
        BIOEQ(1:3) = 'STA'
     else if (EQMDL.eq.'SH') then
        !           SHAW EQUSTION
        BIOEQ(1:3) = 'SHA'
     else if (EQMDL.eq.'G1'.or.EQMDL.eq.'G2') then
        !           GHOLZ EQUSTION
        BIOEQ(1:3) = 'GHZ'
     end if
     !         FIA equation 000051 and 000072 use local biomass config equation for bark and branches
     !         the local config equation, so here using Jenkins bark and branches for now 2019/07/26
     if (EQMDL.eq.'51'.or.EQMDL.eq.'72') then
        call JENKINS(SPN,DBH,BIOMS)
        STEM_BK = BIOMS(3)
        BRCH_L = BIOMS(6)
        if (EQMDL.eq.'72') then
           call MILESDATA(SPN, SG)
           STEM_WDTOT = STEM_WDTOT*(1.0+SG(8)/100.0)
           STEM_BRCH = STEM_WDTOT + BRCH_L
        else
           STEM_BRCH = STEM_WDTOT + STEM_BK + BRCH_L
        end if
     else
        BIOEQ(7:9) = 'BRL'
        BIOEQ(10:12) = '01D'
        call BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,&
             &HT2PRD,TOPD,STEMS,VOL,BRCH_L,ERRFLG)

        !         STEM BARK
        BIOEQ(7:9) = 'STB'
        call BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,&
             &STEMS,VOL,STEM_BK,ERRFLG)
        STEM_BRCH = STEM_WDTOT + STEM_BK + BRCH_L
     end if

     if (EQMDL.eq.'G2') then
        !           THE STEM WOOD IS ALSO CALCULATED USING GHOLZ EQUATION
        BIOEQ(7:9) = 'BRD'
        call BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,&
             &STEMS,VOL,BRCH_D,ERRFLG)

        BIOEQ(7:9) = 'STW'
        call BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,&
             &STEMS,VOL,STEM_WDTOT,ERRFLG)

        STEM_BRCH = STEM_WDTOT + STEM_BK + BRCH_L + BRCH_D
     end if

  else if (COMP.eq.'AGT') then
     !         CALCULATE ABOVE GROUND TOTAL USING GHOLZ EQUATION
     BIOEQ(1:3) = 'GHZ'
     BIOEQ(7:9) = 'BRL'
     BIOEQ(10:12) = '01D'
     call BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,&
          &STEMS,VOL,BRCH_L,ERRFLG)

     !         STEM BARK
     BIOEQ(7:9) = 'STB'
     call BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,&
          &STEMS,VOL,STEM_BK,ERRFLG)

     BIOEQ(7:9) = 'STW'
     call BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,&
          &STEMS,VOL,STEM_WDTOT,ERRFLG)

     BIOEQ(7:9) = 'FOT'
     call BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,&
          &STEMS,VOL,FOL,ERRFLG)

     BIOEQ(7:9) = 'BRD'
     call BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,&
          &STEMS,VOL,BRCH_D,ERRFLG)

     ABVGRD_TOT = STEM_WDTOT + STEM_BK + BRCH_L + BRCH_D + FOL

  else if (COMP.eq.'BRT'.and.EQMDL.eq.'SN') then
     !         CALCULATE BRANCHES USING SNALL EQUATION
     BIOEQ(1:3) = 'SNE'
     BIOEQ(7:9) = 'CRW'
     call BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,&
          &STEMS,VOL,CRWN,ERRFLG)

     BIOEQ(7:9) = 'FOT'
     call BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,&
          &STEMS,VOL,FOL,ERRFLG)

     BRCH_TOT = CRWN - FOL
  else if (COMP.eq.'MSB'.and.EQMDL.eq.'PI') then
     call PILLSBSTMBRKVOL(SPN,DBHOB,HTTOT, BRKVOL)
     !CALL MILESDATA(SPN, SG)
     !STEM_BK = BRKVOL*SG(5)    !SG(5) = bark weight (lb/cf)
     STEM_BK = BRKVOL*BKDEN
  else if (SPN.eq.211.and.COMP.eq.'STB') then
     !         Redwood stem bark
     !         It uses Harmon for large trees and Shaw cedar for small trees.
     BIOEQ(7:9) = 'STB'
     BIOEQ(10:11) = '01'
     if (DBHOB.gt.39.37) then
        BIOEQ(1:3) = 'HAN'
        BIOEQ(4:6) = '212'
     else
        BIOEQ(1:3) = 'SHA'
        BIOEQ(4:6) = '242'
     end if
     call BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,&
          &STEMS,VOL,STEM_BK,ERRFLG)
  else if (COMP.eq.'STB'.or.COMP.eq.'MSB') then
     BRATIO = 0.0
     if (SPN.eq.312) then
        BRATIO = 0.14
     else if (SPN.eq.351) then
        BRATIO = 0.18
     else if (SPN.eq.361) then
        BRATIO = 0.05
     else if (SPN.eq.431) then
        BRATIO = 0.22
     else if (SPN.eq.631) then
        BRATIO = 0.24
     end if
     if (COMP.eq.'STB') then
        STEM_BK = STEM_WDTOT*BRATIO
     else if (COMP.eq.'MSB') then
        STEM_BK = STEM_WDMCH*BRATIO
     end if
  end if

  !       Return the biomass for the BEQ
  !       Total stem wood
  if (COMP.eq.'STW') then
     if (VOL(1).eq.0.0) then
        ERRFLG = 18
        return
     end if
     BMS = STEM_WDTOT
     !       TOTAL STEM BARK
  else if (COMP.eq.'STB'.or.COMP.eq.'MSB') then
     BMS = STEM_BK
     !       Merch stem wood
  else if (COMP.eq.'MSW') then
     if (VOL(4).eq.0.0) then
        ERRFLG = 18
        return
     end if
     BMS = STEM_WDMCH
     !       STEM AND BRANCHES
  else if (COMP.eq.'AWB') then
     BMS = STEM_BRCH
     !       BRANCHES
  else if (COMP.eq.'BRT') then
     BMS = BRCH_TOT
     !       ABOVE GROUND TOTAL
  else if (COMP.eq.'AGT') then
     BMS = ABVGRD_TOT
  end if

  !ENDIF
end

  ! Computes DBH+DBT at breast high then feeds both DBH and DBH+DBT into Pillsbury CV4 to
  ! get bark volume.
subroutine PILLSBSTMBRKVOL(SPN, DBHOB, HTTOT, BRKVOL)
  ! Variables
  integer :: SPN,SPLIST(13),DONE,FIRST,LAST,I,ERRFLG
  real    :: DBHOB,HTTOT,MSTMBRK
  real    :: OUTDBH,CMDBH,OUTCV4,INCV4,BRKVOL
  real    :: T1(13),T2(13),C1(13),C2(13),C3(13),A,B,C

  data (SPLIST(I), I = 1,13)/&
       &312,361,431,631,801,805,807,811,815,818,821,839,981/

  data (T1(I), I = 1,13)/&
       &0.21235, -0.03425, 0.39534, -4.1177, -1.92379,&
       &-0.48584, -0.44003, -1.99573, -0.78034, -0.68133,&
       &-0.97254, 0.12237, -0.32491/

  data (T2(I), I = 1,13)/&
       &0.94782, 0.98155, 0.90182, 0.95354, 0.93475,&
       &0.96147, 0.94403, 0.92472, 0.95956, 0.95767,&
       &0.93545, 0.92953, 0.96579/

  data (C1(I), I = 1,13)/&
       &0.003421416, 0.002561643, 0.005521294, 0.000577497, 0.002457485,&
       &0.00316706, 0.004232407, 0.005386635, 0.002427703, 0.00367957,&
       &0.000968436, 0.004119226, 0.001638075/

  data (C2(I), I = 1,13)/&
       &2.35347, 1.99295, 2.07202, 2.19576, 2.53284,&
       &2.32519, 2.53987, 2.61268, 2.25575, 2.12635,&
       &2.39565, 2.14915, 2.0591/

  data (C3(I), I = 1,13)/&
       &0.69586, 1.01532, 0.77467, 1.14078, 0.60764,&
       &0.74348, 0.50591, 0.31103, 0.87108, 0.83339,&
       &0.98878, 0.77843, 1.05293/

  LAST = 13
  DONE = 0

  !     First check the species for in the SPLIST
  call SEARCH(LAST,SPLIST,SPN,DONE,ERRFLG)

  if (DONE.gt.0) then
     CMDBH = DBHOB*2.540005
     OUTDBH = (CMDBH + T1(DONE))/T2(DONE)/2.540005
     A = C1(DONE)
     B = C2(DONE)
     C = C3(DONE)
     OUTCV4 = A*OUTDBH**B*HTTOT**C
     INCV4 = A*DBHOB**B*HTTOT**C
     BRKVOL = OUTCV4 - INCV4
  end if
  return
end
