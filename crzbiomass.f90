!================================!
!   VolumeLibrary : crzbiomass   !
!================================!
  ! YW 2016/03/08 Hard coded the weight factor for DF in Rogue Rive - Siskiyou NF
  ! YW 07/05/2017 R6 requests to remove the hardcoded weight factor
  ! YW 2018/11/02 CHANGED TO NOT REMOVE TIP FROM BRANCHES TO AVOID NEGATIVE NUMBER FOR SMALL TREES
  ! YW 2022/08/12 Check branches equation if using Crown (CRW) equation and reset foliage to 0.
  !      THIS SUBROUTINE SEARCH THE REGIONAL SPECIES DEFAULT RECORDS TO FIND THE SPECIES
  !      DEFAULT WEIGHT FACTOR AND COMPONENT BIOMASS EQUATION FOR THE REGIONAL/FOREST
subroutine CRZSPDFT(REGN, FORST, SPCD, WF, BMSEQ, REF)
  implicit none
  ! Common blocks
  include 'wdbkwtdata.inc' !'WDBKWTDATA.INC'
  include 'regndftdata.inc' !'REGNDFTDATA.INC'
  include 'beqrefinfo.inc'

  ! Variables
  integer :: REGN,IFORST,SPCD,I,J,K
  real    :: WF(3),DWF,REGNWF(3),NATLWF
  character(len=2) :: FORST
  character(len=12) :: BMSEQ(8)
  character(len=12) :: BIOEQ
  character(len=50) :: REF(8)
  character(len=3) :: REFABRV
  integer :: FIRST,LAST,HALF,DONE

  read(FORST,'(i2)') IFORST
  do 3, J = 1,8
  BMSEQ(J) = '-'
  REF(J) = ''
  3 continue
  DONE = 0
  I = 0
  !      First to check if the species has a regional/forest default record
  !      Skip search regional default if REGN = 0 (YW 20190918)
  if (REGN.eq.0) DONE = -1
  do 5, while (DONE.eq.0)
  I = I + 1
  if (SPREGNDFTWF(I,1).eq.REGN) then
     if ((SPREGNDFTWF(I,2).eq.IFORST .and.&
          &SPREGNDFTWF(I,3).eq.SPCD) .or.&
          &(SPREGNDFTWF(I,2).eq.0.and.&
          &SPREGNDFTWF(I,3).eq.SPCD)) then
        DONE = I
        REGNWF(1) = SPREGNDFTWF(I,4)
        REGNWF(2) = SPREGNDFTWF(I,5)
        REGNWF(3) = SPREGNDFTWF(I,6)
        BMSEQ(1) = SPREGNDFTBEQ(I,1)
        BMSEQ(2) = SPREGNDFTBEQ(I,2)
        BMSEQ(3) = SPREGNDFTBEQ(I,3)
        BMSEQ(4) = SPREGNDFTBEQ(I,4)
        BMSEQ(7) = SPREGNDFTBEQ(I,5)
     end if
  end if
  if (I.ge.TOTDFT.and.DONE.eq.0) DONE = -1
  5 continue
  if (DONE.gt.0) then
     !Found regional species default, then get the default equation and reference
     !SECONDARY WEIGHT FACTOR
     if (WF(2).gt.0) then
        REF(6) = 'USER PROVIDED WEIGHT FACTOR (Non-Saw)'
     else if (REGNWF(2).gt.0.1) then
        WF(2) = REGNWF(2)
        REF(6) = 'USE REGIONAL DEFAULT WEIGHT FACTOR (Non-Saw)'
     else
        if (WF(1).gt.0) then
           WF(2) = WF(1)
           REF(6) = 'USER PROVIDED WEIGHT FACTOR(Sawtimber)'
        else
           WF(2) = REGNWF(1)
           REF(6) = 'USE REGIONAL DEFAULT WEIGHT FACTOR (Sawtimber)'
        end if
     end if
     !PRIMARY WEIGHT FACTOR
     if (WF(1).gt.0.1) then
        REF(5) = 'USER PROVIDED WEIGHT FACTOR(Sawtimber)'
     else if (REGNWF(1).gt.0.1) then
        WF(1) = REGNWF(1)
        REF(5) = 'USE REGIONAL DEFAULT WEIGHT FACTOR (Sawtimber)'
     end if
     !MOISTURE CONTENT
     if (WF(3).gt.0) then
        REF(8) = 'USER PROVIDED MOISTURE CONTENT'
     else if (REGNWF(3).gt.0.1) then
        WF(3) = REGNWF(3)
        REF(8) = 'USE REGIONAL DEFAULT MOISTURE CONTENT'
     end if
     !BIOMASS EQN REFERENCE
     do K = 1, 5
        REFABRV = " "
        if (K.le.4) then
           BIOEQ = BMSEQ(K)
        else
           BIOEQ = BMSEQ(7)
        end if
        if (LEN_TRIM(BIOEQ).eq.12) REFABRV = BIOEQ(1:3)

        if (LEN_TRIM(REFABRV).eq.3) then
           do I = 1,REFCNT
              if (REFABRV.eq.BEQREF(I)%ABRV) then
                 if (K.le.4) then
                    REF(K) = BEQREF(I)%AUTH
                 else
                    REF(7) = BEQREF(I)%AUTH
                 end if
              end if
           end do
        end if
     end do
  end if
  !  Region 6 Rogue river - Siskiyou NF Douglas-fir has different Weight factor based on DBH
  !  DBH < 15 weight factor = 59, DBH > 15 weight factor = 51
  !  The weight factor is hardcoded here (YW 2016/03/08)
  if (SPCD.eq.204) SPCD = 202
  !  R6 requests to remove the following lines (YW 07/05/2017)
  !       IF(REGN.EQ.6.AND.IFORST.EQ.10.AND.SPCD.EQ.202)THEN
  !         WF(1) = 51
  !         WF(2) = 59
  !       ENDIF;
  if (DONE.le.0.or. WF(1).eq.0.or. WF(3) .eq.0) then
     !      IF THE SPECIES DOES NOT HAVE REGION DEFAULT, SEARCH NATIONAL DEFAULT
     DONE = 0
     I = 0
     FIRST = 1
     LAST = TOTSPC
     if (WF(1).lt.0.1.or. WF(3).lt.0.1) then
        do 10, while (DONE.eq.0)
        HALF = (LAST - FIRST +1)/2 + FIRST
        if (WDBKWT(HALF,1).eq.SPCD) then
           DONE = HALF
        else if (FIRST .eq. LAST) then
           DONE = -1
        else if (WDBKWT(HALF,1).lt.SPCD) then
           FIRST = HALF
        else
           LAST = HALF - 1
        end if
        10 continue
        !IF(DONE.LT.0) DONE = TOTSPC
        ! Use species 999 for invalid species (2024/05/30)
        if (DONE.lt.0) DONE = CNT999
        if (WF(1).lt.0.1) then
           WF(1) = WDBKWT(DONE,9)
           REF(5) = 'WEIGHT FACTOR FROM MILES & SMITH 2009'
           if (WF(2).lt.0.1) then
              WF(2) = WF(1)
              REF(6) = 'WEIGHT FACTOR FROM MILES & SMITH 2009'
           end if
           if (WF(3).lt.0.1) then
              DWF = WDBKWT(DONE,10)
              WF(3) = (WF(1) - DWF)/DWF*100
              REF(8) = 'MOISTURE CONTENT FROM MILES & SMITH 2009'
           end if
        else
           if (WF(2).lt.0.1) WF(2) = WF(1)
           if (WF(3).lt.0.1) then
              DWF = WDBKWT(DONE,10)
              WF(3) = (WF(1) - DWF)/DWF*100
              REF(8) = 'MOISTURE CONTENT FROM MILES & SMITH 2009'
           end if
        end if
     end if
  end if
end

  ! THIS SUNROUTINE IS SEARCHING THE SPECIES DEFAULT DATA(SMITH AND MILES 2009) TO GET WOOD DENSITY
subroutine WOODDEN(SPCD, WDEN, BDEN)
  implicit none
  ! Common blocks
  include 'wdbkwtdata.inc' !'WDBKWTDATA.INC'
  ! Variables
  integer :: SPCD,FIRST,LAST,HALF,DONE
  real    :: WDEN,BDEN
  DONE = 0
  FIRST = 1
  LAST = TOTSPC
  do 50, while (DONE.eq.0)
  HALF = (LAST - FIRST +1)/2 + FIRST
  if (WDBKWT(HALF,1).eq.SPCD) then
     DONE = HALF
  else if (FIRST .eq. LAST) then
     DONE = -1
  else if (WDBKWT(HALF,1).lt.SPCD) then
     FIRST = HALF
  else
     LAST = HALF - 1
  end if
  50 continue
  !IF(DONE.LT.0) DONE = TOTSPC
  if (DONE.lt.0) DONE = CNT999
  WDEN = WDBKWT(DONE,4)
  BDEN = WDBKWT(DONE,5)
end

  ! THIS SUNROUTINE IS SEARCHING THE SPECIES DEFAULT DATA(SMITH AND MILES 2009) TO GET SAPLING ADJUSTMENT FACTOR
subroutine SAPLINGADJ(SPCD, ADJ)
  implicit none
  ! Common blocks
  include 'wdbkwtdata.inc' !'WDBKWTDATA.INC'
  ! Variables
  integer :: SPCD,FIRST,LAST,HALF,DONE
  real    :: ADJ
  DONE = 0
  FIRST = 1
  LAST = TOTSPC
  do 55, while (DONE.eq.0)
  HALF = (LAST - FIRST +1)/2 + FIRST
  if (WDBKWT(HALF,1).eq.SPCD) then
     DONE = HALF
  else if (FIRST .eq. LAST) then
     DONE = -1
  else if (WDBKWT(HALF,1).lt.SPCD) then
     FIRST = HALF
  else
     LAST = HALF - 1
  end if
  55 continue
  !IF(DONE.LT.0) DONE = TOTSPC
  if (DONE.lt.0) DONE = CNT999
  ADJ = WDBKWT(DONE,11)
end

subroutine CRZBIOMASS(REGN,FORST,SPCD,DBHOB,DRCOB, HTTOT,FCLASS,&
     &VOL,WF,BMS,ERRFLG,PROD)
  implicit none
  ! Variables
  integer :: REGN,IFORST,SPCD,ERRFLG,I,J,LENGTH,FCLASS
  real    :: DBHOB,HTTOT,DBH,THT,VOL(15),WF(3),BMS(8),DRCOB
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=12) :: BMSEQ(8)
  character(len=12) :: BEQ
  character(len=50) :: REF(8)
  real    :: MERCHSTEM,P3,BIOMS,DRY2GRN,STEMGWT
  real    :: JNKBMS(8),RATIO,DEN,STUMPDRY,STUMPGRN
  integer :: stm,Pied,Quga
  real    :: drcp,drcq
  !      THE COMPONENTS IN BMS VARIABLES AS:
  !      1 ABOVEGROUND TOTAL
  !      2 BRANCHES + TOP
  !      3 DEAD BRANCHES
  !      4 FOLIAGE
  !      5 PRIMARY PROD (WOOD + BARK)
  !      6 SECONDARY (TOPWOOD) (WOOD + BARK)
  !      7 STEM TIP

  read(FORST,'(i2)') IFORST
  do 13, J = 1,8
  BMS(J) = 0.0
  13 continue
  !      Convert DRC to DBH (Chojnacky 1999)
  call DRC2DBH(SPCD,DRCOB,FCLASS,DBHOB)

  if (DBHOB.eq.0) goto 40
  DBH = DBHOB
  THT = HTTOT
  RATIO = 0
  !      IF WEIGHT FACTOR IS NOT PROVIDED, GET IT FROM REGIONAL OR NATIONAL DEFAULT
  if (WF(1).lt.0.1) then
     call CRZSPDFT(REGN,FORST,SPCD,WF,BMSEQ,REF)
     !For Non-saw product, use WF(2) from the default (2024/05/30)
     if (REGN.eq.1) then
        if (PROD.ne.'01') WF(1) = WF(2)
     else if (REGN.eq.5.and.PROD.eq.'20') then
        WF(1) = WF(2)
     end if
  end if
  ! Reset weight factor for DF based on DBH (YW 2016/03/08)
  if (SPCD.eq.204) SPCD = 202
  ! R6 requests to remove the following lines (YW 07/05/2017)
  !       IF(REGN.EQ.6.AND.IFORST.EQ.10.AND.SPCD.EQ.202)THEN
  !         IF(DBHOB.LT.15) WF(1) = 59
  !       ENDIF
  if (WF(2).lt.0.1) WF(2) = WF(1)
  !      GET THE MERCH STEM MASS (WOOD AND BARK) FROM VOL AND WEIGHT FACTOR
  BMS(5) = VOL(4)*WF(1)
  BMS(6) = VOL(7)*WF(2)
  BMS(7) = VOL(15)*WF(2)
  !      GET THE DRY WEIGHT FOR MERCH WOOD ONLY
  !       CALL WOODDEN(SPCD, DEN)
  !       MERCHSTEM = (VOL(4) + VOL(7))*DEN

  !      CALL JENKINS FUNCTION TO GET BIOMASS FOR TREE COMPONENT
  !      NOTE: JENKINS FUNCTION RETURN DRY BIOMASS FOR THE FOLLOWING COMPONENT
  !     1 ABOVE GROUND TOTAL
  !     2 STEM WOOD
  !     3 STEM BARK
  !     4 FOLIAGE
  !     5 ROOTS
  !     6 BRANCHES
  !     7 CROWN
  !     8 STEM WOOD AND BARK

  call JENKINS(SPCD, DBH, JNKBMS)

  !      CONVERT DRY BIOMASS TO GREEN BIOMASS
  DRY2GRN = 1 + WF(3)/100
  JNKBMS = JNKBMS * DRY2GRN
  !      THE TOTAL STEM WEIGHT FROM STUMP TO 4' TOP
  STEMGWT = BMS(5) + BMS(6)
  !      IF NO VOLUME FOR THE MAIN STEM IS PROVIDED, USING THE BIOMASS CALCULATED FRON JENKINS EQUATION
  !       IF(DBHOB.GE.5.AND.STEMGWT.EQ.0) STEMGWT = JNKBMS(8)
  !      BECAUSE IT IS BELIEVED THE VOLUME FROM VOLLIB AND LOCAL WEIGHT FACTOR HAS
  !      BETTER ESTIMATE GREEN BIOMASS FOR MERCH STEM, SO A RATIO FACTOR IS GOTTEN
  !      TO ADJUST THE BIOMASS FROM JENKINS EQUATION. FOR SAPLING THE ADJUSTMENT FACTOR
  !      IS USING THE DEFAULT VALUE FROM THE DEFAULT VALUE
  !      GET THE MASS RATIO FOR THE STEM
  !       RATIO = MERCHSTEM/JNKBMS(2)
  if (STEMGWT.gt.0.and.JNKBMS(8).gt.0) RATIO = STEMGWT/JNKBMS(8)

  !      Added check if merch vol is calculated for small trees(2017/02/21)
  if (DBHOB.lt.5.and.STEMGWT.le.0) then
     call SAPLINGADJ(SPCD, RATIO)
     !        Small tree does not have stem component biomass
     !         BMS(5) = 0
     !         BMS(6) = 0
     BMS(7) = 0
  end if
  if (RATIO.le.0) RATIO = 1

  !      ADJUST BIOMASS FROM JENKINS
  BMS(1) = JNKBMS(1)*RATIO
  BMS(2) = JNKBMS(6)*RATIO
  BMS(4) = JNKBMS(4)*RATIO
  !      If tip is calculated seperately, remove it from branches
  !      CHANGED TO NOT REMOVE TIP FROM BRANCHES TO AVOID NEGATIVE NUMBER FOR SMALL TREES (YW 2018/11/02)
  !       IF(BMS(7).GT.0) BMS(2) = BMS(2)-BMS(7)
  !      RETURN MAINSTEM BIOMASS FROM JENKINS WHEN THERE IS NO VOLUME FOR MAINSTEM(6/13/2016)
  !      For tree DBH>=5 and no merch vol calculated, stem goes to branches (03/07/2017)
  if (BMS(5).eq.0.and.BMS(6).eq.0.and.DBHOB.ge.5) then
     BMS(2) = BMS(1) - BMS(4)
  end if
  !      For small trees, if merch vol is calculated to topd < 4", adjust the branches total.
  !      2017/02/21
  if (JNKBMS(8).eq.0.and.STEMGWT.gt.0) BMS(2) = BMS(2)-BMS(5)-BMS(6)

  !      IF BIOMASS EQUSTION IS SET IN REGIONAL DEFAULT, USE IT TO CALCULATE BIOMASS
  do 30, J = 1,5
  if (J.lt.5) then
     I = J
  else
     I = 7
  end if
  BEQ = BMSEQ(I)
  if (BEQ.ne.''.and.(BEQ(12:12).eq.'D'.or.BEQ(12:12).eq.'G')) then
     call CalcBiomass(BEQ, DBHOB, THT, P3, BIOMS)
     !      The BIOMS returned from CalcBiomass is green or dry based on BIOEQ
     !      So convert to GREEN biomass for cruise processing
     if (BEQ(12:12).eq.'D') BIOMS = BIOMS*DRY2GRN
     BMS(I) = BIOMS
     !           IF(BEQ(12:12).EQ.'D') BMS(I) = BIOMS*DRY2GRN
     !      Check branches using crown equation which include foliage, then reset foliage to 0 (2022/08/12)
     if (J.eq.2.and.BEQ(7:9).eq.'CRW') BMS(4) = 0.0
     !      chek foliage equation and remove it from Crown
     if (J.eq.4.and.BMSEQ(2)(7:9).eq.'CRW') BMS(2) = BMS(2)-BMS(4)
  end if
  30 continue
  40 return
end

  ! Convert DRC to DBH for woodland species
  ! Chojnack, D. C. and Rogers, P. Converting Tree Diameter Measured at Root Collar
  ! to Diameter at Breast Height. WJAF 14(1), 1999
subroutine DRC2DBH (SPCD,DRCOB,FCLASS, DBHOB)
  ! Variables
  integer :: SPCD,Pied,Quga,stm,FCLASS
  real    :: DRCOB,DBHOB,drcp,drcq

  if (DRCOB.gt.0) then
     Pied = 0
     Quga = 0
     stm = 1
     if (FCLASS.gt.1) stm = 0
     !        Pinyon pine species
     if (SPCD.eq.106.or.SPCD.eq.133.or.SPCD.eq.134&
          &.or.SPCD.eq.138.or.SPCD.eq.140.or.SPCD.eq.141&
          &.or.SPCD.eq.143) then
        Pied = 1
        drcp = DRCOB
        !        Gambel oak species
     else if (SPCD.eq.803.or.SPCD.eq.810.or.SPCD.eq.814&
          &.or.SPCD.eq.829.or.SPCD.eq.843.or.SPCD.eq.846&
          &.or.SPCD.eq.847) then
        Quga = 1
        drcq = DRCOB
     end if
     DBHOB = -2.6843+1.0222*DRCOB+0.7433*stm&
          &+0.7469*Pied-0.0399*drcp+1.2244*Quga-0.0689*drcq
     if (DBHOB.lt.0) DBHOB = 0
  end if
  return
end
