!============================!
!   VolumeLibrary : fia_rm   !
!============================!
subroutine FIA_RM(BEQ, DBHOB, HTTOT, STEMS, VOL, BMS)
  ! Variables
  character(12) :: BEQ
  real    :: DBHOB,HTTOT,BMS,DBH,HT
  real    :: VOL(15),WD_DEN(15),BK_RAT(15),WT2(15),WT4(15),SG(13)
  integer :: SPN,SPLIST1(15),SPLIST2(13),DONE,LAST,ERRFLG,STEMS,I
  real    :: BOLE_WD,BOLE_BK,WDDEN,BKRAT,BOLE,TOP_LIMB,K,ABOVE_STUMP
  real    :: SPSG,Bio3_M,WT_FOL,WT_BRA,WT_FOL_M
  ! VALID EQUATION NUMBER:
  ! FROM Van Hooser and Chojnacky 1983. INT-29
  ! FRM***WB101D, FRM***MST01D, FRM***MSW01D, FRM***MSB01D, FRM***BTP01D
  ! WHERE *** IS 15, 17, 19, 73, 93,108, 119, 122, 202, 242, 263, 299, 746, 740, 999
  ! FRM202WB102D, FRM202MST02D, FRM202MSW02D, FRM202MSB02D, FRM202BTP02D
  ! FRM999WB102D, FRM999MST02D, FRM999MSW02D, FRM999MSB02D, FRM999BTP02D
  ! FROM Chojnacky CV TO 3" TOP
  ! FRM***WB101D, FRM***MST01D, FRM***AST01D, FRM***BRT01D, FRM***FOT01D
  ! FROM Chojnacky CV TO 1.5" TOP
  ! FRM***WB102D, FRM***MST02D, FRM***AST02D, FRM***BRT02D, FRM***FOT02D
  ! WHERE *** IS 57,63,65,69,106,310,475,755,756,757,758,810,814
  data (SPLIST1(I),I = 1,15)/15, 17, 19, 73, 93,&
       &108, 119, 122, 202, 242, 263, 299, 746, 740, 999/
  data (WD_DEN(I),I = 1,15)/23.09,21.84,19.34,29.95,20.59,&
       &23.71,21.84,23.71,28.08,19.34,26.21,23.71,21.84,19.34,24.96/
  data (BK_RAT(I),I = 1,15)/0.18,0.18,0.18,0.10,0.13,&
       &0.08,0.16,0.19,0.15,0.10,0.13,0.10,0.27,0.27,0.27/
  data (WT2(I),I = 1,15)/10,10,11,10,12,10,9,10,11,9,10,10,4,4,4/
  data (WT4(I),I = 1,15)/43,43,43,50,43,40,42,40,49,38,38,43,19,19,19/
  data (SPLIST2(I),I = 1,13)/57,63,65,69,106,&
       &310,475,755,756,757,758,810,814/
  data (SG(I),I = 1,13)/0.533,0.517,0.523,0.558,0.496,&
       &0.65,0.7,0.69,0.69,0.69,0.69,0.567,0.634/

  read(BEQ(4:6),'(I3)') SPN
  LAST = 15
  DONE = 0
  DBH = DBHOB
  HT = HTTOT
  WT_BRA = 0.0
  WT_FOL = 0.0

  !     First check the species for in the SPLIST1
  call SEARCH(LAST,SPLIST1,SPN,DONE,ERRFLG)
  if (DONE.gt.0) then
     if (VOL(4).gt.0.and. DBHOB.ge.5) then
        !         For species in most locations
        if (BEQ(10:11).eq.'01') then
           BOLE_WD = VOL(4)*WD_DEN(DONE)
           BOLE_BK = BOLE_WD*BK_RAT(DONE)
           !         For Douglas-fir and other hardwood species in CO, UT,
        else
           if (SPN.eq.202) then
              WDDEN = 26.83
              BKRAT = 0.15
           else
              WDDEN = 34.32
              BKRAT = 0.27
           end if
           BOLE_WD = VOL(4)*WDDEN
           BOLE_BK = BOLE_WD*BKRAT
        end if
        BOLE = BOLE_WD + BOLE_BK
        if (SPN.eq.122.or.SPN.eq.119.or.(SPN.eq.108.and.DBH.lt.14)) then
           TOP_LIMB = 193.5-43.5412*DBH+3.1659*DBH**2
        else if (SPN.lt.300) then
           TOP_LIMB = 0.191+2.0304*DBH+0.7031*DBH**2
        else
           K = EXP((-1.*(ABS((((DBH/27.56)-1.)/.56))**3.5)))
           TOP_LIMB = (129.69*K-.05)*(2.2046)
        end if
        ABOVE_STUMP = BOLE + TOP_LIMB
     else if (DBH.ge.3) then
        ABOVE_STUMP = WT4(DONE);
     else if (DBH.ge.1) then
        ABOVE_STUMP = WT2(DONE)
     end if

     !       Return the biomass for the BEQ
     !       Stem and branches wood and bark
     if (BEQ(7:9).eq.'WB1') then
        BMS = ABOVE_STUMP
        !       Merch stem total (wood and bark)
     else if (BEQ(7:9).eq.'MST') then
        BMS = BOLE
        !       Merch stem wood only
     else if (BEQ(7:9).eq.'MSW') then
        BMS = BOLE_WD
        !       Merch stem bark only
     else if (BEQ(7:9).eq.'MSB') then
        BMS = BOLE_BK
        !       Top limb (branched plus stem tip)
     else if (BEQ(7:9).eq.'BTP') then
        BMS = TOP_LIMB
     end if
  else
     !     Check species in the SPLIST2 (woodland species)
     LAST = 13
     DONE = 0
     call SEARCH(LAST,SPLIST2,SPN,DONE,ERRFLG)
     if (DONE.gt.0) then
        SPSG = SG(DONE)
        !       First check if the input vol(4) is the volume to 1.5 inch branch diameter.
        !       This is the volume calculated from Chojnacky (1985) equation. In this case,
        !       the VOL(4) is set to be same as VOL(1)
        !          IF(VOL(4).EQ.VOL(1))THEN
        !       CHANGED TO USE EQUATION NUMBER 02 FOR USING CV FOR 1.5" TOP.(YW 2019/05/20)
        if (BEQ(10:11).eq.'01') then
           BIO3 = VOL(4)*SPSG*62.4
        else if (BEQ(10:11).eq.'02') then
           if (VOL(1).eq.0) VOL(1) = VOL(4)
           call CHO_WDBK_1530(SPN,DBH,HTTOT,VOL(1),STEMS,SPSG,BIO3)
        end if
        BIO3_M = BIO3/2.2046
        if (SPN.ge.300) then
           WT_FOL = 10**(-.5655+(.8382*(LOG10(BIO3)))+(-.0094*HT))
           WT_BRA = 10**(.3036+(.7752*(LOG10(BIO3)))+(-.0049*HT))
        else
           if (SPN.eq.106) then
              if (BIO3_M.le.468.028) then
                 WT_FOL_M = EXP(1.0254+.559*LOG(BIO3_M))
              else
                 WT_FOL_M = EXP(1.0254+.559*(1.0+LOG(468.028)&
                      &-468.028/BIO3_M))
              end if
           else
              if (BIO3_M.le.150.0) then
                 WT_FOL_M = EXP(1.2867+.649*LOG(BIO3_M))
              else
                 WT_FOL_M = EXP(1.2867+.649*(1.0+LOG(150.0)-150.0/BIO3_M))
              end if
           end if
           WT_FOL = WT_FOL_M*2.2046
           if (BEQ(10:11).eq.'02') then
              WT_BRA = WT_FOL*0.75
              WT_FOL = WT_FOL*0.25
           end if
        end if

        !         Return component biomass
        !         Above ground wood and bark (stem plus branches)
        if (BEQ(7:9).eq.'WB1') then
           BMS = BIO3 + WT_BRA
           !         Above ground total (stem, branches and foliages)
        else if (BEQ(7:9).eq.'AST') then
           BMS = BIO3 + WT_BRA + WT_FOL
           !         merch stem total TO 1.5"/3.0* TOP DIB
        else if (BEQ(7:9).eq.'WB2') then
           BMS = BIO3
           !         Total branches
        else if (BEQ(7:9).eq.'BRT') then
           BMS = WT_BRA
           !         Foliage total
        else if (BEQ(7:9).eq.'FOT') then
           BMS = WT_FOL
        end if
     end if
  end if
end

  !     This routine calculate wood and bark biomass to 3 inch branch diameter with
  !     the given volume (CV15) to a 1.5 inch branch diameter
subroutine CHO_WDBK_1530(SPN, DRC, HT, CV15, STEMS, SG, BIO3)
  ! Variables
  integer :: SPN,STEMS,LAST,DONE,ERRFLG
  real    :: DRC,HT,BIO3,SG,VR,DSH,CV3,CV15

  if (SPN.eq.106) then
     if (STEMS.le.1) then
        DSH = -0.03 + (0.9826*DRC) - 0.20
        VR = 1 - (0.27612*(1.5**0.67360))/(DSH**0.21114)
     else
        DSH = -0.30 + (0.9826*DRC)
        VR = 1 -(0.66949*(1.5**0.62895))/(DSH**0.44205)
     end if
  else
     if (SPN.eq.57.or.SPN.eq.63.or.SPN.eq.65.or.SPN.eq.69) then
        if (STEMS.le.1) then
           DSH = -0.77 + (0.9603*DRC) + 0.22
           VR = 1 - (0.44761*(1.5**0.65698))/(DSH**0.38835)
        else
           DSH = -0.77 + (0.9603*DRC)
           VR = 1 -(0.82565*(1.5**0.59404))/(DSH**0.45831)
        end if
     end if
  end if
  if (SPN.eq.57.or.SPN.eq.63.or.SPN.eq.65.or.&
       &SPN.eq.69.or.SPN.eq.106) then
     CV3 = CV15*VR
  else if (SPN.eq.475) then
     CV3 = .00356*(DRC**2.920)
  else
     CV3 = Cv15
  end if
  !     Compute small tree volume if none provided
  if (DRC.ge.1.and.DRC.lt.5.and.SPN.ne.475) then
     CV3 = .25*.005454*DRC**2*HT
  end if
  BIO3 = SG*CV3*62.4

end

  !This subroutine calculate biomass for woodland species
subroutine WOODLAND_BIO(SPCD, DRC,THT,STEMS,CV15,DRYBIO,ERRFLG)
  implicit none
  ! Variables
  integer :: SPCD,SPN,ERRFLG,DONE,LAST,STEMS,I
  integer :: SPLIST(13)
  real    :: DRC,THT,SPSG,CV15,BIO3,BIO3_M,WT_FOL,WT_BRA,WT_FOL_M,WT_BRA_M
  real    :: DRYBIO(15),SG(13)
  data (SPLIST(I),I = 1,13)/57,63,65,69,106,&
       &310,475,755,756,757,758,810,814/
  data (SG(I),I = 1,13)/0.533,0.517,0.523,0.558,0.496,&
       &0.65,0.7,0.69,0.69,0.69,0.69,0.567,0.634/

  SPN = SPCD
  DONE = 0
  LAST = 13
  !CV15 = VOL(1)
  !First check if the SPCD is in the woodland species list
  !This is to match FIA BIOMASS_SP configuration
  !IF(SPN.EQ.62.OR.SPN.EQ.66)THEN
  !Added check for species 63 and 65 (2025/05/08)
  if ((SPN.ge.57.and.SPN.le.66).and.(SPN.ne.63.and.SPN.ne.65)) then
     SPN = 57
  else if (SPN.ge.133.and.SPN.le.143) then
     SPN = 106
  else if (SPN.eq.303.or.SPN.eq.304.or.SPN.eq.363) then
     SPN = 757
  else if (SPN.eq.321.or.SPN.eq.322) then
     SPN = 310
  else if (SPN.eq.522.or.SPN.eq.7532) then
     SPN = 475
  else if (SPN.ge.523.and.SPN.le.758) then
     SPN = 757
  else if (SPN.eq.803) then
     SPN = 810
  else if (SPN.ge.829.and.SPN.le.867) then
     SPN = 810
  end if
  call SEARCH(LAST,SPLIST,SPN,DONE,ERRFLG)
  if (DONE.eq.0) then
     ERRFLG = 6
     return
     !    IF(SPN.LT.70)THEN
     !        SPN = 57
     !    ELSEIF(SPN.LT.200)THEN
     !        SPN = 106
     !    ELSEIF(SPN.LT.330)THEN
     !        SPN = 310
     !    ELSE
     !        SPN = 814
     !    ENDIF
     !    ERRFLG = 0
     !    CALL SEARCH(LAST,SPLIST,SPN,DONE,ERRFLG)
  end if
  SPSG = SG(DONE)
  ! For woodland species, the VOL(1) from woodland volume equation is the outside bark volume from ground to 1.5" top
  ! For the DRYBIO, the wood and bark are not seperated and saved in the wood element
  DRYBIO(2) = CV15*SPSG*62.4
  ! Stump wood and bark
  !DRYBIO(4) = 3.14*(DRC/2/12)**2*SPSG*62.4
  ! Get the BIO to 3" top
  call CHO_WDBK_1530(SPN,DRC,THT,CV15,STEMS,SPSG,BIO3)
  !Changed SPN to SPCD (2025/05/08)
  !CALL CHO_WDBK_1530(SPCD,DRC,THT,CV15,STEMS,SPSG,BIO3)
  DRYBIO(6) = BIO3
  ! Get the tip from 3" to 1.5"
  !DRYBIO(10) = DRYBIO(2) - BIO3
  ! Branches and foliage
  BIO3_M = BIO3/2.2046
  if (SPN.ge.300) then
     WT_FOL = 10**(-.5655+(.8382*(LOG10(BIO3)))+(-.0094*THT))
     WT_BRA = 10**(.3036+(.7752*(LOG10(BIO3)))+(-.0049*THT))
  else
     if (SPN.eq.106) then
        if (BIO3_M.le.468.028) then
           WT_FOL_M = EXP(1.0254+.559*LOG(BIO3_M))
        else
           WT_FOL_M = EXP(1.0254+.559*(1.0+LOG(468.028)&
                &-468.028/BIO3_M))
        end if
     else
        if (BIO3_M.le.150.0) then
           WT_FOL_M = EXP(1.2867+.649*LOG(BIO3_M))
        else
           WT_FOL_M = EXP(1.2867+.649*(1.0+LOG(150.0)-150.0/BIO3_M))
        end if
     end if
     WT_FOL = WT_FOL_M*2.2046
     WT_BRA = WT_FOL*0.75
     WT_FOL = WT_FOL*0.25
  end if
  DRYBIO(12) = WT_BRA
  DRYBIO(13) = WT_FOL
  DRYBIO(1) = DRYBIO(6)+DRYBIO(12)

  return
end
