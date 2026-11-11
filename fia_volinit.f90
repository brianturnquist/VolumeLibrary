!=================================!
!   VolumeLibrary : fia_volinit   !
!=================================!
  ! This subroutine initiate call to its subroutine baseed on VOLEQ
  ! Created by YW 2018/08/08
  ! 2020/11/09 Corrected the vol for CVT and CVTS and made subroutine FIAVOLUME for FIA equations call
  ! 2020/12/04 Reverse the changes made on 2020/11/09
subroutine FIA_VOLINIT(VOLEQ,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,&
     &STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLG,&
     &FIAVTYPE,FIAVOL,BFMIND)
  ! Variables
  character(len=10) :: VOLEQ
  character(len=10) :: FIAVTYPE
  character(len=10) :: VOLEQ2
  character(len=10) :: GEOSUB
  character(len=10) :: FIAEQ
  real    :: DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,VOL(15),BFMIND,STUMP
  real    :: DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,FIAVOL
  real    :: CENTROID_HT,CENTROID_DIA
  integer :: BA,SI,ERRFLG,SPN,IREGN
  character(len=3) :: MDL
  character(len=1) :: REGN
  real    :: CROWN,TOTTREE
  !  VARIABLE FOR VOLINIT
  character(len=1) :: HTTYPE
  character(len=1) :: LIVE
  character(len=1) :: CTYPE
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=4) :: CONSPEC
  !   MERCH VARIABLES
  integer :: HTTFLL
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
  !   Tree variables
  real    :: DBTBH,BTR,CR
  integer :: FCLASS,HTLOG
  !    3RD POINT VARIABLES
  real    :: AVGZ1,AVGZ2
  integer :: HTREF
  !   OUTPUTS
  real    :: NOLOGP,NOLOGS
  integer :: TLOGS,IDIST
  !   ARRAYS
  integer :: I15,I21,I20,I7,I3,I,J
  real    :: LOGVOL(7,20)
  real    :: LOGDIA(21,3),LOGLEN(20),BOLHT(21)

  VOL = 0.0
  ERRFLG = 0
  FIAVOL = 0.0
  FIAEQ = VOLEQ
  if (BFMIND.eq.0.0) BFMIND = 9.0
  if (VOLEQ(1:8).eq.'CU999999') then
     FIAVOL = 0.000001
     return
  end if
  if (VOLEQ(1:8).eq.'BD999999') then
     FIAVOL = 0.000005
     return
  end if
  !     CONVERT FIA EQUATION NUMBER TO NVEL EQUATION FORMAT
  if (VOLEQ(1:2).eq.'cu') VOLEQ(1:2) = 'CU'
  if (VOLEQ(1:2).eq.'bd') VOLEQ(1:2) = 'BD'
  if (BFMIND.le.0.0) then
     if (SPN.ge.300) then
        BFMIND = 11.0
     else
        BFMIND = 9.0
     end if
  end if
  if (MTOPP.eq.0.0) then
     MTOPP = 4.0
     if (DBHOB.ge.BFMIND) MTOPP = 6.0
  end if
  if (VOLEQ(1:2).eq.'CU'.or.VOLEQ(1:2).eq.'BD') then
     call FIAEQ2NVELEQ(VOLEQ,SPN,GEOSUB,MTOPP,VOLEQ2,FIAVTYPE,&
          &ERRFLG)
     if (ERRFLG.gt.0) then
        return
     else
        VOLEQ = VOLEQ2
     end if
  end if

  REGN = VOLEQ(1:1)
  if (REGN.eq.'P'.or.REGN.eq.'R'.or.REGN.eq.'N'.or.REGN.eq.'S') then
     call FIAVOLUME(VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,&
          &STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLG,&
          &BFMIND)

  else
     ! REGULAR NVEL EQUATION CALL VOLINIT SUBROUTINE
     I15 = 15
     I21 = 21
     I20 = 20
     I7 = 7
     I3 = 3
     DRCOB = 0.0
     MTOPS = 4.0
     STUMP = 1.0
     HTTYPE = 'F'
     HTREF = 0
     UPSHT1 = 0.0
     UPSD1 = 0.0
     UPSHT2 = 0.0
     UPSD2 = 0.0
     HT1PRD = 0.0
     HT2PRD = 0.0
     BA = 0
     SI = 0
     AVGZ1 = 0.0
     AVGZ2 = 0.0
     FCLASS = 80
     DBTBH = 0.0
     BTR = 0.0
     LOGVOL = 0.0
     LOGDIA = 0.0
     LOGLEN = 0.0
     BOLHT = 0.0
     HTLOG = 0
     TLOGS = 0
     NOLOGP = 0
     NOLOGS = 0
     CUTFLG = 1
     BFPFLG = 1
     CUPFLG = 1
     CDPFLG = 0
     SPFLG = 1
     PROD = '01'
     if (DBHOB.lt.BFMIND) PROD = '02'
     !        IF(SPN.LT.300)THEN
     !          IF(DBHOB.LT.BFMIND) PROD = '02'
     !        ELSE
     !          IF(DBHOB.LT.BFMIND) PROD = '02'
     !        ENDIF
     HTTFLL = 0
     LIVE = 'L'
     CTYPE = 'F'
     CONSPEC = "    "
     FORST = '01'
     IDIST = 1
     !       NEED TO SET MTOPP BASED ON FIAVTYPE
     !       MTOPP = ?

     if (VOLEQ(1:1).eq.'A'.or.VOLEQ(1:1).eq.'a') then
        IREGN = 10
     else if (VOLEQ(1:1).eq.'I'.or.VOLEQ(1:1).eq.'i') then
        IREGN = 1
     else if (VOLEQ(1:1).eq.'H'.or.VOLEQ(1:1).eq.'h') then
        IREGN = 5
     else if (VOLEQ(1:1).eq.'F'.or.VOLEQ(1:1).eq.'F') then
        IREGN = 6
     else if (VOLEQ(1:1).eq.'B'.or.VOLEQ(1:1).eq.'b') then
        IREGN = 7
     else
        read (VOLEQ(1:1), '(I1)') IREGN
     end if
     call VOLINIT(IREGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,&
          &DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,&
          &UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,&
          &VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
          &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,&
          &BA,SI,CTYPE,ERRFLG,IDIST)
  end if
  if (ERRFLG.gt.0) return
  if (VOL(1).eq.0.0) VOL(1) = VOL(4)+VOL(7)+VOL(15)
  if (FIAVTYPE(1:4).eq.'CVTS') then
     FIAVOL = VOL(1) + VOL(14)
  else if (FIAVTYPE(1:3).eq.'CVT') then
     FIAVOL = VOL(1)
  else if (FIAVTYPE(1:3).eq.'CV4') then
     FIAVOL = VOL(4) + VOL(7)
  else if (FIAVTYPE(1:2).eq.'SV') then
     FIAVOL = VOL(2)
  else if (FIAVTYPE(1:2).eq.'IV') then
     FIAVOL = VOL(10)
     if (FIAEQ(1:8).eq.'BD000030') FIAVOL = VOL(2)
  else if (FIAVTYPE(1:2).eq.'CV') then
     FIAVOL = VOL(4)
  else if (FIAVTYPE(1:3).eq.'TIP') then
     FIAVOL = VOL(15) + VOL(7)
  else if (FIAVTYPE(1:5).eq.'CROWN') then
     FIAVOL = CROWN
  else if (FIAVTYPE(1:7).eq.'TOTTREE') then
     FIAVOL = TOTTREE
  end if
  return
  end subroutine FIA_VOLINIT

  ! FIA volume equations brought to NVEL
subroutine FIAVOLUME(VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,&
     &STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLG,&
     &BFMIND)
  ! Variables
  character(len=10) :: VOLEQ
  character(len=10) :: GEOSUB
  real    :: DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,VOL(15),BFMIND,STUMP
  real    :: DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2
  real    :: CENTROID_HT,CENTROID_DIA
  integer :: BA,SI,ERRFLG
  character(len=3) :: MDL
  character(len=1) :: REGN
  real    :: CROWN,TOTTREE

  REGN = VOLEQ(1:1)
  MDL = VOLEQ(4:6)
  if (STUMP.le.0.0) STUMP = 1.0
  !     PACIFIC NORTHWEST
  if (REGN.eq.'P') then
     if (MDL.eq.'BDE') then
        call BROWNE_DEMARS_EMBRY(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,&
             &BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'BEL') then
        call BELL_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'BIG') then
        call BIGING7(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'BRC'.or.MDL.eq.'BRI'.or.MDL.eq.'BRO') then
        call BROWNE_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'CEN') then
        if (UPSHT1.gt.0.1.and.UPSD1.gt.0.1) then
           CENTROID_HT = UPSHT1
           CENTROID_DIA = UPSD1
        else if (UPSHT2.gt.0.1.and.UPSD2.gt.0.1) then
           CENTROID_HT = UPSHT2
           CENTROID_DIA = UPSD2
        else
           ERRFLG = 9
           return
        end if
        call CENTROID_CV(DBHOB,HTTOT,CENTROID_DIA,CENTROID_HT,MTOPP,&
             &STUMP,VOL,ERRFLG)
     else if (MDL.eq.'CHA') then
        call CHAMBERS_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'CHT') then
        call CHT_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'CUR') then
        call CURTIS_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'DEE') then
        call DEMARS_EMBRY(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,&
             &ERRFLG)
     else if (MDL.eq.'DEM') then
        call DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,&
             &ERRFLG)
     else if (MDL.eq.'DIP') then
        call DIPPOLD_PNW147(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'EMB') then
        call EMBRY_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'FRU') then ! PACIFIC ISLANDS
        call FRUSTRUM_VOL(DBHOB,DRCOB,HTTOT,UPSHT1,UPSD1,&
             &UPSHT2,UPSD2,MTOPP,STUMP,VOL,ERRFLG)
     else if (MDL.eq.'GRE') then
        call GREGORY_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'HOR') then
        call HORN67(VOLEQ,DBHOB,HTTOT,VOL,ERRFLG)
     else if (MDL.eq.'ISL') then ! PACIFIC ISLANDS
        call PACIFIC_ISLAND(VOLEQ,DBHOB,DRCOB,HTTOT,UPSHT1,UPSD1,&
             &UPSHT2,UPSD2,MTOPP,STUMP,VOL,ERRFLG)
     else if (MDL.eq.'KIN') then
        call KING_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'KRU') then
        if (VOLEQ(7:7).eq.'0') then
           !         DNR24
           call KRUMLAND_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,&
                &ERRFLG)
        else if (VOLEQ(7:7).eq.'1') then
           call KRUMLAND_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG)
        else if (VOLEQ(7:7).eq.'2') then
           call KRUMLAND_VOL2(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG)
        end if
     else if (MDL.eq.'MAC') then
        if (VOLEQ(7:7).eq.'0') then
           !         DNR24
           call MAC_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
        else
           !         PNW266 only for CV4, International BD and Scribner BD and also tarif conversion CV4 to other volumes
           call PNW266_TARIF(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
        end if
     else if (MDL.eq.'PIL') then
        call PILLSBURY_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,&
             &ERRFLG)
     else if (MDL.eq.'SMF') then
        call SUMMERFIELD_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,&
             &ERRFLG)
     else if (MDL.eq.'WEN') then
        if (VOLEQ(7:7).eq.'0') then
           call WENSEL_RN37(VOLEQ,DBHOB,HTTOT,HT1PRD,MTOPP,BFMIND,VOL,&
                &ERRFLG)
        else if (VOLEQ(7:7).eq.'1') then
           call WENSEL_1907(VOLEQ,DBHOB,HTTOT,HT1PRD,MTOPP,BFMIND,VOL,&
                &ERRFLG)
        end if
     end if
     !     ROCKY MOUNTAIN
  else if (REGN.eq.'R') then
     if (MDL.eq.'ALN') then
        call ALLEN_SMALLTREE(VOLEQ,DBHOB,HTTOT,VOL,ERRFLG)
     else if (MDL.eq.'CHO') then
        if (DRCOB.eq.0.0.and.DBHOB.gt.0.0) DRCOB = DBHOB
        call CHOJNACKY15_30(VOLEQ,DRCOB,HTTOT,MTOPP,VOL,ERRFLG)
     else if (MDL.eq.'CLE') then
        call CLENDENEN_SMALLTREE(VOLEQ,DBHOB,DRCOB,HTTOT,VOL,ERRFLG)
     else if (MDL.eq.'FAU') then
        call FAUROT_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,MTOPP,VOL,ERRFLG)
     else if (MDL.eq.'KEM') then
        call KEMP(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'MCT') then
        call MACTAGUE_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,MTOPP,VOL,ERRFLG)
     else if (MDL.eq.'MOI') then
        call MOISEN_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
     else if (MDL.eq.'MYE') then
        call MYERS_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,MTOPP,VOL,ERRFLG)
     else if (MDL.eq.'RMR') then
        call EAST_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG)
     end if
     !     NORTH
  else if (REGN.eq.'N') then
     if (MDL.eq.'BAR') then
        if (HT1PRD.gt.0.0.and.HT2PRD.eq.0.0) HT2PRD = HT1PRD
        call BARNARD_VOL(VOLEQ,DBHOB,HT2PRD,VOL,ERRFLG)
     else if (MDL.eq.'GRE') then
        call GREEN_VOL(VOLEQ,DBHOB,HTTOT,VOL,ERRFLG)
     else if (MDL.eq.'HAH') then
        call HAHN_VOL(VOLEQ,DBHOB,MTOPP,BFMIND,SI,VOL,ERRFLG)
     else if (MDL.eq.'HIL') then
        call HILT_VOL(DBHOB,HTTOT,VOL,ERRFLG)
     else if (MDL.eq.'SCH') then
        call SCHLAEGEL_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG,&
             &TOTTREE)
     else if (MDL.eq.'SCT') then
        call SCOTT_VOL(VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,BFMIND,&
             &VOL,ERRFLG)
     else if (MDL.eq.'STN') then
        call STONE_VOL(VOLEQ,DBHOB,MTOPP,SI,BA,VOL,ERRFLG)
     end if
     !     SORTH
  else if (REGN.eq.'S') then
     if (MDL.eq.'BER') then
        call BERG_VOL(VOLEQ,DBHOB,HTTOT,VOL,ERRFLG)
     else if (MDL.eq.'BRA') then
        call BRANDEIS_VOL(VOLEQ,DBHOB,HTTOT,VOL,ERRFLG)
     else if (MDL.eq.'BUR') then
        call BURKHART_VOL(VOLEQ,DBHOB,HTTOT,VOL,ERRFLG)
     else if (MDL.eq.'CLK') then
        call CLARK_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG,CROWN)
     else if (MDL.eq.'FAR') then
        call FARRAR_VOL(DBHOB,HTTOT,MTOPP,VOL,ERRFLG)
     else if (MDL.eq.'SCH') then
        call SCHLAEGEL_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG,&
             &TOTTREE)
     else if (MDL.eq.'SRS') then
        call SRS_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG)
     end if
  end if
  if (VOL(1).eq.0.0) VOL(1) = VOL(4)+VOL(7)+VOL(15)
  return
end
