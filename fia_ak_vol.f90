!================================!
!   VolumeLibrary : fia_ak_vol   !
!================================!
  ! This is from NIMS_AK_VOL package
  ! Created by YW 2018/08/15
  !--Gregory Robert A. and Paul M. Haack
  !--Tables and Equations for Estimating Cubic Foot Volumes of Interior Alaska Tree species
  !--NOR-6, May 1964
  ! CU000107 FUNCTION GREGORY_CVTS(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
  ! CU000108 FUNCTION GREGORY_CV4(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
  ! CU000113 FUNCTION GREGORY_CV6(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
  ! CU000114 FUNCTION GREGORY_CV8(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
  ! Valid species code: 094, 375, 746, 747
  ! NVEL Equation Number:
  ! P01GRE0094, P01GRE0375, P01GRE0746, P01GRE0747,
subroutine GREGORY_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,&
     &ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  real    :: DBHOB,HTTOT,MTOPP,BFMIND,VOL(15)
  integer :: ERRFLG,SPN,I,J
  real    :: CV4,CV6,CV8,DBH,THT,CVTS,TOP
  VOL = 0.0
  DBH = DBHOB
  THT = HTTOT
  if (DBH.lt.1.0.or.THT.lt.0.0) return
  if (BFMIND.lt.0.1) BFMIND = 9.0
  if (MTOPP.lt.0.1) MTOPP = 6.0
  read(VOLEQ(8:10),'(I3)')SPN
  if (SPN.eq.94) then
     CVTS = -1.1843+0.205*DBH+.01639*THT+.00187*DBH**2*THT
     CV4 = -2.0555+.2982*DBH+.00181*DBH**2*THT
  else if (SPN.eq.746) then
     CVTS = -0.011*DBH**2+.00662*THT+.00235*DBH**2*THT
     CV4 = -.5553-.02216*DBH**2+.00246*DBH**2*THT
  else if (SPN.eq.375) then
     CVTS = -.01408*DBH**2+.00815*THT+.00227*DBH**2*THT;
     CV4 = -2.5767+.9524*DBH-.10446*DBH**2-.03303*THT+.00282*DBH**2*THT
  else if (SPN.eq.747) then
     CVTS = .00806*THT+.00175*DBH**2*THT
     CV4 = -3.2187+.828*DBH-.05908*DBH**2-.01985*THT+.00199*DBH**2*THT
  end if;
  VOL(1) = CVTS
  VOL(14) = 0.0001
  if (CV4.lt.0.0) then
     TOP = 4.0
     call CONE_CV(DBH,THT,TOP,CV4)
  end if
  VOL(4) = CV4
  if (DBH.ge.BFMIND) then
     if (MTOPP.eq.6.0) then
        call GREGORY_CV6(SPN, DBH, THT, CV6)
        VOL(7) = CV4 - CV6
        VOL(4) = CV6
     else if (MTOPP.eq.8.0) then
        if (SPN.eq.94) then
           CV8 = -16.7852+4.6690*DBH-.25937*DBH*DBH-.28088*THT&
                &+.00313*DBH*DBH*THT
        else if (SPN.eq.746) then
           if (DBH.gt.16.5.and. THT.gt.87.5) then
              call GREGORY_CV6(SPN, DBH, THT, CV6)
              CV8 = CV6 - .005454*DBH**2
           else
              CV8 = -10.0346+2.9603*DBH-.20966*DBH**2-.25373*THT&
                   &+.00375*DBH*DBH*THT
           end if;
        else if (SPN.eq.375) then
           if ((DBH.gt.12.5.and. THT.lt.47.5) .or.&
                &(DBH.gt.13.5.and. THT.lt.52.5) .or.&
                &(DBH.gt.14.5.and. THT.lt.62.5) .or.&
                &(DBH.gt.15.5.and. THT.lt.72.5) .or.&
                &(DBH.gt.16.5.and. THT.lt.77.5) .or.&
                &(DBH.gt.17.5.and. THT.lt.87.5) .or.&
                &(DBH.gt.18.5)) then
              call GREGORY_CV6(SPN, DBH, THT, CV6)
              CV8 = CV6 - .005454*DBH**2
           else
              CV8 = -.16588*THT+.00249*DBH**2*THT
           end if;
        else if (SPN.eq.747) then
           if ((DBH.gt.12.5.and. THT.lt.47.5) .or.&
                &(DBH.gt.13.5.and. THT.lt.52.5) .or.&
                &(DBH.gt.14.5.and. THT.lt.62.5) .or.&
                &(DBH.gt.15.5.and. THT.lt.72.5) .or.&
                &(DBH.gt.16.5.and. THT.lt.77.5) .or.&
                &(DBH.gt.17.5.and. THT.lt.87.5) .or.&
                &(DBH.gt.18.5)) then
              call GREGORY_CV6(SPN, DBH, THT, CV6)
              CV8 = CV6 - .005454*DBH**2
           else
              CV8 = -14.9026+.9689*DBH+.00140*DBH*DBH*THT
           end if;
        end if;
        if (CV8.gt.0.0) then
           !         THE PUBLICATION IS FROM 2 FOOT STUMP TO 8 INCH TOP
           ! I added a 1 foot cylinder the size of DBH outside bark to approximate volume to 1 foot stump. KRC
           CV8 = CV8+.005454*DBH**2
        end if
        if (CV8.lt.0.0) then
           TOP = 8.0
           call CONE_CV(DBH,THT,TOP,CV8)
        end if
        VOL(7) = CV4 - CV8
        VOL(4) = CV8
     end if
  end if
  return
  end subroutine GREGORY_VOL

subroutine GREGORY_CV6(SPN, DBH, THT, CV6)
  ! Variables
  integer :: SPN
  real    :: DBH,THT,CV6
  if (SPN.eq.94) then
     if ((THT.gt.112.5.and. DBH.gt.14.5.and. DBH.lt.15.5) .or.&
          &(THT.gt.107.5.and. DBH.ge.15.5.and. DBH.lt.18.5) .or.&
          &(THT.gt.112.5.and. DBH.ge.18.5.and. DBH.lt.20.5) .or.&
          &(THT.gt.117.5.and. DBH.ge.20.5.and. DBH.lt.21.5)) then
        CV6 = -2.0555+.2982*DBH+.00181*DBH**2*THT
     else
        CV6 = -10.8448+2.6242*DBH-.14074*DBH*DBH-.09628*THT&
             &+.00246*DBH*DBH*THT
     end if;
  else if (SPN.eq.746) then
     if ((DBH.gt.14.5.and. THT.lt.52.5) .or.&
          &(DBH.gt.15.5.and. THT.lt.62.5) .or.&
          &(DBH.gt.16.5.and. THT.lt.67.5) .or.&
          &(DBH.gt.17.5.and. THT.lt.77.5) .or.&
          &(DBH.gt.18.5.and. THT.lt.82.5) .or.&
          &(DBH.gt.19.5.and. THT.lt.87.5) .or.&
          &(DBH.gt.20.5.and. THT.lt.92.5) .or.&
          &(DBH.gt.22.5)) then
        CV6 = -.5553-.02216*DBH**2+.00246*DBH**2*THT
     else
        CV6 = -3.5054+.00229*DBH**2*THT
     end if;
  else if (SPN.eq.375) then
     if ((DBH.gt.10.5.and. THT.lt.47.5) .or.&
          &(DBH.gt.11.5.and. THT.lt.57.5) .or.&
          &(DBH.gt.12.5.and. THT.lt.67.5) .or.&
          &(DBH.gt.13.5.and. THT.lt.77.5) .or.&
          &(DBH.gt.14.5.and. THT.lt.87.5) .or.&
          &(DBH.gt.15.5)) then
        CV6 = -2.5767+.9524*DBH-.10446*DBH**2-.03303*THT&
             &+.00282*DBH**2*THT
     else
        CV6 = -.07842*THT+.00242*DBH**2*THT
     end if;
  else if (SPN.eq.747) then
     if ((DBH.gt.11.5.and. THT.lt.37.5) .or.&
          &(DBH.gt.12.5.and. THT.lt.42.5) .or.&
          &(DBH.gt.13.5.and. THT.lt.62.5) .or.&
          &(DBH.gt.14.5.and. THT.lt.67.5) .or.&
          &(DBH.gt.15.5.and. THT.lt.72.5) .or.&
          &(DBH.gt.16.5.and. THT.lt.82.5) .or.&
          &(DBH.gt.18.5.and. THT.lt.87.5) .or.&
          &(DBH.gt.18.5.and. THT.lt.92.5) .or.&
          &(DBH.gt.21.5.and. THT.lt.97.5) .or.&
          &(DBH.gt.26.5.and. THT.lt.102.5) .or.&
          &(DBH.gt.29.5.and. THT.lt.105.5) .or.&
          &(DBH.gt.34.5)) then
        CV6 = -3.2187+.828*DBH-.05908*DBH**2-.01985*THT&
             &+.00199*DBH**2*THT;
     else
        CV6 = -7.7571+.6546*DBH+.00151*DBH**2*THT
     end if;
  end if;
  if (CV6.lt.0.0) then
     TOP = 6.0
     call CONE_CV(DBH,THT,TOP,CV6)
  end if
  return
  end subroutine GREGORY_CV6

  !--Embry, Robert S. and Paul M. Haack
  !--Volume Tables and Equations for Young-Growth Western Hemlock and Sitka Spruce in Southeast Alaska
  !--Northern Forest Experiment Station, U. S. Forest Service Research Note NOR-12,
  !--NOR-12, December 1965.
  ! CU000117 FUNCTION EMBRY_CV4(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
  ! CU000118 FUNCTION EMBRY_CV6(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
  ! BD000092 FUNCTION EMBRY_SV(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
  ! BD000093 FUNCTION EMBRY_IV(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
  ! Valid species code: 098, 263
  ! NVEL Equation Number:
  ! P01EMB0098, P01EMB0263,
subroutine EMBRY_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,&
     &ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  real    :: DBHOB,HTTOT,MTOPP,BFMIND,VOL(15)
  integer :: ERRFLG,SPN,I,J
  real    :: CV4,SV6,IV,DBH,HT,TOP
  VOL = 0.0
  DBH = DBHOB
  HT = HTTOT
  if (DBH.lt.1.0.or.HT.lt.0.0) return
  if (BFMIND.lt.0.1) BFMIND = 9.0
  if (MTOPP.lt.0.1) MTOPP = 6.0
  read(VOLEQ(8:10),'(I3)')SPN
  if (SPN.eq.263) then
     CV4 = -2.85632 + 0.0633 * HT + 0.00202 * DBH**2 * HT
  else if (SPN.eq.98) then
     CV4 = 0.0022 * DBH**2 * HT - 7.32683 / DBH**2
  end if;
  if (CV4.lt.0.0) then
     TOP = 4.0
     call CONE_CV(DBH,HT,TOP,CV4)
  end if
  VOL(4) = CV4
  if (DBH.ge.BFMIND) then
     if (SPN.eq.263) then
        if (DBH.lt.19.5) then
           CV6 = -3.70342 + 0.02856 * HT + 0.00213 * DBH**2 * HT
        else
           CV6 = -2.85632 + 0.0633 * HT + 0.00202 * DBH**2 * HT
        end if;
        SV = -0.53887*DBH**2 + 0.01614*DBH**2*HT
        IV = -0.63535*DBH**2 + 0.32091*HT + 0.01741*DBH**2*HT
     else if (SPN.eq.98) then
        CV6 = 0.00216 * DBH**2 * HT - 154.15834 / DBH**2
        SV = -4.60417*DBH +  0.01427*DBH**2*HT
        IV = -0.43761 * DBH**2 + 0.01733 * DBH**2 * HT
     end if;
     if (CV6.lt.0.0) then
        TOP = 6.0
        call CONE_CV(DBH,HT,TOP,CV6)
     end if
     VOL(7) = CV4 - CV6
     VOL(4) = CV6
     VOL(2) = SV
     VOL(10) = IV
  end if
  return
  end subroutine EMBRY_VOL

  ! Cap to prevent negative volumes  KRC - 4/18/09
subroutine CONE_CV(DBH,THT,TOP,CV)
  ! Variables
  real    :: DBH,THT,TOP,CV
  real    :: HT_TOP,CONE_STEM,CONE_TOP,CYLINDER_TO_DBH,CONE
  HT_TOP = TOP*(THT-4.5)/DBH
  CONE_STEM = (3.14*((DBH/24)*(DBH/24))*(THT-3.5))/3
  CONE_TOP = 0.029088*HT_TOP
  CYLINDER_TO_DBH = (3.14*((DBH/24)*(DBH/24))*(3.5))
  CONE = CYLINDER_TO_DBH+CONE_STEM-CONE_TOP
  if (CONE.gt.CV) then
     CV = CONE
  end if
  return
  end subroutine CONE_CV

  ! The following code is from NIMS_VOL_AK_DEM
  ! FIA implement Demars Taper Equation slightly different than R10

  !--compute gross volume from top of stump(HT_STUMP) to given top diameter inside bark (TOP)
  !--for individual tree using translation of Dave Bruce/Don Demars program as provided for AFSL use (DBVOL3.FOR)
  ! CU000116 FUNCTION DEMARS_VOL_CU
  !--Computes Scribner Volume using Demars Taper equation as it is done by Alaska FIA
  !--Tree divided into 16.6 foot logs above stump and each log computed with regression formula
  !--Last partial log is computed as if whole log with 6 inch top and then porpotioned
  !--by ratio of length/16.3
  ! BD000090  FUNCTION DEMARS_SV
  !--Computes International 1/4 Volume using Demars Taper equation as it is done by Alaska FIA
  !--Tree divided into 16.6 foot logs above stump and each log computed with regression formula
  !--Last partial log is computed as if whole log with 6 inch top and then porpotioned
  !--by ratio of length/16.3
  ! BD000091 FUNCTION DEMARS_IV
  ! Valid species code: 098, 263, 042, 242
  ! NVEL Equation Number:
  ! P01DEM0042, P01DEM0098, P01DEM0242, P01DEM0263,
  ! For Afognak Island trees <= 110 ft
  ! P01DEMA098, P01DEMA263
  ! THE 7TH CHARACTER IN THE EQUATION NUMBER: 0 = AK WIDE, A = Afognak Island trees <= 110 ft
subroutine DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,&
     &ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  integer :: SPN,ERRFLG
  character(len=1) :: GEOSUB
  real    :: DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL(15)
  real    :: DBH,HT,BRSQ,MHT,CVT,TIP,VOLCYLINDER,TOP,VR,CV4
  real    :: MLOGS,RELHT,RELD,DIB,HTUP,CL,LOGSV,LOGIV,SV6,IV6,CV6

  read(VOLEQ(8:10),'(I3)')SPN
  if (SPN.ne.42.and.SPN.ne.98.and.SPN.ne.242.and.SPN.ne.263) then
     ERRFLG = 6
     return
  end if
  DBH = DBHOB
  HT = HTTOT
  TOP = 4.0
  GEOSUB = VOLEQ(7:7)
  if (STUMP.lt.0.1) STUMP = 1.0
  if (MTOPP.lt.0.1) MTOPP = 6.0
  VOL = 0.0
  !--  ratio of diameter inside bark to diameter outside bark
  call BRSQ_DEMARS(SPN, DBH, HT, GEOSUB, BRSQ)
  !--  volume of cylinder with diameter equal to diameter at breast height inside bark and length equal to bole
  !--  length above breast height
  VOLCYLINDER = 0.005454154 * DBH ** 2 * (HT - 4.5) * BRSQ
  !--  height to merchantable top (top diameter point), in feet
  call MERCH_HT(SPN,DBH,HT,TOP,BRSQ, GEOSUB,MHT)
  !--  volume stump to tip
  !--  FF function returns total volume inside bark for a given taper function relative to volume
  !--  of cylinder (FV) with diameter = D and length =  total length above breast height
  RELHT = (HT - STUMP) / (HT - 4.5)
  call VOLUME_RATIO(SPN,DBH,HT,RELHT,GEOSUB,VR)
  CVT = VR * VOLCYLINDER
  !--  volume top diameter point to tip
  RELHT = (HT - MHT) / (HT - 4.5)
  call VOLUME_RATIO(SPN,DBH,HT,RELHT,GEOSUB,VR)
  TIP = VR * VOLCYLINDER
  CV4 = CVT - TIP !-- cubic volume, stump to 4 inch top dib
  VOL(1) = CVT
  VOL(4) = CV4
  VOL(15) = TIP
  if (DBH.ge.BFMIND) then
     TOP = MTOPP
     MHT = 0.0
     SV6 = 0.0
     IV6 = 0.0
     call MERCH_HT(SPN, DBH, HT, TOP, BRSQ, GEOSUB, MHT)
     !--  volume top diameter point to tip
     RELHT = (HT - MHT) / (HT - 4.5)
     call VOLUME_RATIO(SPN,DBH,HT,RELHT,GEOSUB,VR)
     TIP = VR * VOLCYLINDER
     CV6 = CVT - TIP
     VOL(7) = CV4 - CV6
     VOL(4) = CV6
     !--  Number of whole logs in merch portion
     MLOGS = INT((MHT-STUMP)/16.3)
     !--  Compute and accumulate whole log volumes
     HTUP = STUMP
     do I = 1,MLOGS
        HTUP = HTUP+16.3
        RELHT = (HT - HTUP) / (HT - 4.5)
        call RELATIVE_DIA(SPN,DBH,HT,RELHT,GEOSUB,RELD)
        DIB = (RELD*BRSQ)**0.5*DBH
        LOGSV = 0.79*DIB**2-2.0*DIB-4.0
        SV6 = SV6+LOGSV
        LOGIV = 0.796*DIB**2-1.374*DIB-1.23
        IV6 = IV6 + LOGIV
        10 continue
     end do
     !--  Compute and accumulate last chunk volumes
     CL = MHT-16.3*MLOGS-STUMP
     RELHT = (HT - MHT) / (HT - 4.5)
     call RELATIVE_DIA(SPN,DBH,HT,RELHT,GEOSUB,RELD)
     DIB = (RELD*BRSQ)**0.5*DBH
     LOGSV = (0.79*DIB**2-2.0*DIB-4.0)*CL/16.3
     SV6 = SV6+LOGSV
     VOL(2) = SV6
     if (CL.gt.0.0.and. CL.lt.4.075) then
        LOGIV = CL / 4.075 * (0.199 * DIB**2 - 0.642 * DIB)
     else if (CL.lt.8.15) then
        LOGIV = CL / 8.15 * (0.398 * DIB**2 - 1.086 * DIB - 0.271)
     else if (CL.lt.12.225) then
        LOGIV = CL / 12.225 * (0.597 * DIB**2 - 1.33 * DIB - 0.715)
     else if (CL.lt.16.3) then
        LOGIV = CL / 16.3 * (0.796 * DIB**2 - 1.375 * DIB - 1.23)
     end if
     IV6 = IV6+LOGIV
     VOL(10) = IV6
  end if
  return
  end subroutine DEMARS_VOL

  !--Switches between Demars and Embry/Haack equations depending on dbh and ht
  !--Only the Sitka Spruce Embry equation is used by Alaska FIA regardless of species
  !--CU000119 FUNCTION DEMARS_EMBRY_CV4
  !--CU000120 FUNCTION DEMARS_EMBRY_CV6
  ! Valid species code: 098, 263, 042, 242
  ! NVEL Equation Number:
  ! P01DEE0098, P01DEE0263, P01DEE0042, P01DEE0242,
  ! P01DEEA098, P01DEEA263, P01DEEA042, P01DEEA242,
  ! THE 7TH CHARACTER IN THE EQUATION NUMBER: 0 = AK WIDE, A = Afognak Island trees <= 110 ft
subroutine DEMARS_EMBRY(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,&
     &ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  character(len=10) :: VOLEQTEM
  integer :: SPN,ERRFLG
  real    :: DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL(15)
  VOL = 0.0
  if (DBHOB.lt.9.0.or.HTTOT.lt.40.0) then
     VOLEQTEM = VOLEQ(1:7)//'098'
     call EMBRY_VOL(VOLEQTEM,DBHOB,HTTOT,MTOPP,BFMIND,VOL,&
          &ERRFLG)
  else
     call DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,&
          &ERRFLG)
  end if
  return
  end subroutine DEMARS_EMBRY

  !--Switches between Browne, Demars and Embry/Haack equations depending on dbh and ht
  !--Only the Sitka Spruce Embry equation is used by Alaska FIA regardless of species
  !--Browne, J. E. 1962, Standard Cubic Foot Volume for Commercial Tree Species of British Columbia
  !--Embry, Robert S. and Paul M. Haack, NOR-12, December 1965.
  !--DeMars
  ! CU042001 FUNCTION CHNO_BROWNE_DEMARS_EMBRY_CV4
  ! CU042002 FUNCTION CHNO_BROWNE_DEMARS_EMBRY_CV6
  ! BD042002 FUNCTION CHNO_BROWNE_DEMARS_EMBRY_IV
  ! CU242001 FUNCTION THPL_BROWNE_DEMARS_EMBRY_CV4
  ! CU242002 FUNCTION THPL_BROWNE_DEMARS_EMBRY_CV6
  !          FUNCTION THPL_BROWNE_DEMARS_EMBRY_IV
  ! Valid species code: 042, 242
  ! NVEL Equation Number:
  ! P01BDE0042, P01BDE0242, P01BDEA042, P01BDEA242
  ! THE 7TH CHARACTER IN THE EQUATION NUMBER: 0 = AK WIDE, A = Afognak Island trees <= 110 ft
subroutine BROWNE_DEMARS_EMBRY(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,&
     &BFMIND,VOL,ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  character(len=10) :: VOLEQTMP
  character(len=10) :: VOLEQTMPI
  integer :: SPN,ERRFLG
  real    :: DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL(15),VOLTMP(15)
  read (VOLEQ(8:10),'(I3)')SPN
  if (SPN.ne.42.and.SPN.ne.242) then
     ERRFLG = 6
     return
  end if
  VOL = 0.0
  VOLEQTMP = VOLEQ
  if (SPN.eq.42) then
     VOLEQTMP(4:7) = 'BRO0'
  else if (SPN.eq.242) then
     VOLEQTMP(4:7) = 'BRC2'
  end if
  VOLEQTMPI = VOLEQTMP
  if (DBHOB.lt.5.0) then
     call DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,&
          &ERRFLG)
  else if (DBHOB.lt.9.0) then
     call BROWNE_DNR24(VOLEQTMP,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
     !       Recalc international boardfoot volume using Browne no location and no age split
     !       242 is not a valid species for no location and no age split, so has o use 042
     VOLEQTMPI(4:10) = 'BRO0042'
     call BROWNE_DNR24(VOLEQTMPI,DBHOB,HTTOT,MTOPP,BFMIND,VOLTMP,&
          &ERRFLG)
     VOL(10) = VOLTMP(10)
  else if ((DBHOB.lt.38.0.and.SPN.eq.42).or.&
       &(DBHOB.lt.56.0.and.SPN.eq.242)) then
     if (HTTOT.lt.40.0) then
        call BROWNE_DNR24(VOLEQTMP,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
     else
        call DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,&
             &ERRFLG)
     end if
     !       Recalc international boardfoot volume
     if (HTTOT.lt.25.0) then
        VOLEQTMPI(4:10) = 'BRO0042'
        call BROWNE_DNR24(VOLEQTMPI,DBHOB,HTTOT,MTOPP,BFMIND,VOLTMP,&
             &ERRFLG)
     else
        call DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,&
             &VOLTMP,ERRFLG)
     end if
     VOL(10) = VOLTMP(10)
  else
     VOLEQ = VOLEQ(1:7)//'098'
     if (HTTOT.lt.40.0) then
        call EMBRY_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,&
             &ERRFLG)
     else
        call DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,&
             &ERRFLG)
     end if
     !       Recalc international boardfoot volume
     if (HTTOT.lt.25.0) then
        call EMBRY_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOLTMP,&
             &ERRFLG)
     else
        call DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOLTMP,&
             &ERRFLG)
     end if
     VOL(10) = VOLTMP(10)
  end if
  return
  end subroutine BROWNE_DEMARS_EMBRY

  !--Taper equation, yields ratio of squared top diameter inside bark to squared base
  !--diameter outside bark given base diameter, total tree height, and ratio of total
  !--tree length above breast height to length of section (relative ht) from breast height
  !--to top diameter inside bark point
  !--RELHT = Relative Height or (HT-HTUP/(HT-4.5)
  ! FUNCTION RELATIVE_DIA
subroutine RELATIVE_DIA(SPN,DBH,HT,RELHT,GEOSUB,RELD)
  ! Variables
  integer :: SPN
  real    :: DBH,HT,RELHT,RELD
  character(len=1) :: GEOSUB
  if (SPN .eq.98.or. SPN .eq.263) then
     if (GEOSUB .eq. 'A' .and. HT.le.110) then !Afognak Island trees <= 110 ft.
        RELD = RELHT ** 1.5 + (-0.0269451 * HT + 0.00023794 * HT ** 2&
             &+ 0.148759 * HT / DBH) * (RELHT ** 1.5 - RELHT ** 3)&
             &+ (0.0974044-0.0000145706 * HT ** 2- 0.0156971*HT/DBH)&
             &* (RELHT ** 1.5 - RELHT ** 32)
     else
        RELD = RELHT**1.5 + (-0.0052554*HT + 0.000034947*HT** 2&
             &+ 0.104477*HT/DBH) * (RELHT ** 1.5 - RELHT ** 3)&
             &+ (7.76807 / DBH ** 2 - 0.0000094852 * HT ** 2 -&
             &0.011351 * HT / DBH) * (RELHT ** 1.5 - RELHT ** 32)
     end if;
  else if (SPN .eq.42) then
     RELD = RELHT ** 1.5 + ((RELHT ** 1.5 - RELHT ** 3)&
          &* (-0.02834001 * DBH + 0.00007123 * HT ** 2&
          &+ 0.06709114 * HT / DBH)) + ((RELHT ** 1.5 - RELHT ** 32)&
          &* (0.00282021*DBH - 0.00002277*HT**2 + 1.06064717/DBH**2&
          &- 0.00528349 * HT / DBH))
  else if (SPN .eq.242) then
     RELD = RELHT ** 1.5 + ((RELHT ** 1.5 - RELHT ** 3)&
          &* (5.17703194 / DBH ** 2 - 0.12516819 *DBH + 0.02537037 * HT&
          &- 0.00004193 * HT ** 2 + 0.00155481 * DBH ** 2)) +&
          &((RELHT ** 1.5 - RELHT ** 32) * (-0.0000207 * HT ** 2&
          &+ 0.24125235 / DBH ** 2))
  end if;
  return
  end subroutine RELATIVE_DIA

  !--Compute integral of taper equation for tree given "relative height", dbh outside bark in
  !--inches, and total tree height from ground to tip in feet
  ! FUNCTION VOLUME_RATIO
subroutine VOLUME_RATIO(SPN,DBH,HT,RELHT,GEOSUB,VR)
  ! Variables
  integer :: SPN
  real    :: DBH,HT,RELHT,VR
  character(len=1) :: GEOSUB
  if (SPN.eq.98.or. SPN.eq.263) then
     if (GEOSUB.eq.'A' .and. HT.le.110) then !--Afognak Island trees <= 110 ft.
        VR = 0.4*RELHT**2.5+(-0.0269451*HT+0.00023794*HT**2&
             &+ 0.148759*HT/DBH)*(0.4*RELHT**2.5-0.25*RELHT**4)&
             &+ (0.0974044-0.0000145706*HT**2-0.0156971*HT/DBH)&
             &* (0.4 * RELHT ** 2.5 - (1.0 / 33.0) * RELHT ** 33)
     else
        VR = 0.4*RELHT**2.5+(-0.0052554*HT+0.000034947*HT**2&
             &+ 0.104477*HT/DBH)*(0.4*RELHT**2.5 - 0.25*RELHT**4)&
             &+ (7.76807/DBH**2 - 0.0000094852*HT**2 - 0.011351*HT/DBH)&
             &* (0.4 * RELHT ** 2.5 - (1.0 / 33.0) *RELHT ** 33)
     end if;
  else if (SPN.eq.42) then
     VR = 0.4 * RELHT**2.5 + ((0.4*RELHT**2.5 - 0.25*RELHT**4)&
          &* (-0.02834001 *DBH + 0.00007123*HT**2 + 0.06709114*HT/DBH))&
          &+ ((0.4 * RELHT ** 2.5 - 1.0 / 33.0 * RELHT ** 33) *&
          &(0.00282021 * DBH - 0.00002277 * HT ** 2 + 1.06064717 / DBH**2&
          &- 0.00528349 * HT / DBH))

  else if (SPN.eq.242) then
     VR = 0.4 * RELHT**2.5 + ((0.4 * RELHT ** 2.5 - 0.25 * RELHT**4)&
          &* (5.17703194 / DBH **2 - 0.12516819 * DBH + 0.02537037*HT&
          &- 0.00004193 * HT ** 2 + 0.00155481 *DBH ** 2))&
          &+ ((0.4 * RELHT ** 2.5 - 1.0 / 33.0 * RELHT ** 33)&
          &* (-0.0000207 * HT ** 2 + 0.24125235 / DBH ** 2))
  end if;
  return
  end subroutine VOLUME_RATIO

  !--Ratio of inside bark diameter squared to outside bark diameter squared
  !  FUNCTION BRSQ_DEMARS_AK
subroutine BRSQ_DEMARS(SPN,DBH,HT,GEOSUB,BRSQ)
  ! Variables
  integer :: SPN
  real    :: DBH,HT,BRSQ
  character(len=1) :: GEOSUB
  if (SPN.eq.98.or. SPN.eq.263) then
     if (GEOSUB.eq.'A' .and. HT.le.110) then
        !--      Spruce-hemlock bark thickness equation, Afognak Island trees <= 110 ft
        BRSQ = 0.773 + 0.00134 * DBH + 0.000958 * HT
     else
        !--      Spruce-hemlock bark thickness equation
        BRSQ = 0.8467 + 0.0009144 * DBH + 0.0003568 * HT
     end if;
  else if (SPN.eq.42) then
     !--    AK yellow cedar
     BRSQ = 0.95866817 + 0.00064402 * DBH - 3.1299972 / HT
  else if (SPN.eq.242) then
     !--    Western red cedar
     BRSQ = 0.86031485 + 0.00059638 * HT - 0.18335961 / DBH
  end if;
  return
  end subroutine BRSQ_DEMARS

  !--Computes height to a specified top diameter
  ! FUNCTION MERCH_HT_SPRUCE_HEMLOCK
  ! FUNCTION MERCH_HT_CHNO
  ! FUNCTION MERCH_HT_CEDAR
subroutine MERCH_HT(SPN,DBH,HT,TOP,BRSQ,GEOSUB,MHT)
  ! Variables
  integer :: SPN,K,KNOL
  real    :: DBH,HT,TOP,BRSQ,MHT
  character(len=1) :: GEOSUB
  character(len=1) :: SMALLD
  character(len=1) :: ATLIMD
  real    :: DST,XLL,RELHT,RXL,MHEST,DS,DXL,TAPER,DIB
  real    :: RELD_TOLERANCE = 0.0001
  real    :: DSX,TOHIGH,TOLOW,DIA,HTUP
  real    :: HITOP,DSI,C,STUMP,D_TOLERANCE = 0.0001
  if (SPN.eq.98.or.SPN.eq.263) then
     !--  initial estimate of height to merchantable top (TOP),
     !--  from Bruce, 1984 (Proceedings), page 100
     MHEST = HT * (1.0 - (2.0/3.0) * (TOP/DBH))
     !--  relative height based on estimated merch. height and total tree height,
     !--  from Bruce, 1984, pg. 98
     RELHT = (HT - MHEST) / (HT - 4.5)
     !--  relative diameter based on initial estimate of height to top diameter inside bark,
     !--  measured diameter outside bark, and total tree height
     call RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB,DS)
     !--  ratio of target top dib squared to dbh inside bark squared
     !--  target realtive diameter
     DST = (TOP**2) / (BRSQ * DBH**2)
     !--  90% of initial estimate of relative height - just to use in estimating taper near top
     RXL = 0.9 * RELHT
     !--  relative diameter (dib squared / DBH ib squared) for RXL
     call RELATIVE_DIA(SPN, DBH, HT, RXL, GEOSUB,DXL)
     !--  change in relative diameter per change in relative height, to be used in adjusting RELHT for input to
     !--  taper function in process of converging on target relative diameter
     TAPER = (DS - DXL) / (0.1 * RELHT)
     !--  iterate up to 10 times to converge on target top diameter inside bark
     K = 1
     do while ((K.le.10).and.((DS.le.(DST - RELD_TOLERANCE)) .or.&
          &(DS.ge.(DST + RELD_TOLERANCE))))
        RXL = RELHT + (DST - DS) / TAPER
        RELHT = RXL
        call RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB,DS)
        K = K + 1
     end do
     MHT = HT - (RELHT * (HT - 4.5))
  else if (SPN.eq.42.or.SPN.eq.242) then
     TOLOW = 4.5
     TOHIGH = HT
     HTUP = HT-TOP*(HT-4.5)/DBH
     RELHT = (HT - HTUP) / (HT - 4.5) !-- relative height for top of full log
     call RELATIVE_DIA(SPN,DBH,HT,RELHT, GEOSUB,DSX) !-- relative diameter at top of full log
     if (DSX.lt.0) then
        DIA = 0
     else
        DIA = (DSX * BRSQ) ** 0.5 * DBH !-- diameter inside bark at top of full log
     end if
     K = 0
     do while(K.lt.100.and.(DIA.lt.(TOP-.001).or.DIA.gt.(TOP+.001)))
        if (DIA.gt.TOP) then
           TOLOW = HTUP
        else
           TOHIGH = HTUP
        end if
        HTUP = (TOLOW+TOHIGH)/2.0
        RELHT = (HT - HTUP) / (HT - 4.5) !-- relative height for top of full log
        call RELATIVE_DIA(SPN,DBH,HT,RELHT,GEOSUB,DSX) !-- relative diameter at top of full log
        if (DSX.lt.0.0) then
           DIA = 0
        else
           DIA = (DSX * BRSQ) ** 0.5 * DBH !-- diameter inside bark at top of full log
        end if
        K = K + 1
     end do
     MHT = HTUP
     ! THE FOLLOWING IS NOT USED IN FIA CODE
  else if (SPN.eq.242) then
     KNOL = 0 !-- number of whole logs below top diameter inside bark limit
     HITOP = STUMP + 16.3 !-- height at top of first full log
     SMALLD = 'N' !-- flags section top d smaller than top d limit
     K = 0
     do while ((SMALLD.eq.'N'.and.HITOP.le.HT.and.K.lt.100))
        K = K+1
        KNOL = KNOL + 1
        RELHT = (HT - HITOP) / (HT - 4.5) !-- relative height for top of full log
        call RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB,DSX) !-- relative diameter at top of full log
        if (DSX.lt.0.0) then
           DSI = 0
        else
           DSI = (DSX * BRSQ) ** 0.5 * DBH !-- diameter inside bark at top of full log
        end if
        if (DSI.ge.TOP) then
           SMALLD = 'Y'
        else
           HITOP = HITOP + 16.3 !-- diameter at top of full log >= merch top diameter; add another full log length
        end if
     end do
     if (DSI.lt.TOP) then
        KNOL = KNOL - 1 !-- top diameter is smaller than merch d, subtract a full log length
     end if
     HITOP = HITOP - 16.3 !-- height to top of last full log
     C = 1.0 !-- height adjustment factor initially set to 1 foot
     !--  Iterate, adjusting height, to converge on specified top diameter inside bark.
     !--  I capped the number of iterations at 100 because original code seemed to loop endlessly.
     !--  Tried fewer iterations but could not get reasonable match with periodic data with fewer
     !--  iterations. There has got to be a more efficient way to code this. KRC
     ATLIMD = 'N'
     K = 0
     do while (ATLIMD.eq.'N'.and.K.lt.100)
        K = K+1
        HITOP = HITOP + C !-- new top height: add a foot to height to top of last full log
        RELHT = (HT - HITOP) / (HT - 4.5) !-- relative height to new top height
        call RELATIVE_DIA(SPN,DBH,HT,RELHT,GEOSUB,DSX) !-- relative diameter at top of last full log
        DSI = (DSX * BRSQ) ** 0.5 * DBH !-- diameter inside bark at top of full log
        if ((((DSI.gt.(TOP - D_TOLERANCE)) .and.&
             &(DSI.lt.(TOP + D_TOLERANCE))) .or. K.gt.100)) then
           !--      top diameter at limit +/- .001 inch tolerance
           MHT = HITOP !-- height to top d limit (LIMD)
           ATLIMD = 'Y'
        else if (DSI.lt.TOP) then
           !--      top diameter smaller than limit, reduce top height and height adjustment factor, estimate next top diameter
           HITOP = HITOP - C
           !            C = C * 0.1
        end if;
     end do
  end if
  return
  end subroutine MERCH_HT
