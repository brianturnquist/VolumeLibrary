!===================================!
!   VolumeLibrary : fia_vol_carib   !
!===================================!
  ! This is from FIA package code NIMS_VOL_FIA_CARIB
  ! Created by YW 2018/09/18

  ! Brandeis, T.J. etal 2005
  ! Equations for Merchantable Volume for Subtropical Moist and Wet Forests of Puerto Rico
  ! USDA Forest Service Southern Research Station Research Paper SRS-39
  ! CU000122 CV4 FUNCTION CARIB_BRANDEIS_2005(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
  ! Brandeis, T.J. etal 2006
  ! Development of equations for predicting Puerto Rican subtropical dry forest biomass and volume
  ! Forest Ecology and Management 233 (2006) 133–142
  ! CU000123 CV4 FUNCTION CARIB_BRANDEIS_2006(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
  ! Valid species code:
  ! NVEL EQUATION NUMBER:
  ! S99BRA****
subroutine BRANDEIS_VOL(VOLEQ,DBHOB,HTTOT,VOL,ERRFLG)
  ! Variables
  character(len=10) :: VOLEQ
  integer :: SPN,ERRFLG
  real    :: DBHOB,HTTOT,DBH_CM,HT_MT,CVT_M,CV4_M,VOL(15)
  real    :: A0,A1,B0,B1,B2,B3,C0,C1
  ERRFLG = 0
  VOL = 0.0
  if (DBHOB.lt.5.0) return
  read(VOLEQ(7:10),'(I4)')SPN
  DBH_CM = DBHOB*2.54
  HT_MT = HTTOT*0.3048
  if (SPN.eq.854.or.SPN.eq.6008.or.SPN.eq.6284) then
     !  --   Dry forests
     !  --BUBU
     if (SPN.eq.6284) then
        A0 = 0.0001
        A1 = 0.000047
        C0 = -0.0697
        C1 = 0.000046
     else if (SPN.eq.854) then
        !  --BUSI
        A0 = 0.0370
        A1 = 0.000028
        C0 = 0.02890
        ! THE 0.2890 ON THE PUBLICATION PAPER SEEMS NOT RIGHT
        C1 = 0.000025
     else if (SPN.eq.6008) then
        !  --ACMA
        A0 = -0.0173
        A1 = 0.000047
        C0 = -0.0593
        C1 = 0.000046
     end if
     CVT_M = A0 + A1 * (DBH_CM ** 2) * HT_MT
     CVT = CVT_M*35.314667
     CV4_M = C0 + C1 * (DBH_CM ** 2) * HT_MT
     CV4 = CV4_M*35.314667
     VOL(1) = CVT
     VOL(4) = CV4
  else
     !  -- Wet/moist/lower montane wet/rain forests
     if (SPN.eq.6114) then
        !--ANIN
        B0 = -10.7353102290492
        B1 = 2.33132884613613
        B2 = 0.774680339670667
        B3 = -0.0074807863759593
     else if (SPN.eq.6443) then
        !--CESC9
        B0 = -13.5633594255738
        B1 = 3.5507061416754
        B2 = 0.787970783049223
        B3 = -0.0469524275398158
     else if (SPN.eq.6728) then
        !--COAL
        B0 = -11.9506641352761
        B1 = 3.06860415362186
        B2 = 0.576660209391705
        B3 = -0.029577504262509
     else if (SPN.eq.7011) then
        !--ERPO5
        B0 = -14.2743202553787
        B1 = 3.63718110738798
        B2 = 0.576660209391705
        B3 = -0.0326995133584038
     else if (SPN.eq.7290) then
        !--GUGU
        B0 = -12.5614979281204
        B1 = 3.05691245763579
        B2 = 0.748845391883614
        B3 = -0.0261745352292469
     else if (SPN.eq.7470) then
        !--INLA
        B0 = -12.7021831143844
        B1 = 2.83592579314056
        B2 = 1.01688921838352
        B3 = -0.0206949734897774
     else if (SPN.eq.7474) then
        !--INVE
        B0 = -10.8303561289069
        B1 = 2.50262939251883
        B2 = 0.731086851638329
        B3 = -0.015600143438074
     else if (SPN.eq.885) then
        !--MAIN3
        B0 = -10.1626005016519
        B1 = 1.92057572622235
        B2 = 1.08296645946787
        B3 = -0.00293548732354752
     else if (SPN.eq.7994) then
        !--OCLE
        B0 = -11.1068755264823
        B1 = 2.19819049034765
        B2 = 1.05004167825012
        B3 = -0.00256209197493238
     else if (SPN.eq.8558) then
        !--SCMO10
        B0 = -10.7075669936423
        B1 = 2.1613616649571
        B2 = 0.979394658095212
        B3 = -0.00322527623337223
     else if (SPN.eq.8644) then
        !--SPCA2
        B0 = -11.8315998001912
        B1 = 2.7089366261757
        B2 = 0.869934254242142
        B3 = -0.0166886450211204
     else if (SPN.eq.8701) then
        !--SYJA
        B0 = -10.5433370090313
        B1 = 2.08250274792873
        B2 = 0.893677401952679
        B3 = 0.0000773127731972417
     else if (SPN.eq.8713) then
        !--TAHE
        B0 = -11.5743019513462
        B1 = 2.49204956936753
        B2 = 0.89277350958636
        B3 = -0.00702587297575205
     else if (SPN.eq.6001) then
        !--ACAN4
        B0 = -12.0234809372091
        B1 = 2.75276356832771
        B2 = 0.88961180433259
        B3 = -0.0207220137274765
     else
        ERRFLG = 6
        return
     end if
     CV4_M = EXP(B0+B1*LOG(DBH_CM)+B2*LOG(HT_MT)+B3*DBH_CM)
     CV4 = CV4_M*35.314667
     VOL(4) = CV4
  end if
  return
  end subroutine BRANDEIS_VOL
