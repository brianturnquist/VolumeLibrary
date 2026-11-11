!============================!
!   VolumeLibrary : r10d2h   !
!============================!
  !== last modified  2-21-2012
subroutine r10d2h(VOLEQ,DBHOB,HTTOT,VOL,CUTFLG,CUPFLG,BFPFLG,&
     &MTOPP,ERRFLAG)

  !    subroutine implements volume equations developed by Larsen and Winterberger.
  !            PNW-RN-478.
  !    aug, 2000.  KLC
  !    2/21/2012 Y. Wang
  !    Modified A00DVEW094 and added the following equation based on document from R10
  !    A01DVEW094, A00DVEW375, A01DVEW375, A00DVEW747, A01DVEW747,
  !    A00DVEW108, A00DVEW310, and A00DVEW351
  !    A00 is used by costal Alaska and Interior Alaska south and east of the Alaska Range
  !    A01 is used by Interior Alaska north and west of the Alaska Range

  !    A00DVEW094, A00DVEW375, and A00DVEW747 are from PNW-RN-478 and PNW-RN-495 by Larson et al 1988 and 1990
  !    A01DVEW094, A01DVEW375, and A01DVEW747 are from NOR-5 by Haack 1963 and NOR-6 by Gregory et al 1964
  !    A00DVEW108, A00DVEW310, and A00DVEW351 are from Brackett 1973
  !    2019/04/04 Added A02DVEW094 from Malone et al 2013. The equation is for AK statewide
  !    2020/12/21 Added volume correction factor for small tree (DBH<6) total cubic volume of Larson equation.
  ! Variables
  character(len=10) :: VOLEQ
  real    :: DBHOB,HTTOT,VOL(15),MTOPP
  integer :: CUTFLG,CUPFLG,BFPFLG,ERRFLAG,spn
  character(len=2) :: EQN

  real    :: D2H,VOLM
  real    :: A,B,C,pi,BA,CVTS,TATS,TARIF,TV4,CV4,R64,CV6,B4,BCU1,SV6
  real    :: BCU2,IV6,R84,CV8,RS86,SV8,RI86,IV8,CVSL,SCFT,XINTT

  ERRFLAG = 0
  if (DBHOB.le.1.0) then
     ERRFLAG = 3
     return
  end if
  if (HTTOT.le.0) then
     ERRFLAG = 4
     return
  end if

  !     Initialize local variables DW 11/22
  A = 0.0
  B = 0.0
  C = 0.0
  BA = 0.0
  CVTS = 0.0
  TATS = 0.0
  TARIF = 0.0
  TV = 0.0
  CV4 = 0.0
  R64 = 0.0
  CV6 = 0.0
  B6 = 0.0
  BCU1 = 0.0
  SV6 = 0.0
  BCU2 = 0.0
  IV6 = 0.0
  R84 = 0.0
  CV8 = 0.0
  RS86 = 0.0
  SV8 = 0.0
  RI86 = 0.0
  IV8 = 0.0
  CVSL = 0.0
  SCFT = 0.0
  XINIT = 0.0

  if ((voleq(3:3) .eq. '0') .or. (voleq(3:3) .eq. 'O') .or.&
       &(voleq(3:3) .eq. 'o')) then
     EQN = '00'
  else if ((voleq(3:3) .eq. '1') .or. (voleq(3:3) .eq. 'l')&
       &.or. (voleq(3:3) .eq. 'I') .or. (voleq(3:3) .eq. 'L')&
       &.or. (voleq(3:3) .eq. 'i')) then
     EQN = '01'
  else if (VOLEQ(2:3).eq.'02') then
     EQN = '02'
  else
     ERRFLAG = 1
     return
  end if
  D2H = DBHOB*DBHOB * HTTOT

  if (voleq(8:10).eq.'094' .or. voleq(8:10).eq.'095') then
     !  A00DVEW094
     if (EQN .eq. '00') then
        if (CUTFLG .eq.1) then
           VOL(1) = 0.65559+0.00191*D2H
           if (DBHOB.lt.6.0.and.DBHOB.gt.0.0) then
              VOL(1) = VOL(1)-(0.65559*(1.0-(DBHOB/6.0)**3))
           end if
        end if

        if (CUPFLG .eq.1.and. DBHOB .gt.4.0) then
           VOL(4) = -0.21849 + 0.00189*D2H
        end if

        if (BFPFLG.eq.1.and. DBHOB .gt.6.0) then
           VOL(2) = 0.000136 * (D2H**1.40338)
           ! XINTT from equation 6 (PNW-RN-495)
           VOL(10) = 0.00078*(D2H**1.26485)
        end if
     else if (EQN.eq.'01') then
        ! A01DVEW094
        ! NOR-6
        if (CUTFLG .eq.1) then
           VOL(1) = -1.1843+0.205*DBHOB+0.01639*HTTOT+0.00187*D2H
        end if

        ! Equations for Subdivision 2 and Unit 4 (NOR-5)
        if (CUPFLG .eq.1.and. DBHOB .gt.4.0) then
           VOL(4) = -2.0555+(0.2982*DBHOB)+(0.00181*D2H)
           ! THE ABOVE FORMULA IS FROM NOR-6. I CHANGED TO THE ONE FROM NOR-5 (YW 2018/11/07)
           !            VOL(4) = -.69934+.0021294646*D2H
        end if

        if (BFPFLG.eq.1.and. DBHOB .gt.6.0) then
           VOL(2) = 98.7701+(0.02022*D2H) -&
                &0.77651*DBHOB**2 - 1.63023*HTTOT
           VOL(10) = -67.1116+(0.013663011*D2H) + (3344.33/(DBHOB**2))
        end if
     else if (EQN.eq.'02') then
        ! Malone 2013 equation for statewide AK white spruce
        VOL(1) = EXP(-6.1352+1.8517*LOG(DBHOB)+1.069*LOG(HTTOT))
        !         Merch volume to 2, 4, 6-in top
        if (MTOPP.lt.3.0) then
           VOL(4) = -0.987+0.948*VOL(1)+0.0825*DBHOB+0.0138*HTTOT
        else if (MTOPP.lt.5.0) then
           VOL(4) = -1.96+0.941*VOL(1)+0.144*DBHOB+0.0116*HTTOT
        else
           VOL(4) = -5.14+0.92*VOL(1)+0.386*DBHOB-0.00095*HTTOT
        end if
        if (VOL(4).gt.VOL(1)) VOL(4) = VOL(1)
     end if
  else if (voleq(8:10) .eq. '375' .or.&
       &voleq(8:10) .eq. '746' .or.&
       &voleq(8:10) .eq. '920') then
     ! A00DVEW375
     if (EQN .eq. '00') then
        if (CUTFLG .eq.1) then
           VOL(1) = 0.64456+0.00206*D2H
           if (DBHOB.lt.6.0.and.DBHOB.gt.0.0) then
              VOL(1) = VOL(1)-(0.64456*(1.0-(DBHOB/6.0)**3))
           end if
        end if

        if (CUPFLG .eq.1.and. DBHOB .gt.4.0) then
           VOL(4) = -0.7126+(0.00211*D2H)
        end if

        if (BFPFLG.eq.1.and. DBHOB .gt.6.0) then
           VOL(2) = 0.000081*(D2H**1.48459)
           VOL(10) = 0.00043*(D2H**1.34294)
        end if
        ! A01DVEW375
     else
        if (CUTFLG .eq.1) then
           VOL(1) = -0.01408*DBHOB**2+0.00815*HTTOT+0.00227*D2H
        end if

        if (CUPFLG .eq.1.and. DBHOB .gt.4.0) then
           VOL(4) = -1.02411+(0.0022034075*D2H)
        end if

        if (BFPFLG.eq.1.and. DBHOB .gt.6.0) then
           VOL(2) = -27.163+(0.00995*D2H)
           VOL(10) = -29.8848+(0.011913048*D2H)
        end if
     end if
  else if (voleq(8:10) .eq. '747') then
     ! A00DVEW747
     if (EQN .eq. '00') then
        if (CUTFLG .eq.1) then
           VOL(1) = 0.9864+0.00181*D2H
           if (DBHOB.lt.6.0.and.DBHOB.gt.0.0) then
              VOL(1) = VOL(1)-(0.9864*(1.0-(DBHOB/6.0)**3))
           end if
        end if

        if (CUPFLG .eq.1.and. DBHOB .gt.4.0) then
           VOL(4) = -1.39764+(0.00188*D2H)
        end if

        if (BFPFLG.eq.1.and. DBHOB .gt.6.0) then
           VOL(2) = -28.0674+(0.00937*D2H)
           VOL(10) = -17.4877+(0.01119*D2H)
        end if
        ! A01DVEW747
     else
        if (CUTFLG .eq.1) then
           VOL(1) = 0.00806*HTTOT+0.00175*D2H
        end if

        if (CUPFLG .eq.1.and. DBHOB .gt.4.0) then
           VOL(4) = -0.8722+(0.001811522*D2H)
        end if

        if (BFPFLG.eq.1.and. DBHOB .gt.6.0) then
           VOL(2) = -46.7415+(0.00956*D2H)
           VOL(10) = -49.1199+(0.010941441*D2H)
        end if
     end if
  else if ((voleq(8:10) .eq. '108') .or.&
       &(voleq(8:10) .eq. '310') .or.&
       &(voleq(8:10) .eq. '351') .or.&
       &(voleq(8:10) .eq. '660')) then
     ! Equation 20 for A00DVEW108, A00DVEW310, and A00DVEW351
     read(voleq(8:10),'(I3)')spn
     if (voleq(8:10) .eq. '108') then
        A = -2.615591
        B = 1.847504
        C = 1.085772
     else if (voleq(8:10) .eq. '351') then
        A = -2.672775
        B = 1.920617
        C = 1.074024
     else if (voleq(8:10).eq.'310' .or. voleq(8:10).eq.'660') then
        A = -2.770324
        B = 1.885813
        C = 1.119043
     end if

     !       basal area, squre feet
     BA = 0.005454154*(DBHOB**2)
     !       Volume, cubic feet, entire stem (incl. stump and top)
     CVTS = (10**A)*(DBHOB**B)*(HTTOT**C)
     VOL(1) = CVTS
     if (DBHOB .gt.4.0) then
        !       Tarif access constants
        TATS = 0.912733/((1.033*(1+1.382937*EXP(-4.015292*(DBHOB/10))))&
             &*(BA+0.087266) - 0.174533)
        TARIF = CVTS * TATS
        TV4 = (BA-0.087266)/0.912733
        !       Volume, cubic feet, stump to 4" top
        CV4 = TARIF * TV4
        if (DBHOB .gt.6.0) then
           !       Volume, cubic feet, stump to 6" top
           R64 = 0.993 - 0.993*0.62**(DBHOB-6)
           CV6 = CV4*R64
           !       Volume, board feet, Scribner, stump to 6" top
           if (((spn .ge.300) .and. (DBHOB .ge.11)) .or.&
                &((spn .lt.300) .and. (DBHOB .ge.9))) then
              B4 = TARIF / 0.912733
              BCU1 = 10**(0.174439+0.117594* LOG10(DBHOB) * LOG10(B4) -&
                   &8.210585/DBHOB**2 + 0.236693 * LOG10(B4) -&
                   &0.00001345*B4**2 - 0.00001937*DBHOB**2)
              SV6 = CV6 * BCU1
              !      Volume, bosrd feet, International 1/4", stump to 6" top
              BCU2 = -2.904154 + 3.466328 * LOG10(DBHOB * TARIF) -&
                   &0.02765985 * DBHOB - 0.00008205*TARIF**2 +&
                   &11.29598/DBHOB**2
              IV6 = CV6 * BCU2
              !      Volume, cubic feet, to 8" top
              R84 = 0.983 - 0.983*0.65**(DBHOB-8.6)
              CV8 = CV4 * R84
              !      Volume, board feet, Scribner, to 8" top
              RS86 = 0.99-0.58*(0.484**(DBHOB-9.5))
              SV8 = SV6 * RS86
              !      Volume, board feet International 1/4", to 8" top
              RI86 = 0.99 - 0.55*(0.485**(DBHOB-9.5))
              IV8 = IV6 * RI86
           end if
        end if
     end if
     if (spn.ge.300) then
        CVSL = CV8
        SCFT = SV8
        XINTT = IV8
     else
        SCFT = SV6
        XINTT = IV6
        CVSL = CV6
     end if

     if (CUTFLG .eq.1) then
        VOL(1) = CVTS
     end if
     if (CUPFLG .eq.1) then
        VOL(4) = CVSL
     end if
     VOL(7) = CV4-VOL(4)
     if (BFPFLG.eq.1) then
        VOL(2) = SCFT
        VOL(10) = XINTT
     end if

  end if

  if (VOL(2) .lt.0) VOL(2) = 0
  if (VOL(4) .lt.0) VOL(4) = 0
  if (VOL(1) .lt.0) VOL(1) = 0
  if (VOL(10) .lt.0) VOL(10) = 0
  if (VOL(7) .lt.0) VOL(7) = 0
  return
end
