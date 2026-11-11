!============================!
!   VolumeLibrary : r3d2hv   !
!============================!
  !== last modified  12-3-2008
subroutine R3D2HV(VOLEQU,UNT,HTTOT,HT1PRD,DBHOB,DRC,FCLASS,&
     &HTTFLL,VOL,ERRFLAG)

  !--  THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE
  !--  USING REGION 3 D*D*H VOLUME DETERMINATION EQUATIONS.
  !==klc  June 5, 2000
  ! YW 2018/09/04 Added 301DVEW060, 302DVEW060, 301DVEW106, 302DVEW106, 301DVEW800
  ! YW 2018/09/06 Added 301DVEW122 AND 302DVEW122 FROM FIA VOLUME EQUATION CODE
  ! YW 2018/09/12 Changed the FCLASS to be: 1 = sngle, others = multistem

  ! Variables
  character(len=10) :: VOLEQU
  character(len=10) :: VOLEQTMP
  character(len=3) :: SPC
  character(len=2) :: PROD
  integer :: UNT,FCLASS,HTTFLL,ERRFLAG,I

  real    :: DBHOB,D2H,DRC,D2HA,HTTOT,VOL(15),UM4,UM6,TWVOL,TOPD
  real    :: B1,B2,B3,B4,SCBDFT,HT1PRD,ENTIRE,GCUFT6,GCUFT4,INTBDFT

  do I = 1,15
     VOL(I) = 0.0
     10 continue
  end do

  !--   IF DBHOB OR HTTOT EQUALS ZERO THEN DON'T CALCULATE THE VOLUME
  ERRFLAG = 0

  if (dbhob.lt.1.and. drc.lt.1) then
     ERRFLAG = 3
     go to 1000
  end if

  !== PREDICTED TOTAL HEIGHT, BASED ON REGRESSION OF HEIGHT TO 4-INCH TOP

  if (HTTOT.le.0.0.and. HT1PRD.gt.0.0) HTTOT = 12.211+1.1342*HT1PRD

  !Comment out the HTTOT check to match FIA (20240426)
  !IF (HTTOT.LT.4.5) THEN
  !  ERRFLAG = 4
  !   GOTO 1000
  !ENDIF

  ! BASIC VOLUME CALCULATIONS FOR ALL TREES, ALL UNTS OF MEASURE*

  D2H = DBHOB**2*HTTOT
  ENTIRE = 0.0
  GCUFT6 = 0.0
  GCUFT4 = 0.0
  INTBDFT = 0.0
  SCBDFT = 0.0
  UM4 = 0.0
  UM6 = 0.0
  TWVOL = 0.0
  SPC = VOLEQU(8:10)
  TOPD = 4.0
  if (UNT.eq.1) then
     PROD = '01'
  else
     PROD = '02'
  end if
  !----------------------- PONDEROSA PINE (EAGER MILL STUDY)
  !----------------------- ARIZONA PINE
  !----------------------- APACHE PINE
  if (VOLEQU(8:10).eq.'122'.and.VOLEQU(1:3).eq.'300') then

     if (D2H.le.31629.91964) then
        SCBDFT = -1.786+0.00098814*D2H
     else
        if (HTTFLL .le.0) HTTFLL = 25.0

        SCBDFT = -52.897+(0.12826*HTTFLL)+(0.0017678*D2H)&
             &+(879120.0 / D2H)
     end if
     SCBDFT = SCBDFT * 10.0

     !==---------------------- EAGER CUBIC-FOOT VOLUME

     if (D2H.le.33590.92207) then
        GCUFT6 = -1.7751+0.0018897*D2H
     else
        GCUFT6 = -13.542+0.00224*D2H
     end if

     !==--- HANN & BARE CUBIC-FOOT VOLUMES, COCONINO/LINCOLN FOR ALL FORESTS

     ENTIRE = 0.081072 + (0.001984 * D2H)
     UM4 = -0.125349+(0.003604*((4.0**3 *HTTOT)/DBHOB**1.5)) +&
          &(.005406*DBHOB**2)
     GCUFT4 = ENTIRE - UM4
     UM6 = -0.125349+(0.003604*((6.0**3 *HTTOT)/DBHOB**1.5)) +&
          &(.005406*DBHOB**2)
     TWVOL = UM6 - UM4

     ! ADD 301DVEW122 AND 302DVEW122 (YW 2018/09/06)
     !--Ponderosa Pine, Apache Pine, Arizona Pine
     !--Blackjack Pine in Lincon, Cocinino and Tonto NF's
     !--Yellow pine in R3
     !--Switched to yellow pine equation at 21 inches DBH based on Rmstand code
     !--INT-209, Hann and Bare, 1978
  else if (VOLEQU(8:10).eq.'122'.and.VOLEQU(1:3).eq.'301') then
     if (DBHOB.lt.21.0) then
        VOLEQTMP = '301HAB0122'
     else
        VOLEQTMP = '300HAB1122'
     end if
     call HANN_PP(VOLEQTMP,DBHOB,HTTOT,PROD,TOPD,VOL,ERRFLAG)
     ENTIRE = VOL(1)
     UM4 = VOL(15)
     if (UNT.eq.1) then
        GCUFT6 = VOL(4)
        TWVOL = VOL(7)
        INTBDFT = VOL(10)
        SCBDFT = VOL(2)
     else
        GCUFT4 = VOL(4)
     end if
     !--Ponderosa Pine, Apache Pine, Arizona Pine
     !--Blackjack Pine in Carson and Santa Fe NF's
  else if (VOLEQU(8:10).eq.'122'.and.VOLEQU(1:3).eq.'302') then
     if (DBHOB.lt.21.0) then
        VOLEQTMP = '302HAB0122'
     else
        VOLEQTMP = '300HAB1122'
     end if
     call HANN_PP(VOLEQTMP,DBHOB,HTTOT,PROD,TOPD,VOL,ERRFLAG)
     ENTIRE = VOL(1)
     UM4 = VOL(15)
     if (UNT.eq.1) then
        GCUFT6 = VOL(4)
        TWVOL = VOL(7)
        INTBDFT = VOL(10)
        SCBDFT = VOL(2)
     else
        GCUFT4 = VOL(4)
     end if
     !----------------------- DOUGLAS FIR
     !---------------LINCOLN,COCONINO,TONTO

  else if (VOLEQU(8:10).eq.'202'.and.VOLEQU(1:3).eq.'301') then
     ENTIRE = 0.438374 + (0.001756 * D2H)
     UM6 = -.083149+(.001219*((6.0**3*HTTOT)/DBHOB))+(.005417*&
          &DBHOB**2)
     GCUFT6 = ENTIRE-UM6
     if (GCUFT6.lt.0) GCUFT6 = 0

     INTBDFT = GCUFT6*(6.587353-(.892716*DBHOB**(-1))-(243.514909*&
          &DBHOB**(-2)))
     SCBDFT = INTBDFT*(1.000897-(4.100072*DBHOB**(-1.177748)))

     UM4 = -0.083149+(0.001219*((4.0**3 *HTTOT)/DBHOB)) +&
          &(.005417*DBHOB**2)
     GCUFT4 = ENTIRE - UM4
     TWVOL = UM6 - UM4
     !------  DF ADJUSTMENT TO VOLUME TABLES-- BASED ON FY 87
     !------  VOLUME VALIDATION PROJECT

     SCBDFT = SCBDFT*.932

     !----------------------- DOUGLAS FIR
     !-------------------- CARSON,SANTA FE
  else if (VOLEQU(8:10).eq.'202'.and.VOLEQU(1:3).eq.'302') then
     ENTIRE = 0.341133+(.001918*D2H)

     UM6 = -0.187631 + (0.006719*((6.0**3*HTTOT)/DBHOB**1.5)) +&
          &(.005364 * DBHOB**2)
     GCUFT6 = ENTIRE-UM6
     if (GCUFT6.lt.0) GCUFT6 = 0

     INTBDFT = GCUFT6*(6.597174-(.894047*DBHOB**(-1))-(243.877967*&
          &DBHOB**(-2)))
     SCBDFT = INTBDFT*(.870260 - (19.495942*DBHOB**(-2)))

     UM4 = -0.187631+(0.006719*((4.0**3 *HTTOT)/DBHOB**1.5)) +&
          &(.005364*DBHOB**2)
     GCUFT4 = ENTIRE - UM4

     TWVOL = UM6 - UM4

     !----------------------- WHITE FIR
     !-------------- LINCOLN,COCONINO,TONTO
  else if (VOLEQU(8:10).eq.'015'.and.VOLEQU(1:3).eq.'301') then
     ENTIRE = 0.210904 + (0.001840*D2H)
     UM6 = -0.182700+(0.001248*((6.0**3*HTTOT)/DBHOB))+(0.006245*&
          &DBHOB**2)
     GCUFT6 = ENTIRE-UM6
     if (GCUFT6.lt.0) GCUFT6 = 0

     INTBDFT = GCUFT6*(6.246875-(7.019940*DBHOB**(-1))-(201.958728*&
          &DBHOB**(-2)))
     SCBDFT = INTBDFT*(1.0-(1.888144*DBHOB**(-1))-(8.851449*&
          &DBHOB**(-2)))

     UM4 = -0.182700+(0.001248*((4.0**3 *HTTOT)/DBHOB**1.0)) +&
          &(.006245*DBHOB**2)
     GCUFT4 = ENTIRE - UM4
     TWVOL = UM6 - UM4

     !----------------------- WHITE FIR
     !-------------------- CARSON,SANTA FE

  else if (VOLEQU(8:10).eq.'015'.and.VOLEQU(1:3).eq.'302') then
     ENTIRE = 0.157777+(.002009*D2H)
     UM6 = -0.187563+(.006326*((6.0**3*HTTOT)/DBHOB**1.5))&
          &+(.006041*DBHOB**2)
     GCUFT6 = ENTIRE-UM6
     if (GCUFT6.lt.0) GCUFT6 = 0

     INTBDFT = GCUFT6*(5.736445-(1.720934*DBHOB**(-1))-(74.573790*&
          &DBHOB**(-2)))

     SCBDFT = INTBDFT * (1.017248 - (1.870568 * DBHOB**(-1)) -&
          &(8.514451 * DBHOB**(-2)))

     UM4 = -0.187563+(0.006326*((4.0**3 *HTTOT)/DBHOB**1.5)) +&
          &(.006041*DBHOB**2)
     GCUFT4 = ENTIRE - UM4
     TWVOL = UM6 - UM4

     !----------------------- CORKBARK FIR
     !----------------------- SPRUCE

  else if (VOLEQU(8:10).eq.'093') then
     ENTIRE = 0.225466+(.002170*D2H)
     UM6 = -0.2664752+(.006129*((6.0**3*HTTOT)/DBHOB**1.5))&
          &+(0.007431*DBHOB**2)
     GCUFT6 = ENTIRE-UM6
     if (GCUFT6.lt.0) GCUFT6 = 0

     INTBDFT = GCUFT6 * (5.987363 - (9.847918 * DBHOB**(-1)) -&
          &(-300.812808*DBHOB**(-2)) - (2855.342454*DBHOB**(-3)))
     SCBDFT = INTBDFT * (.878454 - (15.998458 * DBHOB**(-2)))

     UM4 = -0.2664752+(0.006129*((4.0**3 *HTTOT)/DBHOB**1.5)) +&
          &(.007431*DBHOB**2)
     GCUFT4 = ENTIRE - UM4
     TWVOL = UM6 - UM4

     !----------------------- SOUTHWEST WHITE PINE
     !----------------------- BRISTLECONE PINE
     !----------------------- LIMBER PINE
     !----------------------- FOXTAIL

  else if (VOLEQU(8:10).eq.'113') then
     ENTIRE = 0.160889 + (.002032*D2H)
     UM6 = -0.213005 + (.004912*((6.0**3*HTTOT)/DBHOB**1.5))&
          &+(.006061*DBHOB**2)
     GCUFT6 = ENTIRE-UM6
     if (GCUFT6.lt.0) GCUFT6 = 0

     INTBDFT = GCUFT6*(6.691967-(7.520114*DBHOB**(-1))-(216.348366*&
          &DBHOB**(-2)))
     SCBDFT = INTBDFT*(1.006086-(2.384660*DBHOB**(-1)))

     UM4 = -0.213005+(0.004912*((4.0**3 *HTTOT)/DBHOB**1.5)) +&
          &(.006061*DBHOB**2)
     GCUFT4 = ENTIRE - UM4
     TWVOL = UM6 - UM4

     !------------------------------ ASPEN

  else if (VOLEQU(8:10).eq.'746') then
     ENTIRE = 0.0327+(.002311*D2H)
     UM6 = -0.236432+(.005802*((6.0**3*HTTOT)/DBHOB**1.5))&
          &+(.006080*DBHOB**2)
     GCUFT6 = ENTIRE-UM6
     if (GCUFT6.lt.0) GCUFT6 = 0

     INTBDFT = GCUFT6*(6.688085-(-1.276851*DBHOB**(-1))-(-4.504804*&
          &DBHOB**(-2))-(1423.985244*DBHOB**(-3)))
     SCBDFT = INTBDFT*(.887891-(17.19374*DBHOB**(-2)))

     UM4 = -0.236432+(0.005802*((4.0**3 *HTTOT)/DBHOB**1.5)) +&
          &(.006080*DBHOB**2)
     GCUFT4 = ENTIRE - UM4
     TWVOL = UM6 - UM4

     !--------------- JUNIPERS, ARIZONA CYPRESS, AND MISC PINES. (PULPWOOD ONLY)

  else if (VOLEQU(8:10).eq.'060') then
     if (DBHOB.gt.3.0.or. DRC.gt.3.0) then
        if (DRC .gt.0)D2H = DRC*DRC*HTTOT

        D2HA = D2H/1000.0
        ! Added VOLEQ(2:3).EQ.'01' and '02' YW 2018/09/04
        if (VOLEQU(2:3).eq.'01') then
           !--From INT-363, Chojnacky, 1986
           !--Pinyon-Juniper Volume Equations for Arizona Hualapai and Havasupai Indian Reservations
           if (D2HA.le.5.0) then
              GCUFT4 = -.05+2.48*D2HA+.057*D2HA*D2HA
           else
              GCUFT4 = 4.24+2.48*D2HA-14.29/D2HA
           end if
        else if (VOLEQU(2:3).eq.'02') then
           !--From INT-379, Chojnacky, 1988
           !--Woodland Volume Equations for Arizona Fort Apache and San Carlos Indian Reservations
           if (FCLASS.ne.1) then
              if (D2HA.le.5) then
                 GCUFT4 = -.169+1.9246*D2HA+.053*D2HA*D2HA
              else
                 GCUFT4 = 3.805+1.9246*D2HA-13.249/D2HA
              end if
           else
              if (D2HA.le.5) then
                 GCUFT4 = -.002+1.7385*D2HA+.181*D2HA*D2HA
              else
                 GCUFT4 = 13.572+1.7385*D2HA-45.246/D2HA
              end if
           end if

        else
           !--From INT-391, Chojnacky, 1988
           !--Juniper, Pinyon, Oak and Mesquite Volume Equations for Arizona
           if (FCLASS.ne.1) then
              if (D2HA.le.6.0) then
                 GCUFT4 = -0.129 + (2.0255*D2HA)+(0.1011*D2HA*D2HA)
              else
                 GCUFT4 = 10.786 + (2.0255 * D2HA) - (43.663/D2HA)
              end if
           else
              if (D2HA.le.6.0) then
                 GCUFT4 = -0.032 + (2.1076*D2HA)+(0.1454*D2HA*D2HA)
              else
                 GCUFT4 = 15.675 + (2.1076 * D2HA) - (62.827/D2HA)
              end if
           end if
        end if
     else
        GCUFT4 = 0.0
     end if
     ENTIRE = GCUFT4
     if (UNT.eq.1) GCUFT6 = GCUFT4

     !---------------------- PINYON PINES (PULPWOOD ONLY)

  else if (VOLEQU(8:10).eq.'106') then
     if (DBHOB.gt.3.0.or. DRC.gt.3.0) then
        if (DRC .gt.0)D2H = DRC*DRC*HTTOT
        D2HA = D2H/1000.0
        if (VOLEQU(2:3).eq.'01') then
           !--From INT-363, Chojnacky, 1986
           !--Pinyon-Juniper Volume Equations for Arizona Hualapai and Havasupai Indian Reservations
           if (D2HA.le.5) then
              GCUFT4 = -.07+2.51*D2HA+.098*D2HA*D2HA
           else
              GCUFT4 = 7.29+2.51*D2HA-24.53/D2HA
           end if
        else if (VOLEQU(2:3).eq.'02') then
           !--From INT-379, Chojnacky, 1988
           !--Woodland Volume Equations for Arizona Fort Apache and San Carlos Indian Reservations
           if (D2HA.le.5) then
              TCVOL = -.073+2.1608*D2HA+.0804*D2HA*D2HA
           else
              TCVOL = 5.961+2.1608*D2HA-20.112/D2HA
           end if
           GCUFT4 = TCVOL
        else
           !--INT-391, Chojnacky, 1988
           !--Juniper, Pinyon, Oak and Mesquite Volume Equations for Arizona
           !--TM_EQ: 300DVEQ106
           if (D2HA.le.3.0) then
              GCUFT4 = -0.060 + (2.5139*D2HA)+(0.1466*D2HA*D2HA)
           else
              GCUFT4 = 3.898 + (2.5139 * D2HA) - (7.917/D2HA)
           end if
        end if
     else
        GCUFT4 = 0.0
     end if
     ENTIRE = GCUFT4
     if (UNT.eq.1) GCUFT6 = GCUFT4

     !----------------------- MISC HARDWOODS / maple (PULPWOOD ONLY)

  else if (VOLEQU(8:10).eq.'310') then
     if (DBHOB.gt.3.0.or. DRC.gt.3.0) then
        if (DRC .gt.0)D2H = DRC*DRC*HTTOT
        if (FCLASS.ne.1) then
           B4 = 0.0
        else
           B4 = 1.0
        end if

        !--  SET COEFFICIENTS FOR THE EQUATION

        B1 = -0.29013
        B2 = 0.126114
        B3 = 0.14489

        GCUFT4 = (B1 + (B2 * D2H**0.3333) + (B3*B4))**3
        ENTIRE = GCUFT4
     else
        GCUFT4 = 0.0
     end if
     ENTIRE = GCUFT4
     if (UNT.eq.1) GCUFT6 = GCUFT4

     !----------------------- OAKS (PULPWOOD ONLY)
  else if (VOLEQU(8:10).eq.'800') then
     if (DBHOB.gt.3.0.or. DRC.gt.3.0) then
        if (DRC .gt.0)D2H = DRC*DRC*HTTOT
        D2HA = D2H/1000.0
        if (VOLEQU(2:3).eq.'01') then
           !--From INT-379, Chojnacky, 1988
           !--Woodland Volume Equations for Arizona Fort Apache and San Carlos Indian Reservations
           if (FCLASS.ne.1) then
              if (D2HA.le.5.0) then
                 GCUFT4 = -.181+2.1917*D2HA+.0208*D2HA*D2HA
              else
                 GCUFT4 = -1.742+2.1917*D2HA+5.205/D2HA
              end if
           else
              if (D2HA.le.5.0) then
                 GCUFT4 = -.02+2.1896*D2HA+.127*D2HA*D2HA
              else
                 GCUFT4 = 9.502+2.1896*D2HA-31.74/D2HA
              end if
           end if
        else
           !--From INT-391, Chojnacky, 1988
           !--Juniper, Pinyon, Oak and Mesquite Volume Equations for Arizona
           if (FCLASS.ne.1) then
              if (D2HA.le.4.0) then
                 GCUFT4 = -0.028 + (1.9545*D2HA)+(0.1400*D2HA*D2HA)
              else
                 GCUFT4 = 6.691 + (1.9545 * D2HA) - (17.918/D2HA)
              end if
           else
              if (D2HA.le.4.0) then
                 GCUFT4 = -0.068 + (2.4048*D2HA)+(0.1383*D2HA*D2HA)
              else
                 GCUFT4 = 6.571 + (2.4048 * D2HA) - (17.704/D2HA)
              end if
           end if
        end if
     else
        !Set to 0.1 to match FIA (20240426)
        GCUFT4 = 0.1
     end if
     ENTIRE = GCUFT4
     if (UNT.eq.1) GCUFT6 = GCUFT4
     !-------------------- MESQUITE  (PULPWOOD ONLY)

  else if (VOLEQU(8:10).eq.'999') then
     if (DBHOB.gt.3.0.or. DRC.gt.3.0) then
        if (DRC .gt.0)D2H = DRC*DRC*HTTOT
        D2HA = D2H/1000.0
        if (FCLASS.ne.1) then
           if (D2HA.le.2.0) then
              GCUFT4 = 0.020 + (1.8972*D2HA)+(0.5756*D2HA*D2HA)
           else
              GCUFT4 = 6.927 + (1.8972 * D2HA) - (9.210/D2HA)
           end if
        else
           if (D2HA.le.2.0) then
              GCUFT4 = -0.043 + (2.3378*D2HA)+(0.8024*D2HA*D2HA)
           else
              GCUFT4 = 9.586 + (2.3378 * D2HA) - (12.839/D2HA)
           end if
        end if
     else
        GCUFT4 = 0.0
     end if
     ENTIRE = GCUFT4
     if (UNT.eq.1) GCUFT6 = GCUFT4

     !--------------------- HACKBERRY, ALDERLEAF, MAHOGANY  (PULPWOOD ONLY)

  else if (VOLEQU(8:10).eq.'314') then
     if (DBHOB.gt.3.0.or. DRC.gt.3.0) then
        if (DRC .gt.0)D2H = DRC*DRC*HTTOT
        if (FCLASS.ne.1) then
           B4 = 0.0
        else
           B4 = 1.0
        end if

        !--  SET COEFFICIENTS FOR THE EQUATION

        B1 = -0.29013
        B2 = 0.126114
        B3 = 0.14489

        GCUFT4 = (B1 + (B2 * D2H**0.3333) + (B3*B4))**3
     else
        GCUFT4 = 0.0
     end if
     ENTIRE = GCUFT4
     if (UNT.eq.1) GCUFT6 = GCUFT4

     !*************---------- SMALIANS FORMULA (PULPWOOD ONLY)

  else
     B1 = DBHOB**2 * 0.005454
     B2 = 4.**2 * 0.005454
     GCUFT4 = (B1 + B2)/2. * HT1PRD
     ENTIRE = GCUFT4
     if (UNT.eq.1) GCUFT6 = GCUFT4

  end if

  !===     END OF VOLEQU BLOCKS

  !-       DBH LT 9.0 HAS NO BDFT

  if (DBHOB.lt.9) then
     SCBDFT = 0
     INTBDFT = 0
  end if

  !               TOTAL CUBIC (MIGHT BE ZERO)
  VOL(1) = ENTIRE
  !               SAWTIMBER TREES (PRODUCT = 01) (BDFT, CUFT, TOPWOOD)
  if (UNT .eq.1) then
     VOL(2) = SCBDFT
     VOL(10) = INTBDFT
     VOL(4) = GCUFT6
     VOL(7) = TWVOL
     !               NON-SAWTIMBER TREES (PRODUCT != 01) (CUFT, CORDS)
  else if (UNT .eq.3) then
     VOL(4) = GCUFT4
     VOL(6) = VOL(4)/79.0
  end if
  VOL(15) = UM4

  if (VOL(1).lt.0.0) then
     VOL(1) = 0.0
  end if
  if (VOL(2).lt.0.0) then
     VOL(2) = 0.0
  end if
  if (VOL(4).lt.0.0) then
     VOL(4) = 0.0
  end if
  if (VOL(6).lt.0.0) then
     VOL(6) = 0.0
  end if
  if (VOL(7).lt.0.0) then
     VOL(7) = 0.0
  end if
  if (VOL(10).lt.0.0) then
     VOL(10) = 0.0
  end if
  if (VOL(15).lt.0.0) VOL(15) = 0.0

  1000 continue

  return
end

  !********VOLUME EQUATION NUMBERS********

  !--   THE FOLLOWING SPECIES ARE GROUPED FOR EQUATION NUMBERS

  !--       300DVEW122 = APACHE PINE, CHIHUAHUA PINE, ARIZONA PINE, PONDEROSA PINE
  !--       301DVEW202 = DOUGLAS FIR; LINCOLN, COCONINO, TONTO, APS, COR, GIL, PRE
  !--       302DVEW202 = DOUGLAS FIR; CARSON, SANTE FE, CIB, KAI
  !--       301DVEW015 = WHITE FIR; LINCOLN, COCONINO, TONTO, APS, COR, GIL, PRE
  !--       302DVEW015 = WHITE FIR; CARSON, SANTE FE,  CIB, KAI
  !--       300DVEW093 = CORKBARK, SUBALPINE, ENGELMANN SPRUCE, BLUE SPRUCE
  !--       300DVEW113 = BRISTLECONE PINE, LIBMBER PINE, SOUTHWESTERN WHITE PINE
  !--       300DVEW746 = ASPEN

  !--       300DVEW060 = ALLIGATOR JUNIPER, UTAH JUNIPER, ONE SEED JUNIPER,
  !--                    ROCKY MTN JUNIPER, PINCHOT JUNIPER, REDBERRY JUNIPER,
  !--                    ARIZONA CYPRESS, MISC SOFTWOODS
  !--       300DVEW106 = PINON PINE, SINGLE LEAF PINON PINE, BORDER PINE
  !--       300DVEW310 = BIGTOOTH MAPLE, BOX ELDER, ASH, WALNUT, WILLOW,
  !--                    ARIZONA SYCAMORE, COTTONWOOD, N.M. LOCUST, TAMARISK,
  !--                    MISC HARDWOODS
  !--       300DVEW800 = EMERY OAK, GAMBEL OAK, ARIZONA WHITE OAK, GREY OAK,
  !--                    SILVERLEAF OAK
  !--       300DVEW999 = MESQUITE
  !--       300DVEW314 = HACKBERRY, ALDERLEAF, MNT MAHOGANY, HAIRY MTN MAHOGANY

  !--  HTTOT - REAL -  **TREE HEIGHT IN FT. FROM GROUND TO TIP**

subroutine HANN_PP(VOLEQ,DBHOB,HTTOT,PROD,MTOPP,VOL,ERRFLAG)
  !     HANN & BARE Equation for ponderosa pine young-growth  (blackjack)
  !     in Lincon, Cocinino and Tonto NF and Carson and Santa Fe NF
  !     and old-growth (yellow pine) in region wide
  !     Volume Equation number:
  !     301HAB0122 -- Young-growth (blackjack pine) in Lincon, Cocinino and Tonto
  !     302HAB0122 -- Young-growth (blackjack pine) in Carson and Santa Fe
  !     300HAB1122 -- Old-growth (yellow pine)

  ! Variables
  character(len=10) :: VOLEQ
  character(len=2) :: PROD
  real    :: DBHOB,HTTOT,MTOPP,TOP,VOL(15)
  integer :: ERRFLAG
  real    :: D2H,UM4,UM6,TWVOL,SCBDFT,ENTIRE,GCUFT6,GCUFT4,INTBDFT

  do I = 1,15
     VOL(I) = 0.0
     100 continue
  end do

  !--   IF DBHOB OR HTTOT EQUALS ZERO THEN DON'T CALCULATE THE VOLUME
  ERRFLAG = 0
  if (VOLEQ(8:10).ne.'122') then
     ERRFLAG = 1
     return
  end if
  if (dbhob.lt.1.0) then
     ERRFLAG = 3
     return
  end if
  if (HTTOT.lt.4.5) then
     ERRFLAG = 4
     return
  end if

  D2H = DBHOB**2*HTTOT
  ENTIRE = 0.0
  GCUFT6 = 0.0
  GCUFT4 = 0.0
  INTBDFT = 0.0
  SCBDFT = 0.0
  UM4 = 0.0
  UM6 = 0.0
  TWVOL = 0.0
  TOP = MTOPP
  if (TOP.eq.0.0) TOP = 4.0
  !     valid top diameter is between 3 and 8
  if (TOP.lt.3.0) TOP = 3.0
  if (TOP.gt.8.0) TOP = 8.0
  ! ----Blackjack pine
  if (VOLEQ(7:7).eq.'0') then
     ! ------Lincon, Cocinino and Tonto
     if (VOLEQ(2:3).eq.'01') then
        ENTIRE = 0.0810724804 + 0.00198351037 * D2H
        UM6 = -0.125349396+(0.00360421889*((6.0**3*HTTOT)/DBHOB**1.5))&
             &+ (.00540634204*DBHOB**2)
        GCUFT6 = ENTIRE-UM6
        if (GCUFT6.lt.0.0) GCUFT6 = 0.0
        UM4 = -0.125349396+(0.00360421889*((TOP**3*HTTOT)/DBHOB**1.5))&
             &+ (.00540634204*DBHOB**2)
        GCUFT4 = ENTIRE - UM4
        TWVOL = UM6 - UM4
        INTBDFT = GCUFT6*(6.84751736-(7.69491322*DBHOB**(-1))&
             &-(221.377226*DBHOB**(-2)))
        SCBDFT = INTBDFT*(.96579222-.40579028*DBHOB**(-1)&
             &-16.93678414*DBHOB**(-2))
        ! ------Carson and Santa Fe NF
     else if (VOLEQ(2:3).eq.'02') then
        ENTIRE = 0.0483082948 + 0.00204968419 * D2H
        UM6 = -0.133967845+(0.00650174839*((6.0**3*HTTOT)/DBHOB**1.5))&
             &+ (.00490223789*DBHOB**2)
        GCUFT6 = ENTIRE-UM6
        if (GCUFT6.lt.0.0) GCUFT6 = 0.0
        UM4 = -0.133967845+(0.00650174839*((TOP**3*HTTOT)/DBHOB**1.5))&
             &+ (.00490223789*DBHOB**2)
        GCUFT4 = ENTIRE - UM4
        TWVOL = UM6 - UM4
        INTBDFT = GCUFT6*(7.58122078-(8.51941410*DBHOB**(-1))&
             &-(245.097535*DBHOB**(-2)))
        SCBDFT = INTBDFT*(.993986685-1.463486622*DBHOB**(-1)&
             &-12.40584877*DBHOB**(-2))
     else
        ERRFLAG = 1
        return
     end if
     ! ----Yellow pine
  else if (VOLEQ(7:7).eq.'1') then
     ENTIRE = 0.237204154 + 0.00221122919 * D2H
     UM6 = .0185465259+(0.000788175798*((6.0**3*HTTOT)/DBHOB**1.0))&
          &+ (.00505513624*DBHOB**2)
     GCUFT6 = ENTIRE-UM6
     if (GCUFT6.lt.0.0) GCUFT6 = 0.0
     UM4 = .0185465259+(0.000788175798*((TOP**3*HTTOT)/DBHOB**1.0))&
          &+ (.00505513624*DBHOB**2)
     GCUFT4 = ENTIRE - UM4
     TWVOL = UM6 - UM4
     INTBDFT = GCUFT6*(7.10051404-(7.97921881*DBHOB**(-1))&
          &-(229.556497*DBHOB**(-2)))
     SCBDFT = INTBDFT*(.982101210-.926027395*DBHOB**(-1)&
          &-14.49443523*DBHOB**(-2))
  else
     ERRFLAG = 1
     return
  end if
  VOL(1) = ENTIRE
  !     SAWTIMBER TREES (PRODUCT = 01) (BDFT, CUFT, TOPWOOD)
  if (PROD.eq.'01') then
     VOL(2) = SCBDFT
     VOL(10) = INTBDFT
     VOL(4) = GCUFT6
     VOL(7) = TWVOL
     !     NON-SAWTIMBER TREES (PRODUCT != 01) (CUFT, CORDS)
  else
     VOL(4) = GCUFT4
     VOL(6) = VOL(4)/79.0
  end if
  VOL(15) = UM4
  if (VOL(1).lt.0.0) then
     VOL(1) = 0.0
  end if
  if (VOL(2).lt.0.0) then
     VOL(2) = 0.0
  end if
  if (VOL(4).lt.0.0) then
     VOL(4) = 0.0
  end if
  if (VOL(6).lt.0.0) then
     VOL(6) = 0.0
  end if
  if (VOL(7).lt.0.0) then
     VOL(7) = 0.0
  end if
  if (VOL(10).lt.0.0) then
     VOL(10) = 0.0
  end if
  if (VOL(15).lt.0.0) VOL(15) = 0.0
  return
end
