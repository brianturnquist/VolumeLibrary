!============================!
!   VolumeLibrary : r10tap   !
!============================!
  !== last modified  03-20-2006
subroutine R10TAP(TAPEQU,DBHOB,HTTOT,HTUP,D2)

  !     TAPEQU  IS THE NUMBER OF THE PROFILE EQUATION THAT WILL BE USED
  !              IN THE CALCULATION
  !              TAPEQU 1051 IS SPRUCE-HEMLOCK - 16 FOOT LOGS
  !              TAPEQU 1052 IS SPRUCE-HEMLOCK - 32 FOOT LOGS
  !              TAPEQU 1053 IS WESTERN REDCEDAR - 16 FOOT LOGS
  !              TAPEQU 1054 IS WESTERN REDCEDAR - 32 FOOT LOGS
  !              TAPEQU 1055 IS ALASKA CEDAR - 16 FOOT LOGS
  !              TAPEQU 1056 IS ALASKA CEDAR - 32 FOOT LOGS
  !     DBHOB   IS DIAMETER BREAST HEIGHT OUTSIDE BARK
  !     HTTOT   IS TOTAL HEIGHT OF TREE FROM GROUND TO TIP
  !     HTUP    IS THE HEIGHT ON THE TREE STEM WHERE A DIB IS TO BE ESTIMATED
  !     D2      IS THE ESTIMATED DIAMETER AT HEIGHT "HTUP"

  !     LIST REAL VARIABLES

  ! Variables
  character(len=10) :: TAPEQU
  character(len=2) :: ISP
  character(len=2) :: IAC
  character(len=2) :: IRC
  character(len=2) :: IRA
  real    :: RH,DSI,RH32,BK,RH40
  real    :: HTTOT,HTUP,DBHOB,D2,D,H,BKAYC,BKWRC
  real    :: BKAC,DVA,BKWR,DVR,BB,DD2MI,DVREDA

  !     PROFILE EQUATION  (USED FOR DIAMETER CALCULATIONS)

  !     SPRUCE-HEMLOCK TAPER EQUATION

  !     THIS EQUATION IS USED WHEN CALCULATING DIB'S ALONG THE TREE STEM
  !     THIS EQUATION NUMBER 1051 AND 1052

  DD2MI(RH,RH32,D,H) = RH**1.5 + (-.0052554*H + 0.000034947*H**2&
       &+ 0.104477*H/D) * (RH**1.5 - RH**3) + (7.76807/D**2&
       &- 0.0000094852*H**2 - 0.011351*H/D) * (RH**1.5-RH32**32)

  !     BARK THICKNESS EQUATION

  BB(D,H) = (0.8467 + 0.0009144*D + 0.0003568*H)

  !     WESTERN REDCEDAR

  !     THIS EQUATION IS USED WHEN CALCULATING DIB'S ALONG THE TREE STEM
  !     THIS EQUATION NUMBER 1053 AND 1054

  DVR(RH,RH32,H,DBHOB) = RH**1.5 + ((RH**1.5 - RH**3) *&
       &(5.17703194/DBHOB**2 - 0.12516819*DBHOB + 0.02537037*H&
       &- 0.00004193*H**2 + 0.00155481 * DBHOB**2)) + ((RH**1.5&
       &- RH32**32) * (-0.00002070*H**2 + 0.24125235/DBHOB**2))

  !     BARK THICKNESS EQUATION

  BKWR(DBHOB,H) = 0.86031485 + 0.00059638*H - 0.18335961/DBHOB

  !     ALASKA YELLOW CEDAR

  !     THIS EQUATION IS USED WHEN CALCULATING DIB'S ALONG THE TREE STEM
  !     THIS EQUATION NUMBER 1055 AND 1056

  DVA(RH,RH32,H,DBHOB) = RH**1.5+((RH**1.5-RH**3)*(-0.02834001*DBHOB&
       &+ 0.00007123*H**2 + 0.06709114*H/DBHOB))&
       &+ ((RH**1.5 - RH32**32) * (.00282021*DBHOB - 0.00002277*H**2&
       &+ 1.06064717/DBHOB**2 - 0.00528349*H/DBHOB))

  !    BARK THICKNESS EQUATION FOR ALASKA YELLOW CEDAR

  BKAC(DBHOB,H) = 0.95866817 + 0.00064402 * DBHOB - 3.1299972/H

  !     THE FOLLOWING TAPER EQUATION IS FOR RED ALDER
  !         BASED ON DEPENDENT VARIABLE  (DIBI/DBHIB)

  DVREDA(RH,RH32,RH40,H,DBHOB) = 0.91274*(RH**1.5)&
       &-(1.9758*(RH**1.5-RH**3.)*(DBHOB*10**(-2.)))&
       &+ (8.2375*(RH**1.5 - RH**3.)*(H)*(10**(-3.)))&
       &- (4.964*(RH**1.5 - RH32**32.)*(H*DBHOB)*(10**(-5.)))&
       &+ (3.773*(RH**1.5 - RH32**32.)*(H**0.5)*(10**(-3.)))&
       &- (7.417*(RH**1.5 - RH40**40.)*(H**2.)*(10**(-6.)))

  IAC = 'AC'
  IRC = 'RC'
  IRA = 'RA'
  !      ISS='SS'
  if (TAPEQU(8:10).eq.'042') ISP = 'AC'
  if (TAPEQU(8:10).eq.'242') ISP = 'RC'
  if (TAPEQU(8:10).eq.'098') ISP = 'SS'
  if (TAPEQU(8:10).eq.'351') ISP = 'RA'

  !     ZERO OUT ARRAYS

  D = DBHOB
  H = HTTOT

  !   FIND SCALING DIAMETERS FOR ALL LOG HEIGHTS

  !  THIS IS THE ALASKA CEDAR PROFILE EQUATION CALCULATION

  if (ISP.eq.IAC .and. DBHOB.lt.38.01) then
     RH = (H - HTUP)/(H - 4.5)

     !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
     !      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
     !      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.

     if (RH.le.0.0) then
        RH = 0
        RH32 = 0
        D2 = 0
        return
     else if (RH .lt.0.078) then
        RH32 = 0.078
     else
        RH32 = RH
     end if

     DSI = DVA(RH,RH32,H,DBHOB)
     if (dsi.lt.0.0) dsi = 0.0
     BKAYC = BKAC(DBHOB,H)
     D2 = (DSI*BKAYC)**0.5*DBHOB

     !  THIS IS THE WESTERN REDCEDAR PROFILE EQUATION CALCULATION

  else if (ISP.eq.IRC .and. DBHOB.lt.56.01) then
     RH = (H - HTUP)/(H - 4.5)

     !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
     !      THE (.)^32 TERM OF THE DVR FUNCTION. THIS SOLVES NDP ERROR,
     !      NUMERICAL UNDERFLOW, IN THE DVR FUNCTION.

     if (RH.le.0.0) then
        RH = 0
        RH32 = 0
        D2 = 0
        return
     else if (RH .lt.0.078) then
        RH32 = 0.078
     else
        RH32 = RH
     end if

     DSI = DVR(RH,RH32,H,DBHOB)
     if (dsi.lt.0.0) dsi = 0.0
     BKWRC = BKWR(DBHOB,H)
     D2 = (DSI*BKWRC)**.5*DBHOB

     !     THIS IS THE SPRUCE-HEMLOCK AND RED ALDER PROFILE EQUATION CALCULATION

  else
     if (ISP.eq.IRA) then
        BK = 0
     else
        BK = BB(D,H)
     end if
     RH = (H - HTUP)/(H - 4.5)

     !      IF R<0.078 (R^32<10^-38), R32 IS TRUNCATED AND USED IN
     !      THE (.)^32 TERM OF THE DD2MI FUNCTION. THIS SOLVES NDP ERROR,
     !      NUMERICAL UNDERFLOW, IN THE DD2MI FUNCTION.

     if (RH.le.0.0) then
        RH = 0
        RH32 = 0
        RH40 = 0
        D2 = 0
        return
     else if (RH .lt.0.078) then
        RH40 = 0.15
        RH32 = 0.078
     else if (RH .lt.0.15) then
        RH40 = 0.15
        RH32 = RH
     else
        RH40 = RH
        RH32 = RH
     end if

     if (ISP.eq.IRA) then
        D2 = DVREDA(RH,RH32,RH40,H,D)
     else
        D2 = DD2MI(RH,RH32,D,H)
     end if
     if (d2.lt.0.0) d2 = 0.0
     if (ISP.eq.IRA) then
        D2 = (D2)**.5*D
     else
        D2 = (D2*BK)**.5*D
     end if
  end if

  !     THIS IS THE END OF THE DIAMETER CALCULATIONS

  return
end
