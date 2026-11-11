!=============================!
!   VolumeLibrary : r10tapo   !
!=============================!
  !== last modified  4-9-2002
subroutine R10TAPO(DBHOB,HT2,LTYPE,MTOPP,CHK,NLOG,HMERCH,CL,SH,&
     &DIB,VS,V)

  !     CALCULATES FOR HT IN NUMBER OF LOGS ONLY

  !                              ARGUMENTS                               *

  !  DBHOB      - (Input)  - DBH                                             *
  !  HT2    - (Input)  - Number 16' Logs                                 *
  !  TYPE   - (Input)  - L if height is in logs                          *
  !  NLOG   - (Output) - Number 16' Logs (Calculated)                    *
  !  HMERCH - (Output) - Height, Merch                                   *
  !  DIB    - (Output) - Calculateed Top Dia IB (small end)              *
  !  CL     - (Output) - Chunk Length                         ajk/3de91  *

  ! Variables
  real    :: SH(21),DIB(21),TOPDIA(2),MMH(2),VS(21),DST(2),DSLO(2),DSHI(2),XLL(21),DS(21),V(21),HH(21)

  real    :: HT2,HT2TEMP,DBHOB,HMERCH,NLOG,MTOPP,ST
  real    :: BK,BB,H,FV,HEST,RXL,TAPER,CL,FF,DD2MI,DXL,R32,RH
  character(len=1) :: LTYPE
  integer :: CHK,I,NOL16,NOL32,K,N,NN,KBD,KB,NLOG3,JJ

  TOPDIA(1) = MTOPP
  TOPDIA(2) = 4.0

  do 15, I = 1,2
  MMH(I) = 1.0
  15 continue

  do 16, I = 1,21
  VS(I) = 0.0
  DS(I) = 0.0
  DIB(I) = 0.0
  XLL(I) = 0.0
  V(I) = 0.0
  16 continue

  ST = 1.0

  if (DBHOB.gt.36) ST = DBHOB/36.

  !****   CHECK FOR HEIGHT OF TREE IN LOGS  ***

  if (LTYPE.eq.'L' .or. LTYPE.eq.'l') then

     HT2TEMP = HT2
     NOL16 = INT(HT2TEMP)
     NOL32 = 0

     if (NOL32.eq.0.and.NOL16.eq.0) then
        BK = BB(DBHOB,H)
        FV = 0.005454154 * DBHOB**2 * (H - 4.5) * BK
     end if

     if (NOL32.gt.0) NOL16 = NOL32 * 2

     if (NOL16.gt.0) then
        HH(1) = ST + NOL16 * 16.3
        H = 0.0
     end if

     do 20, I = 1,2

     XLL(I) = 1. - (2./3.) * (TOPDIA(I)/DBHOB)
     if (H.lt.0.01) HEST = HH(1)/XLL(I)

     if (HEST.gt.0.01) BK = BB(DBHOB,HEST)

     DST(I) = (TOPDIA(I)**2)/(BK * DBHOB**2)
     DSLO(I) = DST(I) - 0.00001
     DSHI(I) = DST(I) + 0.00001

     if (H.le.0.01) then
        RH = (HEST - HH(1))/(HEST-4.5)

        !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
        !      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
        !      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.

        if (RH.le.0.0) then
           RH = 0
           R32 = 0
        else if (RH .lt.0.078) then
           R32 = 0.078
        else
           R32 = RH
        end if

        DS(I) = DD2MI(RH,R32,DBHOB,HEST)
        RXL = 0.9 * RH

        !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
        !      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
        !      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.

        if (RXL.le.0.0) then
           RXL = 0
           R32 = 0
        else if (RXL .lt.0.078) then
           R32 = 0.078
        else
           R32 = RXL
        end if

        DXL = DD2MI(RXL,R32,DBHOB,HEST)

        TAPER = (DS(I)-DXL)/(0.1*RH)
     else
        HH(I) = H * XLL(I)
        RH = (H - HH(I))/(H - 4.5)

        !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
        !      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
        !      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.

        if (RH.le.0.0) then
           RH = 0
           R32 = 0
        else if (RH .lt.0.078) then
           R32 = 0.078
        else
           R32 = RH
        end if

        DS(I) = DD2MI(RH,R32,DBHOB,H)

        ! ***     ESTIMATE TAPER NEAR R

        RXL = 0.9 * RH

        !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
        !      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
        !      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.

        if (RXL.le.0.0) then
           RXL = 0
           R32 = 0
        else if (RXL .lt.0.078) then
           R32 = 0.078
        else
           R32 = RXL
        end if

        DXL = DD2MI(RXL,R32,DBHOB,H)
        TAPER = (DS(I) - DXL)/(0.1 * RH)
     end if

     do K = 1,10

        if (DS(I).gt.DSLO(I).and.DS(I).lt.DSHI(I))  go to 40

        RXL = RH + (DST(I) - DS(I))/TAPER
        RH = RXL

        !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
        !      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
        !      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.

        if (RH.le.0.0) then
           RH = 0
           R32 = 0
        else if (RH .lt.0.078) then
           R32 = 0.078
        else
           R32 = RH
        end if

        if (H.le.0.01) then
           HEST = (HH(I) - 4.5 * RH)/(1. - RH)

           DS(I) = DD2MI(RH,R32,DBHOB,HEST)
        else
           DS(I) = DD2MI(RH,R32,DBHOB,H)
        end if

        30 continue
     end do

     40 continue

     if (NOL16.le.0.and.H.ge.0.01) then
        HH(I) = H-(RH*(H-4.5))
     end if

     if (H.lt.0.01) H = HEST

     ! ***   FIND NUMBER (NN) OF LOGS AND CHUNKS AND FIND CHUNK LENGTH
     ! ***   FOR TOPDIA(1) ONLY (I.E., DIB TOP=6.0 INCHES).

     if (CHK.eq.1) go to 25
     20 continue

     25 do 50, N = 1,21
     MMH(1) = 16.3 * N + ST + 0.0001
     NN = N
     if (MMH(1) .ge. HH(1)) go to 60
     50 continue

     60 continue
     KBD = NN - 1

     ! ***   CALCULATE CHUNK LENGTH

     CL = HH(1) - 16.3 * KBD - ST

     ! ***   NEXT, FIND SCALING DIAMETERS AND VOLUMES FOR ALL LOG HEIGHTS TO
     ! ***   TIP OF TREE.  START WITH SCALING HEIGHT LIST.

     SH(1) = 4.5
     !        SH(2) = 4.5

     do 70, KB = 1,KBD
     SH(KB+1) = ST + KB * 16.3
     70 continue

     SH(KBD+2) = HH(1)
     SH(KBD+3) = HH(2)
     SH(KBD+4) = H
     NLOG3 = NN + 3
     NLOG = NN
     BK = BB(DBHOB,H)
     FV = 0.005454154 * DBHOB**2 * (H-4.5) * BK

     do 80, JJ = 1,NLOG3
     RH = (H - SH(JJ))/(H - 4.5)

     !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
     !      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
     !      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.

     if (RH.le.0.0) then
        RH = 0
        R32 = 0
     else if (RH .lt.0.078) then
        R32 = 0.078
     else
        R32 = RH
     end if
     DIB(JJ) = DD2MI(RH,R32,DBHOB,H)
     DIB(JJ) = (DIB(JJ) * BK)**0.5 * DBHOB
     VS(JJ) = FF(RH,R32,DBHOB,H) * FV
     80 continue

     ! ***   NEXT, GET LOG AND TREE VOLUMES

     do 85, JJ = 1,NLOG3
     V(JJ) = VS(JJ) - VS(JJ+1)
     85 continue

     HMERCH = H

     !*****     IF TOTAL HEIGHT IS IN FEET        ***

  else
     H = HT2
     BK = BB(DBHOB,H)
     FV = 0.005454154 * DBHOB**2 * (H - 4.5) * BK
     do 200, I = 1,2
     DST(I) = (TOPDIA(I)**2)/(BK * DBHOB**2)
     DSLO(I) = DST(I) - 0.00001
     DSHI(I) = DST(I) + 0.00001

     XLL(I) = 1. - (2./3.) * (TOPDIA(I)/DBHOB)
     HH(I) = H * XLL(I)
     RH = (H - HH(I))/(H - 4.5)

     !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
     !      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
     !      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.

     if (RH.le.0.0) then
        RH = 0
        R32 = 0
     else if (RH .lt.0.078) then
        R32 = 0.078
     else
        R32 = RH
     end if

     DS(I) = DD2MI(RH,R32,DBHOB,H)

     ! ***     ESTIMATE TAPER NEAR R

     RXL = 0.9 * RH

     !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
     !      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
     !      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.

     if (RXL.le.0.0) then
        RXL = 0
        R32 = 0
     else if (RXL .lt.0.078) then
        R32 = 0.078
     else
        R32 = RXL
     end if
     DXL = DD2MI(RXL,R32,DBHOB,H)
     TAPER = (DS(I) - DXL)/(0.1 * RH)

     do K = 1,10
        if (DS(I).gt.DSLO(I).and.DS(I).lt.DSHI(I)) go to 110
        RXL = RH + (DST(I) - DS(I))/TAPER
        RH = RXL

        !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
        !      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
        !      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.

        if (RH.le.0.0) then
           RH = 0
           R32 = 0
        else if (RH .lt.0.078) then
           R32 = 0.078
        else
           R32 = RH
        end if
        DS(I) = DD2MI(RH,R32,DBHOB,H)

        100 continue
     end do

     110 continue
     HH(I) = H - (RH * (H - 4.5))

     200 continue

     do 250, N = 1,21
     MMH(1) = 16.3 * N + ST
     NN = N
     if (MMH(1).ge.HH(1))  go to 260
     250 continue

     260 continue
     KBD = NN - 1

     ! ***   CALCULATE CHUNK LENGTH

     CL = HH(1) - 16.3 * KBD - ST

     ! ***   NEXT, FIND SCALING DIAMETERS AND VOLUMES FOR ALL LOG HEIGHTS TO
     ! ***   TIP OF TREE.  START WITH SCALING HEIGHT LIST.

     SH(1) = 4.5
     !        SH(2) = 4.5

     do 300, KB = 1,KBD
     SH(KB+1) = ST + KB * 16.3
     300 continue

     SH(KBD+2) = HH(1)
     SH(KBD+3) = HH(2)
     SH(KBD+4) = H
     NLOG3 = NN + 3
     NLOG = NN

     do 400, JJ = 1,NLOG3
     RH = (H - SH(JJ))/(H - 4.5)

     !      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
     !      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
     !      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.

     if (RH.le.0.0) then
        RH = 0
        R32 = 0
     else if (RH .lt.0.078) then
        R32 = 0.078
     else
        R32 = RH
     end if
     DIB(JJ) = DD2MI(RH,R32,DBHOB,H)
     if (dib(jj) .lt.0.0) dib(jj) = 0.0
     DIB(JJ) = (DIB(JJ) * BK)**.5 * DBHOB
     VS(JJ) = FF(RH,R32,DBHOB,H) * FV
     400 continue

     ! ***   NEXT, GET LOG AND TREE VOLUMES

     do 450, JJ = 1,NLOG3
     V(JJ) = VS(JJ) - VS(JJ+1)
     450 continue

     HMERCH = MMH(1)

  end if

  900 return
end

  !***  PROFILE EQUATION  (USED FOR DIAMETER CALCULATIONS)  ***

function DD2MI(RH,R32,DBHOB,H)
  ! Variables
  real    :: RH,DBHOB,H,DD2MI,R32

  DD2MI = RH**1.5 +&
       &((-0.0052554*H + 0.000034947*H**2 + (0.104477*H)/DBHOB) *&
       &(RH**1.5 - RH**3)) +&
       &((7.76807/DBHOB**2 - 0.0000094852*H**2 - (0.011351*H)/DBHOB) *&
       &(RH**1.5 - R32**32))

  !     XA = R**1.5
  !     XB = R**3
  !     XC = R**32
  !     XD = DBHOB**2
  !     XE = H**2
  !     XF = -0.0052554 * H
  !     XG = 0.000034947 * XE
  !     XH = (0.104477 * H) / DBHOB
  !     XI = XA - XB
  !     XJ = 7.76807 / XD
  !     XK = 0.0000094852 * XE
  !     XL = (0.011351 * H) / DBHOB
  !     XM = XA - XC
  !     XN = XF + XG + XH
  !     XO = XJ - XK - XL
  !     XP = XN * XI
  !     XQ = XO * XM

  !     WRITE (*,*) '   FUNCTION DD2MI'
  !     WRITE (*,*) '      R = ',R,'  DBHOB = ',DBHOB,'  H = ',H
  !     WRITE (*,*) '      R**1.5 =                                 ',XA
  !     WRITE (*,*) '      R**3 =                                   ',XB
  !     WRITE (*,*) '      R**32 =                                  ',XC
  !     WRITE (*,*) '      DBHOB**2 =                                   ',XD
  !     WRITE (*,*) '      H**2 =                                   ',XE
  !     WRITE (*,*) '      -0.0052554 * H =                         ',XF
  !     WRITE (*,*) '      0.000034947 * H**2 =                     ',XG
  !     WRITE (*,*) '      (0.104477 * H) / DBHOB =                     ',XH
  !     WRITE (*,*) '      R**1.5 - R**3 =                          ',XI
  !     WRITE (*,*) '      7.76807 / DBHOB**2 =                         ',XJ
  !     WRITE (*,*) '      0.0000094852 * H**2 =                    ',XK
  !     WRITE (*,*) '      (0.011351 * H) / DBHOB =                     ',XL
  !     WRITE (*,*) '      R**1.5 - R**32 =                         ',XM
  !     WRITE (*,*) '      (-0.0052554 * H) + (0.000034947 * H**2) +'
  !     WRITE (*,*) '         ((0.104477 * H) / DBHOB) =                ',XN
  !     WRITE (*,*) '      (7.76807 / DBHOB**2) - (0.0000094852 * H**2) -'
  !     WRITE (*,*) '         ((0.011351 * H) / DBHOB) =                ',XO
  !     WRITE (*,*) '      ((-0.0052554 * H) + (0.000034947 * H**2) +'
  !     WRITE (*,*) '         ((0.104477 * H) / DBHOB)) *'
  !     WRITE (*,*) '         (R**1.5 - R**3) =                     ',XP
  !     WRITE (*,*) '      ((7.76807 / DBHOB**2) - (0.0000094852 * H**2) -'
  !     WRITE (*,*) '         ((0.011351 * H) / DBHOB)) * '
  !     WRITE (*,*) '         (R**1.5 - R**32) =                    ',XQ
  !     WRITE (*,*) '   END FUNCTION DD2MI'

  return
end

  !***  INTEGRAL OF PROFILE EQUATION (USED IN VOLUME CALCULATION) ***

function FF(RH,R32,DBHOB,H)
  ! Variables
  real    :: RH,DBHOB,H,FF,R32
  FF = 0.4 * RH**2.5 + (-0.0052554 * H + 0.000034947 * H**2&
       &+ 0.104477 * H/DBHOB) * (0.4 * RH**2.5 - 0.25 * RH**4.) +&
       &(7.76807/DBHOB**2 - 0.0000094852*H**2 - 0.011351*H/DBHOB)*&
       &(0.4 * RH**2.5 - (1./33.) * R32**33)
  return
end

  !***  BARK THICKNESS EQUATION  ***

function BB(DBHOB,H)
  ! Variables
  real    :: DBHOB,H,BB
  BB = (0.8467 + 0.0009144 * DBHOB + 0.0003568 * H)
  return
end
