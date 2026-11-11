!============================!
!   VolumeLibrary : blmtap   !
!============================!
  !== last modified  1-6-2014
  ! 1/6/2014 YW corrected coefficient for hemlock(48) B3=0.00000546
  ! 3/26/2024 YW Added initial value 0 to D2 in BLMTAP and BEHTAP
subroutine BLMTAP(DBHOB,HTTOT,TLH,HTUP,D17,TOP,XLEN,D2,Profile)
  !###########################################################
  use DEBUG_MOD

  ! Variables
  real    :: BLMTHT (4,10),BLMBA (6,9)
  real    :: HTUP,D2,TLH,H,SMALLH,C
  !     REAL HBUTT, HTDIB, HTUP
  real    :: D17,TOP,HBUTT,XLEN,HTDIB,RA,TOLERANCE,TOL_LIMIT
  real    :: DBHOB,HTTOT,DIBCOR,A,B,LN
  real    :: E
  parameter (E = 2.7182818284)
  integer :: Profile,ILIMIT,ICOUNT,profileht

  !          COFFICIENTS FOR BEHRE'S HYPERBOLA (B0,B1,B2,B3)
  !  01 zone 01
  data (( BLMTHT(I,J), I = 1,4), J = 1,10)&
       &/ 0.6448,  -0.00196,  0.0,       0.0,&
       !  01 zone 02
       &0.6096,  -0.00196,  0.0,       0.0,&
       !  10, 11
       &0.31385,  0.0,      0.002985, -0.00003386,&
       !  13
       &0.4779,   0.0,      0.0,       0.0,&
       !  14
       &0.5455,  -0.00196,  0.0,       0.0,&
       !  31 zone 1, 33
       &0.45648,  0.00289,  0.0,       0.0,&
       !  32, 34, 35
       &0.6014,   0.0,      0.0,       0.0,&
       !  48
       &0.54568,  0.0,      0.0,       0.00000546,&
       !  51, 54, 55
       &0.4606,   0.0,      0.0,       0.0,&
       !  ALL OTHER SPECIES
       &0.6200,   0.0,      0.0,       0.0/

  !          COFFICIENTS FOR BEHRE'S HYPERBOLA (B0,B1,B2,B3,B4,B5)
  !  DOUGLAS FIR, COAST
  data (( BLMBA(I,J), I = 1,6), J = 1,9)&
       &/ -0.5366,  1.0,     0.07124, 1.0,  0.07816,  0.0,&
       !  DOUGLAS FIR, CASCADE
       &-0.8015,  1.0,     0.14317, 1.0,  0.12271,  0.0,&
       !  DOUGLAS FIR, SOUTHWEST,EAST,CALIFORNIA & W LARCH
       &-0.9611,  1.0,     0.16403, 1.0,  0.11527,  0.0,&
       !  PONDEROSA, SUGAR, JEFFREY & W WHITE PINE
       &-1.0170,  1.0,     0.16343, 1.0,  0.10446,  0.0,&
       !  GRAND, WHITE FIR, SPRUCES EAST & WEST SIDE
       &0.5869, -5.9782, -1.0,     1.0,  0.0,      0.0,&
       !  SHASTA, SILVER, & NOBLE FIR
       &0.0,     1.0,     0.0,     1.0,  0.1036,  -3.0482,&
       !  HEMLOCK
       &-0.1761,  1.0,     0.0,     1.0,  0.12568,  0.0,&
       !  CEDARS
       &0.5919, -0.00484, 0.0,     1.0, -1.0,      0.0,&
       !  ALL OTHER SPECIES
       &0.6200,  0.0,     1.0,     1.0,  1.0,      0.0    /

  !     THIS IS THE START OF THE MAIN LOGIC

  if (DEBUG%MODEL) then
     write  (LUDBG, '(A)') ' -->Enter BLMTAP'
  end if
  D2 = 0.0

  !   DETERMINE IF HEIGHT IS IN LOGS OR FEET
  if (TLH.eq.0.0) then

     !--     FOR TOTAL HEIGHT IN FEET FROM STUMP TO TIP *********************

     !-- BEHRE'S HYPERBOLA PORTION TO Calculate DIB *************************

     HBUTT = HTTOT - (XLEN + 1.5)
     if (HBUTT.le.0) return
     HTDIB = HTTOT - HTUP

     A = BLMTHT(1,Profile) +&
          &BLMTHT(2,Profile)*DBHOB +&
          &BLMTHT(3,Profile)*HTTOT +&
          &BLMTHT(4,Profile)*DBHOB*HTTOT

     B = 1.0 - A

     DIBCOR = D17 * ( (HTDIB/HBUTT) / (A*(HTDIB/HBUTT)+(B)) )
     D2 = DIBCOR

     !     Return a Zero Value for the Diameter-Inside-Bark IFF height is
     !     given in feet.  RJM, OSO 931.5, 20 Sep 93, in consultation with
     !     Jim Alegria, OSO 930.

     !         D2 = 0.0
  else

     !--      FOR TOTAL HEIGHT IN NUMBER OF LOGS PER TREE ******************

     !--  BEHRE'S HYPERBOLA PORTION TO CALCULATE THE "A" COEFFICIENT *******
     !--  using an iterative method devised by Jim Alegria, September, 1993.
     !--  Following checks per J. Alegria, 7 Dec 93.  RJM, 931.5.
     if (D17.lt.5.0)  D17 = 5.0
     if (D17.lt. TOP)  D17 = TOP

     if (TLH .eq.1.0 ) then
        D2 = D17
        return
     else if (TLH.eq.2.0) then
        if (HTUP.eq.1.0) then
           D2 = D17
        else
           D2 = TOP
        end if
        return
     end if

     if (TOP .eq. D17) then
        D2 = D17
        return
     end if

     RA = 0.0
     Tolerance = 1.0
     Tol_Limit = 0.01

     !        Estimate the coefficient as 0.62, then perform a count limited
     !        iterative procedure using the given DIB @ 17 feet, <D17>, the
     !        Tree Height in LOGS <TLH>, the DBHOB, <D>, and the Top Diameter <TOP>:
     A = 0.62
     B = 1 - A
     C = (TLH -  1.0) * XLEN
     H = C / (1.0 - TOP * B / (D17 - A * TOP) )
     HtTot = H   + 17.8

     iLimit = 20
     iCount = 0

     !         Write (LUDBG,1) Profile, iCount,  TLH, DBHOB, D17, TOP,
     !     & C, H, HtTot, A, RA
     !1      Format  ( ' BLMTap:', 2I3,  ';',     4F6.2,     ';', 5F7.2)

     do while (Tolerance .gt. Tol_Limit .and. iCount .lt. iLimit)

        iCount = iCount + 1
        A = BLMTHT(1,Profile) +&
             &BLMTHT(2,Profile)*DBHOB +&
             &BLMTHT(3,Profile)*HTTOT +&
             &BLMTHT(4,Profile)*DBHOB*HTTOT
        !           A         = BLMBA(1,Profile) +
        !     >                (BLMBA(2,Profile) * HtTot**BLMBA(3,Profile)) /
        !     >                (BLMBA(4,Profile) *   DBHOB**BLMBA(5,Profile)) +
        !     >                 BLMBA(6,Profile) /   DBHOB
        B = 1.0 - A

        H = C / (1.0 - TOP * B / (D17 - A * TOP) )
        HtTot = H + XLEN + 1.5

        Tolerance = Abs ( RA - A )

        !           Write (LUDBG,2) Profile, iCount,  TLH, DBHOB, D17, TOP,
        !     & C, H, HtTot, A, RA
        !2      Format  ( ' BLMTap:', 2I3,  ';',     4F6.2,     ';', 5F7.2)

        RA = A

     end do

     !-- The "A" Co-efficient has now been corrctly computed.

     !-- BEHRE'S HYPERBOLA PORTION TO Calculate DIB ************************

     SMALLH = H - (HTUP - 1.0) * XLEN
     LN = SMALLH / H
     DIBCOR = D17 * LN / (A * LN + B)

     D2 = DIBCOR

     if (DEBUG%MODEL) then
        write  (LUDBG, 300)'  A      B     C    H    HTTOT'&
             &//'   SMALLH  LN  DIBCOR  TOP  PROFILE  B0     B1   B2'&
             &//'   B3'
        300 format (A)
        write  (LUDBG, 320)A, B, C, H, HTTOT, SMALLH, LN, DIBCOR&
             &,TOP, PROFILE, BLMTHT(1,PROFILE), BLMTHT(2,PROFILE),&
             &BLMTHT(3,PROFILE), BLMTHT(4,PROFILE)
        320 format(5F6.1, 3X, 5F6.1, 2X,4F6.3)
     end if
  end if

  !    THIS IS THE END OF THE MAIN LOGIC

  if (DEBUG%MODEL) then
     write  (LUDBG, '(A)') ' <--Exit BLMTAP'
  end if

  return
end
  !      Input Variables:
  !      D       {Real   } The "DBHOB" in inches.  Copied to <DBHOB>.
  !      TTH     {Real   } Tree Height in FEET.  NOT currently used.
  !      TLH     {Real   } If <0.0> Then Height Variable Unit is Feet,
  !                                 Else Height Variable Unit is Logs.
  !      TLH     {Real   } Tree Height in LOGs (iff not zero).  Fixed for a tree.
  !      HT2     {Real   } Subject log number.  Set in <BLMVol> before invocation.
  !      D17     {Real   } DIB at 17.8 feet OR (Form_Class * DBHOB / 100)
  !      Profile {Integer} The Stem Model Number, Derived from the Species Code.

  !      Output Variables:
  !      TOP     {Real   } Passed from "6A" record but set to 5.0" if < 5.0.
  !      D2      {Real   } The computed diameter-inside-bark @ 17 feet
  !                        if <TLH> is NOT equal to 1.00.

  !      Modifications:
  !      20 Sep 93; R. Miller, OSO 931.5
  !      ====> Replace the method used to compute the "A" co-efficient.

  !      BLM STEM PROFILE MODEL USES BEHRE'S HYPERBOLA.

  !      D      - SINGLE PRECISION VALUE FOR DBHOB
  !      D2     - SINGLE PRECISION VALUE FOR DIBCOR
  !      DBHOB    - DOUBLE PRECISION VALUE FOR DBHOB
  !      D17    - DIB AT 17.8 FEET OR (FORM CLASS * DBHOB / 100)
  !      DIBCOR - DOUBLE PRECISION COMPUTED DIAMETER SECOND STAGE
  !      DIBUP  - DOUBLE PRECISION COMBUTED DIAMETER FIRST STAGE MODEL
  !      HT1    - SINGLE PRECISION FOR TOTAL TREE HT FROM GROUND TO TIP
  !      HT2    - SINGLE PRECISION VALUE FOR UPPER STEM HT FROM GROUND
  !      HTTOT  - DOUBLE PRECISION VALUE FOR HT1
  !      HTUP   - DOUBLE PRECISION VALUE FOR HT2
  !      BLMBA  - BEHRE'S HYPERBOLA "B" COFFICIENTS FOR THE "A" COEFFICIENT
  !      Profile- SUBSCRIPT TO USE FOR SPECIES COEFFICIENTS in Array <BLMBA>

  !      SPECIES SUBSCRIPTS ARE AS FOLLOWS
  !      1 = DOUGLAS-FIR COAST
  !      2 = DOUGLAS-FIR CASCADE
  !      3 = DOUGLAS-FIR SOUTHWEST      & Eastern Oregon
  !      4 = DOUGLAS-FIR EAST SIDE      NOT USED, 20 Sep 93, RJM
  !      5 = REDWOOD
  !      6 = DOUGLAS-FIR CALIFORNIA     NOT USED, 20 Sep 93, RJM
  !     10 = PONDEROSA PINE (BLACKJACK) NOT USED, 20 Sep 93, RJM
  !     11 = PONDEROSA PINE (YELLOW)
  !     12 = JEFFREY PINE
  !     13 = SUGAR PINE
  !     14 = WESTERN WHITE PINE
  !     15 = LODGEPOLE PINE
  !     20 = PACIFIC YEW
  !     21 = TAN OAK
  !     22 = RED ALDER
  !     23 = OREGON MYRTLE
  !     24 = BIG LEAF MAPLE
  !     25 = PACIFIC MADRONE
  !     26 = GOLDERN CHINQUAPIN
  !     27 = OREGON ASH
  !     28 = BLACK COTTONWOOD
  !     29 = OAK SPECIES
  !     30 = WHITE FIR EAST SIDE        NOT USED, 20 Sep 93, RJM
  !     31 = WHITE FIR WEST SIDE
  !     32 = RED SHASTA FIR
  !     33 = GRAND FIR
  !     34 = PACIFIC SILVER FIR
  !     35 = NOBLE FIR
  !     41 = ENGELMANN SPRUCE
  !     42 = SITKA SPRUCE
  !     48 = HEMLOCK
  !     51 = INCENSE CEDAR
  !     52 = ALASKA YELLOW CEDAR
  !     53 = PORT ORFORD CEDAR
  !     54 = WESTERN RED CEDAR
  !     55 = WESTERN LARCH
  !     56 = MISCELLANEOUS SPECIES

subroutine BEHTAP(VOLEQ,DBHOB,HTTOT,TLH,HTUP,FCLASS,TOP,D2)
  ! Variables
  character(len=10) :: VOLEQ
  real    :: DBHOB,HTTOT,TLH,HTUP,D17,TOP,XLEN,D2,DBHIB
  integer :: PROFILE,FCLASS,TAPEQU
  real    :: H1,HX,HR,DR,AT,BT,T,A

  XLEN = 16.3
  D2 = 0.0
  D17 = (DBHOB*FCLASS) / 100.0

  if (VOLEQ(1:1).eq.'B'.or.VOLEQ(1:1).eq.'b') then
     D17 = ANINT(D17)
     call BLMTAPEQ(VOLEQ,PROFILE,TAPEQU)
     call BLMTAP(DBHOB,HTTOT,TLH,HTUP,D17,TOP,XLEN,D2,Profile)
  else
     if (VOLEQ(1:3).eq.'632') then
        XLEN = 32.6
     else
        XLEN = 16.3
     end if
     A = 0.62
     if (HTTOT.gt.0.0) then
        H1 = HTTOT-XLEN-1.0
        if (H1.le.0) return
        HX = HTTOT-HTUP
        HR = HX/H1
        DR = HR/(0.62*HR+0.38)
        D2 = D17*DR
     else
        !Height in number of logs
        T = TOP/D17
        AT = A/(1.0-A*T)
        BT = (1.0/(1.0-T))-AT
        H1 = (TLH-1.0)*XLEN-1.0
        HX = TLH*XLEN-HTUP
        HR = HX/H1
        DR = T+(HR/(AT*HR+BT))
        D2 = D17*DR
     end if
  end if
  return
end
