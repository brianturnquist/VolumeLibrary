!=============================!
!   VolumeLibrary : r1allen   !
!=============================!
  !== last modified  12-12-2002
  ! YW 04/13/2017 Added 260, 240 and 090 to be valid species code for R1ALLENB
  !               This is to make species code match with R1ALLENC.
subroutine R1ALLENC(VOLEQ,DBHOB,HTTOT,TOPD,BTR,CUVOL,TCVOL,CFSTMP,&
     &ERRFLAG)

  !  **CFVOL--BS  DATE OF LAST REVISION:  04/20/95

  !  DIMENSION STATEMENT FOR INTERNAL ARRAYS.

  ! Variables
  character(len=10) :: VOLEQ
  real    :: BRATIO(11),BTR,D2H,BARK,VMAX,DBHMIN,HTRUNC,PHT,CFSTMP
  real    :: DBHOB,HTTOT,CUVOL,TCVOL,TOTVOL,BHAT,AHAT,VOLT,BHRE,VOLM
  real    :: DTRUNC,VOLTK,STUMP,TOPD,HTMRCH,S3,DMRCH
  integer :: ISPC,ITHT,ERRFLAG

  logical :: TKILL,LCONE

  !---- BARK RATIO LOOK-UP TABLE

  data BRATIO /0.964, 0.851, 0.867, 0.915, 0.934, 0.950,&
       &0.969, 0.956, 0.937, 0.890, 0.934/

  !  MAIN LOGIC

  TKILL = .false.
  ITHT = 0
  ERRFLAG = 0

  if (VOLEQ(8:10).eq.'119') then
     ISPC = 1
  else if (VOLEQ(8:10).eq.'073') then
     ISPC = 2
  else if (VOLEQ(8:10).eq.'202') then
     ISPC = 3
  else if (VOLEQ(8:10).eq.'017') then
     ISPC = 4
  else if (VOLEQ(8:10).eq.'263'.or.VOLEQ(8:10).eq.'260') then
     ISPC = 5
  else if (VOLEQ(8:10).eq.'242'.or.VOLEQ(8:10).eq.'240') then
     ISPC = 6
  else if (VOLEQ(8:10).eq.'108') then
     ISPC = 7
  else if (VOLEQ(8:10).eq.'093'.or.VOLEQ(8:10).eq.'090') then
     ISPC = 8
  else if (VOLEQ(8:10).eq.'019') then
     ISPC = 9
  else if (VOLEQ(8:10).eq.'122') then
     ISPC = 10
  else if (VOLEQ(8:10).eq.'999') then
     ISPC = 11
  else if (VOLEQ(8:10).eq.'375') then
     ISPC = 12
  else if (VOLEQ(8:10).eq.'740') then
     ISPC = 13
  else
     ERRFLAG = 1
     CUVOL = 0.0
     TCVOL = 0.0
     return
  end if

  D2H = DBHOB*DBHOB*HTTOT
  ! PAPER BIRCH (NORTH CENTRAL STATION EQUATION 7/2001)
  if (ISPC.eq.12) then
     if (DBHOB.lt.5.0) then
        CUVOL = 0.0
     else if (DBHOB.ge.5.0.and. DBHOB.lt.11.0) then
        CUVOL = 0.988264+0.002732*D2H
     else if (DBHOB.ge.11.0) then
        CUVOL = 2.512836+0.002446*D2H
     end if
     TCVOL = CUVOL
     ! COTTONWOOD EDMINSTER RESEARCH NOTE RM-351  7/2001)
  else if (ISPC.eq.13) then
     CUVOL = 0.00142526*(D2H**1.0636)
     TCVOL = CUVOL
     ! ALL OTHER SPECIES
  else

     DBHMIN = 6.0
     if (btr.gt.0.0) then
        bark = btr
     else
        BARK = BRATIO(ISPC)
     end if

     VMAX = TOTVOL(ISPC,DBHOB,HTTOT,D2H)

     !  IF TOP IS DEAD OR MISSING AND VOLUME IS GREATER THAN ZERO,
     !  ADJUST VOLUME ESTIMATE FOR TOPKILL; OTHERWISE RETURN.

     if (VMAX.lt.0.0) then
        CUVOL = 0.0
        TCVOL = 0.0
        return
     else

        !  INITIALIZE TOTAL CUBIC FOOT VOLUME ESTIMATE TO VMAX.

        TCVOL = VMAX

        !  THE BEHRE HYPERBOLA TAPER MODEL IS USED TO COMPUTE VOLUME TO POINT
        !  OF TOPKILL OR TO A MINIMUM TOP DIAMETER.  BEHRE HYPERBOLA PARAMETERS
        !  ARE COMPUTED HERE.  BHAT IS INITIALIZED WITH VALUE OF CYLINDRICAL
        !  FORM FACTOR.

        BHAT = VMAX / (.00545415*DBHOB*DBHOB*BARK*BARK*HTTOT)
        if (BHAT.gt.0.95) BHAT = 0.95

        !        AHAT = AN ESTIMATE OF THE BEHRE TAPER CURVE PARAMETER 'A'

        AHAT = 0.44277 - 0.99167/BHAT - 1.43237*ALOG(BHAT) +&
             &1.68581*SQRT(BHAT) - 0.13611*BHAT*BHAT
        LCONE = .false.
        if (ABS(AHAT) .lt.0.05) then
           LCONE = .true.
           if (AHAT.lt.0.) then
              AHAT = -0.05
           else
              AHAT = 0.05
           end if
        end if
        BHAT = 1.-AHAT
        if (BHAT .lt.0.0001) BHAT = 0.0001
        VOLT = bhre(0.0,1.0,AHAT,BHAT)

        !       COMPUTE TOTAL CUBIC VOLUME LOSS DUE TO TOPKILL.

        if (TKILL) then

           !         A TRUNCATED TREE HAS BEEN DETECTED.  HEIGHT AT POINT OF TRUNCATION
           !         (HTRUNC) IS COMPUTED FROM ITRUNC.  HEIGHT AND DIAMETER AT THE
           !         POINT OF TRUNCATION ARE THEN EXPRESSED AS RATIOS OF "NORMAL" HEIGHT
           !         AND DBH (PHT AND DTRUNC, RESPECTIVELY).

           HTRUNC = ITHT / 100.0
           PHT = 1. - (HTRUNC/HTTOT)
           DTRUNC = PHT / (AHAT*PHT + BHAT)

           !         CORRECT FOR THE TRUNCATED TOP IN THE VN ESTIMATE USING THE
           !         RATIO OF VOLUME BELOW TRUNCATION TO TOTAL VOLUME.

           if (.not.LCONE) then
              VOLTK = bhre(PHT,1.0,AHAT,BHAT)
              !              VN = TCVOL*VOLTK/VOLT

              !         PROCESS CONES.

           else
              !              VN = VMAX*(1.0-PHT**3)
           end if
        end if
     end if

     !  COMPUTE MERCHANTABLE CUBIC VOLUME USING TOP DIAMETER, MINIMUM
     !  DBH, AND STUMP HEIGHT SPECIFIED BY THE USER.  CORRECT FOR TOPKILL.

     STUMP = 1.-CFSTMP/HTTOT

     if (DBHOB.lt.DBHMIN .or. DBHOB.lt.TOPD) then
        CUVOL = 0.0
     else
        DMRCH = TOPD/DBHOB
        HTMRCH = ((BHAT*DMRCH)/(1.0-(AHAT*DMRCH)))
        if (.not.LCONE) then
           VOLM = bhre(HTMRCH,STUMP,AHAT,BHAT)
           CUVOL = VMAX*VOLM/VOLT
           !           WRITE(7,*)VOLM,VMAX,VOLT,HTMRCH,CUVOL,AHAT,BHAT
           if (TKILL.and.DTRUNC.gt.DMRCH) then

              !           CORRECT MERCHANTABLE VOLUME FOR TOP KILL.

              VOLTK = bhre(PHT,STUMP,AHAT,BHAT)
              !             VM = VMAX*VOLTK/VOLT
           end if
        else

           !         PROCESS CONES.

           S3 = STUMP**3
           VOLM = S3-HTMRCH**3
           CUVOL = VMAX*VOLM
           !           WRITE(*,*)'PROCESS CONES',CUVOL
           if (TKILL.and.DTRUNC.gt.DMRCH) then

              !           CORRECT MERCHANTABLE VOLUME FOR TOP KILL (CONES ONLY).

              VOLTK = S3-PHT**3
              CUVOL = CUVOL*VOLTK/VOLM
           end if
        end if
     end if
  end if
  return
end

subroutine R1ALLENB (VOLEQ,DBHOB,HTTOT,TOPD,BTR,BFVOL,BFSTMP,&
     &ERRFLAG)

  !  **BFVOL--BS   DATE OF LAST REVISION:   07/10/95

  !  ************** BOARD FOOT MERCHANTABILITY SPECIFICATIONS ********

  !  BFVOL CALCULATES BOARD FOOT VOLUME OF ANY TREE LARGER THAN A MINIMUM
  !  DBH SPECIFIED BY THE USER.  MINIMUM DBH CAN VARY BY SPECIES,
  !  BUT CANNOT BE LESS THAN 2 INCHES.  DEFAULTS ARE 6 IN. FOR LODGEPOLE
  !  PINE AND 7 IN. FOR ALL OTHER SPECIES.  MINIMUM MERCHANTABLE DBH IS
  !  SET WITH THE BFVOLUME KEYWORD.  FOR METHB = 1, 3, OR 4, MERCHANTABLE
  !  TOP DIAMETER CAN BE SET TO ANY VALUE BETWEEN 2 IN. AND MINIMUM DBH.
  !  MINIMUM DBH AND TOP DIAMETER ARE ASSUMED TO BE MEASURED OUTSIDE
  !  BARK--IF DIB IS DESIRED, ALLOW FOR DOUBLE BARK THICKNESS IN
  !  SPECIFICATIONS.

  !  VOLUME IS BE COMPUTED BY FORMULA.  THE VOLUME FORMULA IS BASED ON
  !  KEMP'S EQUATIONS, BUT ALL PARAMETERS CAN BE REPLACED BY THE USER WITH
  !  THE BFVOLEQ KEYWORD.  KEMP'S EQUATIONS IMPLY SPECIFIC MERCHANTABILITY
  !  STANDARDS (9" MINIMUM DBH, 8" MINIMUM TOP DIAMETER, AND 1' STUMP).
  !  WHEN METHB=1, EQUATIONS DEVELOPED BY  ALLEN, ADAMS, AND PRAUSA (1974:
  !  IDAHO FORESTRY, WILDLIFE, AND RANGE EXPERIMENT STATION NOTE # 21)
  !  ARE APPLIED TO COMPUTE VOLUMES FOR THE PORTION OF THE TREE BETWEEN
  !  AN 8" TOP DIAMETER AND THE USER SPECIFIED MERCHANTABILITY LIMIT. THE
  !  VALUE OF D2H FOR WHICH ALLEN'S AND KEMP'S EQUATIONS ARE EQUAL IS
  !  CALLED D2HBRK, AND VARIES BY SPECIES.  FOR TREES WITH DBH < D2HBRK,
  !  ALLEN'S EQUATIONS ARE USED; FOR LARGER TREES, KEMP'S EQUATIONS ARE
  !  USED.  THE TRANSITION IS SMOOTH AND TREES DO NOT LOSE VOLUME AS THEY
  !  INCREASE IN SIZE.  FOR METHB=2, ONLY THE KEMP EQUATIONS ARE USED,
  !  AND VOLUME ABOVE THE 8" TOP IS IGNORED.

  !  ASSUMED SPECIES CORRESPONDENCE IS AS FOLLOWS:

  !                   ACTUAL SPECIES     ALLEN'S EQUATIONS

  !         1     WESTERN WHITE PINE   LODGEPOLE PINE
  !         2     WESTERN LARCH        WESTERN LARCH
  !         3     DOUGLAS-FIR          DOUGLAS-FIR
  !         4     GRAND FIR            GRAND FIR
  !         5     WESTERN HEMLOCK      GRAND FIR
  !         6     WESTERN REDCEDAR     WESTERN LARCH
  !         7     LODGEPOLE PINE       LODGEPOLE PINE
  !         8     ENGELMANN SPRUCE     GRAND FIR
  !         9     SUBALPINE FIR        GRAND FIR
  !        10     PONDEROSA PINE       DOUGLAS-FIR
  !        11     MOUNTAIN HEMLOCK     GRAND FIR

  !  VOLUME LOSS DUE TO TOP DAMAGE (TKILL=.TRUE.) IS ESTIMATED WITH A
  !  BEHRE HYPERBOLA TAPER MODEL, WITH PARAMETERS ESTIMATED FROM TOTAL
  !  CUBIC FOOT VOLUME, HEIGHT AND DIAMETER.

  !  COFBVS -- COEFFICIENT IN BOARD FOOT VOLUME EQUATION FOR TREES
  !            HAVING D2H LESS THAN D2HBRK(ISPC)
  !  D2HBRK -- THE VALUE OF D2H BELOW WHICH ALLEN'S EQUATIONS
  !            ARE USED TO COMPUTE BOARD FOOT VOLUME.
  !  HDRATM -- ESTIMATED HEIGHT/DIAMETER RATIO THAT IS INDEXED BY
  !            EITHER DBH OR TOP DIAMETER.
  !  HDRATA -- ACTUAL HEIGHT/DIAMETER RATIO FOR THE SUBJECT TREE.

  ! Variables
  character(len=10) :: VOLEQ
  integer :: ISPC,ITHT,ERRFLAG,ID,ITD,IVTD

  real    :: DBHOB,HTTOT,TOPD,BFVOL,btr,D2H,BARK,TSIZE,VT,VOLM
  real    :: COFBVS(11),HDRATM(100),D2HBRK(11),BRATIO(11),STUMP,BFSTMP
  real    :: BFVEQS(7,11),BFVEQL(7,11),IBTRAN(11),BTRAN(11),BHRE,HTMRCH
  real    :: HDRATA,DTOPK,VMAX,TOTVOL,BHAT,AHAT,HTRUNC,PHT,DTRUNC,VOLTK

  logical :: LCONE,TKILL

  data D2HBRK/&
       &16917.1, 19231.8, 15763.5, 10016.7, 14883.9, 29922.0,&
       &4567.8,  6024.9, 19425.9, 14034.0, 14883.9/

  data HDRATM/&
       &10.0, 10.0, 9.0, 8.0, 7.8, 7.65, 7.5, 7.25, 7.0, 6.75,&
       &6.5, 6.25, 18*6.0, 10*5.5, 10*5.0, 10*4.5, 10*4.0,&
       &10*3.5, 10*3.0, 10*2.5/

  data COFBVS/&
       &0.01031,0.008423,0.008423,0.009523,0.009523,0.008421,&
       &0.01031,0.009523,0.009523,0.008423,0.009523/

  !  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE SMALLER THAN
  !  THE TRANSITION SIZE

  data BFVEQS/&
       &-26.729,      0.0,     0.0, 0.01189,     0.0,     0.0,    0.0,&
       &-29.790,      0.0,     0.0, 0.00997,     0.0,     0.0,    0.0,&
       &-25.332,      0.0,     0.0, 0.01003,     0.0,     0.0,    0.0,&
       &-34.127,      0.0,     0.0, 0.01293,     0.0,     0.0,    0.0,&
       &-37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,&
       &-10.742,      0.0,     0.0, 0.00878,     0.0,     0.0,    0.0,&
       &-8.085,      0.0,     0.0, 0.01208,     0.0,     0.0,    0.0,&
       &-11.851,      0.0,     0.0, 0.01149,     0.0,     0.0,    0.0,&
       &-11.403,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,&
       &-50.340,      0.0,     0.0, 0.01201,     0.0,     0.0,    0.0,&
       &-37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0/

  !  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE LARGER THAN
  !  THE TRANSITION SIZE

  data BFVEQL/&
       &-32.516,      0.0,     0.0, 0.01181,     0.0,     0.0,    0.0,&
       &85.150,      0.0,     0.0, 0.00841,     0.0,     0.0,    0.0,&
       &-9.522,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,&
       &10.603,      0.0,     0.0, 0.01218,     0.0,     0.0,    0.0,&
       &-50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,&
       &-4.064,      0.0,     0.0, 0.00799,     0.0,     0.0,    0.0,&
       &14.111,      0.0,     0.0, 0.01103,     0.0,     0.0,    0.0,&
       &1.620,      0.0,     0.0, 0.01158,     0.0,     0.0,    0.0,&
       &124.425,      0.0,     0.0, 0.00694,     0.0,     0.0,    0.0,&
       &-298.784,      0.0,     0.0, 0.01595,     0.0,     0.0,    0.0,&
       &-50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0/

  !  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=DBHOB, 1=D2H

  data IBTRAN/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/

  !  TRANSITION SIZE.  TREES OF LARGER SIZE (DBHOB OR D2H) WILL USE COEFFICIENTS
  !  FOR LARGER SIZE TREES.

  data BTRAN/&
       &20.5,     20.5,    20.5,    20.5,    20.5,    20.5,   20.5,&
       &20.5,     20.5,    20.5,    20.5/

  !---- BARK RATIO LOOK-UP TABLE

  data BRATIO /0.964, 0.851, 0.867, 0.915, 0.934, 0.950,&
       &0.969, 0.956, 0.937, 0.890, 0.934/

  ! Initialize the volume estimate
  ITHT = 0
  TKILL = .false.
  BFVOL = 0.0
  ERRFLAG = 0

  if (VOLEQ(8:10).eq.'119') then
     ISPC = 1
  else if (VOLEQ(8:10).eq.'073') then
     ISPC = 2
  else if (VOLEQ(8:10).eq.'202') then
     ISPC = 3
  else if (VOLEQ(8:10).eq.'017') then
     ISPC = 4
  else if (VOLEQ(8:10).eq.'263'.or.VOLEQ(8:10).eq.'260') then
     ISPC = 5
  else if (VOLEQ(8:10).eq.'242'.or.VOLEQ(8:10).eq.'240') then
     ISPC = 6
  else if (VOLEQ(8:10).eq.'108') then
     ISPC = 7
  else if (VOLEQ(8:10).eq.'093'.or.VOLEQ(8:10).eq.'090') then
     ISPC = 8
  else if (VOLEQ(8:10).eq.'019') then
     ISPC = 9
  else if (VOLEQ(8:10).eq.'122') then
     ISPC = 10
  else if (VOLEQ(8:10).eq.'999') then
     ISPC = 11
  else if (VOLEQ(8:10).eq.'375') then
     BFVOL = 0.0
     return
  else if (VOLEQ(8:10).eq.'740') then
     BFVOL = 0.0
     return
  else
     ERRFLAG = 1
     BFVOL = 0.0
     return
  end if

  D2H = DBHOB*DBHOB*HTTOT
  if (BTR.gt.0.0) then
     BARK = BTR
  else
     BARK = BRATIO(ISPC)
  end if

  !  ASSIGN TRANSITION SIZE.

  TSIZE = DBHOB
  if (IBTRAN(ISPC).gt.0) TSIZE = D2H

  !  SET TOP VOLUME (VT) TO 0.  BYPASS ALLEN EQUATIONS FOR TOP = 8.

  VT = 0.0
  if (TOPD.eq.8.0) go to 15
  HDRATA = HTTOT/DBHOB
  ID = DBHOB - 0.5

  if (ID.gt.100) ID = 100
  if (TOPD.lt.1.0) TOPD = 6.0 !ADDED TO AVOID ITD = 0
  ITD = TOPD - 0.5
  if (ITD.gt.100) ITD = 100

  if (D2H.gt.D2HBRK(ISPC)) go to 10

  !  D2H LESS THAN D2H BREAKPOINT: COMPUTE TOP AND STEM VOLUME WITH
  !  ALLEN-ADAMS-PRAUSA EQUATIONS.

  VT = -COFBVS(ISPC)*(TOPD**3*HDRATM(ITD)-80.0) *&
       &SQRT(HDRATA/HDRATM(ID)) - (TOPD**2-4.0)*0.12153

  BFVOL = COFBVS(ISPC)*D2H + VT
  go to 30

  !  D2H GREATER THAN D2H BREAKPOINT:  COMPUTE TOP VOLUME WITH ALLEN-
  !  ADAMS-PRAUSA EQUATIONS AND STEM VOLUME WITH KEMP EQUATIONS.

  10 DTOPK = 0.4*DBHOB
  if (DTOPK.lt.4.0) DTOPK = 4.0
  if (DTOPK.gt.8.0) DTOPK = 8.0
  IVTD = DTOPK - 0.5
  VT = COFBVS(ISPC) * SQRT(HDRATA/HDRATM(ID)) *&
       &(DTOPK**3*HDRATM(IVTD) - TOPD**3*HDRATM(ITD)) -&
       &(DTOPK**2-TOPD**2)*0.12153

  15 continue
  if (TSIZE.ge.BTRAN(ISPC)) go to 20

  BFVOL = VT + BFVEQS(1,ISPC) + BFVEQS(2,ISPC)*DBHOB +&
       &BFVEQS(3,ISPC)*DBHOB*HTTOT + BFVEQS(4,ISPC)*D2H +&
       &BFVEQS(5,ISPC)*DBHOB**BFVEQS(6,ISPC)*HTTOT**BFVEQS(7,ISPC)
  go to 30
  20 continue
  BFVOL = VT + BFVEQL(1,ISPC) + BFVEQL(2,ISPC)*DBHOB +&
       &BFVEQL(3,ISPC)*DBHOB*HTTOT + BFVEQL(4,ISPC)*D2H +&
       &BFVEQL(5,ISPC)*DBHOB**BFVEQL(6,ISPC)*HTTOT**BFVEQL(7,ISPC)

  !  THE FOLLOWING PIECE OF FOOLISHNESS (J.E.B.) IS REQUIRED
  !  BY NATIONAL FOREST SCALING AND CRUISING RULES.  BE CAREFUL IF
  !  YOU BUY A CORRAL POLE SALE BASED ON A SCRIBNER CRUISE VOLUME.

  30 continue
  if (BFVOL.lt.10.0) BFVOL = 10.0

  !  BRANCH TO STMT 600 TO COMPUTE VOLUME LOSS DUE TO TOPKILL.

  !  THE BEHRE HYPERBOLA TAPER MODEL IS USED TO COMPUTE VOLUME TO POINT
  !  OF TOPKILL.  BEHRE HYPERBOLA PARAMETERS ARE COMPUTED HERE.  BHAT IS
  !  INITIALIZED WITH VALUE OF CYLINDRICAL FORM FACTOR BASED ON TOTAL
  !  CUBIC FOOT VOLUME (PASSED IN FROM VOLS).

  if (TKILL) then
     VMAX = TOTVOL(ISPC,DBHOB,HTTOT,D2H)
     BHAT = VMAX / (.00545415*DBHOB*DBHOB*BARK*BARK*HTTOT)
     if (BHAT.gt.0.95) BHAT = 0.95

     !        AHAT = AN ESTIMATE OF THE BEHRE TAPER CURVE PARAMETER 'A'

     AHAT = .44277 - .99167/BHAT - 1.43237*ALOG(BHAT)&
          &+ 1.68581*SQRT(BHAT) - .13611*BHAT*BHAT
     LCONE = .false.
     if (ABS(AHAT) .lt.0.05) then
        LCONE = .true.
        if (AHAT.lt.0.) then
           AHAT = -0.05
        else
           AHAT = 0.05
        end if
     end if
     BHAT = 1.-AHAT
     if (BHAT .lt.0.0001) BHAT = 0.0001

     !       COMPUTE TOTAL BOARD FOOT VOLUME LOSS DUE TO TOPKILL.
     !       HEIGHT AND DIAMETER AT THE POINT OF TRUNCATION ARE EXPRESSED
     !       AS RATIOS OF "NORMAL" HEIGHT AND DBH (PHT AND DTRUNC,
     !       RESPECTIVELY).

     HTRUNC = ITHT / 100.0
     PHT = 1. - (HTRUNC/HTTOT)
     DTRUNC = PHT / (AHAT*PHT + BHAT)

     !       IF THE TOP IS DAMAGED AND THE DIAMETER AT THE POINT OF
     !       TRUNCATION IS GREATER THAN THE MERCHANTABLE TOP DIAMETER,
     !       CORRECT BOARD FOOT VOLUME ESTIMATE.

     if (DTRUNC .gt. TOPD/DBHOB) then
        VOLTK = bhre(PHT,1.0,AHAT,BHAT)

        !         HTMRCH = RATIO OF MERCH. HT (HEIGHT WHERE DBHOB=TOPD) TO TOTAL
        !         HEIGHT.

        HTMRCH = ((BHAT*TOPD)/DBHOB)/(1. - (AHAT*TOPD/DBHOB))

        !         ADJUST THE STUMP HEIGHT.

        STUMP = 1.0-BFSTMP/HTTOT

        !         CORRECT FOR THE TRUNCATED TOP IN THE BFV ESTIMATE

        if (LCONE) then
           VOLM = STUMP**3-HTMRCH**3.
           VOLTK = STUMP**3. - PHT**3.
           BFVOL = BFVOL*VOLTK/VOLM
        else
           BFVOL = (BFVOL*VOLTK) / bhre(HTMRCH,STUMP,AHAT,BHAT)
        end if
     end if
  end if
  return
end

function bhre(L1,L2,AHAT,BHAT)

  !  **BEHRE  DATE OF LAST REVISION:  01/16/91

  !  THIS FUNCTION CALCULATES THE VOLUME OF A SOLID OF REVOLUTION
  !  DESCRIBED BY A BEHRE TAPER CURVE USING PARAMETERS 'AHAT' AND 'BHAT'.
  !  THE LIMITS OF INTEGRATION ARE 'L1' AND 'L2'.

  !  IT SHOULD BE NOTED THAT THE VOLUME CALCULATED IS OFF BY A FACTOR
  !  OF PI/(A CUBED) - THIS VALUE WOULD CANCEL OUT IN THE RATIOS OF
  !  BEHRE VOLUMES USED IN CALLING ROUTINE 'VOLS'.

  ! Variables
  real    :: L1,L2,BHRE,ALB1,ALB2,BHAT,AHAT

  ALB1 = AHAT*L1 + BHAT
  ALB2 = AHAT*L2 + BHAT
  bhre = ALB2 - ALB1 - 2.0*BHAT*(ALOG(ALB2) - ALOG(ALB1))&
       &- BHAT*BHAT/ALB2 + BHAT*BHAT/ALB1
  return
end

function TOTVOL(ISPC,DBHOB,HTTOT,D2H)

  ! Variables
  real    :: CFVEQS(7,11),CFVEQL(7,11),TOTVOL
  real    :: ICTRAN(11),CTRAN(11)
  real    :: DBHOB,HTTOT,TSIZE,D2H,TERM1
  integer :: ISPC

  !  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE SMALLER THAN
  !  THE TRANSITION SIZE

  data CFVEQS/&
       &0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,&
       &0.030288,      0.0,     0.0,0.002213,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0/

  !  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE LARGER THAN
  !  THE TRANSITION SIZE

  data CFVEQL/&
       &0.0,      0.0,     0.0, 0.00233,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0, 0.00184,     0.0,     0.0,    0.0,&
       &0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0, 0.00234,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0, 0.00205,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0,     0.0,0.002782,  1.9041, 1.0488,&
       &0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,&
       &0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,&
       &-1.557103,      0.0,     0.0,0.002474,     0.0,     0.0,    0.0,&
       &0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0/

  !  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=DBHOB, 1=D2H

  data ICTRAN/0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0/

  !  TRANSITION SIZE.

  data CTRAN/&
       &0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,&
       &0.0,      0.0,  6000.0,     0.0/

  !---- MAIN LOGIC

  TSIZE = DBHOB

  if (ICTRAN(ISPC).gt.0) TSIZE = DBHOB*DBHOB*HTTOT

  if (TSIZE.lt.CTRAN(ISPC)) then
     if (CFVEQS(5,ISPC) .gt.0) then
        TERM1 = CFVEQS(5,ISPC)*DBHOB**CFVEQS(6,ISPC)*&
             &HTTOT**CFVEQS(7,ISPC)
     else
        TERM1 = 0
     end if
     TOTVOL = CFVEQS(1,ISPC) + CFVEQS(2,ISPC)*DBHOB +&
          &CFVEQS(3,ISPC)*DBHOB*HTTOT + CFVEQS(4,ISPC)*D2H + TERM1
  else
     if (CFVEQL(5,ISPC) .gt.0) then
        TERM1 = CFVEQL(5,ISPC)*DBHOB**CFVEQL(6,ISPC)*&
             &HTTOT**CFVEQL(7,ISPC)
     else
        TERM1 = 0
     end if
     TOTVOL = CFVEQL(1,ISPC) + CFVEQL(2,ISPC)*DBHOB +&
          &CFVEQL(3,ISPC)*DBHOB*HTTOT + CFVEQL(4,ISPC)*D2H + TERM1
  end if

  return
end
