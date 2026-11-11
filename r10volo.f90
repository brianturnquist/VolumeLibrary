!=============================!
!   VolumeLibrary : r10volo   !
!=============================!
  !== last modified  01-18-2013
  ! 01/18/2013 Added stump and tip vol calculation
  ! 04/13/2017 moved stum and tip vol calc to volinit
subroutine R10VOLO(EQNUM,DBHOB,HTTOT,HT1PRD,HTYPE,MTOPP,TLOGS,&
     &VOL,LOGSPAN,LOGDIA,logvol,BFPFLG,CUPFLG,ERRFLAG)

  !  SUBROUTINE NAME: R10VOL                                            *
  !          AUTHOR: D.DeMars/M.Schmidt                                 *
  !    CONVERTED BY: K.CORMIER/B.JONES                                  *

  ! Variables
  character(len=10) :: EQNUM
  character(len=1) :: LTYPE
  character(len=1) :: HTYPE

  integer :: DI1,DI2,bfpflg,cupflg,ERRFLAG,I
  integer :: NCNT,NCHK,JBF,KBF,KAJ,IKD,KBF2,KDBFF,KDBFS,KDBFC
  integer :: ITEMP,CHK

  real    :: TAB16(66),TAB32(66),BF1(21),BF2(21),DBF(21)
  real    :: VOL(15),LOGVOL(7,20),DIB(21),VS(21),V(21),HTSEG(21)
  real    :: CV(21),SCF(21),XINT(21),CUBVOL
  real    :: MTOPP,BF16,BF32,TDBF,AJ
  real    :: LOGDIA(21,3),CL,DI,W2,W1,BF11,BF12,LENGTH,LOGV
  real    :: LOGSPAN(20),BF21,BF22,XINTT,SCFT,DIBL,DIBS
  real    :: DBHOB,HTTOT,HT1PRD,THT,HMERCH,BFCHUG,BFTONG
  real    :: CUBMS,TLOGS,NLOG,STUMP

  logical :: CHECK

  !     (BD.FT.)/10 SCRIBNER 16 FT. LOGS; 5 IN. TO 70 IN.

  data TAB16/2.,2.,3.,3.,4.,6.,7.,8.,10.,11.,14.,16.,18.,21.,24.,&
       &28.,30.,33.,38.,40.,46.,50.,55.,58.,61.,66.,71.,74.,78.,80.,&
       &88.,92.,103.,107.,112.,120.,127.,134.,140.,148.,152.,159.,&
       &166.,173.,180.,187.,195.,202.,210.,218.,227.,235.,244.,252.,&
       &261.,270.,280.,289.,299.,309.,319.,329.,339.,350.,361.,372./

  !      (BD.FT.)/10 SCRIBNER 32 FT. LOGS; 5 IN. TO 70 IN.

  data TAB32/3.,5.,6.,7.,9.,12.,14.,16.,19.,23.,28.,32.,37.,43.,&
       &48.,56.,61.,67.,75.,81.,92.,100.,110.,116.,122.,131.,142.,&
       &147.,157.,160.,175.,185.,206.,214.,224.,241.,254.,269.,279.,&
       &296.,304.,317.,331.,346.,359.,374.,389.,405.,421.,437.,453.,&
       &470.,487.,505.,523.,541.,560.,578.,597.,618.,637.,658.,678.,&
       &699.,722.,744./

  ERRFLAG = 0

  ! *************  RESET ZEROES  **************

  do I = 1,15
     VOL(I) = 0.0
     10 continue
  end do

  do 25, I = 1,21
  BF1(I) = 0.0
  BF2(I) = 0.0
  DBF(I) = 0.0
  CV(I) = 0.0
  XINT(I) = 0.0
  SCF(I) = 0.0
  VS(I) = 0.0
  V(I) = 0.0
  25 continue

  BF16 = 0.0
  BF32 = 0.0
  TDBF = 0.0
  BFCHUG = 0.0
  BFTONG = 0.0
  CUBMS = 0.0
  CHECK = .false.

  !     IF DBHOB OR HT1PRD EQUALS ZERO THEN DON'T CALCULATE THE VOLUME
  if (DBHOB.lt.1.0) then
     ERRFLAG = 3
     goto 1000
  end if
  if (HTTOT.le.0.0.and. HT1PRD.le.0.0) then
     ERRFLAG = 4
     goto 1000
  end if

  !***********HEIGHT TYPE IS CHECKED

  if (HTYPE.eq.'L' .or. HTYPE.eq.'l') then
     LTYPE = 'L'
     THT = HT1PRD
  else
     LTYPE = 'F'
     THT = HTTOT

  end if

  ! do this only once for both cubic and boards
  ! place values into logdia and loglen first

  if (BFPFLG.eq.1.or. CUPFLG.eq.1) then
     CHK = 1

     call R10TAPO (DBHOB,THT,LTYPE,MTOPP,CHK,NLOG,HMERCH,CL,HTSEG,&
          &DIB,VS,V)

     NCNT = NLOG+4

     ! fill logdia array
     HTSEG(1) = 1.0
     ! Correction for stump when DBH > 36 (08/19/2021)
     if (DBHOB.gt.36) HTSEG(1) = HTSEG(2)-16.3
     LOGDIA(1,2) = DIB(1)
     LOGDIA(1,1) = ANINT(DIB(1))
     do 30, I = 1,INT(NLOG)
     LOGDIA(I+1,2) = DIB(I+1)
     LOGDIA(I+1,1) = ANINT(DIB(I+1))

     LOGSPAN(I) = HTSEG(I+1) - HTSEG(I) - .3
     30 continue
     if (LOGDIA(INT(NLOG)+1,1).lt.MTOPP) LOGDIA(INT(NLOG)+1,1) = MTOPP

     TLOGS = int(nlog)

     ! BOARD FOOT LOGIC

     if (BFPFLG.eq.1) then
        do 100, I = 1,INT(TLOGS)

        DI = LOGDIA(I+1,1)
        DI1 = DI
        DI2 = DI1 + 1
        W2 = 10. * (DI - DI1)
        W1 = 10. - W2

        !      LOOK UP BD. FT. FOR 16 FT. LOGS

        BF11 = TAB16(DI1-4)
        BF12 = TAB16(DI2-4)
        BF1(I) = W1 * BF11 + W2 * BF12
        if (LTYPE.eq.'L' .or. LTYPE.eq.'l') then
           BF16 = BF16 + BF1(I)
           CHK = I/2
           NCHK = CHK
           NCHK = 2 * NCHK
        else
           if (I.eq.INT(NLOG)) BF1(I) = 20. * CL/16.3
           JBF = BF1(I) +0.5
           BF1(I) = JBF
           BF16 = BF16 + BF1(I)
           CHK = I/2
           NCHK = CHK
           NCHK = 2 * NCHK
        end if

        if (NCHK.eq.I) then

           !  LOOK UP BD. FT. 32 FT. LOGS

           BF21 = TAB32(DI1-4)
           BF22 = TAB32(DI2-4)
           BF2(I) = W1 * BF21 + W2 * BF22

           if (LTYPE.eq.'L' .or. LTYPE.eq.'l') then
              BF32 = BF32 + BF2(I)
           else
              if (I.eq.INT(NLOG)) BF2(I) = 50. * (16.3 + CL)/32.6
              KBF = BF2(I) + 0.5
              BF2(I) = KBF
              BF32 = BF32 + BF2(I)
           end if

           !      DISTRIBUTE 32 FT. BD.FT. IN PROPORTION TO 16 FT. BD.FT.

           DBF(I-1) = BF1(I-1)/(BF1(I-1) + BF1(I)) * BF2(I)
           DBF(I) = BF2(I) - DBF(I-1)
           TDBF = TDBF + DBF(I-1) + DBF(I)

        else if (I.eq.TLOGS) then

           BF2(I) = BF1(I)
           DBF(I) = BF2(I)
           TDBF = TDBF + BF2(I)
           BF32 = BF32 + BF2(I)
        end if

        !    INTERNATIONAL LOGIC
        call INTL14(DI,LOGSPAN(I),LOGV)
        VOL(10) = VOL(10) + LOGV

        100 continue

        AJ = NLOG/2
        KAJ = AJ
        KAJ = KAJ * 2

        do 140, IKD = 2,KAJ,2
        KBF2 = BF2(IKD) + 0.5
        KDBFF = DBF(IKD-1) + 0.50000001
        KDBFS = DBF(IKD) + 0.50000001
        KDBFC = KDBFF + KDBFS

        if (KDBFC.ne.KBF2) then
           DBF(IKD-1) = KDBFF
           DBF(IKD) = KDBFS - 1
        end if

        140 continue

        do  150, I = 1,INT(TLOGS)
        ITEMP = BF1(I) + 0.5
        BF1(I) = ITEMP
        ITEMP = DBF(I) + 0.5
        DBF(I) = ITEMP

        BFCHUG = BFCHUG + ANINT(BF1(I)*10.0)/10.0
        BFTONG = BFTONG + ANINT(DBF(I)*10.0)/10.0

        if (EQNUM(1:3).eq.'A01' .or. EQNUM(1:3).eq.'a01') then
           LOGVOL(1,I) = ANINT(BF1(I)*10.0)/10.0
        else if (EQNUM(1:3).eq.'A02' .or. EQNUM(1:3).eq.'a02') then
           LOGVOL(1,I) = ANINT(DBF(I)*10.0)/10.0
        end if
        !          LOGS=NLOG
        CHECK = .true.

        150 continue

        if (EQNUM(1:3).eq.'A01' .or. EQNUM(1:3) .eq. 'a01') then
           VOL(2) = BFCHUG
        else if (EQNUM(1:3).eq.'A02' .or. EQNUM(1:3).eq.'a02') then
           VOL(2) = BFTONG
        end if

        !  END BOARD FOOT ROUTINES

     end if

     !***               CUBIC VOLUMES               ***

     if (CUPFLG.eq.1) then

        !       VTIP=VS(NLOG3)

        XINTT = 0.0
        SCFT = 0.0
        !--   GET CUBIC VOLUME FOR THE PIECES SMALIAN FORMULA

        !--   USE DIB AT DBHOB FOR LARGE END BUTT LOG
        DIBL = LOGDIA(1,1)
        do I = 1,INT(TLOGS)
           DIBS = LOGDIA(I+1,1)
           LENGTH = LOGSPAN(I)
           LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH
           LOGVOL(4,I) = ANINT(LOGV*10.0)/10.0
           VOL(4) = VOL(4)+LOGVOL(4,I)
           DIBL = DIBS
           350 continue
        end do
     end if

     if (VOL(4).le.0.0) then
        VOL(4) = 0.0
     end if

  end if

  !--    DETERMINE TOTAL CUBIC FIBER CONTENT (ALL TREES)       *

  !--   ENTER LOGIC FOR TOTAL CUBIC VOLUME HERE
  if ((DBHOB.ge.9.0.and. HTTOT.gt.40.0) .and. CUPFLG.eq.1) then
     call R10TCO(EQNUM,DBHOB,HTTOT,CUBVOL)
     VOL(1) = CUBVOL
  end if

  !         TOPWOOD CUBIC VOLUME          *****

  !        IF(CUB64.LE.0.0) THEN
  !          VOL(7) = 0.0
  !        ELSE
  !          VOL(7) = CUB64
  !        ENDIF

  !        VOL(8) = VOL(7)

  !      ENDIF

  !--  CHECK FOR LOG DETAIL INFORMATION AND ASSIGN LOG GRADES,  *
  !--     LOG DEFECTS, AND NET VOLUMES FOR THE TREE             *
  !--        MOVED TO NETVOL.FOR

  !     Calculate stump and tip volume
  !      STUMP=1.0
  !      IF(DBHOB.LT.36.0) STUMP=DBHOB/36.0
  !      IF(LOGDIA(1,2).GT.0.0) VOL(14)=0.005454154*LOGDIA(1,2)**2*STUMP
  !      IF(VOL(4).GT.0.0 .AND. VOL(1).GT.0.0)THEN
  !        VOL(15)=VOL(1)-VOL(4)-VOL(14)
  !      ENDIF
  1000 continue

  return
end

subroutine R10TCO(VOLEQ,DBHOB,HTTOT,TCVOL)

  !--   DETERMINE TOTAL CUBIC FIBER CONTENT.  THE VOLUME WILL BE
  !--   DETERMINED BY USING THE VOLUME OF A CYLINDER WITH A DIAMETER
  !--   OF DIB AT 1 FOOT FOR THE STUMP VOLUME AND USING THE SMALIAN
  !--   FORMULA TO COMPUTE VOLUMES FOR 4 FOOT SECTIONS FROM THE STUMP
  !--   TO THE TIP.

  !--   VARIABLES OF INTEREST ARE:
  !--   totht - REAL - **TOTAL TREE HT INCLUDING THE STUMP.**
  !--   TCVOL - REAL - TOTAL VOLUME FOR THE TREE IN CUBIC FOOT,
  !--                  DOES NOT INCLUDE THE LIMBS OR ROOTS.

  ! Variables
  character(len=10) :: VOLEQ
  integer :: HTLOOP,I
  real    :: D2,D2OLD,HT2,TCVOL,VOLTMP
  real    :: DBHOB,HTTOT,R

  HTLOOP = INT((HTTOT - 1.0)/4.0)
  HT2 = 1.0
  !##########
  call R10TAP(VOLEQ,DBHOB,HTTOT,HT2,D2)
  !--   STUMP VOLUME IS VOLUME FOR A 1 FT HIGH CYLINDER
  R = D2/2.0
  TCVOL = (3.1416*R*R)/144.0
  do I = 1,HTLOOP
     D2OLD = D2
     HT2 = HT2+4.0
     !##########
     call R10TAP(VOLEQ,DBHOB,HTTOT,HT2,D2)
     !--     USE SMALIAN FOR NON STUMP SEGMENTS
     VOLTMP = .00272708*(D2OLD*D2OLD+D2*D2)*4.0
     TCVOL = TCVOL + VOLTMP
     10 continue
  end do
  !--   USE SMALIAN FOR TIP, WITH A TIP DIAMETER OF 0.0
  if ( (HTTOT-HT2) .gt.0.0 ) then
     VOLTMP = .00272708*(D2*D2)*(HTTOT-HT2)
     TCVOL = TCVOL + VOLTMP
  end if
  return
end

  !                              ARGUMENTS                              *

  !  EQNUM    - (Input)  - Holds equation numbers and top dibs          *
  !  DBHOB      - (Input)  - DBHOB                                          *
  !  HT1PRD      - (Input)  - Number 16' Logs                              *
  !  TYPE     - (Input)  - L if height is in logs (ALWAYS L FOR THIS    *
  !                        PROGRAM
  !  PCTDF    - (Input)  - Tree Defect                                  *
  !  NLOG     - (Output) - Number 16' Logs Calculated                   *
  !  HMERCH   - (Output) - Height, Merch                                *

  !--  VOL      - (Output) - Volume array
  !--    VOL(1) -  TOTAL VOLUME FOR THE TREE IN CUBIC FOOT, DOES NOT
  !--              INCLUDE LIMBS AND ROOTS.
  !--    VOL(2) - GROSS AMOUNT OF BOARD FOOT PRODUCT IN MAIN STEM.
  !--    VOL(3) - NET AMOUNT OF BOARD FOOT PRODUCT IN MAIN STEM.
  !--    VOL(4) - GROSS AMOUNT OF CUBIC FOOT PRODUCT IN MAIN STEM.
  !--    VOL(5) - NET AMOUNT OF CUBIC FOOT PRODUCT IN MAIN STEM.
  !--    VOL(6) - AMOUNT OF CORD WOOD PRODUCT IN MAIN STEM
  !--    VOL(7) - GROSS AMOUNT OF CUBIC PRODUCT IN TOP WOOD.
  !--    VOL(8) - NET AMOUNT OF CUBIC PRODUCT IN TOP WOOD.
  !--    VOL(9) - AMOUNT OF CORD WOOD PRODUCT IN TOP WOOD.
  !--    VOL(10) - GROSS AMOUNT OF INT'L 1/4" BOARD FOOT PRODUCT IN MAIN STEM.
  !--    VOL(11) - NET AMOUNT OF INT'L 1/4" BOARD FOOT PRODUCT IN MAIN STEM.
  !--    VOL(12) - GROSS AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
  !--    VOL(13) - NET AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.

  !--    LOGVOL(1,X) - GROSS BOARD FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(2,X) - GROSS REMOVED BOARD FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(3,X) - NET BOARD FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(4,X) - GROSS CUBIC FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(5,X) - GROSS REMOVED CUBIC FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(6,X) - NET CUBIC FT LOG VOLUME (20 LOGS)
  !--    LOGVOL(7,X) - GROSS INT'L 1/4 VOLUME (20 LOGS)
