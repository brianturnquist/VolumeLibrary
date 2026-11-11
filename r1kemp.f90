!============================!
!   VolumeLibrary : r1kemp   !
!============================!
  !== last modified  6-04-2008
subroutine R1KEMP(VOLEQ,HTTOT,DBHOB,VOL,LIVE,PROD,TLOGS,errflag)

  ! PURPOSE: THIS ROUTINE CALCULATES GROSS & NET BOARD & CUBIC
  !          VOLUMES IN ENGLISH OR METRIC INCLUDING PRODUCT & POLE VOL

  !     INTERNAL SUBROUTINE R1LOGS

  ! Variables
  character(len=1) :: LIVE
  character(len=2) :: PROD
  character(len=10) :: VOLEQ

  integer :: ISPEC,JTAB,I,ERRFLAG,IWHEN,KLASS,NONSAW

  real    :: VOL(15),HTTOT,DBHOB,D2H100,BFGRS,CBGRS,BFNET
  real    :: BFVOL(15,5,4),CBVOLE(15,11),TLOGS,CBSAW

  !             DATA STATEMENTS           *

  !      BLOCK DATA

  ! INITIALIZATION OF VARIABLES IN COMMON BLOCKS

  ! MISCELLANEOUS

  ! SPECIES - 2 NAMES FOR EACH...

  !      DATA NAMSPC/'QA','QA ','CW','CW ','GF ','GF ','AF','AF ','L ',
  !     *'L  ','S  ','S  ','WB ','WB ','LP ','LP ','WP ','WP ','PP ','PP ',
  !     *'DF ','DF ','C  ','C  ','MH ','MH ','TOT','TOT'/, IETFOR/1,2,8,9,
  !     *11,12,15/, INDXP1/5,8,11,12/, INDXP2/4*0,1,2*0,2,2*0,3,4,0/,
  !     &MNGLST/'NONE',19*' '/

  ! BOARD FOOT VOLUME - 01 TABLES

  ! AS,CW,GF,AF,WL,S,WBP,LP,WP,PP,DF,C,H,J,PY - IN ORDER, ONE LINE EACH
  data (BFVOL(1,1,I),I = 1,4)/1.197,-18.544,1.216,-21.309/,&
       &(BFVOL(2,1,I),I = 1,4)/1.046,-15.966, 1.140, -46.735/,&
       &(BFVOL(3,1,I),I = 1,4)/1.293, -34.127, 1.218, 10.603/,&
       &(BFVOL(4,1,I),I = 1,4)/1.011, -11.403, 0.694, 124.425/,&
       &(BFVOL(5,1,I),I = 1,4)/0.997, -29.790, 0.841, 85.150/,&
       &(BFVOL(6,1,I),I = 1,4)/1.149, -11.851, 1.158, 1.620/,&
       &(BFVOL(7,1,I),I = 1,4)/1.208, -8.085, 1.103, 14.111/,&
       &(BFVOL(8,1,I),I = 1,4)/1.208, -8.085, 1.103, 14.111/,&
       &(BFVOL(9,1,I),I = 1,4)/1.189, -26.729, 1.181, -32.516/,&
       &(BFVOL(10,1,I),I = 1,4)/1.201, -50.340, 1.595, -298.784/,&
       &(BFVOL(11,1,I),I = 1,4)/1.003, -25.332, 1.011, -9.522/,&
       &(BFVOL(12,1,I),I = 1,4)/0.878, -10.742, 0.799, -4.064/,&
       &(BFVOL(13,1,I),I = 1,4)/1.203, -37.314, 1.306, -50.680/&
       &(BFVOL(14,1,I),I = 1,4)/1.208, -8.085, 1.103, 14.111/,&
       &(BFVOL(15,1,I),I = 1,4)/0, 0, 0, 0/

  ! BOARD FOOT 02 TABLES - WBP,LP,PP,DF - IN ORDER, ONE LINE EACH

  data (BFVOL(7,2,I),I = 1,2)/0.0135436, -33.632/,&
       &(BFVOL(8,2,I),I = 1,2)/0.0135436, -33.632/,&
       &(BFVOL(10,2,I),I = 1,3)/1.6399, -1.01249, -4.60157/,&
       &(BFVOL(11,2,I),I = 1,3)/0.848633, 0.000102364, -9.84419/

  ! BOARD FOOT 03 TABLES - LP,PP - IN ORDER, ONE LINE EACH

  data (BFVOL(7,3,I),I = 1,2)/1.1656, -13.5219/,&
       &(BFVOL(8,3,I),I = 1,2)/1.1656, -13.5219/,&
       &(BFVOL(10,3,I),I = 1,3)/1.8422, 1.11986, -2.78156/

  ! BOARD FOOT 04 TABLE - PP

  data (BFVOL(10,4,I),I = 1,3)/0.17148, -1.97703, 7.034/

  ! BOARD FOOT 05 TABLE - PP

  data (BFVOL(10,5,I),I = 1,3)/0.13412, -1.65764, 7.4384/

  ! CUBIC FOOT VOLUME TABLES

  ! AS
  data (CBVOLE(1,I),I = 1,8)/0.3482, -0.0384, 0.001427, -0.842503,&
       &0.224, -0.343, 0.217, 1.071/
  ! CW
  data (CBVOLE(2,I),I = 1,8)/0.1064, -0.00778, 0.000176, -0.265342,&
       &0.204, -0.749, 0.194, 4.285/
  ! GF
  data (CBVOLE(3,I),I = 1,11)/0.3386, -0.03359, 0.001109, -0.918645,&
       &0.219, -0.563, 0.197, 9.969, .2153, -0.00167, .50/
  ! AF
  data (CBVOLE(4,I),I = 1,11)/0.4529, -0.052, 0.002003, -1.113416,&
       &0.183, 1.449, 0.117, 26.222, .2153, .00167, .67/
  ! WL
  data (CBVOLE(5,I),I = 1,11)/0.4172, -0.04693, 0.001782, -1.086592,&
       &0.17, -0.056, 0.132, 19.409, .1922, .09023, .35/
  ! S
  data (CBVOLE(6,I),I = 1,11)/0.2619, -0.02345, 0.000671, -0.716502,&
       &0.214, 0.48, 0.174, 19.041, .2306, .14528, .35/
  ! WB
  data (CBVOLE(7,I),I = 1,11)/0.6808, -0.07974, 0.003113, -1.692512,&
       &0.221, 1.052, .197, 5.369, .2306, .14528, .35/
  ! LP
  data (CBVOLE(8,I),I = 1,11)/0.6808, -0.07974, 0.003113, -1.692512,&
       &0.221, 1.052, 0.197, 5.369, .2306, .14528, .35/
  ! WP
  data (CBVOLE(9,I),I = 1,11)/0.4544, -0.05119, 0.001945, -1.14765,&
       &0.206, 0.166, 0.194, 4.508, .2306, .14528, .35/
  ! PP
  data (CBVOLE(10,I),I = 1,11)/0.4041, -0.04535, 0.001726,-1.054732,&
       &0.203, -1.656, 0.218, -9.637, .2306, .14528, .25/
  ! DF
  data (CBVOLE(11,I),I = 1,11)/0.5125, -0.05817, 0.002208,-1.320519,&
       &0.178, 0.437, 0.165, 7.702, 0.1795, 0.16949, 0.47/
  ! C
  data (CBVOLE(12,I),I = 1,11)/0.3349, -0.03565, 0.001273,-0.851441,&
       &0.174, 1.141, 0.146, 8.931, 0.1922, 0.09023, 0.67/
  ! H
  data (CBVOLE(13,I),I = 1,11)/0.2213, -0.01913, 0.000533,-0.635045,&
       &0.209, -0.991, 0.210, 2.544, 0.2153, -0.00167, 0.43/
  ! J
  data (CBVOLE(14,I),I = 1,11)/0.0, 0.0, 0.0, 0.0,&
       &0.211, -0.597, 0.211,-0.597, 0.0, 0.0, 0.0/
  ! PY
  data (CBVOLE(15,I),I = 1,11)/0.0, 0.0, 0.0, 0.0,&
       &0.211, -0.597, 0.211,-0.597, 0.0, 0.0, 0.0/

  ! BOARD FOOT POLE CONVERSIONS

  ! WL
  !      DATA (POLEBF(1,I),I=1,17)/2*26.0, 2*63.0, 2*120.0, 2*240.0,
  !     *2*343.0, 2*441.0, 5*544.0/
  ! LP
  !      DATA (POLEBF(2,I),I=1,17)/2*25.0, 2*57.0, 2*99.0, 11*234.0/
  ! DF
  !      DATA (POLEBF(3,I),I=1,17)/2*27.0, 2*80.0, 2*114.0, 2*216.0,
  !     *9*359.0/

  !      DATA (POLEBF(4,I),I=1,17)/22.0, 36.0, 50.0, 70.0, 101.0, 161.0,
  !     *239.0, 261.0, 304.0, 418.0, 462.0, 512.0, 595.0, 736.0, 792.0,
  !     *892.0, 929.0/

  ! CUBIC FOOT POLE CONVERSIONS

  ! WL
  !      DATA (POLECB(1,I),I=1,17)/2*9.0, 2*12.0, 2*25.0, 2*38.0, 2*65.0,
  !     *2*90.0, 5*120.0/
  ! LP
  !      DATA (POLECB(2,I),I=1,17)/2*7.8, 2*10.9, 2*22.6, 11*32.1/
  ! DF
  !      DATA (POLECB(3,I),I=1,17)/2*10.0, 2*17.0, 2*24.0, 2*32.0, 9*69.0/

  !      DATA (POLECB(4,I),I=1,17)/7.4, 10.4, 14.1, 18.3, 25.4, 34.3, 46.0,
  !     *54.8, 65.5, 75.0, 86.6, 96.7, 112.5, 120.8, 136.3, 150.0, 170.4/

  !--  SET ALL POTENTIAL VOLUMES AND AVG NUM OF TLOGS TO ZERO
  do I = 1,15
     VOL(I) = 0.0
     10 continue
  end do
  ERRFLAG = 0

  !  DW 08/22 initializations of BFNET
  BFNET = 0

  !  ASSUMES CRUISES ARE FROM YR 95 ON
  IWHEN = 9500000

  ! GO TO SECTION TO PROCESS METRIC OR ENGLISH VOLUMES

  !      IF (MEASUR .EQ. 1) GO TO 2000        ! never implemented....

  ! SECTION FOR ENGLISH COMPUTATIONS

  D2H100 = DBHOB ** 2 * HTTOT / 100

  !  DETERMINE SPECIES NUMBER AND TABLE NUMBER TO BE USED *

  if (VOLEQ(2:3).eq.'02') then
     if (VOLEQ(8:10).eq.'746') then
        ISPEC = 1
     else if (VOLEQ(8:10).eq.'740') then
        ISPEC = 2
     else if (VOLEQ(8:10).eq.'017') then
        ISPEC = 3
     else if (VOLEQ(8:10).eq.'019') then
        ISPEC = 4
     else if (VOLEQ(8:10).eq.'070' .or. VOLEQ(8:10).eq.'073') then
        ISPEC = 5
     else if (VOLEQ(8:10).eq.'090'.or.VOLEQ(8:10).eq.'093') then
        ISPEC = 6
     else if (VOLEQ(8:10).eq.'101') then
        ISPEC = 7
     else if (VOLEQ(8:10).eq.'108') then
        ISPEC = 8
     else if (VOLEQ(8:10).eq.'119') then
        ISPEC = 9
     else if (VOLEQ(8:10).eq.'122') then
        ISPEC = 10
     else if (VOLEQ(8:10).eq.'202') then
        ISPEC = 11
     else if (VOLEQ(8:10).eq.'240'.or.VOLEQ(8:10).eq.'242') then
        ISPEC = 12
     else if (VOLEQ(8:10).eq.'260'.or.VOLEQ(8:10).eq.'263') then
        ISPEC = 13
     else if (VOLEQ(8:10).eq.'060') then
        ISPEC = 14
     else if (VOLEQ(8:10).eq.'106') then
        ISPEC = 15
     else
        ERRFLAG = 1
        go to 2000
     end if
     JTAB = 1

  else if (VOLEQ(2:3).eq.'03') then
     if (VOLEQ(8:10).eq.'101') then
        ISPEC = 7
     else if (VOLEQ(8:10).eq.'108') then
        ISPEC = 8
     else if (VOLEQ(8:10).eq.'122') then
        ISPEC = 10
     else if (VOLEQ(8:10).eq.'202') then
        ISPEC = 11
     else
        ERRFLAG = 1
        go to 2000
     end if
     JTAB = 2

  else if (VOLEQ(2:3).eq.'04') then
     if (VOLEQ(8:10).eq.'108') then
        ISPEC = 8
     else if (VOLEQ(8:10).eq.'122') then
        ISPEC = 10
     else
        ERRFLAG = 1
        go to 2000
     end if
     JTAB = 3

  else if (VOLEQ(2:3).eq.'05') then
     ISPEC = 10
     JTAB = 4

  else if (VOLEQ(2:3).eq.'06') then
     ISPEC = 10
     JTAB = 5
  end if

  !  DETERMINE KLASS  *

  if (LIVE.eq.'D'.and.ISPEC.eq.8) then
     KLASS = 2
  else if (LIVE.eq.'D') then
     KLASS = 1
  else if (PROD.eq.'02') then
     KLASS = 3
     !     ELSEIF (PROD.EQ.?????) THEN
     !        KLASS=8
  else
     KLASS = 4
  end if

  ! CALCULATE GROSS BOARD FOOT VOLUME

  !      JTAB = NTABLE (ISPEC)
  go to (300,400,500,600,700), JTAB

  ! COMPUTATIONS FOR BOARD FOOT 01 TABLES
  !   ! for all species
  300 if (DBHOB .lt.21.0) then
     BFGRS = BFVOL (ISPEC,1,1) * D2H100 + BFVOL (ISPEC,1,2)
  else
     BFGRS = BFVOL (ISPEC,1,3) * D2H100 + BFVOL (ISPEC,1,4)
  end if
  go to 1000

  ! COMPUTATIONS FOR BOARD FOOT 02 TABLES
  !! for all species
  400 if (ISPEC .eq.8.or. ISPEC .eq.7) then
     BFGRS = DBHOB**2 * HTTOT * BFVOL(ISPEC,2,1) + BFVOL(ISPEC,2,2)
  else if (ISPEC .eq.10) then
     BFGRS = 10**(BFVOL(10,2,1)*LOG10(DBHOB**2*HTTOT)+&
          &BFVOL(10,2,2)*LOG10(DBHOB) + BFVOL(10,2,3)) * 10.0
  else if (ISPEC .eq.11) then
     BFGRS = BFVOL(11,2,1) * D2H100 + BFVOL(11,2,2) * D2H100 **2 +&
          &BFVOL(11,2,3)
  else
     go to 300
  end if
  go to 1000

  ! COMPUTATIONS FOR BOARD FOOT 03 TABLES

  500 if (ISPEC .eq.10) then
     BFGRS = 10.0**(BFVOL(10,3,1)*LOG10(DBHOB)+BFVOL(10,3,2)*&
          &LOG10(HTTOT) + BFVOL(10,3,3))
     BFGRS = BFGRS * 10.0
  else if (ISPEC .eq.8.or. ISPEC .eq.7) then
     BFGRS = BFVOL(ISPEC,3,1) * D2H100 + BFVOL(ISPEC,3,2)
  else
     go to 300
  end if
  go to 1000

  ! COMPUTATIONS FOR BOARD FOOT 04 TABLES

  600 if (ISPEC .eq.10) then
     BFGRS = DBHOB**2 * BFVOL(10,4,1) + DBHOB * BFVOL(10,4,2) +&
          &BFVOL(10,4,3)
     BFGRS = BFGRS * 10.0
  else
     go to 300
  end if
  go to 1000

  ! COMPUTATIONS FOR BOARD FOOT 05 TABLES

  700 if (ISPEC .eq.10) then
     BFGRS = DBHOB**2 * BFVOL(10,5,1) + DBHOB * BFVOL(10,5,2) +&
          &BFVOL(10,5,3)
     BFGRS = BFGRS * 10.0
  else
     go to 300
  end if
  1000 continue

  ! DEDUCT 20 BF FOR DEAD LP OR DEAD WP (WP CRUISED BEFORE JAN 1,1994)

  if (KLASS .eq.2.and.(ISPEC .eq.8.or. (ISPEC .eq.9.and.&
       &IWHEN .lt.9400000))) BFGRS = BFGRS - 20.0

  ! CHECK FOR MINIMUM BOARD  OOT VOLUME
  !! 10 MIN EXCEPT FOR BELOW...
  if (BFGRS .lt.10.0) BFGRS = 10.0

  ! SET MIN TO 30 FOR DEAD LP OR DEAD WP (WP CRUISED BEFORE JAN 1, 1994)

  if ((ISPEC.eq.8.or. (ISPEC .eq.9.and. IWHEN .lt.9400000))&
       &.and. KLASS .eq.2.and.  BFGRS.lt.30.0) BFGRS = 30.0

  ! CALCULATE GROSS CUBIC FOOT VOLUME

  if (ISPEC.eq.14.or. ISPEC.eq.15) then
     if (DBHOB .lt.5.0) then
        CBGRS = 0.0
     else if (DBHOB .le.20.5.and. DBHOB .ge.5.0) then
        CBGRS = CBVOLE(ISPEC,5) * D2H100 + CBVOLE(ISPEC,6)
     else
        CBGRS = CBVOLE(ISPEC,7) * D2H100 + CBVOLE(ISPEC,8)
     end if
  else
     if (DBHOB .lt.5.0) then
        CBGRS = (CBVOLE(ISPEC,9)*D2H100+CBVOLE(ISPEC,10)) *&
             &CBVOLE(ISPEC,11)

     else if (DBHOB .le.9.5.and. DBHOB .ge.5.0) then
        CBGRS = D2H100 * (CBVOLE(ISPEC,1)*DBHOB + CBVOLE(ISPEC,2) *&
             &DBHOB**2 + CBVOLE(ISPEC,3)*DBHOB**3 + CBVOLE(ISPEC,4))
     else if (DBHOB .le.20.5.and. DBHOB .gt.9.5) then
        CBGRS = CBVOLE(ISPEC,5) * D2H100 + CBVOLE(ISPEC,6)
     else
        CBGRS = CBVOLE(ISPEC,7) * D2H100 + CBVOLE(ISPEC,8)
     end if
  end if

  ! DEDUCT FROM GROSS FOR DEAD LP AND WP (WP CRUISED BEFORE JAN 1, 1994)

  if (KLASS .eq.2.and. (ISPEC .eq.8.or. (ISPEC .eq.9.and.&
       &IWHEN .lt.9400000))) CBGRS = CBGRS - 3.8

  ! SET CUBIC MINIMUMS FOR GROSS

  if (KLASS .eq.2.and.(ISPEC .eq.8.or. (ISPEC .eq.9.and.&
       &IWHEN .lt.9400000))) then
     if (CBGRS .lt.4.3) CBGRS = 4.3
  else if (KLASS .le.2) then
     if (CBGRS .lt.1.6) CBGRS = 1.6
  else if (KLASS .eq.3) then
     if (CBGRS .lt.2.4) CBGRS = 2.4
  else
     if (CBGRS .lt.0.1) CBGRS = 0.1
  end if

  ! CALCULATE NET BOARD FOOT VOLUME

  !     IDEF   = IDEF + ICB (ISPEC,KLASS+1)   ! ADD SEEN AND ADDITIONAL
  !      IDEF=SDEF1+SDEF2
  !      IF (IDEF   .GT. 100.0) IDEF   = 100.0
  !      BFNET = BFGRS - IDEF   * BFGRS / 100.0

  ! CHECK FOR MINIMUM OF 10 BF NET

  if (BFNET .lt.10.0) BFNET = 10.0

  ! MIN OF 30 FOR DEAD LP...

  if (ISPEC.eq.8.and.KLASS.eq.2.and.BFNET.lt.30.0) BFNET = 30.0

  ! CALCULATE CUBIC SAWLOG VOLUME

  ! SET NON-SAWLOG PERCENT TO SUM OF BD FT DEFECTS

  !      IF (KLASS .LE. 2 .OR. KLASS .EQ. 8) NONSAW = IDEF
  !      nonsaw = idef
  if (NONSAW .gt.100) NONSAW = 100
  CBSAW = CBGRS - NONSAW / 100.0 * CBGRS

  ! SET CUBIC MINIMUMS FOR SAWLOG (NET)

  if (KLASS .eq.2.and.(ISPEC .eq.8.or. (ISPEC .eq.9.and.&
       &IWHEN .lt.9400000))) then
     if (CBSAW .lt.4.3) CBSAW = 4.3
  else if (KLASS .le.2) then
     if (CBSAW .lt.0.5) CBSAW = 0.5
  else if (KLASS .eq.3) then
     if (CBSAW .lt.2.4) CBSAW = 2.4
  else
     if (CBSAW .lt.0.1) CBSAW = 0.1
  end if

  ! CALCULATE OTHER CUBIC VOLUME

  !       CBOTH = CBGRS * (IALLOC / 100.0) ! CUBIC ALLOCATED TO OTHER PRODS

  ! CALCULATE POLE VOLUME - BOARD FOOT + CUBIC

  !      BFPOLE = 0.0
  !      CBPOLE = 0.0
  !      IF (LENPOL .NE. 0) THEN    ! TABLE LOOKUP BY HEIGHT CLASS
  !           J = INDXP2(ISPEC)
  !           I =  (LENPOL / 05 ) - 3
  !           IF (I .LT. 1) I = 1
  !           IF (I .GT.17) I = 17
  !           BFPOLE = POLEBF (    J,I)
  !           CBPOLE = POLECB (    J,I)
  !      ENDIF

  !  WRITE VOLUMES TO THE VOL ARRAY  *

  VOL(1) = CBGRS
  VOL(2) = BFGRS
  VOL(3) = BFNET
  VOL(4) = CBGRS
  VOL(5) = CBSAW

  !     FIND NUMBER OF TLOGS
  if (ispec.lt.14) then
     call R1LOGS(ISPEC,DBHOB,HTTOT,KLASS,IWHEN,TLOGS)
  end if

  !     RETURN

  ! SECTION FOR METRIC COMPUTATIONS

  2000 continue

  return
end

subroutine R1LOGS (ISPEC,DBHOB,HTTOT,KLASS,IWHEN,TLOGS)

  !     THIS ROUTINE CALCULATES MECHANTABLE LENGTH FOR ALL TREES EXCEPT
  !     FELLED TREES

  !     VARIABLE DESCRIPTIONS
  !        MEASUR = ENGLISH OR METRIC UNITS FLAG
  !        AMERCH = ENGLISH MERCHATABLE LENGTH
  !        AMRLNE = TALBE OF COEFFICIENTS FOR COMPUTING ENGLISH MERCH LENGTH
  !        ISPEC =SPECIES INDEX
  !        HTTOT = TREE HEIGHT
  !        DBHOB = TREE DBHOB

  ! AS,CW,GF,AF,WL,S,WBP,LP,WP,PP,DF,C,H - IN ORDER, ONE LINE EACH
  ! Variables
  real    :: DBHOB,HTTOT,AMRLNE(13,3),AMERCH,TLOGS
  integer :: ISPEC,MEASUR,KLASS,IWHEN

  data (AMRLNE(1,I),I = 1,3)/0.0,0.0,0.0/
  data (AMRLNE(2,I),I = 1,3)/0.0,0.0,0.0/
  data (AMRLNE(3,I),I = 1,3)/0.769102, 8.17961, -39.81773/
  data (AMRLNE(4,I),I = 1,3)/0.617455, 9.00450, -33.62005/
  data (AMRLNE(5,I),I = 1,3)/0.767457, 11.0658, -53.23026/
  data (AMRLNE(6,I),I = 1,3)/0.788744, 6.25054, -36.74818/
  data (AMRLNE(7,I),I = 1,3)/0.722147, 17.8226, -67.2513/
  data (AMRLNE(8,I),I = 1,3)/0.722147, 17.8226, -67.2513/
  data (AMRLNE(9,I),I = 1,3)/0.596947, 14.6879, -49.13522/
  data (AMRLNE(10,I),I = 1,3)/0.838089, 7.29928, -41.14457/
  data (AMRLNE(11,I),I = 1,3)/0.745249, 6.12528, -31.79443/
  data (AMRLNE(12,I),I = 1,3)/0.741795, 6.29675, -33.11715/
  data (AMRLNE(13,I),I = 1,3)/0.716799, 10.5844, -46.78366/

  MEASUR = 0

  if (MEASUR .eq.1) go to 200

  !     COMPUTE ENGLISH MECHANTABLE LENGTH

  AMERCH = AMRLNE(ISPEC,1) * HTTOT + AMRLNE(ISPEC,2)*DBHOB**.5 +&
       &AMRLNE(ISPEC,3)

  if (ISPEC .eq.1.or. ISPEC.eq.2) AMERCH = 0.0

  !     DEDUCT FOR DEAD WP (before 94) or dead lp

  if ((ISPEC.eq.8.or. (ISPEC.eq.9.and. IWHEN.lt.9400000)) .and.&
       &KLASS.eq.2)  AMERCH = AMERCH - 16.5

  if (AMERCH .lt.0.0) AMERCH = 0.0

  TLOGS = AMERCH/16.5

  return

  !     COMPUTE METRIC MERCHANTABLE LENGTH

  200 continue

  return
end

  ! VARIABLE DESCRIPTION:

  ! MEASUR = ENGLISH OR METRIC UNITS FLAG
  ! DBHOB = TREE DBHOB
  ! HTTOT = TREE HEIGHT
  ! ISPEC = SPECIES INDEX
  ! NTABLE = ARRAY FOR VOLUME TABLE SELECTION (has been replaced with VOLEQ)
  ! BFVOL = TABLE OF BOARD FT VOLUME COEFFICIENTS
  ! BFGRS = BOARD FT GROSS VOLUME
  ! CBVOLE = TABLE OF CUBIC FT VOLUME COEFFICIENTS
  ! CBGRS = CUBIC    GROSS VOLUME (1 foot stump to 4" DIB)
  ! SDEF1 = TREE DEFECT
  ! ICB = was ARRAY OF ADD'L CULL+BREAK FAC.
  !       Change to single varible NATCRS' SDEF2
  ! BFNET = BOARD FT NET VOLUME
  ! CBSAW = CUBIC SAWLOG VOLUME
  ! NONSAW = NON-SAWLOG VOLUME PERCENT
  ! IALLOC = PERCENT ALLOCATED TO OTHER PRODUCTS
  ! CBOTH = OTHER CUBIC VOLUME
  ! BFPOLE = BOARD FT POLE VOLUME
  ! CBPOLE = CUBIC    POLE VOLUME
  ! LENPOL = POLE LENGTH
  ! POLEBF = TABLE OF COEFFICIENTS FOR BOARD FT POLE VOLUMES
  ! POLECB = TABLE OF COEFFICIENTS FOR CUBIC FT POLE VOLUMES
