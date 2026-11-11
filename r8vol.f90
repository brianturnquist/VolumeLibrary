!===========================!
!   VolumeLibrary : r8vol   !
!===========================!
  !== last modified  2-7-2014
subroutine R8VOL(VOLEQ,DBHOB,HTTOT,UPSHT1,HT1PRD,MTOPP,PROD,VOL,&
     &FORST,SI,BA,CTYPE,BFPFLG,CUPFLG,SPFLG,ERRFLAG)

  !  CREATED:   06-03-91
  !  PURPOSE:   THIS ROUTINE IS THE DRIVER PROGRAM FOR CALCULATING
  !             R8 VOLUMES.
  !  10/11/2012 YW Changed to set VOL(7) to 0.5 if it is 0 for PROD 08 and for other prod set vol(4) to 0.5.
  !  02/07/2014 YW modified to allow enter broken top height to HT1PRD field for pulp tree
  !  06/04/2014 YW Changed the merch height calculation to use R8_MHT
  !  08/03/2021 YW Enable to use R8_MHTS again for FSVeg to test old R8 DVE equations
  !  DECLARE VARIABLES
  ! Variables
  character(len=1) :: CTYPE
  character(len=2) :: PROD
  character(len=2) :: FORST
  character(len=2) :: VAR
  character(len=2) :: DIST
  character(len=10) :: VOLEQ
  character(len=10) :: VOLEQTMP

  integer :: BFPFLG,CUPFLG,SPFLG
  integer :: ERRFLAG,I,SI,BA,VOLSP

  real    :: DBHOB,HT1PRD,UPSHT1,VOL(15),HTTOT,MTOPP,VOLTMP(15)

  logical :: VOL1,VOL2

  do 100, i = 1,15
  vol(i) = 0
  100 continue

  VOL1 = .false.
  VOL2 = .false.

  if (dbhob .lt.1.0) then
     errflag = 3
     goto 999
  end if
  ! The HT1PRD field is also used by pulpwood broken top height (YW 2/7/14)
  if (UPSHT1.le.0.and. HT1PRD.gt.0.and.PROD.eq.'01')UPSHT1 = HT1PRD

  if (VOLEQ(4:6).eq.'DVE'.or.VOLEQ(4:6).eq.'dve') then
     if (BFPFLG.eq.1) VOL1 = .true.
     if (CUPFLG.eq.1) VOL1 = .true.
     if (SPFLG.eq.1) VOL1 = .true.
  else if (VOLEQ(4:6).eq.'CLK'.or.VOLEQ(4:6).eq.'clk') then
     if (BFPFLG.eq.1) VOL2 = .true.
     if (CUPFLG.eq.1) VOL2 = .true.
     if (SPFLG.eq.1) VOL2 = .true.
  else
     ERRFLAG = 1
     return
  end if

  if (VOL2) then
     call R8VOL2 (VOLEQ,VOL,DBHOB,HT1PRD,UPSHT1,MTOPP,HTTOT,&
          &CTYPE, ERRFLAG)
  else
     if (HT1PRD.le.0) then
        if (CTYPE.eq.'F' .and. HTTOT.gt.0) then
           !           R8_MHTS has problem to calculate merch height because the original species FIA code
           !           is not retained in the VOLEQ. Changed to call R8VOL2 to calc merch height
           !           YW 12/08/2011
           !           YW 08/03/2021 Enable to use R8_MHTS again for FSVeg to test the old R8 DE equation
           call R8_MHTS(FORST,VOLEQ,DBHOB,HTTOT,SI,BA,HT1PRD,ERRFLAG)
           if (HT1PRD.le.0) then
              read (VOLEQ(8:10),'(I3)') VOLSP
              !      get the default Clark equation
              DIST = '01'
              call R8_CEQN(FORST,DIST,VOLSP,PROD,VAR,VOLEQTMP,ERRFLAG)
              ERRFLAG = 0
              VOLEQTMP(3:3) = '7'
              if (VOLSP.ge.300) VOLEQTMP(3:3) = '9'
              call R8VOL2 (VOLEQTMP,VOLTMP,DBHOB,HT1PRD,UPSHT1,MTOPP,&
                   &HTTOT,CTYPE, ERRFLAG)
           end if
        end if
     end if
     if (VOL1) call R8VOL1 (VOLEQ,DBHOB,HT1PRD,UPSHT1,PROD,VOL,ERRFLAG)
  end if
  !     ROUND THE VOLUMES 10TH CUBIC FOOT, NEAREST BDFT
  VOL(2) = ANINT(VOL(2))
  VOL(4) = ANINT(VOL(4)*10.0)/10.0
  VOL(7) = ANINT(VOL(7)*10.0)/10.0
  VOL(10) = ANINT(VOL(10))
  VOL(12) = ANINT(VOL(12))
  ! Changed to NOT reset VOL(4) to 0.5 for PROD 08 (09/06/2012)
  if (PROD.eq.'08') then
     if (VOL(4).lt.0.5.and.VOL(7).lt.0.5) VOL(7) = 0.5
  else
     if (VOL(4) .lt.0.5) VOL(4) = 0.5
  end if

  999 return
end

subroutine R8_MHTS(FORST,VOLEQ,DBH,HTTOT,SI,BA,HT1PRD,ERRFLAG)

  ! CALCULATES THE MERCH HTS BASED ON TOTAL TREE HEIGHT BY FORST AND SPECIES
  ! Variables
  character(len=10) :: VOLEQ
  character(len=2) :: FORST
  integer :: SI,BA,VFLAG,ERRFLAG,IFOR,ISPEC
  real    :: DBH,HTTOT,HT1PRD,B(6),BFMIND,DBHMIN,TOPD
  real    :: FACTOR,MHT,ESTCHT,ESTTHT

  if (FORST(2:2) .lt. '0') then
     FORST(2:2) = FORST(1:1)
     FORST(1:1) = '0'
     if (FORST(2:2) .lt. '0') FORST(2:2) = '0'
  end if

  read(FORST,'(i2)') IFOR

  !     CHECK EQUATION NUMBER FOR INFORMATION
  if (VOLEQ(4:4) .eq. 'D' .or. VOLEQ(4:4).eq.'D') then
     read(VOLEQ(8:10),'(I3)')ISPEC
     if (ISPEC .lt.300) then
        TOPD = 7.0
     else
        TOPD = 9.0
     end if
  else
     if (VOLEQ(3:3) .eq. '0') then
        HT1PRD = HTTOT
        go to 100
     else if (VOLEQ(3:3) .eq. '4') then
        TOPD = 4.0
     else if (VOLEQ(3:3) .eq. '7') then
        TOPD = 7.0
     else if (VOLEQ(3:3) .eq. '9') then
        TOPD = 9.0
     end if
  end if

  if (BA.le.0) BA = 90
  if (IFOR.eq.1.or. IFOR.eq.3.or. IFOR.eq.12) then
     !     SE
     if (SI.le.0)SI = 80
     !      VFLAG = 1
     !        CALL R8INIT(VOLEQ,DBHMIN,BFMIND,B,VFLAG,ERRFLAG)

  else
     !     SN
     if (SI.le.0)SI = 70
     !      VFLAG = 2
     !        CALL R8INIT(VOLEQ,DBHMIN,BFMIND,B,VFLAG,ERRFLAG)
  end if
  !     REGION 8 IS USING SAME COEFFICIENT
  VFLAG = 1
  call R8INIT(VOLEQ,DBHMIN,BFMIND,B,VFLAG,ERRFLAG)

  !  COMPUTE ESTIMATED TOTAL TREE HEIGHT.

  if (DBH .lt. DBHMIN .or. ERRFLAG.gt.0) then
     HT1PRD = 0
     go to 100
  else
     FACTOR = 0.0
     ESTTHT = 4.5+B(1)*(1.0-EXP(-1.0*B(2)*DBH))&
          &**B(3)*SI**B(4)*(1.00001-FACTOR)&
          &**B(5)*BA**B(6)
  end if

  !  COMPUTE THE MERCH HEIGHT TO A 4 INCH TOP DIAMETER (PULPWOOD)

  if (TOPD.eq.4.0.or. DBH.ge.BFMIND) then
     FACTOR = TOPD/DBH
     if (FACTOR .gt.1.0) FACTOR = 1.0
     ESTCHT = 4.5+B(1)*(1.0-EXP(-1.0*B(2)*DBH))&
          &**B(3)*SI**B(4)*(1.00001-FACTOR)&
          &**B(5)*BA**B(6)
     MHT = ESTCHT*(HTTOT/ESTTHT)
     HT1PRD = INT(MHT)
  else
     HT1PRD = 0
  end if

  100 continue
  return
end
