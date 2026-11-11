!===========================!
!   VolumeLibrary : dvest   !
!===========================!
  !== last modified  01-18-2013
  ! 01/18/2013 Added calculation for cordwood VOL(6) for region 1,2,4,5,10
  ! 2018/08/01 YW Added call to HAHN_NC250 for Region 9
  ! 2018/09/05 ADDED PROD AND MTOPP TO CALL R2OLDV
  ! 2019/04/04 YW added call to Canadian Honer equation for BIA
  ! 2022/08/08 YW added variable STUMP to DVEST
  !     SUBROUTINE FINDS VOLUMES USING DIRECT VOLUME ESTIMATORS
subroutine DVEST(VOLEQ,DBHOB,DRC,HTTOT,MTOPP,FCLASS,HTLOG,HT1PRD,&
     &HT2PRD,FORST,BTR,VOL,CUTFLG,BFPFLG,CUPFLG,CDPFLG,&
     &SPFLG,PROD,HTTYPE,HTTFLL,NOLOGP,LIVE,BA,SI,&
     &CTYPE,ERRFLAG,MTOPS,STUMP)

  ! Variables
  character(len=10) :: VOLEQ
  character(len=1) :: HTTYPE
  character(len=1) :: LIVE
  character(len=1) :: CTYPE
  character(len=2) :: FORST
  character(len=2) :: PROD

  integer :: FCLASS,UNIT,HTLOG,BA,SI
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
  integer :: HTTFLL,ERRFLAG
  real    :: DBHOB,HTTOT,VOL(15),HT1PRD,HT2PRD,NOLOGP,BTR
  real    :: MTOPP,STUMP,BDVOL,CUVOL,TCVOL,DRC,MTOPS

  do 100, I = 1,15
  VOL(I) = 0.0
  100 continue

  !     DIRECT VOLUME ESTIMATOR LOGIC
  if (VOLEQ(1:1).eq.'1') then

     !      REGION 1 D2H ROUTINES *

     if (VOLEQ(2:3).eq.'02' .or. VOLEQ(2:3).eq.'03' .or.&
          &VOLEQ(2:3).eq.'04' .or. VOLEQ(2:3).eq.'05' .or.&
          &VOLEQ(2:3).eq.'06') then
        !        KEMP
        call R1KEMP(VOLEQ,HTTOT,DBHOB,VOL,LIVE,PROD,NOLOGP,ERRFLAG)
     end if
     !     BOARD FOOT KEMP
     if (VOLEQ(2:3).eq.'01' .and. BFPFLG.eq.1) then
        if ((VOLEQ(8:10).eq.'108' .and. DBHOB.ge.6)&
             &.or. DBHOB.ge.7) then
           STUMP = 1.0
           call R1ALLENB (VOLEQ,DBHOB,HTTOT,MTOPP,BTR,BDVOL,STUMP,&
                &ERRFLAG)
           VOL(2) = BDVOL
        else
           VOL(2) = 0.0
        end if
     end if
     !     CUBIC  KEMP
     if (VOLEQ(2:3).eq.'01' .and. (CUPFLG.eq.1.or. CUTFLG.eq.1)) then
        STUMP = 1.0
        !          byrne
        call R1ALLENC(VOLEQ,DBHOB,HTTOT,MTOPP,BTR,CUVOL,TCVOL,STUMP,&
             &ERRFLAG)
        VOL(1) = TCVOL
        VOL(4) = CUVOL
     end if

     if (VOLEQ(2:3).eq.'07') then
        STUMP = 1.0

     end if
     VOL(6) = VOL(4)/90
  else if (VOLEQ(1:1).eq.'2') then

     !      REGION 2 D2H ROUTINES *

     call R2OLDV(VOLEQ,HTTOT,DBHOB,DRC,FCLASS,VOL,ERRFLAG,PROD,MTOPP)
     VOL(6) = (VOL(4)+VOL(7))/90
  else if (VOLEQ(1:1).eq.'3') then

     !      REGION 3 D2H ROUTINES *

     !--------- SAWTIMBER TREES
     if (PROD.eq.'01') then
        UNIT = 1
        call R3D2HV (VOLEQ,UNIT,HTTOT,HT1PRD,DBHOB,DRC,FCLASS,HTTFLL,&
             &VOL,ERRFLAG)
        !-------------- PULPWOOD TREES
     else
        UNIT = 3
        call R3D2HV(VOLEQ,UNIT,HTTOT,HT1PRD,DBHOB,DRC,FCLASS,HTTFLL,&
             &VOL,ERRFLAG)
     end if
  else if (VOLEQ(1:1).eq.'4') then

     !      REGION 4 D2H ROUTINES *

     call R4D2H (VOLEQ,HTTOT,DBHOB,DRC,FCLASS,VOL,ERRFLAG)
     VOL(6) = VOL(4)/90
  else if (VOLEQ(1:1).eq.'5') then

     !      REGION 5 D2H ROUTINES *

     call R5HARV(VOLEQ,DBHOB,HTTOT,MTOPP,VOL,BFPFLG,CUPFLG,ERRFLAG)
     VOL(6) = VOL(4)/90
  else if (VOLEQ(1:1).eq.'9') then
     if (VOLEQ(2:3).eq.'25') then
        call HAHN_NC250(VOLEQ,HTTOT,HT1PRD,HT2PRD,DBHOB,VOL,SI,BA,&
             &PROD,CTYPE,BFPFLG,CUPFLG,SPFLG,ERRFLAG,MTOPP,MTOPS)
     else

        !      REGION 9 D2H ROUTINES *

        call R9VOL(VOLEQ,HTTOT,HT1PRD,HT2PRD,DBHOB,VOL,FORST,SI,BA,&
             &PROD,CTYPE,BFPFLG,CUPFLG,CDPFLG,SPFLG,HTTYPE,ERRFLAG,MTOPP)
     end if
  else if (VOLEQ(1:1).eq.'A' .or. VOLEQ(1:1).eq.'a') then

     !      REGION 10 D2H ROUTINES *

     call R10D2H(VOLEQ,DBHOB,HTTOT,VOL,CUTFLG,CUPFLG,BFPFLG,&
          &MTOPP,ERRFLAG)
     VOL(6) = VOL(4)/90
  else if (VOLEQ(1:1).eq.'M' .or. voleq(1:1).eq.'m') then

     !      ARMY BASE D2H ROUTINES *

     if (HTTYPE.eq.'L' .or. HTTYPE.eq.'l') then
        if (VOLEQ(1:3).eq.'M01' .or. VOLEQ(1:3).eq.'m01') then
           call DOYAL78(DBHOB,HT1PRD,VOL,ERRFLAG)
        else if (VOLEQ(1:3).eq.'M02' .or. VOLEQ(1:3).eq.'m02') then
           call INTL78(DBHOB,HT1PRD,VOL,ERRFLAG)
        end if
     else
        ERRFLAG = 7
     end if

  else if (VOLEQ(1:1).eq.'C' .or. voleq(1:1).eq.'c') then

     !      Canadian Honor Equation used by BIA in eastern region*

     call Voleq_Honer(VOLEQ,DBHOB,HTTOT,MTOPP,MTOPS,&
          &VOL,ERRFLAG)
  end if

  !     ROUND ALL VOLUMES 10TH CUBIC AND CORD, NEAREST BDFT
  !Changed to not rounding VOL(1) and VOL(4). They will be used for biomass calculation (2025/05/07)
  !VOL(1) =  ANINT(VOL(1)*10.0)/10.0
  VOL(2) = ANINT(VOL(2))
  !VOL(4) = ANINT(VOL(4)*10.0)/10.0
  VOL(6) = ANINT(VOL(6)*1000.0)/1000.0
  !VOL(7) = ANINT(VOL(7)*10.0)/10.0
  VOL(9) = ANINT(VOL(9)*1000.0)/1000.0
  VOL(10) = ANINT(VOL(10))

  return
end
