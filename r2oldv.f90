!============================!
!   VolumeLibrary : r2oldv   !
!============================!
  !== last modified  3-29-2004
subroutine R2OLDV(VOLEQ,HTTOT,DBHOB,DRC,FCLASS,VOL,ERRFLAG,PROD,&
     &MTOPP)
  implicit none
  !--  THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE
  !--  USING REGION 2 D*D*H VOLUME DETERMINATION EQUATIONS.
  ! YW 2018/09/05 Added International boardfoot volume and sawlog portion cubic volume calculation
  ! for 200DVEW122, 200DVEW746, 200DVEW108, 200DVEW093 and 212DVEW122. This calculation
  ! is from FIA volume codes.
  ! YW 2018/09/06 Added 201DEVW746 AND MYERS(1964) RM-6 TOP=8 (Scribner and international)
  ! YW 2018/09/12 Changed the FCLASS to be: 1 = sngle, others = multistem
  !               This is to make it consistent with R3 DVE equation.
  ! YW 2022/08/05 Added new equation (223DVEW122) for Black Hills non-saw product (DBH<9)
  !               also add the input variable STUMP to the subroutine
  ! YW 2022/11/22 Moved the codes for equation 223DVEW122 to its own subroution

  ! Variables
  character(len=10) :: VOLEQ
  integer :: ERRFLAG,MSTEM,FCLASS,I
  character(len=2) :: PROD
  real    :: MTOPP,TOPWOOD,CV6,SPF,DIB,X1,X2,X3
  real    :: DBHOB,DRC,D2H,HTTOT,VOL(15),grsbdt,GCUFT,TCUFT,trevol

  TREVOL = 0.0
  grsbdt = 0.0
  GCUFT = 0.0
  TCUFT = 0.0
  ERRFLAG = 0
  !     Changed the FCLASS to be: 1 = single, others = multistem
  !     2018/09/12
  if (FCLASS .eq.1) then
     MSTEM = 1
  else
     MSTEM = 0
  end if

  !--   IF DBHOB OR HTTOT EQUALS ZERO THEN DON'T CALCULATE THE VOLUME

  if (DBHOB .le.0.0.and. DRC.le.0.0) then
     ERRFLAG = 3
     go to 1000
  end if
  if ( HTTOT .le.0.0) then
     ERRFLAG = 4
     go to 1000
  end if

  do 100, I = 1,15
  VOL(I) = 0
  100 continue
  !--   CALCULATE THE DIAMETER SQUARED TIMES HEIGHT

  D2H = (DBHOB**2)*(HTTOT)

  !--***********  ASPEN-RM232-TOTAL PG3  TOTAL CUBIC

  if (VOLEQ(8:10).eq.'746' .and. VOLEQ(1:3).eq.'200') then

     if ( D2H .le.12470. ) then
        TCUFT = 0.002219 * D2H
     else
        TCUFT = 0.001896 * D2H + 4.0267
     end if

     !--**************  ASPEN-RM232-TOP=6   BOARD FOOT
     if (DBHOB.gt.7) then
        if ( D2H .le.2500. ) then
           grsbdt = 8
        else if ( D2H .le.8850. ) then
           grsbdt = 0.011389 * D2H - 20.5112
        else
           grsbdt = 0.010344 * D2H - 11.2615
        end if
        ! INTERNATIONAL BOARDFOOT VOLUME (YW 2018/0905)
        if (D2H.le.11460) then
           VOL(10) = .013472*D2H-25.5968
        else
           VOL(10) = .011989*D2H-8.6015
        end if
     end if
     !--***********  ASPEN-RM232-TOP=4

     if ( D2H .le.11800. ) then
        GCUFT = 0.002195 * D2H - 0.9076
     else
        GCUFT = 0.001837 * D2H + 3.3075
     end if
     ! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
     if (PROD.eq.'01'.and.MTOPP.ge.6.0) then
        if (DBHOB.lt.11.0) then
           SPF = 0.0
        else if (DBHOB.gt.42.0) then
           SPF = .96
        else
           SPF = .92-(.22*((42.0-DBHOB)/33.0)**1.4)
        end if
        CV6 = SPF*GCUFT
        if (CV6.gt.0.0) then
           TOPWOOD = GCUFT-CV6
           VOL(7) = TOPWOOD
           GCUFT = CV6
        end if
     end if

     ! --Aspen
     !Peterson, Geraldine. 1961. Volume Tables for Aspen in Colorado. Forest Service, U.S. Department of Agriculture, Rocky Mountain
     !Forest Range Experiment Station. Research Notes, No. 63. 4p.
     !volume to 4" top DIB
  else if (VOLEQ(8:10).eq.'746' .and. VOLEQ(1:3).eq.'201') then
     DIB = (0.8954*DBHOB)+0.3168
     X1 = LOG10(DIB-4.0)
     X2 = LOG10(HTTOT-4.5)
     X3 = X1*X2
     GCUFT = (10**((0.0827*X1)+(0.4045*X2)+(.6593*X3)-0.4721))+0.3
     !--***********  ASPEN-RM232-TOTAL PG3

  else if (VOLEQ(8:10).eq.'746' .and. VOLEQ(1:3).eq.'210') then

     if ( D2H .le.12470. ) then
        GCUFT = 0.002219 * D2H
     else
        GCUFT = 0.001896 * D2H + 4.0267
     end if

     !--*************  LODGEPOLE PINE-RM6-TOTAL PG5  TOTAL CUBIC

  else if (VOLEQ(8:10).eq.'108' .and. VOLEQ(1:3).eq.'200' ) then

     if ( D2H .le.7000.) then
        TCUFT = 0.002777 * D2H + 0.027967
     else
        TCUFT = 0.002332 * D2H + 3.446454
     end if

     !--*************  LODGEPOLE PINE-RM157-TOP=6   BOARD FOOT
     if (DBHOB.gt.7) then

        if ( D2H .le.22800. ) then
           grsbdt = 0.01202 * D2H - 6.00933
        else
           grsbdt = 0.01263 * D2H - 19.76641
        end if
        ! ADD INTERNATIONAL BOARDFOOT VOLUME FROM MYERS(1964) RM-6 TOP=8
        if (MTOPP.ge.8.0) then
           if (D2H.le.15000.0) then
              grsbdt = .012535*D2H-20.19057
              VOL(10) = 0.015097*D2H - 26.28782
           else
              grsbdt = .012893*D2H -29.24566
              VOL(10) = 0.013766*D2H - 6.30999
           end if
        end if
     end if
     !--*************  LODGEPOLE PINE-RM6-TOP=4

     if ( D2H .le.7000.) then
        GCUFT = 0.002798 * D2H - 1.04578
     else
        GCUFT = 0.002256 * D2H + 2.836222
     end if
     ! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
     if (PROD.eq.'01'.and.MTOPP.ge.6.0) then
        if (DBHOB.lt.9.0) then
           SPF = 0.0
        else if (DBHOB.gt.42.0) then
           SPF = .95
        else
           SPF = .98-(.10*((42.0-DBHOB)/33.0)**4.0)
        end if
        CV6 = SPF*GCUFT
        if (CV6.gt.0.0) then
           TOPWOOD = GCUFT-CV6
           VOL(7) = TOPWOOD
           GCUFT = CV6
        end if
     end if

     !--*************  LODGEPOLE PINE-RM6-TOTAL PG5

  else if (VOLEQ(8:10).eq.'108' .and. VOLEQ(1:3).eq.'210') then

     if ( D2H .le.7000.) then
        GCUFT = 0.002777 * D2H + 0.027967
     else
        GCUFT = 0.002332 * D2H + 3.446454
     end if

     !--**************  PONDEROSA PINE PROSSER BLACK HILLS TOTAL  TOTAL CUBIC

  else if (VOLEQ(8:10).eq.'122' .and. VOLEQ(1:3).eq.'203') then

     if ( D2H .lt.6000. ) then
        TCUFT = 0.0024506 * D2H + .3470564
     else
        TCUFT = 0.0022325 * D2H + 3.2829984
     end if

     !--*************  PONDEROSA PINE PROSSER BLACK HILLS-TOP=6   BOARD FOOT
     if (DBHOB.gt.7) then

        if ( D2H .lt.16000. ) then
           grsbdt = .0132429 * D2H - 26.0553842
        else
           grsbdt = 0.0127305 * D2H - 15.5653183
        end if
     end if
     !--**************  PONDEROSA PINE BLACK HILLS MYERS TOP=4

     if ( D2H .le.6700. ) then
        GCUFT = 0.002297 * D2H - 1.032297
     else
        GCUFT = 0.002407 * D2H - 2.257724
     end if
     !--**************  PONDEROSA PINE BLACK HILLS MYERS-RM8-TOT PG5

  else if (VOLEQ(8:10).eq.'122' .and. VOLEQ(1:3).eq.'213') then

     if ( D2H .le.6000. ) then
        GCUFT = 0.002213 * D2H + 0.030288
     else
        GCUFT = 0.002474 * D2H - 1.557103
     end if

     !--**************  PONDEROSA PINE FRONT RANGE-RM218-TOTAL PG4  TOTAL CUBIC

  else if (VOLEQ(8:10).eq.'122' .and. VOLEQ(1:3).eq.'200') then

     TCUFT = 0.00226 * D2H

     !--**************  PONDEROSA PINE FRONT RANGE-RM218-TOP=6   BOARD FOOT
     if (DBHOB.gt.7) then

        if ( D2H .le.2830. ) then
           grsbdt = 8
        else
           grsbdt = 0.01149 * D2H - 24.5404
        end if
        !           ADDED CALCULATION OF INTERNATIONAL BD YW 2018/09/04
        if ( D2H .le.2535. ) then
           VOL(10) = 9.0
        else
           VOL(10) = .01286*D2H-23.5932
        end if
     end if
     !--**************  PONDEROSA PINE FRONT RANGE-RM218-TOP-4

     GCUFT = 0.00216 * D2H - 0.44670

     ! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
     if (PROD.eq.'01'.and.MTOPP.ge.6.0) then
        if (DBHOB.lt.9.0) then
           SPF = 0.0
        else if (DBHOB.gt.42.0) then
           SPF = .93
        else
           SPF = .95-(.26*((42.0-DBHOB)/33.0)**7.0)
        end if
        CV6 = SPF*GCUFT
        if (CV6.gt.0.0) then
           TOPWOOD = GCUFT-CV6
           VOL(7) = TOPWOOD
           GCUFT = CV6
        end if
     end if

     !--**************  PONDEROSA PINE FRONT RANGE-RM218-TOTAL PG4

  else if (VOLEQ(8:10).eq.'122' .and. VOLEQ(1:3).eq.'210') then

     GCUFT = 0.00226 * D2H

     !--**************  ENGELMAN SPRUCE-RM95-TOTAL PG5  TOTAL CUBIC

  else if (VOLEQ(8:10).eq.'093' .and. VOLEQ(1:3).eq.'200') then

     if ( D2H .le.22500. ) then
        TCUFT = 0.00239 * D2H + 0.06439
     else
        TCUFT = 0.00193 * D2H + 10.41663
     end if

     !--**************  ENGELMANN SPRUCE-RM95-TOP=6   BOARD FOOT

     if (DBHOB.gt.7) then

        if ( D2H .le.12200. ) then
           grsbdt = 0.01097 * D2H - 15.14466
        else
           grsbdt = 0.01202 * D2H - 27.91343
        end if
        if (D2H.le.27300.0) then
           VOL(10) = .01391*D2H-25.61022
        else
           VOL(10) = .01235*D2H+17.02079
        end if
     end if

     !--**************  ENGELMAN SPRUCE-RM95-TOP=4

     if ( D2H .le.27900. ) then
        GCUFT = 0.00232 * D2H - 0.83010
     else
        GCUFT = 0.00182 * D2H + 13.11320
     end if
     ! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
     if (PROD.eq.'01'.and.MTOPP.ge.6.0) then
        if (DBHOB.lt.9.0) then
           SPF = 0.0
        else if (DBHOB.gt.42.0) then
           SPF = .98
        else
           SPF = .96-(.15*((42.0-DBHOB)/33.0)**3.0)
        end if
        CV6 = SPF*GCUFT
        if (CV6.gt.0.0) then
           TOPWOOD = GCUFT-CV6
           VOL(7) = TOPWOOD
           GCUFT = CV6
        end if
     end if

     !--**************  ENGELMAN SPRUCE-RM95-TOTAL PG5

  else if (VOLEQ(8:10).eq.'093' .and. VOLEQ(1:3).eq.'210') then

     if ( D2H .le.22500. ) then
        GCUFT = 0.00239 * D2H + 0.06439
     else
        GCUFT = 0.00193 * D2H + 10.41663
     end if

     !--**************  PONDEROSA PINE BLACK HILLS MYERS-RM8-TOT PG5  TOTAL CUBIC

  else if (VOLEQ(8:10).eq.'122' .and. VOLEQ(1:3).eq.'212') then

     if ( D2H .le.6000. ) then
        TCUFT = 0.002213 * D2H + 0.030288
     else
        TCUFT = 0.002474 * D2H - 1.557103
     end if

     !--**************  PONDEROSA PINE BLACK HILLS MYERS-RM8-TOP=8   BOARD FOOT
     if (DBHOB.gt.7) then

        if ( D2H .le.16000. ) then
           grsbdt = 0.012331 * D2H - 34.167170
        else
           grsbdt = 0.016318 * D2H - 99.212720
        end if
        if (D2H.le.13000.0) then
           VOL(10) = .015011*D2H-44.36046
        else
           VOL(10) = .016991*D2H-68.7502
        end if
     end if
     !--**************  PONDEROSA PINE BLACK HILLS MYERS-RM8-TOP=4

     if ( D2H .le.6700. ) then
        GCUFT = 0.002297 * D2H - 1.032297
     else
        GCUFT = 0.002407 * D2H - 2.257724
     end if
     ! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
     if (PROD.eq.'01'.and.MTOPP.ge.6.0) then
        if (DBHOB.lt.9.0) then
           SPF = 0.0
        else if (DBHOB.gt.42.0) then
           SPF = .93
        else
           SPF = .95-(.26*((42.0-DBHOB)/33.0)**7.0)
        end if
        CV6 = SPF*GCUFT
        if (CV6.gt.0.0) then
           TOPWOOD = GCUFT-CV6
           VOL(7) = TOPWOOD
           GCUFT = CV6
        end if
     end if

     !--**************  Oneseed Juniper Chojnacky INT-339
  else if (VOLEQ(8:10).eq.'069') then
     !     look for drc, if not use dbhob
     if (DRC .gt.0) D2H = DRC*DRC*HTTOT

     TCUFT = (-0.19321+0.136101*D2H**(1./3.)+0.038187*MSTEM)**3.
     GCUFT = TCUFT

     !--**************  Rocky Mountain Juniper Chojnacky INT-339
  else if (VOLEQ(8:10).eq.'066') then
     !     look for drc, if not use dbhob
     if (DRC .gt.0) D2H = DRC*DRC*HTTOT

     TCUFT = (0.02434+0.119106*D2H**(1./3.))**3.
     GCUFT = TCUFT

     !--**************  Utah Juniper Chojnacky INT-339
  else if (VOLEQ(8:10).eq.'065') then
     !     look for drc, if not use dbhob
     if (DRC .gt.0) D2H = DRC*DRC*HTTOT

     TCUFT = (-0.08728+0.135420*D2H**(1./3.)-0.019587*MSTEM)**3.
     GCUFT = TCUFT

     !--**************  Gambel Oak Chojnacky INT-339
  else if (VOLEQ(8:10).eq.'814') then
     !     look for drc, if not use dbhob
     if (DRC .gt.0) D2H = DRC*DRC*HTTOT

     TCUFT = (-0.13600 + 0.145743*D2H**(1./3.))**3.
     GCUFT = TCUFT

     !--**************  Bur Oak Chojnacky INT-339
  else if (VOLEQ(8:10).eq.'823') then
     !     look for drc, if not use dbhob
     if (DRC .gt.0) D2H = DRC*DRC*HTTOT

     TCUFT = (0.12853 + 0.105885*D2H**(1./3.))**3.
     GCUFT = TCUFT

     !--**************  Pinyon Pine
  else if (VOLEQ(8:10).eq.'106') then
     !     look for drc, if not use dbhob
     if (DRC .gt.0) D2H = DRC*DRC*HTTOT

     TCUFT = (-0.20296 + 0.150283*D2H**(1./3.)+0.054178*MSTEM)**3.
     GCUFT = TCUFT

     !--**************  Mountain Mahogany Chojnacky INT-339
  else if (VOLEQ(8:10).eq.'475') then
     !     look for drc, if not use dbhob
     if (DRC .gt.0) D2H = DRC*DRC*HTTOT

     TCUFT = (-0.13363 + 0.128222*D2H**(1./3.)+0.080208*MSTEM)**3.
     !Set to 0.1 for tree with DIA < 3 to match FIA (20240426)
     if (DRC.lt.3.and.DBHOB.lt.3) TCUFT = 0.1
     GCUFT = TCUFT

     !--**************  Other Hardwoods Chojnacky INT-339
  else if (VOLEQ(8:10).eq.'998') then
     !     look for drc, if not use dbhob
     if (DRC .gt.0) D2H = DRC*DRC*HTTOT

     TCUFT = (-0.13822 + 0.121850*D2H**(1./3.))**3.
     GCUFT = TCUFT

     !--       (ENDIF FOR VOLEQU EQUAL TO A VALID EQUATION)
  else
     ERRFLAG = 1
  end if

  if (TCUFT .lt.0.0) then
     TCUFT = 0.0
  end if
  VOL(1) = TCUFT

  if (grsbdt .lt.0.0) then
     grsbdt = 0.0
  end if
  VOL(2) = grsbdt

  if (GCUFT .lt.0.0) then
     GCUFT = 0.0
  end if
  VOL(4) = GCUFT

  1000 continue

  return
end

  ! Vol Equation 223DVEW122 for R2 Black Hills Ponderosa pine Non-saw prod
subroutine BH_NonSawPP(DBHOB,HTTOT,STUMP,MTOPP,MTOPS,&
     &HT1PRD,HT2PRD,VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,LOGST,&
     &NOLOGP,NOLOGS,SPFLG,PROD,ERRFLG)
  implicit none
  ! Variables
  real    :: DBHOB,HTTOT,STUMP,MTOPP,MTOPS,HT1PRD,HT2PRD
  real    :: VOL(15),LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3),BOLHT(21)
  real    :: NOLOGP,NOLOGS,LOGLENT(20)
  integer :: LOGST,SPFLG,ERRFLG,I
  character(len=2) :: PROD
  real    :: TCUFT
  ! MRULE VARIABLES
  character(len=10) :: VOLEQ
  character(len=1) :: COR
  character(len=2) :: FORST
  integer :: EVOD,OPT,REGN,NUMSEG
  real    :: MAXLEN,MINLEN,MERCHL,TRIM
  real    :: MINLENT,MINBFD,BTR,DBTBH,LMERCH
  real    :: a,b,HTLOW,HT2,DIB,DIBL

  if (DBHOB.lt.1.0) then
     ERRFLG = 3
     return
  end if
  if (HTTOT.lt.4.5) then
     ERRFLG = 4
     return
  end if
  LOGST = 0
  NOLOGP = 0
  NOLOGS = 0
  VOL = 0.0
  LOGLEN = 0.0
  LOGVOL = 0.0
  LOGDIA = 0.0
  BOLHT = 0.0
  LOGLENT = 0.0
  call BH_PPTCU(DBHOB,HTTOT,TCUFT)
  VOL(1) = TCUFT

  if (MTOPP.lt.0.1) MTOPP = 4.0
  if (MTOPS.lt.0.1) MTOPS = 2.0
  REGN = 2
  FORST = '03'
  VOLEQ = '223DVEW122'
  BTR = 0.0
  DBTBH = 0.0
  HT2 = STUMP
  NUMSEG = 0
  call MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,&
       &MINLEN,MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,&
       &MINBFD,PROD)
  call GetRatioCoef(a,b)
  if (HT1PRD.lt.0.1) then
     HTLOW = 0.0
     call GetHT2TOPD2(TCUFT,a,b,HTTOT,MTOPP,HT1PRD,HTLOW)
  end if
  LMERCH = HT1PRD - STUMP
  if (LMERCH.ge.MERCHL) then
     !Added max number of logs (20) check (20250626)
     if ((LMERCH/(MAXLEN+TRIM)).gt.20) then
        ERRFLG = 12
        return
     end if
     call NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG)
     call SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG,LOGLEN)
     LOGST = 0
     HT2 = 4.5
     call GetHTDIB2(TCUFT,a,b,HTTOT,HT2,DIB)
     LOGDIA(1,2) = DIB
     LOGDIA(1,1) = NINT(DIB)
     BOLHT(1) = HT2
     !--   USE DIB AT DBHOB FOR LARGE END BUTT LOG
     DIBL = LOGDIA(1,1)
     HT2 = STUMP
     call CalcLOGVOL(LOGST,NUMSEG,DIBL,HT2,TCUFT,TRIM,HTTOT,&
          &LOGLEN,LOGDIA,LOGVOL,BOLHT,VOL,COR)
     LOGST = NUMSEG
     NOLOGP = NUMSEG
  end if
  !     Calculate topwood volume
  if (SPFLG.eq.1.and.MTOPS.lt.MTOPP) then
     if (HT2PRD.lt.0.1) then
        call GetHT2TOPD2(TCUFT,a,b,HTTOT,MTOPS,HT2PRD,HT2)
     end if
     LMERCH = HT2PRD - HT2
     if (LMERCH.ge.MINLENT) then
        NUMSEG = 0
        !Added max number of logs (20) check (20250626)
        if ((LMERCH/(MAXLEN+TRIM)).gt.20) then
           ERRFLG = 12
           return
        end if
        call NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG)
        call SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG,LOGLENT)
        NOLOGS = NUMSEG
        !Add secondary log length to LOGLEN
        do I = 1, NUMSEG
           LOGLEN(I+LOGST) = LOGLENT(I)
           600 continue
        end do
        call CalcLOGVOL(LOGST,NUMSEG,DIBL,HT2,TCUFT,TRIM,HTTOT,&
             &LOGLEN,LOGDIA,LOGVOL,BOLHT,VOL,COR)
        LOGST = LOGST + NUMSEG
     end if
  end if
  return
end

subroutine BH_PPTCU(DBHOB,HTTOT,TCUFT)
  implicit none
  ! Variables
  real    :: DBHOB,HTTOT,TCUFT
  real    :: a0,b0,c0
  !         Total cubic volume equation vol=a*DBH^b*THT^c
  !         The coefficients was developed from sample trees collected in
  !         Black Hills volume and biomass study in 2022
  a0 = 0.003573
  b0 = 1.89865505
  c0 = 0.95409984
  TCUFT = a0*DBHOB**b0*HTTOT**c0
  return
end

subroutine GetRatioCoef(a,b)
  implicit none
  !     Coefficients for the ratio of volume to a given height to total vol  using the method
  !     developed by Phil Radtke from Virginia Tech for Black Hills
  ! Variables
  real    :: a,b
  a = 2.187093
  b = 0.9399898
end

subroutine BH_PPRATIO(HTTOT,HT2,R)
  implicit none
  ! Variables
  real    :: HTTOT,HT2,R,X
  real    :: a,b
  !     Calculate the ratio of volume to a given height to total vol  using the method
  !     developed by Phil Radtke from Virginia Tech for Black Hills
  call GetRatioCoef(a,b)
  X = HT2/HTTOT
  R = (1.0-(1.0-X)**a)**b
  return
end

  ! Calculate height to a given TOPD
subroutine GetHT2TOPD(DBHOB,HTTOT,TOPD,MHT,HTLOW)
  implicit none
  ! Variables
  real    :: DBHOB,HTTOT,TOPD,MHT,HTLOW
  real    :: R,low,hi,diff,X,a,b,TCUFT,mid
  integer :: loopcnt

  ! Get total cuft vol
  call BH_PPTCU(DBHOB,HTTOT,TCUFT)
  !     Ratio coefficients developed by Phil Radtke from Virginia Tech
  call GetRatioCoef(a,b)
  call GetHT2TOPD2(TCUFT,a,b,HTTOT,TOPD,MHT,HTLOW)
  return
end

  ! Calculate height to a given TOPD
subroutine GetHT2TOPD2(TCUFT,a,b,HTTOT,TOPD,MHT,HTLOW)
  implicit none
  ! Variables
  real    :: HTTOT,TOPD,MHT,HTLOW
  real    :: low,hi,diff,X,a,b,TCUFT,mid
  integer :: loopcnt

  low = HTLOW
  hi = HTTOT
  diff = 1.0
  loopcnt = 0
  do while (ABS(diff).gt.0.01)
     mid = (low+hi)/2
     X = mid/HTTOT
     diff = TOPD - ((TCUFT/0.005454154/HTTOT*a*b*&
          &(1-X)**(a-1)*(1-(1-X)**a)**(b-1)))**0.5
     if (ABS(diff).lt.0.05) then
        exit
     end if
     if (diff.lt.0.0) then
        low = mid
     else
        hi = mid
     end if
     loopcnt = loopcnt +1
     if (loopcnt.gt.1000) exit
  end do
  MHT = mid

  return
end

  ! Calculate DIB at a given height HT2
subroutine GetHTDIB(DBHOB,THT,HT2,DIB)
  implicit none
  ! Variables
  real    :: DBHOB,HT2,DIB,a,b,TCUFT,THT

  call BH_PPTCU(DBHOB,THT,TCUFT)
  call GetRatioCoef(a,b)
  call GetHTDIB2(TCUFT,a,b,THT,HT2,DIB)
  return
end

  ! Calculate DIB at a given height HT2
subroutine GetHTDIB2(TCUFT,a,b,THT,HT2,DIB)
  implicit none
  ! Variables
  real    :: HT2,DIB,a,b,TCUFT,THT

  DIB = SQRT((TCUFT/ 0.005454 / THT) * (a * b *&
       &(1-HT2/THT)**(a-1)*(1-(1 - HT2/THT)**a)**(b-1)))
  return
end

  ! Calculate LOGDIA, LOGVOL, BOLHT and VOL
subroutine CalcLOGVOL(LOGST,NUMSEG,DIBL,HT2,TCUFT,TRIM,THT,&
     &LOGLEN,LOGDIA,LOGVOL,BOLHT,VOL,COR)
  implicit none
  ! Variables
  integer :: LOGST,NUMSEG,I
  real    :: DIBL,HT2,TCUFT,TRIM,DIB,a,b,THT,DIBS,LOGCV,LENTH,LOGV,BFINT
  real    :: LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3),BOLHT(21),VOL(15)
  character(len=1) :: COR

  call GetRatioCoef(a,b)
  do I = 1+LOGST,NUMSEG+LOGST
     HT2 = HT2+TRIM+LOGLEN(I)
     call GetHTDIB2(TCUFT,a,b,THT,HT2,DIB)
     LOGDIA(I+1,2) = DIB
     LOGDIA(I+1,1) = NINT(DIB)
     BOLHT(I+1) = HT2
     DIBS = LOGDIA(I+1,1)
     LOGCV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LOGLEN(I)
     LOGVOL(4,I) = ANINT(LOGCV*10)/10
     DIBL = LOGDIA(I+1,1)
     !Calculate boardfoot volume
     DIB = LOGDIA(I+1,1)
     LENTH = LOGLEN(I)
     call SCRIB (DIB,LENTH,COR,LOGV)
     if (cor.eq.'Y') then
        LOGVOL(1,I) = LOGV * 10
     else
        LOGVOL(1,I) = ANINT(LOGV)
     end if
     call INTL14(DIB,LENTH,BFINT)
     LOGVOL(7,I) = BFINT
     if (LOGST.gt.0) then
        VOL(7) = VOL(7)+LOGVOL(4,I)
        VOL(12) = VOL(12) + LOGVOL(1,I)
     else
        VOL(4) = VOL(4)+LOGVOL(4,I)
        VOL(2) = VOL(2) + LOGVOL(1,I)
        VOL(10) = VOL(10) + BFINT
     end if
     500 continue
  end do

  return
end
  !********BOARD FOOT VOLUME EQUATION NUMBERS********

  !--    201 =  ASPEN RM-232 - 6" TOP - PAGE 6
  !--    202 = LODGEPOLE PINE RESEARCH NOTE RM-157 - 6" TOP
  !--    203 = PONDEROSA PINE BLACK HILLS PROSSER - 6" TOP
  !--    204 = PONDEROSA PINE FRONT RANGE RM-218 - 6" TOP - PAGE 7
  !--    207 = ENGELMANN SPRUCE RM-95 - 6" TOP - PAGE 8
  !--    212 = PONDEROSA PINE BLACK HILLS MYERS RM-8-8" TOP - PG 8
  !--    FOR LIMBER PINE USE EQUATION 204
  !--    FOR SOUTHWEST PONDEROSA PINE USE EQUATION 204
  !--    FOR DOUGLAS FIR USE EQUATION 207
  !--    FOR TRUE FIRS USE EQUATION 207

  !********CUBIC FOOT VOLUME EQUATION NUMBERS********

  !--    201 =  ASPEN RM-232 - 4" TOP - PAGE 4
  !--    202 = LODGEPOLE PINE RM-6 - 4" TOP - PAGE 6
  !--    203 = PONDEROSA PINE MYERS RM-8-4" TOP
  !--    204 = PONDEROSA PINE FRONT RANGE RM-218 - 4" TOP - PAGE 5
  !--    207 = ENGELMANN SPRUCE RM-95 - 4" TOP - PAGE 6
  !--    212 = PONDEROSA PINE BLACK HILLS MYERS RM-8-4" TOP - PG 6
  !--    FOR LIMBER PINE USE EQUATION 204
  !--    FOR SOUTHWEST PONDEROSA PINE USE EQUATION 204
  !--    FOR DOUGLAS FIR USE EQUATION 207
  !--    FOR TRUE FIRS USE EQUATION 207

  !*****CUBIC FOOT BIOMASS GROUND TO TIP VOLUME EQUATION NUMBERS
  !--    221 =  ASPEN RM-232 - PAGE 3
  !--    222 = LODGEPOLE PINE RESEARCH NOTE RM-6 - PAGE 5
  !--    223 = PONDEROSA PINE BLACK HILLS MYERS RM-8 - PAGE 5
  !--    224 = PONDEROSA PINE FRONT RANGE RM-218 - PAGE 4
  !--    227 = ENGELMANN SPRUCE RM-95 - PAGE 5
  !--    FOR LIMBER PINE USE EQUATION 204
  !--    FOR SOUTHWEST PONDEROSA PINE USE EQUATION 204
  !--    FOR DOUGLAS FIR USE EQUATION 207
  !--    FOR TRUE FIRS USE EQUATION 207

  !--  HTTOT - REAL -  **TREE HEIGHT IN FT. FROM A 1 FOOT STUMP**
