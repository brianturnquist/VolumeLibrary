!===========================!
!   VolumeLibrary : r9vol   !
!===========================!
  !== last modified  12-17-2008
subroutine R9VOL(VOLEQ,HTTOT,HT1PRD,HT2PRD,DBHOB,VOL,FORST,SI,BA,&
     &PROD,CTYPE,BFPFLG,CUPFLG,CDPFLG,SPFLG,HTTYPE,ERRFLAG,MTOPP)

  ! CREATED  : 01-30-90
  ! YW 07/13/2018 added MTOPP variable to the routine
  ! PURPOSE  : THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE
  !            USING REGION 9 VOLUME DETERMINATION ROUTINES.
  !            Gevorkiantz Model
  !            ADDED A FUNCTION TO FIND MERCH HTS GIVEN TOTAL HEIGHT

  !       DECLARE VARIBLES         *

  ! Variables
  character(len=10) :: VOLEQ
  character(len=1) :: CTYPE
  character(len=1) :: HTTYPE
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=2) :: TMPSTR
  character(len=3) :: VOLSPP
  integer :: BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG,I,SI,BA,IFORST

  real    :: VOL(15),DBHOB,R,VC,CF,HT1PRD,HT2PRD,HTTOT,MTOPP
  real    :: GCB,P,PT,CRD
  real*8  :: term1,term2,term3

  do 100, I = 1,15
  VOL(I) = 0.0
  100 continue
  ERRFLAG = 0
  if (DBHOB.lt.1.0) then
     ERRFLAG = 3
     return
  end if
  !     ADDED CALC VOLUME FOR ENTIRE STEM INCLUDE STUMP AND TIP (YW 2018/09/11)
  if (HTTOT.gt.0.0) then
     VOL(1) = 0.42*3.141592*(1.0/144.0)*(1.0/4.0)*DBHOB*DBHOB*HTTOT
     !       SET A SMALL NUMBER FOR STUMP TO AVOID RECALC
     VOL(14) = 0.001
  end if
  if (FORST(2:2) .lt. '0') then
     FORST(2:2) = FORST(1:1)
     FORST(1:1) = '0'
     if (FORST(2:2) .lt. '0') FORST(2:2) = '0'
  end if

  read(FORST,'(i2)') IFORST
  if (VOLEQ(10:10).lt.'0' .or. VOLEQ(10:10).gt.'9') then
     TMPSTR = VOLEQ(8:9)
     VOLEQ(8:10) = '0'//TMPSTR
  end if
  VOLSPP = VOLEQ(8:10)

  !*    CHECK FOR VALID HEIGHTS
  if (HT1PRD.le.0.and. HT2PRD.le.0.and. HTTOT.le.0) then
     if (PROD.eq.'01') then
        ERRFLAG = 7
     else
        ERRFLAG = 8
     end if
     return
  end if

  !     MAKE SURE MERCH HEIGHTS ARE IN LOGS
  if (CTYPE.eq.'F') then
     if (HT1PRD.le.0.and. HT2PRD.le.0) then
        call R9_MHTS(IFORST,VOLEQ,DBHOB,HTTOT,SI,BA,HT1PRD,HT2PRD,&
             &PROD,ERRFLAG,MTOPP)
     else if (HTTYPE.ne.'L' .and. HTTYPE.ne.'l') then
        HT1PRD = INT((HT1PRD+3.0)/8.25)
        HT2PRD = INT((HT2PRD+3.0)/8.25)
     end if
  else
     HT1PRD = INT((HT1PRD+3.0)/8.25)
     if (HT2PRD.gt.0) then
        HT2PRD = INT((HT2PRD+3.0)/8.25)
     else if (HTTOT.gt.0) then
        HT2PRD = INT((HTTOT-12)/8.25)
     end if
  end if

  !               BOARD FOOT MAIN STEM EQUATIONS            *

  !*  SET CORRECTION FACTOR **
  CF = 1.0

  !*************----------------- TABLE A
  if (BFPFLG.eq.1.and.HT1PRD.gt.0) then
     !*  CALCULATE R AND VOLUME OF A TRUNCATED CONE **
     R = ((HT1PRD*DBHOB-3.75)/(24.0*HT1PRD-10.5))
     VC = HT1PRD*(1.0757+3.002*R+8.3776*R**2)

     if (VOLEQ(1:3).eq.'901'.or.IFORST.eq.2.or.IFORST.eq.3&
          &.or.IFORST.eq.6.or.IFORST.eq.7.or.IFORST.eq.9&
          &.or.IFORST.eq.10.or.IFORST.eq.13) then
        if (VOLSPP.eq.'094') then
           CF = 0.90
        else if (VOLSPP.eq.'105') then
           CF = 0.92
        else if (VOLSPP.eq.'125') then
           if (IFORST.eq.3) then
              CF = 0.91
           else
              CF = 0.96
           end if
        else if (VOLSPP.eq.'129') then
           if (IFORST.eq.3) then
              CF = 0.90
           else
              CF = 0.96
           end if
        else if (VOLSPP.eq.'241') then
           CF = 0.80
        else if (VOLSPP.eq.'261') then
           CF = 0.95
        else if (VOLSPP.eq.'318') then
           CF = 0.98
        else if (VOLSPP.eq.'375') then
           CF = 0.93
        else if (VOLSPP.eq.'531') then
           CF = 1.15
        else if (VOLSPP.eq.'541') then
           CF = 1.03
        else if (VOLSPP.eq.'951') then
           CF = 0.97
        else if (VOLSPP.eq.'970') then
           CF = 1.05
        end if
        VOL(2) = 5.527702-(4.22*VC)-(2.4082*DBHOB)+(5.4072*HT1PRD)
        VOL(2) = VOL(2)+(.2114*(DBHOB**2*HT1PRD))-(.48097*HT1PRD**2)-&
             &(.00605*VC**2)+(.291865*DBHOB**2)+&
             &(.00501*(DBHOB**2*HT1PRD**2))-&
             &(.00039606323*(DBHOB**2*HT1PRD**3))+&
             &(.0000013109952*(DBHOB**4*HT1PRD**2))
        VOL(2) = VOL(2)*CF
        !*************----------------- TABLE B
     else if (VOLEQ(1:3).eq.'902'.or.IFORST.eq.4.or.IFORST.eq.5&
          &.or.IFORST.eq.8.or.IFORST.eq.11.or.IFORST.eq.12&
          &.or.IFORST.eq.14.or.IFORST.eq.24) then
        if (VOLSPP.eq.'400') then
           CF = 1.06
        else if (VOLSPP.eq.'602') then
           CF = 0.90
        else if (VOLSPP.eq.'621') then
           CF = 1.10
        else if (VOLSPP.eq.'694') then
           CF = 1.15
        else if (VOLSPP.eq.'731') then
           CF = 0.93
        else if (VOLSPP.eq.'742') then
           CF = 0.97
        else if (VOLSPP.eq.'824') then
           CF = 0.80
        else if (VOLSPP.eq.'830') then
           CF = 0.96
        else if (VOLSPP.eq.'832') then
           CF = 1.03
        else if (VOLSPP.eq.'068'.and.IFORST.ne.4) then
           CF = 0.80
        else if (VOLSPP.eq.'110'.and.&
             &(IFORST.eq.8.or.IFORST.eq.5)) then
           CF = 0.95
        else if (VOLSPP.eq.'125'.and.IFORST.eq.4) then
           CF = 0.96
        else if (VOLSPP.eq.'129') then
           if (IFORST.eq.11.or. IFORST.eq.14.or.&
                &IFORST.eq.12)    CF = 0.95
           if (IFORST.eq.4) CF = 0.96
        else if (VOLSPP.eq.'241'.and.(IFORST.eq.4)) then
           CF = 0.80
        else if (VOLSPP.eq.'802') then
           if (IFORST.eq.8) CF = 1.08
           if (IFORST.eq.5) CF = 0.96
        else if (VOLSPP.eq.'806') then
           if (IFORST.eq.5) then
              CF = 1.03
           else
              CF = 1.10
           end if
        else if (VOLSPP.eq.'833') then
           if (IFORST.eq.8) then
              CF = 1.11
           else
              CF = 1.06
           end if
        else if (VOLSPP.eq.'835') then
           if (IFORST.eq.5) then
              CF = 0.94
           else
              CF = 0.98
           end if
        else if (VOLSPP.eq.'837') then
           if (IFORST.eq.8) CF = 1.05
           if (IFORST.eq.5) CF = 0.96
           if (IFORST.eq.4) CF = 0.95
        end if

        VOL(2) = (-.092685)-(5.98*VC)-(2.9715*DBHOB)+(16.7022*HT1PRD)
        VOL(2) = VOL(2)+(.2471*(DBHOB**2*HT1PRD))-(.91751*HT1PRD**2)-&
             &(.00876*VC**2)+(.351046*DBHOB**2)+&
             &(.00451*(DBHOB**2*HT1PRD**2))-&
             &(.00030183475*(DBHOB**2*HT1PRD**3))+&
             &(.0000019222413*(DBHOB**4*HT1PRD**2))
        VOL(2) = VOL(2)*CF
        VOL(10) = VOL(2)
        !*************----------------- TABLE C
     else if (VOLEQ(1:3).eq.'903'.or.IFORST.eq.19) then
        if (VOLSPP.eq.'129') then
           CF = 1.06
        else if (VOLSPP.eq.'261') then
           CF = 0.88
        else if (VOLSPP.eq.'531') then
           CF = 1.09
        else if (VOLSPP.eq.'541'.or.VOLSPP.eq.'543'&
             &.or.VOLSPP.eq.'621'.or.VOLSPP.eq.'951') then
           CF = 0.91
        else if (VOLSPP.eq.'806'.or.VOLSPP.eq.'813'&
             &.or.VOLSPP.eq.'823'.or.VOLSPP.eq.'824'&
             &.or.VOLSPP.eq.'830'.or.VOLSPP.eq.'833'&
             &.or.VOLSPP.eq.'835'.or.VOLSPP.eq.'837') then
           CF = 0.97
        end if

        VOL(2) = 12.900801+(4.48*VC)+(1.2218*DBHOB)-(18.267*HT1PRD)
        VOL(2) = VOL(2)+(.1993*(DBHOB**2*HT1PRD))-(.37290*HT1PRD**2)+&
             &(.02714*VC**2)-(.15071*DBHOB**2)-&
             &(.01703*(DBHOB**2*HT1PRD**2))+&
             &(.00039033000*(DBHOB**2*HT1PRD**3))-&
             &(.0000066236*(DBHOB**4*HT1PRD**2))
        VOL(2) = VOL(2)*CF
        VOL(10) = VOL(2)
        !*************----------------- TABLE D
     else if (VOLEQ(1:3).eq.'904'.or.IFORST.eq.20.or.IFORST.eq.22&
          &.or.IFORST.eq.30) then
        if (VOLSPP.eq.'012'.or.VOLSPP.eq.'261') then
           CF = 0.86
        else if (VOLSPP.eq.'094'.or.VOLSPP.eq.'097'&
             &.or.VOLSPP.eq.'125'.or.VOLSPP.eq.'129') then
           CF = 0.97
        else if (VOLSPP.eq.'316'.or.VOLSPP.eq.'746'&
             &.or.VOLSPP.eq.'762'.or.VOLSPP.eq.'833'&
             &.or.VOLSPP.eq.'951'.or.VOLSPP.eq.'970') then
           CF = 0.94
        else if (VOLSPP.eq.'375'.or.VOLSPP.eq.'541'&
             &.or.VOLSPP.eq.'376') then
           CF = 0.92
        else if (VOLSPP.eq.'371') then
           if (IFORST.eq.20) then
              CF = 0.92
           else if (IFORST.eq.22) then
              CF = 0.97
           end if
        end if
        VOL(2) = (-3.592279)-(2.74*VC)-(.6274*DBHOB)+(1.5333*HT1PRD)
        VOL(2) = VOL(2)+(.2697*(DBHOB**2*HT1PRD))+(.10400*HT1PRD**2)+&
             &(.00009*VC**2)+(.14129*DBHOB**2)-&
             &(.01104*(DBHOB**2*HT1PRD**2))+&
             &(.00029155*(DBHOB**2*HT1PRD**3))-&
             &(.00000007493*(DBHOB**4*HT1PRD**2))
        VOL(2) = VOL(2)*CF
        VOL(10) = VOL(2)
        !*************----------------- TABLE E
     else if (VOLEQ(1:3).eq.'905'.or.&
          &(IFORST.eq.21.and. VOLSPP.ge.'300')) then
        if (VOLSPP.eq.'316'.or.VOLSPP.eq.'371'.or.VOLSPP.eq.'802'&
             &.or.VOLSPP.eq.'951') then
           CF = 1.03
        else if (VOLSPP.eq.'318') then
           CF = 1.09
        else if (VOLSPP.eq.'373') then
           CF = 0.94
        else if (VOLSPP.eq.'531') then
           CF = 1.12
        else if (VOLSPP.eq.'541') then
           CF = 0.97
        else if (VOLSPP.eq.'621'.or.VOLSPP.eq.'651'&
             &.or.VOLSPP.eq.'261') then
           CF = 1.06
        else if (VOLSPP.eq.'833') then
           CF = 0.91
        end if
        VOL(2) = 16.775055+(4.06*VC)+(.8709*DBHOB)-(18.343*HT1PRD)
        VOL(2) = VOL(2)+(.1940*(DBHOB**2*HT1PRD))-(.19833*HT1PRD**2)+&
             &(.02401*VC**2)-(.13224*DBHOB**2)-&
             &(.0163*(DBHOB**2*HT1PRD**2))+&
             &(.00037154*(DBHOB**2*HT1PRD**3))-&
             &(.0000057358*(DBHOB**4*HT1PRD**2))
        VOL(2) = VOL(2)*CF
        VOL(10) = VOL(2)
        !*************----------------- TABLE F
     else if (VOLEQ(1:3).eq.'906'.or.&
          &(IFORST.eq.21.and.VOLSPP.lt.'300')) then
        if (VOLSPP.eq.'097') then
           CF = 1.15
        else if (VOLSPP.eq.'129') then
           CF = 0.94
        end if
        VOL(2) = 14.522237-(9.40*VC)-(.415*DBHOB)+(15.639*HT1PRD)
        VOL(2) = VOL(2)+(.3655*(DBHOB**2*HT1PRD))+(.00563*HT1PRD**2)-&
             &(.01959*VC**2)+(.3088*DBHOB**2)-&
             &(.00844*(DBHOB**2*HT1PRD**2))+&
             &(.00030875*(DBHOB**2*HT1PRD**3))+&
             &(.0000055105*(DBHOB**4*HT1PRD**2))
        VOL(2) = VOL(2)*CF
        VOL(10) = VOL(2)
     end if
  end if

  !           CUBIC FOOT MAIN STEM EQUATIONS                *

  !*************** CUBIC VOLUME FOR AN ALL PULPWOOD TREE
  if (CUPFLG.eq.1) then
     if ((VOLEQ(1:3).eq.'911'.or.PROD.ne.'01').and.HT2PRD.gt.0) then

        VOL(4) = (0.001*DBHOB**2)*(1.9+(0.01*DBHOB))*((0.208*HT2PRD)-&
             &(0.009984*HT2PRD**2)+(0.04/HT2PRD))*79

        !*************** CUBIC VOLUME FOR A SAWTIMBER TREE

     else if ((VOLEQ(1:3).eq.'912'.or.PROD.eq.'01').and.&
          &HT1PRD.gt.0) then
        if (VOLSPP.eq.'071'.or.VOLSPP.eq.'094'.or.&
             &VOLSPP.eq.'095'.or.VOLSPP.eq.'097'.or.&
             &VOLSPP.eq.'105'.or.VOLSPP.eq.'241'.or.&
             &VOLSPP.eq.'460'.or.VOLSPP.eq.'543'.or.&
             &VOLSPP.eq.'601'.or.VOLSPP.eq.'602'.or.&
             &VOLSPP.eq.'731'.or.VOLSPP.eq.'742'.or.&
             &VOLSPP.eq.'823'.or.VOLSPP.eq.'824') then
           CF = 0.95
        else if (VOLSPP.eq.'400'.or.VOLSPP.eq.'404'.or.&
             &VOLSPP.eq.'651'.or.VOLSPP.eq.'694'.or.&
             &VOLSPP.eq.'813'.or.VOLSPP.eq.'830') then
           CF = 1.05
        else if (VOLSPP.eq.'531') then
           CF = 1.10
        else if (VOLSPP.eq.'920') then
           CF = 0.90
        else if (VOLSPP.eq.'970') then
           CF = 1.08
        else if (DBHOB.lt.13.0.and.VOLSPP.eq.'110') then
           CF = 1.06
        else if (DBHOB.lt.15.0) then
           if (VOLSPP.eq.'621'.or.VOLSPP.eq.'746') then
              CF = 1.03
           else if (VOLSPP.eq.'125') then
              CF = 1.04
           else if (VOLSPP.eq.'837') then
              CF = 1.05
           else if (VOLSPP.eq.'835'.or.VOLSPP.eq.'951') then
              CF = 1.06
           else if (VOLSPP.eq.'371'.or.VOLSPP.eq.'833') then
              CF = 1.08
           else if (VOLSPP.eq.'129'.or.VOLSPP.eq.'318'.or.&
                &VOLSPP.eq.'802') then
              CF = 1.10
           else if (VOLSPP.eq.'806') then
              CF = 1.11
           else if (VOLSPP.eq.'375') then
              CF = 1.12
           else if (VOLSPP.eq.'762') then
              CF = 1.16
           else if (VOLSPP.eq.'316') then
              CF = 1.18
           end if
        else
           if (VOLSPP.eq.'746') then
              CF = 0.95
           else if (VOLSPP.eq.'129') then
              CF = 0.96
           else if (VOLSPP.eq.'835') then
              CF = 1.01
           else if (VOLSPP.eq.'371') then
              CF = 1.03
           else if (VOLSPP.eq.'318'.or.VOLSPP.eq.'375'.or.&
                &VOLSPP.eq.'951') then
              CF = 1.04
           else if (VOLSPP.eq.'833') then
              CF = 1.05
           else if (VOLSPP.eq.'837') then
              CF = 1.06
           else if (VOLSPP.eq.'802') then
              CF = 1.07
           else if (VOLSPP.eq.'621') then
              CF = 1.08
           else if (VOLSPP.eq.'762'.or.VOLSPP.eq.'806') then
              CF = 1.09
           else if (VOLSPP.eq.'316') then
              CF = 1.12
           end if
        end if

        term1 = -1.70774+(0.051321*DBHOB)+(0.58857*HT1PRD)+&
             &(0.0193547*DBHOB**2)+(0.0237324*HT1PRD*DBHOB**2)
        term2 = -(0.04821*HT1PRD**2)-(0.0002174*DBHOB**2*HT1PRD**2)-&
             &(0.0000239*DBHOB**2*HT1PRD**3)+(0.00000795*DBHOB**3*&
             &HT1PRD**2)
        term3 = -(0.00000057*DBHOB**3*HT1PRD**3)-&
             &(0.000000035*DBHOB**4*HT1PRD**2)
        vol(4) = (term1+term2+term3)*cf
     end if
  end if

  !        CORD WOOD VOLUME FOR AN ALL PULPWOOD TREE        *

  if (CDPFLG.eq.1.and.HT2PRD.gt.0.and.&
       &(PROD.ne.'01'.or.VOLEQ(1:3).eq.'921')) then
     VOL(6) = (0.001*DBHOB**2)*(1.9+(0.01*DBHOB))*((0.208*HT2PRD)-&
          &(0.009984*HT2PRD**2)+(0.04/HT2PRD))
  end if

  !            CUBIC FOOT TOP WOOD EQUATIONS                *

  if (SPFLG.eq.1.and.HT2PRD.gt.0.and.PROD.eq.'01') then
     GCB = (0.001*DBHOB**2)*(1.9+(0.01*DBHOB))*((0.208*HT2PRD)-&
          &(0.009984*HT2PRD**2)+(0.04/HT2PRD))*79
     if (HT1PRD.gt.0) then
        P = (HT1PRD)/(HT2PRD)*100
        PT = (98.461-(1.394*P)+(0.004*P**2))*0.01
        VOL(7) = PT*GCB
     else
        VOL(7) = GCB
     end if
     if (VOL(7).lt.0.0) VOL(7) = 0.0
  end if

  !                 CORD TOP WOOD EQUATIONS                 *

  if (SPFLG.eq.1.and.HT2PRD.gt.0.and.PROD.eq.'01') then
     CRD = (0.001*DBHOB**2)*(1.9+(0.01*DBHOB))*((0.208*HT2PRD)-&
          &(0.009984*HT2PRD**2)+(0.04/HT2PRD))
     if (HT1PRD.gt.0) then
        P = (HT1PRD)/(HT2PRD)*100
        PT = (98.461-(1.394*P)+(0.004*P**2))*0.01
        VOL(9) = PT*CRD
     else
        VOL(9) = CRD
     end if
     if (VOL(9).lt.0.0) VOL(9) = 0.0
  end if

  999 return
end

subroutine HAHN_NC250(VOLEQ,HTTOT,HT1PRD,HT2PRD,DBHOB,VOL,SI,BA,&
     &PROD,CTYPE,BFPFLG,CUPFLG,SPFLG,ERRFLAG,MTOPP,MTOPS)
  !     Created: YW  2018/07/31
  !     Hahn 1981 Tree volume and biomass equations for the Lake States
  !     USDA Forest Service North Central Forest Experiment Station Research Paper NC-250
  !     NVEL Equation Number: 900DVEE***
  ! Variables
  character(len=10) :: VOLEQ
  real    :: HTTOT,HT1PRD,HT2PRD,DBHOB,VOL(15),MTOPP,MTOPS
  character(len=2) :: PROD
  integer :: BFPFLG,CUPFLG,SPFLG,ERRFLAG,SI,BA
  character(len=1) :: CTYPE

  integer :: I,J,VOLSP(49),SPN,CNT,IDX,BFMIND
  real    :: HTCOEF(49,7),VOLCOEF(49,6)
  real    :: B1,B2,B3,B4,B5,B6,C0,C1,D0,D1,D,H,S,TOP,B,S0
  real    :: THTEST,HT1EST,HT2EST,RATIO,VOL4
  data VOLSP/&
       &12,68,71,94,95,105,125,129,132,241,&
       &261,299,313,314,316,317,318,371,373,375,&
       &400,460,531,541,543,544,601,602,611,621,&
       &694,731,741,742,743,746,762,802,806,812,&
       &830,833,834,837,920,951,970,998,999/
  !     Coefficients of height equations
  data ((HTCOEF(J,I), I = 1,7), J = 1,49) /&
       &12,14.304,0.19894,1.4195,0.23349,0.76878,0.12399,&
       &68,8.2079,0.19672,1.3112,0.33978,0.76173,0.11666,&
       &71,13.62,0.24255,1.2885,0.25831,0.68128,0.10771,&
       &94,31.957,0.18511,1.702,0,0.68967,0.162,&
       &95,20.038,0.18981,1.2909,0.17836,0.57343,0.10159,&
       &105,16.934,0.12972,1,0.20854,0.77792,0.12902,&
       &125,36.851,0.08298,1,0.00001,0.63884,0.18231,&
       &129,16.281,0.08621,1,0.1622,0.86833,0.23316,&
       &132,36.851,0.08298,1,0.00001,0.63884,0.18231,&
       &241,8.2079,0.19672,1.3112,0.33978,0.76173,0.11666,&
       &261,5.3117,0.10357,1,0.68454,0.7141,0,&
       &299,16.934,0.12972,1,0.20854,0.77792,0.12902,&
       &313,6.86,0.27725,1.4287,0.40115,0.85299,0.12403,&
       &314,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,&
       &316,6.86,0.27725,1.4287,0.40115,0.85299,0.12403,&
       &317,6.86,0.27725,1.4287,0.40115,0.85299,0.12403,&
       &318,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,&
       &371,7.1852,0.28384,1.4417,0.38884,0.82157,0.11411,&
       &373,7.2773,0.22721,1,0.41179,0.76498,0.11046,&
       &375,7.2773,0.22721,1,0.41179,0.76498,0.11046,&
       &400,6.1034,0.17368,1,0.44725,1.0237,0.1461,&
       &460,6.86,0.27725,1.4287,0.40115,0.85299,0.12403,&
       &531,7.1852,0.28384,1.4417,0.38884,0.82157,0.11411,&
       &541,8.1782,0.27316,1.725,0.38694,0.75822,0.10847,&
       &543,11.291,0.2525,1.5466,0.35711,0.7506,0.06859,&
       &544,8.1782,0.27316,1.725,0.38694,0.75822,0.10847,&
       &601,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,&
       &602,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,&
       &611,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,&
       &621,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,&
       &694,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,&
       &731,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,&
       &741,6.4301,0.23545,1.338,0.4737,0.73385,0.08228,&
       &742,13.625,0.28668,1.6124,0.30651,1.0292,0.0746,&
       &743,5.5346,0.22637,1,0.46918,0.72456,0.11782,&
       &746,6.4301,0.23545,1.338,0.4737,0.73385,0.08228,&
       &762,5.3416,0.23044,1.1529,0.54194,0.8344,0.06372,&
       &802,9.2078,0.22208,1,0.31723,0.8256,0.13465,&
       &806,3.8011,0.39213,2.9053,0.55634,0.84317,0.09593,&
       &812,3.8011,0.39213,2.9053,0.55634,0.84317,0.09593,&
       &830,3.8011,0.39213,2.9053,0.55634,0.84317,0.09593,&
       &833,6.6844,0.19049,1,0.43972,0.82962,0.10806,&
       &834,3.8011,0.39213,2.9053,0.55634,0.84317,0.09593,&
       &837,3.8011,0.39213,2.9053,0.55634,0.84317,0.09593,&
       &920,13.625,0.28668,1.6124,0.30651,1.0292,0.0746,&
       &951,6.3628,0.27859,1.8677,0.49589,0.76169,0.05841,&
       &970,8.458,0.27527,1.9602,0.34894,0.89213,0.12594,&
       &998,6.9572,0.26564,1,0.4866,0.76954,0.01618,&
       &999,6.9572,0.26564,1,0.4866,0.76954,0.01618/
  !     Cubic foot volume regression coefficients (COLUMN 2 & 3) AND
  !     Board foot volume regression coefficients (COLUMN 4 & 5)
  !     Stump volume coef (column 6)
  data ((VOLCOEF(J,I), I = 1,6), J = 1,49) /&
       &12,0.2514,0.002679,4.658,0.01694,    .009967,&
       &68,0.5905,0.002168,17.167,0.01404,   .008877,&
       &71,1.4109,0.002227,17.592,0.01427,   .008877,&
       &94,0.3365,0.002599,6.81,0.01611,     .010699,&
       &95,0.2631,0.002706,0,0.01735,        .008877,&
       &105,1.2446,0.002165,19.149,0.01307,  .007017,&
       &125,2.0822,0.002046,24.848,0.01298,  .007176,&
       &129,0,0.002364,0,0.01625,            .008269,&
       &132,2.0822,0.002046,24.848,0.01298,  .007176,&
       &241,1.0406,0.002408,12.532,0.0156,   .011946,&
       &261,0,0.001856,0,0.01054,            .008579,&
       &299,0.5905,0.002168,17.167,0.01404,  .008877,&
       &313,1.7283,0.002189,63.992,0.01215,  .008476,&
       &314,1.3746,0.002206,36.859,0.01534,  .008894,&
       &316,1.7283,0.002189,63.992,0.01215,  .008476,&
       &317,1.7283,0.002189,63.992,0.01215,  .008476,&
       &318,1.3746,0.002206,36.859,0.01534,  .008894,&
       &371,0,0.00248,14.575,0.01766,        .009968,&
       &373,0,0.002485,34.832,0.01458,       .008380,&
       &375,2.3037,0.00181,37.619,0.01404,   .008380,&
       &400,2.4364,0.001881,57.449,0.01122,  .008980,&
       &460,0,0.002325,28.875,0.01466,       .010422,&
       &531,2.2793,0.002395,56.5,0.01465,    .010202,&
       &541,1.528,0.002021,65.124,0.01124,   .008728,&
       &543,2.599,0.001792,70.167,0.01015,   .011016,&
       &544,1.528,0.002021,65.124,0.01124,   .008728,&
       &601,2.6341,0.001887,67.801,0.01109,  .008980,&
       &602,2.6341,0.001887,67.801,0.01109,  .008980,&
       &611,0,0.002485,34.832,0.01458,       .008980,&
       &621,2.6341,0.001887,67.801,0.01109,  .008980,&
       &694,0,0.002485,34.832,0.01458,       .008980,&
       &731,0,0.002485,34.832,0.01458,       .008980,&
       &741,0,0.002335,17.978,0.01578,       .006594,&
       &742,0,0.002485,34.832,0.01458,       .011145,&
       &743,0.9461,0.002247,31.842,0.01483,  .006594,&
       &746,2.0756,0.001913,29.329,0.0148,   .007369,&
       &762,2.6341,0.001887,67.801,0.01109,  .008980,&
       &802,0.7316,0.001951,46.038,0.01173,  .009727,&
       &806,0.7554,0.002174,34.677,0.0137,   .009727,&
       &812,0.7554,0.002174,34.677,0.0137,   .009727,&
       &830,0.7554,0.002174,34.677,0.0137,   .009727,&
       &833,1.6378,0.002032,41.41,0.01326,   .008908,&
       &834,0.7554,0.002174,34.677,0.0137,   .009727,&
       &837,0.7554,0.002174,34.677,0.0137,   .009727,&
       &920,0,0.002485,34.832,0.01458,       .011145,&
       &951,0.9239,0.002206,36.821,0.01435,  .009639,&
       &970,0,0.002325,28.875,0.01466,       .010422,&
       &998,1.4824,0.001796,36.341,0.01339,  .008980,&
       &999,0.867,0.00194,36.341,0.01339,    .008980/
  !     Total and nerch height formula
  HT(B1,B2,B3,B4,B5,B6,D,S,TOP,B) = &
       &4.5+B1*(1-EXP(-B2*D))**B3*S**B4*(1.00001-TOP/D)**B5*B**B6
  !     Cubicfoot and boardfoot volume formula
  V(C0,C1,D,H) = C0+C1*D**2*H

  VOL = 0.0
  ERRFLAG = 0
  VOL4 = 0.0
  if (DBHOB.lt.1.0) then
     ERRFLAG = 3
     return
  end if
  read(VOLEQ(8:10),'(I3)') SPN
  CNT = 49
  IDX = 0
  call SEARCH(CNT,VOLSP,SPN,IDX,ERRFLAG)
  if (IDX.le.0) then
     ERRFLAG = 1
     return
  else if (SPN.eq.VOLCOEF(IDX,1)) then
     B1 = HTCOEF(IDX,2)
     B2 = HTCOEF(IDX,3)
     B3 = HTCOEF(IDX,4)
     B4 = HTCOEF(IDX,5)
     B5 = HTCOEF(IDX,6)
     B6 = HTCOEF(IDX,7)
     C0 = VOLCOEF(IDX,2)
     C1 = VOLCOEF(IDX,3)
     D0 = VOLCOEF(IDX,4)
     D1 = VOLCOEF(IDX,5)
     S0 = VOLCOEF(IDX,6)
  end if
  if (SPN.lt.300) then
     BFMIND = 9.0
     if (MTOPP.lt.0.1) MTOPP = 7.0
  else
     BFMIND = 11.0
     if (MTOPP.lt.0.1) MTOPP = 9.0
  end if
  if (MTOPS.lt.0.1) MTOPS = 4.0

  if (SI.eq.0) SI = 60
  if (BA.eq.0) BA = 90
  S = SI
  B = BA
  D = DBHOB

  !*    CHECK FOR VALID HEIGHTS
  if (HT1PRD.le.0.and. HT2PRD.le.0.and. HTTOT.le.0) then
     if (CTYPE.eq.'C') then
        ERRFLAG = 7
        return
     end if
  end if
  RATIO = 1.0
  TOP = 0.0
  THTEST = HT(B1,B2,B3,B4,B5,B6,D,S,TOP,B)
  if (HTTOT.gt.0.0) then
     RATIO = HTTOT/THTEST
  else if (HT1PRD.gt.0.0) then
     TOP = MTOPP
     HT1EST = HT(B1,B2,B3,B4,B5,B6,D,S,TOP,B)
     RATIO = HT1PRD/HT1EST
  else if (HT2PRD.gt.0.0) then
     TOP = MTOPS
     HT2EST = HT(B1,B2,B3,B4,B5,B6,D,S,TOP,B)
     RATIO = HT2PRD/HT2EST
  end if

  if (HTTOT.lt.0.1) HTTOT = THTEST*RATIO
  !     Compute total cubic volume from stump to tip
  H = HTTOT
  VOL(1) = V(C0,C1,D,H)
  if (HT2PRD.lt.0.1.and.D.ge.5.0) then
     TOP = MTOPS
     HT2EST = HT(B1,B2,B3,B4,B5,B6,D,S,TOP,B)
     HT2PRD = HT2EST*RATIO
  end if
  if (HT1PRD.lt.0.1) then
     TOP = MTOPP
     HT1EST = HT(B1,B2,B3,B4,B5,B6,D,S,TOP,B)
     HT1PRD = HT1EST*RATIO
  end if
  !     Compute merch cubic volume from stump to 4 inch top
  if (HT2PRD.gt.0.1) then
     H = HT2PRD
     VOL(4) = V(C0,C1,D,H)
     VOL4 = VOL(4)
  end if
  !     Compute merch cubic volume from stump to 7/9 inch top
  if (HT1PRD.gt.0.1.and.PROD.eq.'01') then
     H = HT1PRD
     VOL(4) = V(C0,C1,D,H)
     !     Compute topwood cubic volume from 7/9 inch to 4 inch top
     if (VOL4.gt.0.0) VOL(7) = VOL4-VOL(4)
     VOL(10) = V(D0,D1,D,H)
     !       convert International BF to Scribner using the method formerly used by NCRS-FIA
     VOL(2) = VOL(10)*0.89
  end if
  vol(14) = S0*D*D
  VOL(15) = VOL(1) - VOL(4) - VOL(7)
  if (VOL(15).lt.0) VOL(15) = 0.0
  return

  end subroutine HAHN_NC250
