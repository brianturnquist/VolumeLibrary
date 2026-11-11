!============================!
!   VolumeLibrary : mrules   !
!============================!
  !== last modified  08-25-2015
  !REV  Revised TDH 12/15/10 accidentally had set trim to 0
  !     for region 5 for testing and forgot to revert.  fixed.
  !     YW 3/25/14 added PROD as input parameter and changed region 3 MINLEN and MINLENT
  !                added merch rule for DOD (region 11) using R6 rules
  !     YW 04/15/14 Added region 9 Clark merch rule.
  !     YW 02/13/15 Changed the merch rule for Region 3 MINLEN and MINLENT to 2'
  !     YW 08/25/15 Added merch rule for Region 8 Clark equation
  !     YW 12/20/2019 merch rule changes for Region 3
  !     YW 08/11/2021 Set R6 stump default to 1.0
  !     YW 08/23/2021 Reset R3 MINBFD to 1.0
  !     YW 01/18/2023 Changed the min topwood length to 16 for R1
  !     YW 02/23/2023 Changed R1 Prod 08 minimum merch length MERCHL to 16
  !     YW 03/02/2023 Added NVB Eq to use profile merch rules for R1 and R8 and 9
  !     YW 03/10/2023 Corrected the MINLEN, MINLENT and OPT for R3. The previoue was not correct based R3 merch rules
  ! YW 06/22/2023 Added chacking MTOPS <= MTOPP
  ! YW 2023/08/16 Added initial value for MINLENT for R8 and R9 CLK
subroutine MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,&
     &MINLEN,MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,&
     &PROD)
  !     program assigns regional merchandizing rules to be used with
  !      profile models

  use VOLINPUT_MOD
  !     SETS MERCHANDIZING STANDARDS AND SOME ERROR CHECKNING

  ! Variables
  character(len=1) :: COR
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=3) :: MDL
  character(len=10) :: VOLEQ
  integer :: EVOD,OPT,REGN,spp
  real    :: MAXLEN,MINLEN,MERCHL,MTOPP,MTOPS,STUMP,TRIM
  real    :: MINLENT,MINBFD,BTR,DBTBH,DBHOB
  real    :: TREESTUMP,TREEMTOPP,TREEMTOPS,TREEBTR,TREEDBTBH

  !     First save the input from tree variable
  TREESTUMP = STUMP
  TREEMTOPP = MTOPP
  TREEMTOPS = MTOPS
  TREEBTR = BTR
  TREEDBTBH = DBTBH

  if (BTR.gt.0.0.and. DBTBH.le.0) DBTBH = DBHOB-(DBHOB*BTR/100.0)

  MDL = VOLEQ(4:6)
  if (REGN.eq.1) then
     if (MDL.eq.'FW2' .or. MDL.eq.'fw2' .or.&
          &MDL.eq.'FW3' .or. MDL.eq.'fw3'.or.VOLEQ(1:3).eq.'NVB') then

        COR = 'Y'
        EVOD = 2
        MAXLEN = 16.0
        MINLEN = 2.0
        !            MINLENT = 8.0
        ! Changed the min topwood length to 16 (YW 2023/01/18)
        MINLENT = 16.0
        OPT = 22
        if (STUMP.le.0.0) STUMP = 1.0
        if (MTOPP .le.0.0) MTOPP = 5.6
        if (MTOPS .le.0.0) MTOPS = 4.0
        TRIM = .5
        !  MIN SAWTIMBER LENGTH
        MERCHL = 8
        !For prod 08 changed to 16 (20230223)
        if (PROD.eq.'08') then
           MERCHL = 16
           MINLEN = 16.0
        end if
        ! min dbh tree for sawtimber
        MINBFD = 1.0
     else
        COR = 'Y'
        EVOD = 2
        MAXLEN = 20.0
        MINLEN = 10.0
        MINLENT = 2.0
        OPT = 12
        if (STUMP.le.0.0) STUMP = 1.0
        if (MTOPP .le.0.0) MTOPP = 5.6
        if (MTOPS .le.0.0) MTOPS = 4.0
        TRIM = .5
        !  MIN SAWTIMBER LENGTH
        MERCHL = 10
        ! min dbh tree for sawtimber
        MINBFD = 1.0
     end if
  else if (REGN.eq.2) then

     COR = 'Y'
     EVOD = 2
     MAXLEN = 16.0
     MINLEN = 2.0
     minlent = 2.0
     OPT = 22
     if (STUMP.le.0.0) STUMP = 1.0
     if (MTOPP .le.0.0) MTOPP = 6.0
     if (MTOPS .le.0.0) MTOPS = 4.0
     TRIM = 0.5
     !  MIN SAWTIMBER LENGTH
     MERCHL = 8
     ! min dbh tree for sawtimber
     !        MINBFD = 7.0
     MINBFD = 1.0

  else if (REGN.eq.3) then

     COR = 'Y'
     EVOD = 2
     MAXLEN = 16.0
     MINLEN = 2.0
     minlent = 2.0
     OPT = 22
     MINBFD = 1.0
     TRIM = 0.5
     MERCHL = 8
     ! Karen requested to change back to 2' for minimum log length (02/13/2015)
     ! Karen asked to change min log length for 8 for prod 01 and 10 for prod 02 (2019/07/18)
     ! Mrules changed for Region 3 as Karen asked(12/20/2019)
     ! Prod 01 minimum log length 10 TopD 6.0 MinDBH 14.0 Stump 1.0
     ! Prod 08 minimum log length 10 TopD 4.0 MinDBH 9.0 Stump 0.5
     ! prod 14 minimum log length 10 TopD 4.0 MinDBH 6.0 Stump 0.5
     ! prod 20 minimum log length 2 TopD 1.0 MinDBH 2.0 Stump 0.5
     ! prod 07 minimum log length 4 TopD 2.0 MinDBH 5.0 Stump 0.5
     ! Removed MinDBH as request by R3 (Karen 2021/08/21), so it back to MINBFD=1
     if (PROD.eq.'01'.or.PROD.eq.'08') then
        !MINLEN = 4.0 !Corrected to 10 (2023/03/10)
        MINLEN = 10
        minlent = 10.0
        if (PROD.eq.'01'.and.STUMP.le.0.0) STUMP = 1.0
        if (PROD.eq.'01'.and.MTOPP .le.0.0) MTOPP = 6.0
        if (MTOPS .le.0.0) MTOPS = 4.0
        !          MINBFD = 14.0
        MERCHL = 10.0
        if (PROD.eq.'08') then
           if (STUMP.le.0.0) STUMP = 0.5
           if (MTOPP .le.0.0) MTOPP = 4.0
        end if
     else if (PROD.eq.'14') then
        !MINLEN = 4.0
        !minlent = 2.0
        !Corrected to 10 (2023/03/10)
        MINLEN = 10.0
        minlent = 10.0
        if (STUMP.le.0.0) STUMP = 0.5
        if (MTOPP .le.0.0) MTOPP = 4.0
        if (MTOPS .le.0.0) MTOPS = 1.0
        !          MINBFD = 6.0
        MERCHL = 10.0
     else if (PROD.eq.'20') then
        !OPT = 23 !Corrected 2023/03/10
        if (STUMP.le.0.0) STUMP = 0.5
        if (MTOPP .le.0.0) MTOPP = 1.0
        if (MTOPS .le.0.0) MTOPS = 1.0
     else if (PROD.eq.'07') then
        !OPT = 23 !Corrected 2023/03/10 also set MINLENT to 4.0
        MINLENT = 4.0
        MINLEN = 4.0
        if (STUMP.le.0.0) STUMP = 0.5
        if (MTOPP .le.0.0) MTOPP = 2.0
        if (MTOPS .le.0.0) MTOPS = 2.0
     else ! PROD 08 (NON-SAW)
        ! MINLEN = 4.0 Corrected to 10 (2023/03/10)
        MINLEN = 10
        minlent = 10.0
        if (STUMP.le.0.0) STUMP = 0.5
        if (MTOPP .le.0.0) MTOPP = 4.0
        if (MTOPS .le.0.0) MTOPS = 4.0
        !          MINBFD = 9.0
        MERCHL = 10.0
     end if
     !        minlent = 10.0
     if (STUMP.le.0.0) STUMP = 1.0
     if (MTOPP .le.0.0) MTOPP = 6.0
     if (MTOPS .le.0.0) MTOPS = 4.0
     !  MIN SAWTIMBER LENGTH
     !        MERCHL = 8
     ! min dbh tree for sawtimber
     !        MINBFD = 7.0
     ! 2019/07/18 Temp change for Karen to run CruiseProcessing with modified rules
     ! Proposal 1
     !        IF(PROD.EQ.'01')THEN
     !          MINLEN = 16.0
     !          MTOPP = 8.0
     !          MTOPS = 6.0
     !          OPT = 23
     !          MINBFD = 14.0
     !        ELSEIF(PROD.EQ.'08'.OR.PROD.EQ.'14')THEN
     !          MINLEN = 8.0
     !          MTOPP = 6.0
     !          MTOPS = 4.0
     !          OPT = 23
     !          MINBFD = 8.0
     !        ELSEIF(PROD.EQ.'20')THEN
     !          MINLEN = 10.0
     !          MTOPP = 4.0
     !          MTOPS = 2.0
     !          OPT = 23
     !          MINBFD = 5.0
     !        ENDIF
     ! Proposal 3
     !        IF(PROD.EQ.'01')THEN
     !          MINLEN = 8.0
     !          MTOPP = 6.0
     !          MTOPS = 4.0
     !          OPT = 23
     !          MINBFD = 14.0
     !        ELSEIF(PROD.EQ.'08'.OR.PROD.EQ.'14')THEN
     !          MINLEN = 10.0
     !          MTOPP = 4.0
     !          MTOPS = 2.0
     !          OPT = 23
     !          MINBFD = 8.0
     !        ELSEIF(PROD.EQ.'20')THEN
     !          MINLEN = 10.0
     !          MTOPP = 4.0
     !          MTOPS = 2.0
     !          OPT = 23
     !          MINBFD = 5.0
     !        ENDIF
     ! Proposal 4
     !        IF(PROD.EQ.'01')THEN
     !          MINLEN = 10.0
     !          MTOPP = 6.0
     !          MTOPS = 4.0
     !          OPT = 23
     !          MINBFD = 14.0
     !        ELSEIF(PROD.EQ.'08'.OR.PROD.EQ.'14')THEN
     !          MINLEN = 10.0
     !          MTOPP = 4.0
     !          MTOPS = 4.0
     !          OPT = 23
     !          MINBFD = 8.0
     !        ELSEIF(PROD.EQ.'20')THEN
     !          MINLEN = 10.0
     !          MTOPP = 4.0
     !          MTOPS = 4.0
     !          OPT = 23
     !          MINBFD = 5.0
     !        ENDIF
     ! Proposal 5
     !        IF(PROD.EQ.'01')THEN
     !          MINLEN = 10.0
     !          MTOPP = 6.0
     !          MTOPS = 4.0
     !          OPT = 22
     !          MINBFD = 14.0
     !        ELSEIF(PROD.EQ.'08'.OR.PROD.EQ.'14')THEN
     !          MINLEN = 10.0
     !          MTOPP = 4.0
     !          MTOPS = 4.0
     !          OPT = 22
     !          MINBFD = 8.0
     !        ELSEIF(PROD.EQ.'20')THEN
     !          MINLEN = 10.0
     !          MTOPP = 4.0
     !          MTOPS = 4.0
     !          OPT = 22
     !          MINBFD = 5.0
     !        ENDIF

     !        minlent = MINLEN
  else if (REGN.eq.4) then

     COR = 'Y'
     EVOD = 2
     MAXLEN = 16.0
     MINLEN = 2.0
     minlent = 2.0
     OPT = 22
     if (STUMP.le.0.0) STUMP = 1.0
     if (MTOPP .le.0.0) MTOPP = 6.0
     if (MTOPS .le.0.0) MTOPS = 4.0
     TRIM = .5
     !  MIN SAWTIMBER LENGTH
     MERCHL = 8
     ! min dbh tree for sawtimber
     !         MINBFD = 7.0
     MINBFD = 1.0

  else if (REGN.eq.5) then

     COR = 'Y'
     EVOD = 2
     MAXLEN = 16.0
     MINLEN = 2.0
     minlent = 2.0

     OPT = 22
     if (STUMP.le.0.0) STUMP = 1.0
     if (MTOPP .le.0.0) MTOPP = 6.0
     if (MTOPS .le.0.0) MTOPS = 4.0
     TRIM = .5
     !         TRIM = 0
     !  MIN SAWTIMBER LENGTH
     MERCHL = 8
     !         MERCHL = 4
     ! min dbh tree for sawtimber
     !         MINBFD = 7.0
     MINBFD = 1.0

     ! Added Region 11 for DOD, using same as R6 (03/26/2014)
  else if (REGN.eq.6.or.REGN.eq.11) then
     COR = 'N'
     EVOD = 2
     MAXLEN = 16.0
     MINLEN = 2.0
     minlent = 2.0
     OPT = 23
     if (STUMP.le.0.0) STUMP = 0.0
     ! STUMP = 1.0 added for test FSVeg
     !           IF(STUMP.LE.0.0) STUMP = 1.0
     if (MTOPP .le.0.0) MTOPP = 2.0
     if (MTOPS .le.0.0) MTOPS = 2.0
     TRIM = .5
     !  MIN SAWTIMBER LENGTH
     MERCHL = 8
     ! min dbh tree for sawtimber
     !           MINBFD = 6.0
     MINBFD = 1.0

  else if (REGN.eq.7) then
     COR = 'N'
     EVOD = 2
     MAXLEN = 16.0
     MINLEN = 2.0
     minlent = 2.0
     OPT = 23
     if (STUMP.le.0.0) STUMP = 1.0
     if (MTOPP .le.0.0) MTOPP = anint((0.184*DBHOB)+2.24)
     if (MTOPS .le.0.0) MTOPS = 2.0
     TRIM = .5
     !  MIN SAWTIMBER LENGTH
     MERCHL = 8
     ! min dbh tree for sawtimber
     !         MINBFD = 6.0
     MINBFD = 1.0
  else if (REGN.eq.8.and.(MDL.eq.'CLK'.or.VOLEQ(1:3).eq.'NVB')) then
     COR = 'Y'
     EVOD = 2
     MAXLEN = 8.0
     !         MINLEN = 8.0
     MINLEN = 2.0
     minlent = 2.0
     MERCHL = 8.0
     if (PROD.eq.'08') MERCHL = 12.0
     OPT = 22
     read(volEq(8:10),'(i3)') spp
     if (MTOPP.le.0.0) then
        !         For Prod 08, if top diameter 0, means the whole tree volume. So set MTOPP = 0.1
        !         Confirmed with Adam Moore for this default
        if (PROD.eq.'08') then
           MTOPP = 0.1
        else
           if (spp.lt.300) then
              MTOPP = 7.0
           else
              MTOPP = 9.0
           end if
        end if
     end if
     if (MTOPS.le.0.0) MTOPS = 4.0
     TRIM = 0.5
     if (STUMP.le.0.0) then
        if (PROD.eq.'01') then
           STUMP = 1.0
        else
           STUMP = 0.5
        end if
     end if
  else if (REGN.eq.9.and.(MDL.eq.'CLK'.or.VOLEQ(1:3).eq.'NVB')) then
     COR = 'Y'
     EVOD = 2
     MAXLEN = 8.0
     !MINLEN = 4.0
     MINLEN = 2.0
     minlent = 4.0
     MERCHL = 8.0
     OPT = 22
     read(volEq(8:10),'(i3)') spp
     if (MTOPP .le.0.0) then
        if (spp.lt.300) then
           MTOPP = 7.6
        else
           MTOPP = 9.6
        end if
     end if
     if (MTOPS .le.0.0) MTOPS = 4.0
     TRIM = 0.3
     if (STUMP.le.0.0) then
        if (PROD.eq.'01') then
           STUMP = 1.0
        else
           STUMP = 0.5
        end if
     end if
  else if (REGN.eq.10) then

     COR = 'Y'
     EVOD = 2
     MAXLEN = 16.0
     MINLEN = 8.0
     MINLENT = 8.0
     OPT = 23
     if (STUMP.le.0.0) STUMP = 1.0
     if (MTOPP .le.0.0) MTOPP = 6.0
     if (MTOPS .le.0.0) MTOPS = 4.0
     TRIM = .5
     !  MIN SAWTIMBER LENGTH
     MERCHL = 8
     ! min dbh tree for sawtimber
     !         MINBFD = 6.0
     MINBFD = 1.0
     ! default merch rules
  else
     COR = 'Y'
     EVOD = 2
     MAXLEN = 16.0
     MINLEN = 2.0
     MINLENT = 2.0
     OPT = 22
     if (STUMP.le.0.0) STUMP = 1.0
     if (MTOPP .le.0.0) MTOPP = 6.0
     if (MTOPS .le.0.0) MTOPS = 4.0
     TRIM = .5
     !  MIN SAWTIMBER LENGTH
     MERCHL = 8
     ! min dbh tree for sawtimber
     MINBFD = 1.0
  end if

  ! Added for modified MRULE 2017/03/17
  ! IF USER MODIFIED MRULE, THEN NO NEED TO CALL MRULES
  if (MRULEMOD.eq.'Y') then
     !        IF(NEWCOR.NE.COR) COR = NEWCOR
     if (NEWEVOD.gt.0) EVOD = NEWEVOD
     if (NEWOPT.gt.0) OPT = NEWOPT
     if (NEWMAXLEN.gt.0.1) MAXLEN = NEWMAXLEN
     if (NEWMINLEN.gt.0.1) MINLEN = NEWMINLEN
     if (NEWMERCHL.gt.0.1) MERCHL = NEWMERCHL
     if (NEWMINLENT.gt.0.1) MINLENT = NEWMINLENT
     if (NEWMTOPP.gt.0.1.and.TREEMTOPP.le.0) MTOPP = NEWMTOPP
     if (NEWMTOPS.gt.0.1.and.TREEMTOPS.le.0) MTOPS = NEWMTOPS
     if (NEWSTUMP.gt.0.1.and.TREESTUMP.le.0) STUMP = NEWSTUMP
     if (NEWTRIM.gt.0.1) TRIM = NEWTRIM
     if (NEWBTR.gt.0.01.and.TREEBTR.le.0) BTR = NEWBTR
     if (NEWDBTBH.gt.0.01.and.TREEDBTBH.le.0) DBTBH = NEWDBTBH
     if (NEWMINBFD.gt.0.1) MINBFD = NEWMINBFD
     if (NEWBTR.gt.0.01.and. NEWDBTBH.le.0) then
        DBTBH = DBHOB-(DBHOB*NEWBTR/100.0)
     end if
     MRULEMOD = 'N'
  end if

  if (MTOPS.gt.MTOPP) MTOPS = MTOPP
  return
end
  !--  MERCHANDIZING VARIABLES

  !--  REGION - INTEGER - Region number used to set Regional Merchandizing Rules
  !--  COR - CHARACTER - Flag to indicate Scribner table or Scribner
  !--                 factor volumes. "Y" = table volumes, "N" = factor volumes
  !--  EVOD - INTEGER - allow even or odd segment lengths
  !--         segment options 11-14 allow odd lengths by definition
  !--        1 = odd segment lengths allowed
  !--        2 = only even segment lengths will be allowed
  !--  MAXLEN - REAL - Maximum segment length
  !--  MINLEN - REAL - Minimum segment length
  !--  MERCHL - REAL - Minimum length of primary product a tree must have
  !--                  must have be merchantable
  !--  **TOP DIB TO USE**
  !--  MTOPP - REAL - BdFt, CuFt and Cord Wood merch top for primary product
  !--  MTOPS - REAL - CuFt and Cord Wood merch top for secondary product

  !--  OPT - INTEGER - Specifies which segmentation option to use for
  !--        merchandizing tree bole.  Option codes are as follows:
  !--        11 = 16 ft log scale, presented as tree length log. (FSH 2409.11)
  !--        12 = 20 ft log scale, presented as tree length log. (FSH 2409.11)
  !--        13 = 32 ft log scale, presented as tree length log.
  !--        14 = 40 ft log scale, presented as tree length log.
  !--        21 = Nominal log length (NLL), if top log is less then half
  !--             of the NLL then it is combined with the next lowest log and
  !--             this combined piece is then resegmented according to the
  !--             entered merchandising parameters giving two approximately
  !--             equal log lengths.  If the segment length is greater then
  !--             or equal to half the NNL then the segment stands on its' own.
  !--        22 = Nominal log length (NLL), top log is combined with the next
  !--             lowest log and this combined piece is then resegmented
  !--             according to the entered merchandising parameters giving
  !--             two approximately equal log lengths.
  !--        23 = Nominal log length, top segment stands on its' own.
  !--        24 = Nominal log length, if the top segment is less then 1/4 of
  !--             NNL then the segment is droped,  it the segment is 1/4 to
  !--             3/4 of NNL then the segment length is set to 1/2 of NNL,
  !--             if the segment is greater then 3/4 of NNL then the segment
  !--             length is set to NNL.

  !--  STUMP - REAL - height of stump in feet or fractions thereof.
  !--  TRIM - REAL - trim length for each segment in feet or fractions thereof.
