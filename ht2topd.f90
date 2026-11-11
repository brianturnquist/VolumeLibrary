!=============================!
!   VolumeLibrary : ht2topd   !
!=============================!
subroutine HT2TOPD(REGN,FORST,VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,&
     &UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,FCLASS,&
     &STEMDIB,STEMHT,ERRFLAG)

  use CLKCOEF_MOD
  use VOLINPUT_MOD
  implicit none
  !  Purpose:
  !     This subroutine calculates the height to a particular top diameter (insidebark)
  !     for volume equation with profile model.
  ! Variables
  integer :: REGN,ERRFLAG,HTREF,FCLASS
  real    :: DBHOB,HTTOT,STEMDIB,STEMHT
  character(len=10) :: VOLEQ
  character(len=10) :: VOLEQ2
  character(len=2) :: FORST
  character(len=3) :: MDL
  real    :: HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
  real    :: DBTBH,BTR,VOL(15)
  character(len=1) :: HTTYPE
  character(len=1) :: VEQ
  character(len=1) :: CTYPE

  real    :: MAXLEN,MINLEN,MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,MINBFD
  integer :: EVOD,OPT
  character(len=1) :: COR
  character(len=2) :: PROD

  !     Variables to hold flewellings coefficients
  integer :: SETOPT(6),JSP,NEXTRA
  real    :: RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
  real    :: HEX(2),dex(2),ZEX(2),UHT,MHT,LMERCH
  real    :: TOP6,HT2,TOPD,HTTOTC

  !     VARIABLES FOR R4 MAT
  real    :: STUMPD,BUTTCF,CF0,B

  !     Variables for Clark profile
  integer :: SPP,GEOG,IPROD
  real    :: TOPDIB,SAWDIB,PULPDIB,SHRTHT,TOPHT,PLPDIB,DBHIB,DIB17,brokHt
  TYPE(CLKCOEF):: COEFFS,COEFFST
  TYPE(CLKCOEF):: COEFFSO
  logical :: SHORT
  !Variables to call NSVB equation
  character(len=11) :: VOLEQ11
  real    :: Vib,WDSG,CF,SPGRNWF,SPDRYWF
  integer :: SPCD,SPGRPCD,SFTHRD

  MDL = VOLEQ(4:6)
  !      TOPD = STEMDIB
  STEMHT = 0.0
  PROD = '01'
  STUMP = 0.0
  HTTYPE = 'F'
  MTOPP = 0.0
  MTOPS = 0.0
  !     Profile model
  !     MRULES IS EXTERNAL AND CONTAINS THE MERCHANDIZING RULES SETTINGS
  call MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,MINLEN,&
       &MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,&
       &PROD)

  !     Added this for no total height(12/20/2017)
  TOPD = MTOPP
  if (HTTOT.le.0) then
     if (HT1PRD.gt.0) then
        MHT = HT1PRD + STUMP
     else if (HT2PRD.gt.0) then
        MHT = HT2PRD + STUMP
        TOPD = MTOPS
     end if
  end if
  if (MDL.eq.'FW2' .or. MDL.eq.'fw2' .or. MDL.eq.'FW3' .or.&
       &MDL.eq.'fw3' .or. MDL.eq.'CZ2' .or. MDL.eq.'cz2' .or.&
       &MDL.eq.'CZ3' .or. MDL.eq.'cz3' .or. MDL.eq.'WO2' .or.&
       &MDL.eq.'wo2' .or. MDL.eq.'F32' .or. MDL.eq.'f32' .or.&
       &MDL.eq.'F33' .or. MDL.eq.'f33' .or. MDL.eq.'JB2' .or.&
       &MDL.eq.'jb2' .or. MDL.eq.'BEH' .or. MDL.eq.'beh') then

     if (VOLEQ(4:4).eq.'F' .or. VOLEQ(4:4).eq.'f') then
        !--   Initialize Flewelling model for this tree

        call FWINIT(VOLEQ,DBHOB,HTTOT,MHT,TOPD,UPSHT1,UPSHT2,UPSD1,&
             &UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,JSP,RHFW,RFLW,&
             &TAPCOE,F,SETOPT,NEXTRA,HEX,DEX,ZEX,PINV_Z,FMOD,btr,FCLASS,&
             &ERRFLAG)

        AVGZ1 = ZEX(1)
        AVGZ2 = ZEX(2)
        if (ERRFLAG .gt.0)  go to 1000

     else if (VOLEQ(4:6).eq.'CZ3' .or. VOLEQ(4:6).eq.'cz3') then
        !        initialize Czaplewski three point model
        if (HTTOT.le.4.5) then
           ERRFLAG = 4
           goto 1000
        end if
        UHT = HTTOT * 0.95
        if (UPSHT1.le.0.or. UPSD1.le.0) then
           ERRFLAG = 9
           go to 1000
        end if
        if (UPSHT1.le.4.5.or. UPSHT1.gt.UHT) then
           ERRFLAG = 10
           go to 1000
        end if
        HEX(1) = UPSHT1
        DEX(1) = UPSD1
        call TOP6LEN(VOLEQ,HTTOT,DBHOB,DEX,HEX,STUMP,6.0,&
             &TOP6,DBTBH,errflag)
     else if (MDL.eq.'BEH' .or. MDL.eq.'beh') then
        if (FCLASS.le.0) then
           call GETFCLASS(VOLEQ,FORST,DBHOB,FCLASS)
        end if
        FORMCLASS = FCLASS
     end if
     call MERLEN(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,&
          &DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,&
          &STUMP,STEMDIB,LMERCH,errflag)
     STEMHT = LMERCH + STUMP
     !     !OTHER PROFILE MODEL
  else if (MDL.eq.'MAT' .or. MDL.eq.'mat') then
     call R4MATTAPER(VOLEQ,DBHOB,HTTOT,STUMPD,BUTTCF,CF0,B,&
          &STEMHT,STEMDIB,ERRFLAG )
  else if (MDL.eq.'CLK' .or. MDL.eq.'clk') then
     if (REGN.eq.9.or.&
          &(VOLEQ(1:1).eq.'8'.and.VOLEQ(3:3).eq.'1')) then
        if (VOLEQ(1:1).eq.'8') then
           !R8 use UPSHT1 for ht479. Need to set ht4 to HT2PRD
           if (HTTOT.le.0.0.and.UPSHT1.gt.0.0) then
              if (UPSD1.eq.4.0.or.&
                   &(Prod.ne.'01'.and.(UPSD1.ne.7.or.UPSD1.ne.9))) then
                 HT2PRD = UPSHT1
                 UPSHT1 = 0.0
              end if
           end if
           call r8Prep(volEq,dbhOb,topDib,topHt,ht1Prd,ht2Prd,htTot,&
                &spp,geog,COEFFS,forst,maxLen,&
                &minLen,merchL,mTopP,mTopS,stump,trim,minBfD,&
                &prod,iProd,sawDib,plpDib,short,shrtHt,errFlag,&
                &upsHt1,COEFFSO)
           call r9totHt(COEFFS%totHt,htTot,dbhOb,COEFFSO%dib17,topHt,&
                &topDib,COEFFSO%a, COEFFSO%b,errFlag)
           COEFFSO%totHt = COEFFS%totHt

        else
           call r9Prep(volEq,dbhOb,topDib,topHt,ht1Prd,ht2Prd,htTot,&
                &spp,geog,COEFFS,forst,maxLen,&
                &minLen,merchL,mTopP,mTopS,stump,trim,minBfD,&
                &prod,iProd,sawDib,plpDib,short,shrtHt,errFlag,&
                &upsHt1)
           !-----    Get DIBs at heights of 4.5' and 17.3'
           call r9dia417(COEFFS,topDib,dbhOb,topHt,ht1Prd,ht2Prd,&
                &htTot,sawDib,plpDib,errFlag,upsHt1,volEq)
           if (errFlag.ne.0) return
           !-----Get total height
           call r9totHt(COEFFS%totHt,htTot,COEFFS%dbhIb,COEFFS%dib17,&
                &topHt,topDib,COEFFS%a, COEFFS%b,errFlag)

        end if
        if (errFlag.ne.0) return

        if (COEFFS%totHt.le.17.3) errFlag = 8
        if (errFlag.ne.0) return
        if (VOLEQ(1:1).eq.'8') then
           ! COEFFSO is coefficients for outside bark. Should use COEFFS for inside bark (2021/03/02)
           !  CALL r9ht(stemHt,COEFFSO,stemDib,errFlag)
           call r9ht(stemHt,COEFFS,stemDib,errFlag)
        else
           call r9ht(stemHt,COEFFS,stemDib,errFlag)
        end if
     else ! REGION 8
        !         Currently only works with total height equation
        !          IF(VOLEQ(3:3).NE.'0')THEN
        !            VOLEQ2 = VOLEQ(1:2)//'0'//VOLEQ(4:10)
        !            ERRFLAG=1
        !            RETURN
        !          ELSE
        !            VOLEQ2 = VOLEQ
        !          ENDIF
        VEQ = VOLEQ(3:3)
        call R8PREPCOEF(VOLEQ, COEFFS, ERRFLAG)
        DBHIB = COEFFS%A4+COEFFS%B4*DBHOB
        if (VEQ.eq.'0'.or.VEQ.eq.'8') then
           DIB17 = DBHOB*(COEFFS%A17+COEFFS%B17*(17.3/HTTOT)**2)
        else if (VEQ.eq.'4'.or.VEQ.eq.'7'.or.VEQ.eq.'9') then
           if (UPSHT1.eq.0.) then
              !            ! CALC upsht1 from HTTOT (2021/03/03)
              VOLEQ2 = VOLEQ(1:2)//'0'//VOLEQ(4:10)
              call R8PREPCOEF(VOLEQ2, COEFFST, ERRFLAG)
              COEFFST%TOTHT = HTTOT
              COEFFST%DBHIB = COEFFST%A4+COEFFST%B4*DBHOB
              COEFFST%DIB17 = DBHOB*(COEFFST%A17+COEFFST%B17*&
                   &(17.3/HTTOT)**2)
              call r9ht(UPSHT1,COEFFST,COEFFS%FIXDI,errFlag)

              if (UPSHT1.eq.0) then
                 if (VEQ.eq.'4') then
                    if (HT2PRD.gt.0) UPSHT1 = HT2PRD
                 else
                    if (HT1PRD.gt.0) UPSHT1 = HT1PRD
                 end if
              end if
           end if
           if (UPSHT1.eq.0) then
              ERRFLAG = 9
              return
           end if
           DIB17 = DBHOB*(COEFFS%A17+COEFFS%B17*(17.3/UPSHT1)**2)
        end if
        COEFFS%DBHIB = DBHIB
        COEFFS%DIB17 = DIB17
        COEFFS%TOTHT = HTTOT
        if (VEQ.eq.'0'.or.VEQ.eq.'8') then
           call r9ht(STEMHT,COEFFS,STEMDIB,errFlag)
        else if (VEQ.eq.'4'.or.VEQ.eq.'7'.or.VEQ.eq.'9') then
           !-----First Get total height
           topHt = UPSHT1
           topDib = COEFFS%FIXDI
           if (HTTOT.le.0.0) then
              VOLEQ2 = VOLEQ(1:2)//'0'//VOLEQ(4:10)
              call R8PREPCOEF(VOLEQ2, COEFFST, ERRFLAG)
              COEFFST%TOTHT = HTTOT
              COEFFST%DBHIB = DBHIB
              COEFFST%dib17 = DIB17
              call r9totHt(COEFFST%totHt,htTot,COEFFST%dbhIb,&
                   &COEFFST%dib17,topHt,topDib,COEFFST%a, COEFFST%b,errFlag)
              if (COEFFST%totHt.le.17.3) errFlag = 8
              if (errFlag.ne.0) return
              HTTOTC = COEFFST%totHt
           end if
           if (STEMDIB.le.COEFFS%FIXDI) then
              VOLEQ2 = VOLEQ(1:2)//'0'//VOLEQ(4:10)
              call R8PREPCOEF(VOLEQ2, COEFFST, ERRFLAG)
              if (HTTOT.eq.0.0) then
                 COEFFST%TOTHT = HTTOTC
                 COEFFST%DBHIB = DBHIB
                 COEFFST%dib17 = DIB17
              else
                 COEFFST%TOTHT = HTTOT
                 COEFFST%DBHIB = COEFFST%A4+COEFFST%B4*DBHOB
                 COEFFST%dib17 = DBHOB*(COEFFST%A17+COEFFST%B17*&
                      &(17.3/HTTOT)**2)
              end if
              call r9ht(STEMHT,COEFFST,STEMDIB,errFlag)
           else
              call R8CLKHT(VOLEQ,DBHOB,HTTOT,topHt,STEMDIB,STEMHT)
           end if
        end if
     end if
  else if (MDL.eq.'DEM' .or. MDL.eq.'CUR' .or.&
       &MDL.eq.'dem' .or. MDL.eq.'cur') then
     if (VOLEQ(8:10).eq.'000') then
        VOLEQ2 = VOLEQ(1:7)//'098'
     else
        VOLEQ2 = VOLEQ
     end if
     if (VOLEQ2(8:10).eq.'042'.or.VOLEQ2(8:10).eq.'242'.or.&
          &VOLEQ2(8:10).eq.'098'.or.VOLEQ2(8:10).eq.'351') then
        !          region 10 call to determine total height or merch height
        call R10HTS(VOLEQ2,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,STEMDIB,&
             &LMERCH)
        STEMHT = LMERCH + STUMP
     end if
  else if (VOLEQ(1:3).eq.'NVB') then
     VOLEQ11 = VOLEQ
     read (VOLEQ11(8:10),'(i3)') SPCD
     call NVB_RefSpcData(SPCD,SPGRPCD,WDSG,SFTHRD,CF,ERRFLAG,&
          &SPGRNWF,SPDRYWF)
     call NVB_Vib(VOLEQ11,DBHOB,HTTOT,Vib,ERRFLAG,SPGRPCD,WDSG)
     call NVB_HT2TOPDib(VOLEQ11,DBHOB,HTTOT,Vib,STEMDIB,STEMHT,&
          &ERRFLAG,SPGRPCD,WDSG)
  end if
  1000 continue
  return
end

subroutine ht2topd_r(VOLEQ,REGN,FORST,DBHOB_d,HTTOT_d,&
     &STMDIB_d,STMHT_d, ERRFLAG, UPSHT1_d,UPSD1_d)
  ! This subroutine is for R user to calculate stem height to a given top DIB      !
  ! YW 04/10/2017

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::ht2topd_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'ht2topd_r_'::ht2topd_r

  implicit none
  double precision :: DBHOB_d,HTTOT_d,STMDIB_d,STMHT_d
  double precision :: UPSHT1_d,UPSD1_d
  ! Variables
  character(len=2) :: FORST
  character(len=10) :: VOLEQ
  integer :: REGN,ERRFLAG

  integer :: FCLASS,HTREF
  real    :: DBHOB,HTTOT,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2
  real    :: AVGZ1,AVGZ2,DBTBH,BTR,STMDIB,STMHT
  DBHOB = real(DBHOB_d)
  HTTOT = real(HTTOT_d)
  STMDIB = real(STMDIB_d)
  UPSHT1 = real(UPSHT1_d)
  UPSD1 = real(UPSD1_d)
  !     Set the default value for other variable
  !      UPSHT1 = 0.0
  !      UPSD1 = 0.0
  DBTBH = 0.0
  BTR = 0.0
  AVGZ1 = 0.0
  HTREF = 0
  UPSHT2 = 0.0
  UPSD2 = 0.0
  AVGZ2 = 0.0
  STMHT = 0.0
  UPSHT2 = 0.0
  UPSD2 = 0.0
  HTREF = 0
  FCLASS = 0

  call HT2TOPD(REGN,FORST,VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,&
       &UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,FCLASS,&
       &STMDIB,STMHT,ERRFLAG)

  STMHT_d = DBLE(STMHT)
  return
  end subroutine ht2topd_r

subroutine HT2TOPDCS(REGN,FORSTI,VOLEQI,DBHOB,HTTOT,HT1PRD,&
     &HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,&
     &FCLASS,STEMDIB,STEMHT,ERRFLAG)

      !DEC$ ATTRIBUTES DLLEXPORT::HT2TOPDCS
  implicit none
  ! Variables
  character(len=*) :: FORSTI
  character(len=*) :: VOLEQI
  integer :: REGN,ERRFLAG,HTREF,FCLASS
  real    :: DBHOB,HTTOT,STEMDIB,STEMHT
  real    :: HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
  real    :: DBTBH,BTR
  character(len=10) :: VOLEQ
  character(len=2) :: FORST

  FORST = FORSTI(1:2)
  VOLEQ = VOLEQI(1:10)
  call HT2TOPD(REGN,FORST,VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,&
       &UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,FCLASS,&
       &STEMDIB,STEMHT,ERRFLAG)
  FORSTI = FORST // char(0)
  VOLEQI = VOLEQ // char(0)
  return
end

subroutine CALCHT2TOPD(REGN,FORSTI,VOLEQI,DBHOB,HTTOT,HT1PRD,&
     &HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,&
     &FCLASS,STEMDIB,STEMHT,ERRFLAG)
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::CALCHT2TOPD
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: CALCHT2TOPD
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'CALCHT2TOPD'::CALCHT2TOPD
  ! Variables
  character(len=*) :: FORSTI
  character(len=*) :: VOLEQI
  integer :: REGN,ERRFLAG,HTREF,FCLASS
  real    :: DBHOB,HTTOT,STEMDIB,STEMHT
  real    :: HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
  real    :: DBTBH,BTR
  character(len=10) :: VOLEQ
  character(len=2) :: FORST

  FORST = FORSTI(1:2)
  VOLEQ = VOLEQI(1:10)
  call HT2TOPD(REGN,FORST,VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,&
       &UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,FCLASS,&
       &STEMDIB,STEMHT,ERRFLAG)
  return
end
