!=============================!
!   VolumeLibrary : sf_2pth   !
!=============================!
  !== last modified  10-4-2002
subroutine SF_2PTH(JSP,GEOSUB,SETOPT,DBH,H1,d1,DBTBH,TotalH,&
     &HEX,ZEX,F,RHFW,RFLW,TAPCOE)
  !                                                               SF_2PTH
  !                                  separate entry:              SF_2PTHA
  !                                  separate entry:              SF_2PTHB

  !==klc  June 5, 2000
  !     Stem form   Specify the  DBH and some  (height, diameter),
  !                 but total height is unknown, and is to be found.
  !                 SP_2PTHA is identical, but has fewer input arguments.
  !                 SP_2PTHB same as SP_2PTHB, but SF_2PT is
  !                          automatically called by this routine. (Use
  !                          if there is one and only one upper diameter meas.)

  !     DBH_arg   input   R*4    DBH
  !     HT_arg    input   R*4    Total tree height
  !     ASP       input   A2      Species (and coefficient-set) code.
  !                                 HI   (ITT Western Hemlock)
  !                                 DI   (ITT Douglas Fir - proprietary)
  !                                 DF   1994 Douglas Fir
  !                                 WH   1994 Western Hemlock
  !                                 RC   1994 Red Cedar

  !     IERR      output  I       0  No Error
  !                               1  Unrecognized species code
  !                               2  Invalid DBH
  !                               3  Invalid height
  !                               4  This species not available in metric.
  !                               5  Supplemental arguments can not be used
  !                                  because regression coef file not
  !                                  available for this species.
  !                               6  Region code not recognized.
  !                               7  H1, D1 are a bad combination.
  !                               8  Fail to find a solution.

  !     DBT_ARG     input    R*4   Double Bark Thickness at B.H.  (0: unknown)
  !     REGION_ARG  input    A2    Region code (blank: unspecified)
  !     SUPV_ARG    input    R*4(10) Supplemental arguments
  !                                 See documentation. DO NOT PROVIDE all NSUP.
  !                                 A value of zero ALWAYS implies missing.
  !                                 (1)  Breast height Age
  !                                 (2)  Crown Ratio (Fraction of TOTAL HT w crwn)
  !                                 (3)  Site index (in feet, base 50 yrs BH)
  !                                 (4)  Stand density index (SDI)
  !     H1_ARG       input   R*4    Height with a known diameter
  !     D1_ARG       input   R*4    Diameter at H1_ARG
  !     IDTYPE       input   I      1 if D1_ARG is o.b., 2 if inside bark
  !     HT           output  R*4    Total tree height (precision about 0.1 ft)

  !     General note:  This entire procedure is based on a FAULTY statistical
  !                    assumption: that total height (an input variable) in
  !                    regression can now be treated as an output variable.
  !                    There is nothing in regression theory that suggests
  !                    this is a good idea.
  !                    It is IMPERATIVE that the user check for reasonableness.
  !                    Depending on cruising methods, TOTAL height should
  !                    never exceed F0  + F1* H1_ARG where  (F0, F1) is
  !                    about (10 feet, 1.25).

  ! Variables
  real    :: DBH,TOTALH,DBTBH,h1,d1
  real    :: htry,eval,HTLOW,HTHI,EVLOW,EVHI
  !      REAL*4  SUPV_arg(4)
  real*4  :: HEX(2),ZEX(2),BH,HT,DEL_H,SF_2PTH1,AER,TOL
  real*4  :: RHFW(4),RFLW(6),TAPCOE(12),F
  integer :: JSP,SETOPT(6),IDTYPE,FLAG,ITRY,IERROR
  !      LOGICAL METRIC
  !     character*1 unit
  character(len=2) :: GEOSUB

  !              idtype = 2 for inside bark top diameter
  IDTYPE = 2
  BH = 4.5

  if (h1.le.1.33*BH  .or.  d1.ge. dbh) then
     !            IERR=7
     HT = 0.0
     return
  end if

  !       find a suitable DEL_H as a search unit.

  DEL_H = h1 * d1/(dbh -d1)
  if (del_H  .gt.0.25*h1)  del_h = 0.25*h1
  if (del_H  .lt.0.05*h1)  del_h = 0.05*h1

  !      Find a suitable upper bound on total ht

  HTLOW = H1
  EVLOW = D1 * (-1)
  flag = 0
  HTRY = H1+ DEL_H
  do ITRY = 1,25
     EVAL = SF_2PTH1(JSP,GEOSUB,SETOPT,DBH,HTRY,DBTBH,HEX,ZEX,F,RHFW,&
          &RFLW,TAPCOE,h1,d1)
     if (EVAL.gt.0.0.or. FLAG .eq.1) then
        !          have bracketed a viable solution. Now find it.
        !         check to see if eval is within tolerance
        if (abs(eval) .lt.0.01) then
           !                we are done
           TOTALH = HTRY
           call SF_2PT(JSP,GEOSUB,SETOPT,DBH,TOTALH,DBTBH,F,RHFW,&
                &RFLW,TAPCOE)
           goto 120
        else
           flag = 1
           if (EVAL < 0) then
              EVLOW = EVAL
              HTLOW = HTRY
           else
              EVHI = EVAL
              HTHI = HTRY
           end if
           AER = ABS(EVLOW)
           !            using ratios, find the next height to try
           HTRY = (AER/(AER+EVHI))*(HTHI-HTLOW) + HTLOW
        end if
     else
        HTLOW = HTRY
        EVLOW = EVAL
        HTRY = HTRY+ DEL_H
     end if

     100 continue
  end do
  120 continue
  TOL = 0.1
  IERROR = 0
  !         TOTALH = ZBRENT(EVAL, HLAST, HTRY, TOL, izerror)
  !        find total height using ratio of htry, hlast
  !        TOTALH = HTRATIO(EVAL,EVLAST,HTRY,HLAST,TOL,IZERROR)

  !        IERR=IERROR
  !        IF (IZERROR.ne. 0 ) IERR=IZERROR
  !                                           Have solution.
  !               If sf_2PTHB was called, then call 2pt .
  !               Otherwise the user is responsible for making this call.
  !                  We could call SF-3PT, but we won't
  !         NP=1
  !         HV(1)= H1_arg
  !         DV(2) = D1_ARG
  !         CALL SF_3PT(NP, HV, DV, IDTYPE_A, ZV)
  !         ENDIF
  !      endif
  return
end

function SF_2PTH1(JSP,GEOSUB,SETOPT,DBH,HTRY,DBTBH,HEX,ZEX,F,RHFW,&
     &RFLW,TAPCOE,h1,d1)

  !                                                         SF_2PTH1
  !      HTRY      IN   R*4    A trial value for total ht.
  !      SF_2PTH1  OUT  R*4    Predicted dib at h1 minus measured diameter there

  ! Variables
  real    :: SF_2PTH1
  real    :: DBH,HTRY,DBTBH,h1,d1,d2,SLOPE
  !      REAL*4  SUPV_arg(4)
  real*4  :: HEX(2),ZEX(2),DOB,DBT,DIFF
  real*4  :: RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
  integer :: JSP,SETOPT(6),NP
  integer :: ineedsl,IDTYPE
  !      LOGICAL METRIC
  !     character*1 unit
  character(len=2) :: GEOSUB
  !                                develop taper assuming HT=HTRY
  call SF_2PT(JSP,GEOSUB,SETOPT,DBH,HTRY,DBTBH,F,RHFW,&
       &RFLW,TAPCOE)

  !      IF(IERR.gt.0) then
  !          SF_2PTH1=0.
  !          IERROR=IERR
  !          RETURN
  !        ENDIF

  !                                find predicted diameter at H1
  INEEDSL = 0

  call SF_DS(JSP,geosub,NP,SETOPT,ineedsl,slope,DBH,HTRY,H1,HEX,ZEX,&
       &RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,D2)

  if (JSP.ge.22.and. JSP.le.30) then
     call BRK_UP(JSP,geosub,DBH,HTRY,DBTBH,H1,D2,DOB,DBT)
  end if

  !                                  calculate the error.
  IDTYPE = 2
  if (IDTYPE.eq.2) then
     !                                  specified diameter is inside bark
     DIFF = D2 - D1
  else
     !                                  specified diameter is o.b.
     DIFF = DOB - D1
  end if
  SF_2PTH1 = DIFF
  return
end
