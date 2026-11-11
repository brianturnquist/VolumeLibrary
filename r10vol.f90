!============================!
!   VolumeLibrary : r10vol   !
!============================!
  !== last modified  06-29-2004
  ! 01/05/2021 Corrected SECGRO subroutine for checking value less than zero.
subroutine R10VOL(VOLEQ,MTOPP,MTOPS,HTTOT,HT1PRD,DBHOB,&
     &HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,LOGVOL,&
     &BFPFLG,CUPFLG,SPFLG,ERRFLAG)

  !*                 LOCAL VARIABLES                                   *

  ! Variables
  character(len=10) :: VOLEQ
  character(len=10) :: tmpveq
  character(len=1) :: HTTYPE

  integer :: TLOGS,I,ERRFLAG
  integer :: CUPFLG,BFPFLG,SPFLG

  real    :: MTOPP,MTOPS,tmpht2,holdv(2),holdlv(3,20)
  real    :: DBHOB,HTTOT,THT,LHT,HT1PRD
  real    :: LOGDIA(21,3),LOGLEN(20)
  real    :: NOLOGP,NOLOGS,CUBVOL
  real    :: VOL(15),LOGVOL(7,20)

  do I = 1, 21
     LOGDIA(I,1) = 0.0
     LOGDIA(I,2) = 0.0
     15 continue
  end do

  do I = 1,20
     LOGVOL(1,I) = 0.0
     LOGVOL(2,I) = 0.0
     LOGVOL(3,I) = 0.0
     LOGVOL(4,I) = 0.0
     LOGVOL(5,I) = 0.0
     LOGVOL(6,I) = 0.0
     LOGVOL(7,I) = 0.0
     LOGLEN(I) = 0.0
     20 continue
  end do

  if (MTOPP .le.0.0) MTOPP = 6.0
  if (MTOPS .le.0.0) MTOPS = 4.0

  if (HTTYPE.eq.'L'.or.HTTYPE.eq.'l') then
     if (VOLEQ(2:3).eq.'32'.or.VOLEQ(2:3).eq.'62') then
        LHT = HT1PRD/10.0
     else
        LHT = HT1PRD
     end if
     THT = 0.0
  else
     THT = HTTOT
     LHT = 0.0
     ! smaltree check.  If tree is le 40 or dbhob lt 9, small tree logic
     if (HTTOT.le.40.or. DBHOB.lt.9.0) then
        !--      USE D SQUARED H EQUATIONS FOR SMALL TREES
        if (DBHOB.le.3.5.or. HTTOT.lt.18.0) then
           call FSTGRO (DBHOB,HTTOT,CUBVOL)
           VOL(1) = ANINT(cubvol*10.0)/10.0
        else
           call SECGRO (DBHOB,HTTOT,CUBVOL)
           logvol(4,1) = ANINT(cubvol*10.0)/10.0
           logdia(2,1) = mtopp
           NOLOGP = 1.
           TLOGS = 1
           VOL(1) = ANINT(cubvol*10.0)/10.0
           VOL(4) = ANINT(cubvol*10.0)/10.0
        end if
        if (VOL(4) .lt.0.0) then
           VOL(4) = 0.0
        end if
        if (VOL(1) .lt.0.0) then
           VOL(1) = 0.0
        end if
        !           THE VOL(1) IS CVTS. SET A VERY SMALL NUMBER FOR STUMP, SO THERE WILL BE NO STUMP VOL CALC
        VOL(14) = 0.0001
        go to 1000
     end if
  end if

  call R10VOLO(VOLEQ,DBHOB,THT,LHT,HTTYPE,MTOPP,NOLOGP,VOL,&
       &LOGLEN,LOGDIA,logvol,BFPFLG,CUPFLG,ERRFLAG)
  TLOGS = ANINT(NOLOGP)

  1000 return
end

subroutine FSTGRO(D,H,VN)

  !      WHERE:
  !              D      = DBHOB
  !              H      = TOTAL TREE HEIGHT FROM GROUND TO TIP
  !              VN     = CUBIC-FOOT VOLUME ESTIMATED BY THIS SUBROUTINE

  !   **FSTGRO DATE OF LAST REVISION:   11:06:49  06/20/89

  ! FSTGRO COMPUTES CUBIC-FOOT VOLUMES FOR TREES WITH (D LT 4 OR H LT 18)

  ! Variables
  real    :: D,H,VN,TERM1,TERM2,FORM

  if (H .le.4.5) then !3/22/02  small height check
     VN = 0.
     return
  end if

  if (H.le.18) then
     TERM1 = ((H - 0.9)*(H - 0.9)) / ((H - 4.5)*(H - 4.5))
     TERM2 = TERM1*(H - 0.9)/(H - 4.5)
     FORM = 0.406098*TERM1 - 0.0762998*D*TERM2 +&
          &0.00262615*D*H*TERM2
  else
     FORM = 0.480961 + 42.46542/(H*H) - 10.99643*D/(H*H) -&
          &0.107809*D/H - 0.00409083*D
  end if
  VN = 0.005454154*FORM*D*D*H

  if (VN.lt.0.0)  VN = 0.0

  return
end

subroutine SECGRO(D,H,VN)

  !       WHERE
  !                 D      = DBHOB
  !                 H      = TOTAL TREE HEIGHT FROM GROUND TO TIP
  !                 VN     = CUBIC_FOOT VOLUME ESTIMATED BY THIS SUBROUTINE

  !     **SECGRO DATE OF LAST REVISION:   11:06:49  06/22/89

  !     ENTRY SECGRO COMPUTES VOLUMES FOR SECOND GROWTH TREES
  !     (D GE 4 AND H GE 18)  UP TO (D LE 9 AND H LT 40)

  ! Variables
  real    :: D,H,VN

  VN = -5.577 + 1.9067 * ALOG(D) + 0.9416 * ALOG(H)
  ! It is not necessary to check the negative value for VN. This cause volume error.
  ! Commended out the following lines on 01/05/2021 (YW)
  !      IF (VN.GT.0.0) THEN
  !         VN = EXP(VN)
  !      ELSE
  !        VN=0.0
  !      ENDIF
  VN = EXP(VN)

  return
end
