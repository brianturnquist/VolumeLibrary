!============================!
!   VolumeLibrary : r6vol3   !
!============================!
  !== last modified  01-11-2013
  ! 01/11/2013 added calculation for stump vol.
  ! 04/13/2017 moved stump vol calc to voinit

subroutine R6VOL3(DBHOB,DBTBH,FCLASS,HTTOT,ZONE,VOL)

  ! Variables
  real    :: DBHOB,DBTBH,HTTOT,VOL(15),DBHIB,HTUP,HRATIO,S,DR
  real    :: HX,H
  integer :: FCLASS,I,ZONE
  real    :: D(21),LOGVOL,D17,TOPD,A,B,H17

  do 25, I = 1,20
  D(I) = 0.0
  25 continue

  TOPD = 4.0
  D17 = FCLASS/100.0 *DBHOB
  if (ZONE .eq.1) H17 = 17.3
  if (ZONE .eq.2) H17 = 33.6
  DBHIB = DBHOB-DBTBH
  if (DBHIB.le.0.or. DBHIB.gt.DBHOB) DBHIB = DBHOB

  if (DBHIB .lt. TOPD) then
     !          SMAILIANS FOR WHOLE TREE
     VOL(1) = 0.00272708*(DBHIB*DBHIB)*HTTOT
  else if (D17.lt. TOPD) then
     !        SMALL TREE - USE D17 FOR BUTT LOG VOLUME
     LOGVOL = 0.00272708*(DBHIB*DBHIB + D17*D17)*H17
     VOL(1) = LOGVOL
     !        FIND VOLUME TO TOP OF TREE
     LOGVOL = 0.00272708*(D17*D17)*(HTTOT-H17)
     VOL(1) = VOL(1) + LOGVOL
  else
     A = 0.62
     B = 1.0 - A
     !        FIND CUBIC VOLUME FOR BUTT LOG
     LOGVOL = 0.00272708*(DBHIB*DBHIB + D17*D17)*H17
     VOL(1) = LOGVOL
     !        FIND DIAMETER FOR UPPER STEMS
     HTUP = HTTOT - H17
     D(1) = D17
     do I = 2,20
        HRATIO = (HTUP - ((I-1) * 16.3))/HTUP
        if (HRATIO.le.0.0) go to 100

        DR = HRATIO / (A*HRATIO+B)
        D(I) = DR * D(1)

        if (D(I).lt.TOPD)go to 100
        !           FIND VOLUME FOR THE SECTION
        LOGVOL = 0.00272708*(D(I-1)*D(I-1) + D(I)*D(I))*16.3
        VOL(1) = VOL(1) + LOGVOL
        if (D(I) .eq.TOPD) then
           S = 16.3
           go to 130
        end if
        90 continue
     end do

     100 DR = TOPD / D17
     HX = (DR*B*HTUP) / (1.0 - (A*DR))
     H = (I-2) * 16.3
     S = HTUP - HX - H
     LOGVOL = 0.00272708*(D(I-1)*D(I-1) + TOPD*TOPD)*S
     VOL(1) = VOL(1) + LOGVOL
     !       FIND VOLUME FOR TOP OF TREE
     130 HTUP = HTTOT - (16.3 * (I-2) + H17) - S
     LOGVOL = 0.00272708*(TOPD*TOPD)*HTUP
     VOL(1) = VOL(1) + LOGVOL
     !       calculate stump volume
     !        VOL(14)=0.005454154*DBHIB**2
  end if

  return
end
