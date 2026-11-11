!=============================!
!   VolumeLibrary : sf_corr   !
!=============================!
  !== last modified  09-14-2007
function SF_CORR(JSP,geosub,TOTALH,HI,HJ)
  !                     INTERNAL ROUTINES:
  !                                          COR_C2
  !                                          COR_OT
  !                                          COR_BH
  !                                          COR_WS

  !     given 2 heights (hi and hj), estimate the correlation of the
  !       errors in the corresponding dib's
  ! Variables
  character(len=2) :: geosub
  integer :: JSP
  real    :: TOTALH,HI,HJ,SF_CORR,COR_C2,COR_BH,COR_OT,COR_WS,cor_ak

  if ( JSP.ge.11.and.  jsp.le.21 ) then
     SF_CORR = COR_C2(JSP,geosub,TOTALH,HI, HJ)
     return

  else if (JSP.eq.22) then
     SF_CORR = COR_BH(TOTALH,HI,HJ)
     return
  else if (JSP.ge.23.and. JSP.le.30) then
     SF_CORR = COR_OT(JSP,TOTALH,HI,HJ)
     return
  else if (JSP.ge.31.and. JSP.le.36) then
     SF_CORR = COR_AK(JSP,TOTALH,HI,HJ)
     return
  else if ( JSP.eq.1) then
     !          SF_CORR=CORRH(HI,HJ)
     return
  else  if ( JSP.eq.2) then
     !          SF_CORR=CORRD(HI,HJ)
     return
  else if (JSP.ge.3.and. JSP.le.5) then
     SF_CORR = COR_WS(JSP,TOTALH,HI,HJ)
     return
  end if

  !      WRITE(*,11)
  !11    format(' FUNCTION CORR GIVEN INVALID SPECIES.  STOP')
  return
end
