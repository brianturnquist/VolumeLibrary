!============================!
!   VolumeLibrary : sf_shp   !
!============================!
  !== last modified  09-14-2007
subroutine SF_SHP(JSP,GEOSUB,SETOPT,DBHOB,TOTALH,RFLW,RHFW,DBTBH,&
     &DBT_USER,DBHIB)
  ! Variables
  character(len=2) :: GEOSUB
  integer :: JSP,SETOPT(6)
  real    :: DBHOB,TOTALH,dbt_user,dbhib,DBTBH
  real    :: RFLW(6),RHFW(4),fdbt_c1,fdbt_c2,fdbt_ak

  !                  estimate diameter inside bark at breast height .
  if (JSP.ge.3.and. JSP.le.5) then
     if (DBT_USER .le.0.) then
        DBTBH = fdbt_c1(JSP,GEOSUB,DBHOB,TOTALH)
     else
        DBTBH = DBT_USER
     end if
     DBHIB = DBHOB  - DBTBH
     !        write(*,*)dbh,dbh_ib,DBTBH
  else if (JSP.ge.11.and. JSP.le.21) then
     if (DBT_USER .le.0.) then
        DBTBH = FDBT_C2(JSP,GEOSUB,SETOPT,DBHOB,TOTALH)
     else
        DBTBH = DBT_USER
     end if
     DBHIB = DBHOB  - DBTBH

  else if (JSP.ge.31.and. JSP.le.36) then
     if (DBT_USER .le.0.) then
        DBTBH = FDBT_AK(JSP,SETOPT,DBHOB,TOTALH)
     else
        DBTBH = DBT_USER
     end if
     DBHIB = DBHOB - DBTBH

  else if (JSP.ge.22.and. JSP.le.30) then
     DBHIB = DBHOB
  end if
  !                  apply empirical equations to find shape parameters.
  if (JSP.eq.3) then
     call SHP_W3(DBHOB,TOTALH,GEOSUB,RFLW,RHFW)
  else if (JSP.eq.4) then
     call SHP_W4(DBHOB,TOTALH,GEOSUB,RFLW,RHFW)
  else if (JSP.eq.5) then
     call SHP_W5(DBHOB,TOTALH,GEOSUB,RFLW,RHFW)
  else if (JSP.ge.11.and. JSP.le.21) then
     call SHP_C2(jsp,DBHOB,TOTALH,GEOSUB,RFLW,RHFW)
  else if (JSP.eq.22) then
     call SHP_BH(DBHOB,TOTALH,RFLW,RHFW)
  else if (JSP.ge.23.and. JSP.le.30) then
     call SHP_OT(JSP,DBHOB,TOTALH,RFLW,RHFW)
  else if (JSP.ge.31.or. JSP.le.36) then
     call SHP_AK(jsp,GEOSUB,DBHOB,TOTALH,RFLW,RHFW)
  end if

  return
end
