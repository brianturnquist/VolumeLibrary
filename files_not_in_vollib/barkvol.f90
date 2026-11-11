!=============================!
!   VolumeLibrary : barkvol   !
!=============================!
  ! The percentage (in decimal) of bark volume to wood volume is calculated based this paper:
  ! Bark Volume Determinatiopn in Trees, H. A. Meyer (1946),
  ! Journal of Forestry, Vol. 44 No. 12
subroutine BARKVOLPCT(REGN, FORST, DIST, SPN, DBH, HT, BTR, DBTBH,&
     &BVP)
  ! Common blocks
  include FVS_FIAJSP.INC
  include FIASP_SGMCBP.INC
  ! Variables
  integer :: REGN,SPN,FORNUM,IS,CNT,I
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: VAR
  character(len=3) :: SPC
  real    :: DBH,HT,DBTBH,BTR,BVP,k

  BVP = 0.0
  if (BTR.gt.1.0) then
     k = BTR/100
  else if (BTR.gt.0) then
     k = BTR
  else if (DBTBH.gt.0) then
     k = (DBH-DBTBH)/DBH
  else
     write (SPC, '(I0)') SPN
     if (SPC(3:3) .lt. '0') then
        SPC(2:3) = SPC(1:2)
        SPC(1:1) = '0'
     end if
     GETVARIANT(REGN,FORST,DIST,VAR)
     ! get bark ratio (DIB/DOB) from variant equation
     if (VAR.eq.'AK') then
        CNT = COUNT(AK_FIAJSP)
        IS = VAR_SPIDX(CNT, AK_FIAJSP, SPC)
        AK_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'BM') then
        CNT = COUNT(BM_FIAJSP)
        IS = VAR_SPIDX(CNT, BM_FIAJSP, SPC)
        BM_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'CA') then
        CNT = COUNT(CA_FIAJSP)
        IS = VAR_SPIDX(CNT, CA_FIAJSP, SPC)
        CA_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'CI') then
        CNT = COUNT(CI_FIAJSP)
        IS = VAR_SPIDX(CNT, CI_FIAJSP, SPC)
        CI_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'CR') then
        CNT = COUNT(CR_FIAJSP)
        IS = VAR_SPIDX(CNT, CR_FIAJSP, SPC)
        CR_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'CS') then
        CNT = COUNT(CS_FIAJSP)
        IS = VAR_SPIDX(CNT, CS_FIAJSP, SPC)
        CS_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'EC') then
        CNT = COUNT(EC_FIAJSP)
        IS = VAR_SPIDX(CNT, EC_FIAJSP, SPC)
        EC_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'EM') then
        CNT = COUNT(EM_FIAJSP)
        IS = VAR_SPIDX(CNT, EM_FIAJSP, SPC)
        EM_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'IE') then
        CNT = COUNT(IE_FIAJSP)
        IS = VAR_SPIDX(CNT, IE_FIAJSP, SPC)
        IE_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'KT') then
        CNT = COUNT(KT_FIAJSP)
        IS = VAR_SPIDX(CNT, KT_FIAJSP, SPC)
        KT_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'LS') then
        CNT = COUNT(LS_FIAJSP)
        IS = VAR_SPIDX(CNT, LS_FIAJSP, SPC)
        LS_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'NC') then
        CNT = COUNT(NC_FIAJSP)
        IS = VAR_SPIDX(CNT, NC_FIAJSP, SPC)
        NC_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'NE') then
        CNT = COUNT(NE_FIAJSP)
        IS = VAR_SPIDX(CNT, NE_FIAJSP, SPC)
        NE_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'NI') then
        CNT = COUNT(NI_FIAJSP)
        IS = VAR_SPIDX(CNT, NI_FIAJSP, SPC)
        NI_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'PN') then
        CNT = COUNT(PN_FIAJSP)
        IS = VAR_SPIDX(CNT, PN_FIAJSP, SPC)
        PN_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'SN') then
        CNT = COUNT(SN_FIAJSP)
        IS = VAR_SPIDX(CNT, SN_FIAJSP, SPC)
        SN_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'SO') then
        CNT = COUNT(SO_FIAJSP)
        IS = VAR_SPIDX(CNT, SO_FIAJSP, SPC)
        SO_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'TT') then
        CNT = COUNT(TT_FIAJSP)
        IS = VAR_SPIDX(CNT, TT_FIAJSP, SPC)
        TT_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'UT') then
        CNT = COUNT(UT_FIAJSP)
        IS = VAR_SPIDX(CNT, UT_FIAJSP, SPC)
        UT_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'WC') then
        CNT = COUNT(WC_FIAJSP)
        IS = VAR_SPIDX(CNT, WC_FIAJSP, SPC)
        WC_BRATIO(IS,DBH,HT,k)
     else if (VAR.eq.'WS') then
        CNT = COUNT(WS_FIAJSP)
        IS = VAR_SPIDX(CNT, WS_FIAJSP, SPC)
        WS_BRATIO(IS,DBH,HT,k)

     end if
  end if
  ! The percentage of bark volume to wood volume calculate using bark ratio (DIB/DOB) as below
  ! BVP is in decimal, i.e. 18% will be 0.18
  BVP = 1.0/(k*k) - 1.0
  ! If the bark volume percentage cannot be calculated, us the default value from FIADB for the species
  if (BVP.gt.1.0.or. BVP.lt.0.1) then
     BVP = 0
     do I = 1, 455
        if (FIASP_SG(I, 1).eq.SPN) then
           BVP = FIASP_SG(I, 6)/100.0
           return
        end if
     end do
     if (BVP.eq.0) then
        if (SPN.lt.300) then
           ! Use the other softwood 299 for softwood species
           BVP = FIASP_SG(119,6)/100.0
        else
           BVP = FIASP_SG(455,6)/100.0
        end if
     end if
  end if
end

  ! Search the list of the species for the variant to find the position of the spceies
function VAR_SPIDX(CNT, FIAJSP, SPC)
  INTEFER CNT, I, SPIDX
  ! Variables
  character(len=3) :: SPC
  character(len=3) :: FIAJSP(CNT)

  SPIDX = 0
  do I = 1, CNT
     if (FIAJSP(I).eq.SPC) then
        SPIDX = I
        return
     end if;
  end do
  ! If the species is not in the variant list species, use the last species as default
  if (SPIDX.eq.0) SPIDX = CNT
end
