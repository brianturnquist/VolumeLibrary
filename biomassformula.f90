!====================================!
!   VolumeLibrary : biomassformula   !
!====================================!
subroutine BIOMASSFORMULA(EQFORM,DBH,THT,CR, a, b, c, d, e, BMS)
  ! Variables
  integer :: EQFORM
  real    :: DBH,THT,CR,BMS,TopD
  real    :: a,b,c,d,e

  BMS = 0
  !     CR as crown ratio (crown length/total height)
  if (CR.gt.1.or.CR.le.0) CR = 0.5
  !     Default TopD to 4" when no value provided
  TopD = 4

  call BioeqFormula(EQFORM,DBH,THT,CR,TopD,a,b,c,d,e,BMS)
  return
end

subroutine BioeqFormula(EQFORM,DBH,THT,CR,TopD,a,b,c,d,e,BMS)
  ! Variables
  integer :: EQFORM
  real    :: DBH,THT,CR,BMS,TopD,CL
  real    :: a,b,c,d,e

  BMS = 0
  !     CR as crown ratio (crown length/total height)
  if (CR.gt.1.or.CR.le.0) CR = 0.5
  !     Crown Length
  CL = CR*THT
  !     Default TopD to 4" when no value provided
  if (TopD.le.0) TopD = 4
  if (EQFORM.eq.1) then
     !     log10 biomass = a + b * (log10(dia^c))
     BMS = a + b * (LOG10(DBH**c))
     BMS = 10**BMS
  else if (EQFORM.eq.2) then
     !     ln biomass = a + b * dia + c * (ln(dia^d)) + e*tht
     BMS = a + b * DBH + c * (LOG(DBH**d)) + e*THT
     BMS = EXP(BMS)
  else if (EQFORM.eq.3) then
     !     ln biomass = a + b * ln(dia) + c * (d + e * ln(dia))
     BMS = a + b * LOG(DBH) + c * (d + e * LOG(DBH))
     BMS = EXP(BMS)
  else if (EQFORM.eq.4) then
     !     biomass = a + b * dia + c * dia ^ d + e*dia^2*tht
     BMS = a + b * DBH + c * DBH**d + e*DBH**2*THT
  else if (EQFORM.eq.5) then
     !     biomass = a + b * dia + c * dia ^ 2 + d * dia ^ 3 + e*dia*tht
     BMS = a + b * DBH + c * DBH**2 + d * DBH**3 + e*DBH*THT
  else if (EQFORM.eq.6) then
     !     biomass = a * (exp(b + c * ln(dia) + d * dia))
     BMS = a * (EXP(b + c * LOG(DBH) + d * DBH))
  else if (EQFORM.eq.7) then
     !     biomass = a + (b * dia ^ c)/(dia ^ c + d)
     BMS = a + (b * DBH**c)/(DBH**c + d)
  else if (EQFORM.eq.8) then
     !     log100 biomass = a + b * log10(dia)
     BMS = a + b * LOG10(DBH)
     BMS = 100**BMS
  else if (EQFORM.eq.9) then
     !     ln biomass = ln(a) + (b * ln(dia))
     BMS = LOG(a) + (b * LOG(DBH))
     BMS = EXP(BMS)
  else if (EQFORM.eq.10) then
     !     biomass= exp(a + b*ln(dia))*exp(c + d/dia)
     BMS = EXP(a + b*LOG(DBH))*EXP(c + d/DBH)
  else if (EQFORM.eq.11) then
     !     biomass = a + b*dia^2*tht + c*dia^3 + d*dia*tht
     BMS = a + b*DBH**2*THT + c*DBH**3 + d*DBH*THT
  else if (EQFORM.eq.12) then
     !     ln biomass = a + b*ln(dia) + c*ln(tht)
     BMS = a + b*LOG(DBH) + c*LOG(THT)
     BMS = EXP(BMS)
  else if (EQFORM.eq.13) then
     !     biomass = exp(a + b*ln(dia) + d*ln(tht))*c
     BMS = EXP(a + b*LOG(DBH) + d*LOG(THT))*c
  else if (EQFORM.eq.14) then
     !     biomass = exp(a + b*ln(dia))/(c + d*dia^e)
     BMS = EXP(a + b*LOG(DBH))/(c + d*DBH**e)
  else if (EQFORM.eq.15) then
     !     biomass = a + b*tht + c*tht^2
     BMS = a + b*THT + c*THT**2
  else if (EQFORM.eq.16) then
     !     biomass = a * dia^b * tht^c
     BMS = a * DBH**b * THT**c
  else if (EQFORM.eq.17) then
     !     biomass = a + b*dbh + c*dbh^2 + d*tht + e*dbh^2*tht
     BMS = a + b*DBH + c*DBH**2 + d*THT + e*DBH**2*THT
  else if (EQFORM.eq.18) then
     !     ln biomass = a + b*ln(dia) + c*ln(CR) + d*ln(CR*tht)
     !     CR as crown ratio (crown length/total height)
     BMS = a + b*LOG(DBH) + c*LOG(CR) + d*LOG(CR*THT)
     BMS = EXP(BMS)
  else if (EQFORM.eq.19) then
     !     ratio = 1 - EXP(a*TopD^b*DBH^c)
     !     This is the Tip biomass to total stem above stump ratio
     BMS = 1-EXP(a*TopD**b*DBH**c)
  else if (EQFORM.eq.33) then
     if (DBH.lt.11) then
        BMS = a*(DBH**2)**b
     else
        BMS = c*(DBH**2)**d
     end if
  else if (EQFORM.eq.34) then
     if (DBH.lt.11) then
        BMS = a*(DBH**2*THT)**b
     else
        BMS = c*(DBH**2)**d*THT**e
     end if
  else if (EQFORM.eq.35) then
     BMS = a+b*DBH**3+c*DBH**2*CR+d*DBH**2*THT*CR
  else if (EQFORM.eq.36) then
     BMS = a+b*DBH**2+c*DBH**2*THT+d*DBH*THT*CR+e*DBH**2*THT*CR
  else if (EQFORM.eq.37) then
     BMS = a+b*LOG(DBH*THT)+c*LOG(THT*CR)
     BMS = EXP(BMS)
  else if (EQFORM.eq.38) then
     BMS = a+b*THT+c*THT*CR+d*DBH*THT*CR
  else if (EQFORM.eq.39) then
     BMS = a*DBH**b*THT**c*EXP(d*CR)
  else if (EQFORM.eq.40) then
     BMS = a+b*DBH**c*THT**d*CL**e
  else if (EQFORM.eq.41) then
     !       ratio equation
     BMS = 1/(1+a*DBH**b)
  else if (EQFORM.eq.42) then
     BMS = a*DBH**b*EXP(c*THT)
  else if (EQFORM.eq.43) then
     BMS = a*DBH**2*THT*(b+c*TopD/DBH+d*(Topd/DBH)**2)/100.0

  end if
end
