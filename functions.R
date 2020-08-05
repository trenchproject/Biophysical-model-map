Tb_grasshopper=function(T_a, T_g, u, H, K_t, psi, L, Acondfact=0.25, z=0.001, abs=0.7, r_g=0.3){
  
  TaK<- T_a+273.15 #Ambient temperature in K
  T_g<- T_g+273.15 #Ambient temperature in K
  
  #Biophysical parameters
  #IR emissivity
  omega<-5.66 * 10^-8 # stefan-boltzmann constant (W m^-2 K^-4)
  epsilon=1 #Gates 1962 in Kingsolver 1983  #emissivity of surface to longwave IR
  
  Kf=0.025  #Kf=0.024+0.00007*T_a[k] #thermal conductivity of fluid
  
  #kineamatic viscosity of air (m^2/s); http://users.wpi.edu/~ierardi/PDF/air_nu_plot.PDF
  v=15.68*10^-6  #m^2/s, kinematic viscocity of air,  at 300K #http://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html
  
  #AREAS
  #Samietz (2005): The body of a grasshopper female was approximated by a rotational ellipsoid with half the body length as the semi-major axis q.
  #Area from Wolfram math world
  c<- L/2 #c- semi-major axis, a- semi-minor axis
  a<- (0.365+0.241*L*1000)/1000  #regression in Lactin and Johnson (1988)
  e=sqrt(1-a^2/c^2)
  A=2*pi*a^2+2*pi*a*c/e*asin(e)
  
  #------------------------------
  #SOLAR RADIATIVE HEAT FLUX   
  #Separate total radiation into components
  #Use Erbs et al model from Wong and Chow (2001, Applied Energy 69:1991-224)
  
  #kd- diffuse fraction
  kd=1-0.09*K_t #if(K_t<=0.22) 
  kd[K_t>0.22 & K_t<=0.8]= 0.9511 -0.1604*K_t +4.388*K_t^2 -16.638*K_t^3 +12.336*K_t^4
  kd[K_t>0.8]=0.165 #kd = 0.125 #Correction from 16.5 for CO from Olyphant 1984
  
  Httl=H
  Hdir=Httl*(1-kd)
  Hdif=Httl*kd;     
  
  #------------------------------
  #Anderson 1979 - calculates radiation as W without area dependence 
  psi_r=psi*pi/180 #psi in radians
  
  #Calculate Qabs as W
  Qdir=abs*Hdir/cos(psi_r) #direct radiation
  Qdif=abs*Hdif #diffuse radiation
  Qref= r_g *Httl #reflected radiation
  Qabs= Qdir + Qdif + Qref  #W/m2
  
  #------------------------------
  #convection
  
  #Reynolds number- ratio of interval viscous forces
  #L: Characeristic dimension (length)
  # u= windspeed #Lactin and Johnson add 1m/s to account for cooling by passive convection
  Re= u*L/v
  #Nusselt number- dimensionless conductance
  Nu=0.41* Re^0.5 #Anderson 1979 empirical
  h_c= Nu *Kf /L # heat transfer coefficient, Wm^{-2}C^{-1} #reported in Lactin and Johnson 1998
  
  hc_s<- h_c *(-0.007*z/L +1.71) # heat transfer coefficient in turbulent air 
  
  #conduction 
  Thick= 6*10^(-5) #cuticle thickness (m)
  hcut= 0.15 #W m^-1 K^-1
  Acond=A * Acondfact 
  #Qcond= hcut *Acond *(Tb- (T_a+273))/Thick
  
  #------------------------------
  #Energy balance based on Kingsolver (1983, Thermoregulation and flight in Colias butterflies: elevational patterns and mechanistic limitations. Ecology 64: 534–545).
  
  #Thermal radiative flux
  #Areas
  # silhouette area / total area
  sa<-0.19-0.00173*psi #empirical from Anderson 1979, psi in degrees
  Adir= A*sa
  Aref=Adir 
  
  #Calculate Qabs as W/m2
  Qdir=abs*Adir*Hdir/cos(psi_r)
  Qdif=abs*Aref*Hdif
  Qref= r_g * Aref *Httl
  Qabs= Qdir + Qdif + Qref  #W/m2
  
  Tsky=0.0552*(T_a+273.15)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), Kingsolver 1983 estimates using Brunt equation
  
  #Qt= 0.5* A * epsilon * omega * (Tb^4 - Tsky^4) +0.5 * A * epsilon * omega * (Tb^4 - T_g^4) 
  #Convective heat flux
  #Qc= hc_s * A * (Tb- (T_a+273)) 
  #Qs= Qt+ Qc
  
  #WITH CONDUCTION
  #t solved in wolfram alpha #Solve[a t^4 +b t -d, t]
  a<- A * epsilon *omega
  b<-hc_s * A + hcut*Acond/Thick
  d<- hc_s*A*TaK +0.5*A*epsilon *omega*(Tsky^4+T_g^4)+ hcut *Acond*T_g/Thick +Qabs
  
  #WITHOUT CONDUCTION
  #a<- A * epsilon *omega
  #b<-hc_s * A
  #d<- hc_s*A*TaK +0.5*A*epsilon *omega*Tsky^4 +0.5*A*epsilon *omega*T_g^4 +Qabs
  
  #eb<-function(Tb) 0.5* A * epsilon * omega * (Tb^4 - Tsky^4) +0.5 * A * epsilon * omega * (Tb^4 - T_g^4) + hc_s * A * (Tb-TaK)+hcut *Acond *(Tb-T_g)/Thick -Qabs 
  #r <- uniroot(eb, c(-1,373), tol = 1e-5)
  #r$root-273
  
  #roots
  tb = 1/2*sqrt((2*b)/(a*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)))-(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)+(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3))-1/2*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)) 
  
  #convert NaN to NA
  tb[which(is.na(tb))]=NA
  
  return(tb-273.15)
}


Tb_lizard=function(T_a, T_g, u, svl, m, psi, rho_S, elev, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5){

  psi= psi*pi/180 #convert zenith angle to radians
  
  # constants
  sigma=5.67*10^-8 # stefan-boltzman constant, W m^-2 K^-4
  c_p=29.3 # specific heat of air, J/mol °C (p.279) Parentheses all from Campbell & Norman 1998
  
  tau=0.65 # atmospheric transmisivity
  S_p0=1360 # extraterrestrial flux density, W/m^2 (p.159)
  
  # Calculate radiation
  # view angles, parameterize for animal suspended above ground (p181), on ground- adjust F_e, F_r, and F_g
  h=svl/1000 # length of svl in m
  
  A=0.121*m^0.688   # total lizard area, Roughgarden (1981)
  A_p= (-1.1756810^-4*psi^2-9.2594*10^-2*psi+26.2409)*A/100      # projected area
  F_p=A_p/A
  
  # radiation
  p_a=101.3* exp (-elev/8200)  # atmospheric pressure
  m_a=p_a/(101.3*cos (psi))  # (11.12) optical air mass
  m_a[(psi>(80*pi/180))]=5.66
  
  # Flux densities
  epsilon_ac= 9.2*10^-6*(T_a+273)^2 # (10.11) clear sky emissivity
  L_a=sigma*(T_a+273)^4  # (10.7) long wave flux densities from atmosphere 
  L_g=sigma*(T_g+273)^4  # (10.7) long wave flux densities from ground
  
  S_d=0.3*(1-tau^m_a)* S_p0 * cos(psi)  # (11.13) diffuse radiation
  
  dd2= 1+2*0.1675*cos(2*pi*doy/365)
  S_p=S_p0*tau^m_a*dd2 *cos(psi)  #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
  S_b = S_p * cos(psi)
  S_t = S_b + S_d
  S_r= rho_S*S_t # (11.10) reflected radiation
  
  #__________________________________________________
  # conductance
  
  dim=svl/1000 # characteristic dimension in meters
  g_r= 4*epsilon_s*sigma*(T_a+273)^3/c_p # (12.7) radiative conductance
  
  g_Ha=1.4*0.135*sqrt(u/dim) # boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976)
  
  #__________________________________________________
  # operative environmental temperature
  
  #calculate with both surface and air temp (on ground and in tree)
  
  sprop=1 #proportion of radiation that is direct, Sears and Angilletta 2012
  R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
  Te=T_a+(R_abs-epsilon_s*sigma*(T_a+273)^4)/(c_p*(g_r+g_Ha))         # (12.19) Operative temperature            
  Te_surf= T_g+(R_abs-epsilon_s*sigma*(T_g+273)^4)/(c_p*(g_r+g_Ha))        
  
  # calculate in shade, no direct radiation
  sprop=0 #proportion of radiation that is direct, Sears and Angilletta 2012
  R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
  TeS=T_a+(R_abs-epsilon_s*sigma*(T_a+273)^4)/(c_p*(g_r+g_Ha))         # (12.19) Operative temperature                        
  TeS_surf=T_g+(R_abs-epsilon_s*sigma*(T_g+273)^4)/(c_p*(g_r+g_Ha))  
  
  #Select Te to return
  if(sun==TRUE & surface==TRUE) Te= Te_surf
  if(sun==TRUE & surface==FALSE) Te= Te
  if(sun==FALSE & surface==TRUE) Te= TeS_surf
  if(sun==FALSE & surface==FALSE) Te= TeS
  
  return(Te) 
}



zenith_angle=function(doy, lat, lon, hour, offset=NA){
  
  lat=lat*pi/180 #to radians
  
  RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + doy))); # Revolution angle in radians
  DecAng = asin(0.39795 * cos(RevAng));                            # Declination angle in radians           
  
  f=(279.575+0.9856*doy)  # f in degrees as a function of day of year, p.169 Campbell & Norman 2000
  f=f*pi/180 #convert f in degrees to radians
  ET= (-104.7*sin (f)+596.2*sin (2*f)+4.3*sin (3*f)-12.7*sin (4*f)-429.3*cos (f)-2.0*cos (2*f)+19.3*cos (3*f))/3600   # (11.4) Equation of time: ET is a 15-20 minute correction which depends on calendar day
  lon[lon<0]=360+lon[lon<0] #convert to 0 to 360
  LC= 1/15*(lon%%15) # longitude correction, 1/15h for each degree of standard meridian
  LC[LC>0.5]= LC[LC>0.5]-1
  t_0 = 12-LC-ET # solar noon
  
  #Check if offset is as expected. (Is the timezone of the location the same as that of the meridian 
  #that's within 7.5 degrees from that location?)
  lon[lon>180]=lon[lon>180]-360
  if (!is.na(offset)) {
    offset_theory <- as.integer(lon / 15) + lon / abs(lon) * as.integer(abs(lon) %% 15 / 7.5)
    t_0 = t_0 - offset_theory + offset
  }
  
  cos.zenith= sin(DecAng)*sin(lat) + cos(DecAng)*cos(lat)*cos(pi/12*(hour-t_0)); #cos of zenith angle in radians
  zenith=acos(cos.zenith)*180/pi # zenith angle in degrees
  zenith[zenith>90]=90 # if measured from the vertical psi can't be greater than pi/2 (90 degrees)
  
  return(zenith)
}


day_of_year<- function(day, format="%Y-%m-%d"){
  day=  as.POSIXlt(day, format=format)
  return(as.numeric(strftime(day, format = "%j")))
}


diurnal_temp_variation_sine=function(T_max, T_min, t){
  
  W=pi/12;
  gamma= 0.44 - 0.46* sin(0.9 + W * t)+ 0.11 * sin(0.9 + 2 * W * t);   # (2.2) diurnal temperature function
  T = T_max*gamma + T_min * (1 - gamma)
  
  return(T)
}


Tb_butterfly=function(T_a, Tg, Tg_sh, u, H_sdir, H_sdif, z, D, delta, alpha, r_g=0.3, shade=FALSE){

  TaK= T_a+273.15 #ambient temperature in K
  TaK_sh=TaK
  Tg= Tg+273.15 #ground surface temperature in K
  Tg_sh= Tg_sh+273 #shaded ground surface temperature in K
  
  u= u *100;  #u- wind speed, convert m/s to cm/s
  H_sdir=H_sdir/10 #divide by ten to convert W/m2 to W/cm2
  H_sdif=H_sdif/10 #divide by ten to convert W/m2 to W/cm2
  
  #Total solar radiation
  H_sttl= H_sdir + H_sdif
  
  #Butterfly Parameters
  delta<- delta/10     #delta- thoracic fur thickness, cm
  
  epsilon_s=0.97; #surface emisivity, ranges from 0.95-1
  sigma= 5.67*10^-9; #Stefan-Boltzman constant, mW cm^-2 K^04 or 5.67*10^-8 W m-2 K-4
  Ep=1; #Ep- butterfly thermal emissivity
  
  k_e= 1.3; #k_e- thermal conductivity of the fur, 1.3mWcm^-1*K^-1
  r_i=0.15; #r_i- body radius #Kingsolver 1983
  k_a=0.25; #approximate thermal conductivity of air, mWcm^-1*K^-1
  
  v=15.68*10^-2  #cm^2/s, kinematic viscocity of air,  at 300K http://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html
  
  #---------------------------------------------
  
  #Areas, cm^2
  #Calculate total surface area as area of cylinder without ends
  A_sttl= pi*D*2 #2 in length  #cm^2
  
  #For butterflies basking with wings perpendicular to radiation 
  ##A_s,dir, A_s,ref, A_s,ttl- direct, reflected, and total solar radiative heat transfer surface areas 
  A_sdir= A_sttl/2
  A_sref=A_sdir
  
  #RADIATIVE HEAT FLUx, mW
  Q_s= alpha*A_sdir*H_sdir/cos(z*pi/180)+alpha*A_sref*H_sdif+alpha*r_g*A_sref*H_sttl  
  
  #---------------------------------------------		 
  #THERMAL RADIATIVE FLUX
  #Tsky=0.0552*(TaK)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), 
  Tsky= (1.22*T_a -20.4)+273.15 #K, Gates 1980 Biophysical ecology based on Swnback 1960, Kingsolver (1983) estimates using Brunt equation
  
  #Q_t= 0.5* A_sttl * Ep * sigma * (Tb^4 - Tsky^4) +0.5* A_sttl * Ep * sigma * (Tb^4 - Tg^4)
  
  #---------------------------------------------   	               
  # CONVECTIVE HEAT FLUX
  
  #Reynolds number- ratio of interval viscous forces
  R_e=u*D/v
  #Nusselt number- dimensionless conductance
  N_u=0.6*R_e^0.5
  #N_u=2.3; #Kingsolver 1983;
  
  h_c=N_u*k_a/D;
  h_T=(1/h_c+(r_i+delta)*log((r_i+delta)/r_i)/k_e)^-1;  # h_T- total convective heat tranfer coefficient
  #A_c=A_sttl; #A_c- convective heat transfer surface area
  #Q_c= h_T* A_c* (Tb-T_a);     
  #---------------------------------------------   	 
  #HEAT BUDGET              
  
  # Kingsolver 1983
  #Q_s- total radiative heat flux; Q_t- thermal radiative heat flux; Q_c- convective heat flux
  #Q_s=Q_t + Q_c;
  
  #ADJUST PARAMETERS IF SHADE
  if(shade==TRUE){
    #Calculate without basking by dividing areas by two
    A_sttl=A_sttl/2
    #RADIATIVE HEAT FLUX IN SHADE, mW
    A_sdir= A_sttl/2
    A_sref=A_sdir; 
    H_sdir_sh= 0; #No direct radiation
    H_sdif_sh= H_sdif
    H_sttl= H_sdif + H_sdif_sh #only diffuse and reflected
    Q_s= alpha*A_sdir*H_sdir_sh/cos(z*pi/180)+alpha*A_sref*H_sdif_sh+alpha*r_g*A_sref*H_sttl; 
    Tg= Tg_sh #use shaded surface temperature if shade
  }
  
  #t solved in wolfram alpha #Solve[a t^4 +b t -d, t]
  a<- A_sttl * Ep *sigma
  b<-h_T * A_sttl
  d<- h_T*A_sttl*TaK +0.5*A_sttl * Ep *sigma*Tsky^4 +0.5*A_sttl * Ep *sigma*(Tg)^4 +Q_s
  
  {Te=1/2*sqrt((2*b)/(a*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)))-(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)+(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3))-1/2*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)) }
  #IMPROVE SOLUTION?
  
  return(Te-273.15)
} 

Tb_grasshopper=function(T_a, T_g, u, H, K_t, psi, L, Acondfact=0.25, z=0.001, abs=0.7, r_g=0.3){
  
  TaK<- T_a+273.15 #Ambient temperature in K
  T_g<- T_g+273.15 #Ambient temperature in K
  
  #Biophysical parameters
  #IR emissivity
  omega<-5.66 * 10^-8 # stefan-boltzmann constant (W m^-2 K^-4)
  epsilon=1 #Gates 1962 in Kingsolver 1983  #emissivity of surface to longwave IR
  
  Kf=0.025  #Kf=0.024+0.00007*T_a[k] #thermal conductivity of fluid
  
  #kineamatic viscosity of air (m^2/s); http://users.wpi.edu/~ierardi/PDF/air_nu_plot.PDF
  v=15.68*10^-6  #m^2/s, kinematic viscocity of air,  at 300K #http://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html
  
  #AREAS
  #Samietz (2005): The body of a grasshopper female was approximated by a rotational ellipsoid with half the body length as the semi-major axis q.
  #Area from Wolfram math world
  c<- L/2 #c- semi-major axis, a- semi-minor axis
  a<- (0.365+0.241*L*1000)/1000  #regression in Lactin and Johnson (1988)
  e=sqrt(1-a^2/c^2)
  A=2*pi*a^2+2*pi*a*c/e*asin(e)
  
  #------------------------------
  #SOLAR RADIATIVE HEAT FLUX   
  #Separate total radiation into components
  #Use Erbs et al model from Wong and Chow (2001, Applied Energy 69:1991-224)
  
  #kd- diffuse fraction
  kd=1-0.09*K_t #if(K_t<=0.22) 
  kd[K_t>0.22 & K_t<=0.8]= 0.9511 -0.1604*K_t +4.388*K_t^2 -16.638*K_t^3 +12.336*K_t^4
  kd[K_t>0.8]=0.165 #kd = 0.125 #Correction from 16.5 for CO from Olyphant 1984
  
  Httl=H
  Hdir=Httl*(1-kd)
  Hdif=Httl*kd;     
  
  #------------------------------
  #Anderson 1979 - calculates radiation as W without area dependence 
  psi_r=psi*pi/180 #psi in radians
  
  #Calculate Qabs as W
  Qdir=abs*Hdir/cos(psi_r) #direct radiation
  Qdif=abs*Hdif #diffuse radiation
  Qref= r_g *Httl #reflected radiation
  Qabs= Qdir + Qdif + Qref  #W/m2
  
  #------------------------------
  #convection
  
  #Reynolds number- ratio of interval viscous forces
  #L: Characeristic dimension (length)
  # u= windspeed #Lactin and Johnson add 1m/s to account for cooling by passive convection
  Re= u*L/v
  #Nusselt number- dimensionless conductance
  Nu=0.41* Re^0.5 #Anderson 1979 empirical
  h_c= Nu *Kf /L # heat transfer coefficient, Wm^{-2}C^{-1} #reported in Lactin and Johnson 1998
  
  hc_s<- h_c *(-0.007*z/L +1.71) # heat transfer coefficient in turbulent air 
  
  #conduction 
  Thick= 6*10^(-5) #cuticle thickness (m)
  hcut= 0.15 #W m^-1 K^-1
  Acond=A * Acondfact 
  #Qcond= hcut *Acond *(Tb- (T_a+273))/Thick
  
  #------------------------------
  #Energy balance based on Kingsolver (1983, Thermoregulation and flight in Colias butterflies: elevational patterns and mechanistic limitations. Ecology 64: 534–545).
  
  #Thermal radiative flux
  #Areas
  # silhouette area / total area
  sa<-0.19-0.00173*psi #empirical from Anderson 1979, psi in degrees
  Adir= A*sa
  Aref=Adir 
  
  #Calculate Qabs as W/m2
  Qdir=abs*Adir*Hdir/cos(psi_r)
  Qdif=abs*Aref*Hdif
  Qref= r_g * Aref *Httl
  Qabs= Qdir + Qdif + Qref  #W/m2
  
  Tsky=0.0552*(T_a+273.15)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), Kingsolver 1983 estimates using Brunt equation
  
  #Qt= 0.5* A * epsilon * omega * (Tb^4 - Tsky^4) +0.5 * A * epsilon * omega * (Tb^4 - T_g^4) 
  #Convective heat flux
  #Qc= hc_s * A * (Tb- (T_a+273)) 
  #Qs= Qt+ Qc
  
  #WITH CONDUCTION
  #t solved in wolfram alpha #Solve[a t^4 +b t -d, t]
  a<- A * epsilon *omega
  b<-hc_s * A + hcut*Acond/Thick
  d<- hc_s*A*TaK +0.5*A*epsilon *omega*(Tsky^4+T_g^4)+ hcut *Acond*T_g/Thick +Qabs
  
  #WITHOUT CONDUCTION
  #a<- A * epsilon *omega
  #b<-hc_s * A
  #d<- hc_s*A*TaK +0.5*A*epsilon *omega*Tsky^4 +0.5*A*epsilon *omega*T_g^4 +Qabs
  
  #eb<-function(Tb) 0.5* A * epsilon * omega * (Tb^4 - Tsky^4) +0.5 * A * epsilon * omega * (Tb^4 - T_g^4) + hc_s * A * (Tb-TaK)+hcut *Acond *(Tb-T_g)/Thick -Qabs 
  #r <- uniroot(eb, c(-1,373), tol = 1e-5)
  #r$root-273
  
  #roots
  tb = 1/2*sqrt((2*b)/(a*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)))-(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)+(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3))-1/2*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)) 

  return(tb-273.15)
}


Tb_salamander_humid<-function(r_i,r_b,D,T_a,elev,e_a, e_s,Qabs, epsilon=0.96){
  
  if(e_s<e_a) stop("Actual vapor pressure, e_a, must be lower that saturation vapor pressure, e_s.")
  
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  vpd= e_s -e_a #vapor pressure deficit
  
  #radiative conductance function, Campbell and Norman 1998
  radiative_conductance= (4*(5.670373*10^-8)*(T_a+273.15)^3)/29.3
  
  gamma_naut = 0.000666
  a = (r_i*100.0)/41.4
  gva = (r_b*100)/41.4
  rad = (4*5670373*10^(-8)*(T_a+273.15)*3.)/29.3
  gamma = gamma_naut*((a+(1./gva))/((1./rad)+(1./gva)))
  s = ((((17.502*240.97))*0.611*exp((17.502*T_a)/(T_a+240.97)))/(240.97+T_a)^2)/(101.3*exp(-elev/8200))
  Tbh = T_a+(gamma/(gamma+s))*(((Qabs - (epsilon*sigma*((T_a+273.15)^4)))/(29.3*(radiative_conductance+gva)))-(vpd/(gamma*(101.3*exp(-elev/8200)))))
  
  return(Tbh)
}


Tb_snail = function(temp, Len, solar, WS, CC, WL, WSH){

  #temperatures
  Ktemp <- temp + 273 #temperature in Kelvin
  Gtemp <- Ktemp #ground temperature, assume equal to air temperature
  
  #areas
  PSA <- 3.1415*((Len/2)^2) ##3.14*wid*Len # Projected SA (Short-wave) snails SA of circle
  SA <- 4*3.15*((Len/2)^2) # Surface Area (Rad/Conv) for snails SA of sphere
  
  #Aradsky is surface area subject to long-wave radiation from sky (m^2)
  #Aradground is surface area subject to radiation from the ground (m^2)
  #Aproj is projected surface area (m^2)
  #Aground is surface area in contact with the ground (m^2)
  A1 <- (SA/2)/PSA # Aradsky/Aproj
  A2 <- 0.05 # Aground/Aproj`
  A3 <- (SA/2)/PSA # Aradground/Aproj
  
  #constants
  SB <- 5.67E-08 # Stephan Boltzman constant
  
  #convection
  u <- WS*0.03 # U* m/s, shear velocity
  c_prho=1200 #c_p*rho=1200 J m^{-3}*K^{-1}
  k=0.4 #von Karman constant
  z0=0.0017 #m, roughness height
  C <- (A1*2*u*c_prho*k)/(log(WSH/z0))
  
  #absorptivities
  if (Len >=0.037){  # Absorptivity from Luke Miller 
    Abs <- 0.615
  } else {if (Len <= 0.02225 ){Abs <-0.689
  } else Abs <- 0.68 } 
  
  # emissivities
  eskyclear = 0.72 +0.005*temp # Helmuth 1999 from Idso and Jackson 1969
  esky <- eskyclear + CC*(1 - eskyclear - (8 / Ktemp))  # Helmuth 1999
  Emm <- 0.97 # Emmissivity # long-wave emissivity shell
  
  # Steady-state heat balance model
  # Solve steady state energy balance equation:
  # T_b*mflux*c= Q_rad,sol +- Q_rad,sky +- Q_rad,ground +- Q_conduction +- Qconvection -Qevaporation
  com1 <- Abs * solar + 4 * Emm * SB * esky * A1 *(Ktemp^4)  + (Gtemp^4) * 4 * Emm * SB * A3  + C * Ktemp + 0.6 * A2 * Gtemp / (Len/2) + 2.48 * WL
  com2 <- 4180 * WL + (Ktemp^3) * 4 * Emm * SB *(esky^0.75) * A1 + 4 * Emm * SB * A3 *(Gtemp^3) + C + 0.6 * A2/(0.5*Len)
  T_b <- com1 / com2
  
  return (T_b - 273.15)
}

Tb_mussel = function(L, H, T_a, T_g, S, k_d, u, psi, evap=FALSE, cl, group = "solitary"){

  T_a = T_a + 273.15   # conversion to kelvin
  T_g = T_g + 273.15   # conversion to kelvin
  A = 1.08 * L^2 + 0.0461 * L - 0.0016   # total mussel shell surface area (m^2)
  m= 191*L^3.53  #mussel body mass, kg
  psi = psi * pi / 180  # conversion to radians
  
  #____________________________________________________________
  # constants
  sigma = 5.67 * 10^-8   # stefan-boltzmann constant (W m^-2 K^-4)
  lambda = 2.48         # latent heat of vaporization of water (J/kg)
  c = 4180               # specific heat of water (J kg^-1 K^-1)
  
  #___________________________________________________________
  #Short-wave solar flux  
  alpha = 0.75           # solar absorptivity
  k1 = alpha / sin(psi)
  
  S_p= S*(1-k_d) #direct radiation
  S_d= S*(k_d) #diffuse radiation
  #omit reflected radiation
  
  #____________________________________________________________
  # Long-wave radiaion
  
  # emissivities
  eps_ac = 0.72 +0.005*(T_a-273) # Helmuth 1999 from Idso and Jackson 1969
  eps_sky = eps_ac + cl*(1-eps_ac-8/T_a)  # Helmuth 1999
  eps_org = 1.0         # infrared emissivity of organism (same as above, p.163)
  
  #Estimate lumped coefficients
  k2 = 4 * sigma * eps_org * eps_sky^(3/4)
  k3 = eps_sky^(1/4)
  k4 = 4 * sigma * eps_org
  
  # Conduction (Coefficient)
  kb = 0.6      # thermal conductivity of heat in body (W m^-2 K^-1). Approximated to that of water because mussels are mostly made of water
  k5 = kb / (0.5 * H)
  
  #_______________________________________________________________
  # Convection
  # Denny and Harley. 2006, Hot limpets: predicting body temperature in a conductance-mediated thermal system 
  Ka = 0.00501 + 7.2 * 10^-5 * T_a        # Conductivity of air (W m^-1 K^-1)
  v = -1.25 * 10^-5 + 9.2 * 10^-8 * T_a   # Kinematic viscosity of air (m^2 s^-1)
  
  d = L * 2 / 3   # average body dimensions (Helmuth 1998 p.74)
  Re = u * d / v  # Reynolds number
  
  if (group == "aggregated") {
    a = 0.67
    b = 0.42
  } else if (group == "solitary"){
    a = 0.38
    b = 0.51
  } else {
    a = 0.63
    b = 0.47
  }
  Nu = a * Re^b    # Nusselt number
  hc = Nu * Ka / d     # heat transfer coefficient (W m^-2 K^-1)
  
  #evaporative mass loss
  mflux= ifelse(evap==FALSE, 0, m*0.05/(60*60) )
  # set maximum mflux rate to 5% of body mass over 1 hour, level that results in dessication
  # see Helmuth 1998 for a more detailed evaporation model based on mussel gaping
  
  #____________________________________________________________
  # calculating areas of interest
  A_radSky = A / 2    # surface area subject to long-wave radiation from sky (m^2). Half facing the sky, the other half facing the ground
  A_radGround = A / 2 # surface area subject to long-wave radiation from ground (m^2)
  A_cond = 0.05 * A   # area of contact between mussel and ground (m^2)
  A_conv = A          # surface area exposed to convective heat loss (m^2)
  A_d = A / 2         # surface area exposed to diffuse and albedo flux (m^2)
  
  if (group == "aggregated") {
    A_sol = 0.15 * A   # projected area in direction of sun (m^2)
  } else {
    A_sol = 0.25 * A
  }
  
  # Steady-state heat balance model
  
  # Solve steady state energy balance equation:
  # T_b*mflux*c= Q_rad,sol +- Q_rad,sky +- Q_rad,ground +- Q_conduction +- Qconvection -Qevaporation
  
  T_b = (k1 * (A_sol * S_p + A_d * S_d) + k2 * A_radSky * k3 * T_a^4 + k4 * A_radGround * T_g^4 + k5 * A_cond * T_g + 
           hc * A_conv * T_a - lambda * mflux) / (k2 * A_radSky * T_a^3 + k4 * A_radGround * T_g^3 + k5 * A_cond + 
                                                    hc * A_conv + mflux * c)
  
  return (T_b - 273.15)
}
