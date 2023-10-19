# Simple simulation model to consider the impact of increasing CO2 on
# biomass

# NPP is defined by a
# beta function describing its sensitivity to CO2, so that:  NPP(t) =
# NPP(t=0)*(1+Beta*ln([CO2]/[CO20]), the latter term being the
# pre-industrial CO2.  This is a classic, commonly used formulation that
# has been around for a long time - e.g. Lloyd and Farquhar 1996,
# Kicklighter et al. 1999.   

# This beta formulation needs to be coupled to a simple biomass model, such
# as that underpinning the first generation of DGVMs (e.g. Equation 1 from
# Galbraith et al. 2013).  
# Assume NPP fractional allocation to wood coefficient of 0.33 (from Malhi et al. 2015).  
# Assume woody biomass residence time of 50 years (from Galbraith et al.
# 2013).

# Matlab code - David Galbraith
# R translation - Martin Sullivan


# Step 1 - getting Beta from literature values
# beta.calc - takes end (.t) and start (.0) values of NPP and CO2
# Set npp.0 to 1 and npp.t to ratio if values are reported as ratios

beta.calc<-function(npp.t,npp.0,co2.t,co2.0){
num<-(npp.t/npp.0)-1
denom<-log(co2.t/co2.0)
num/denom
}


# Step 2 - Model Initialisation
CO2_0 = 286 #Pre-industrial atmospheric CO2 in 1860
Beta = 0.426233 # UPDATE THIS BASED ON beta.calc OUTPUT
Tau = 59.1 #Woody biomass residence time from plot data. Units: years
Wood_Alloc = 0.33; #Mean allocation to wood from Malhi et al. 2015. Dimensionless.
#NPP_0 = 11.0; # Starting (pre-industrial NPP).  Back-calculated from Malhi et al. 2015 Units: t C ha-1 yr-1. 
Scen = "26" # RCP scenario to read in - "45" or "26"

NPP_0 = 13.3*(1+(Beta*log(286/390))) # Back-calculated from Malhi et al 2015

Biomass_t0 = (NPP_0*Wood_Alloc)*Tau #Initial biomass under pre-industrial CO2. Units: t C ha-1.
Biomass_Start = Biomass_t0 #

# Define annual woody biomass turnover rate (k0)
k0 = 1/Tau #

# Pre-allocate arrays - run from 1861 to 2500 [precise pre-allocation only needed in Matlab original]
Biomass_Start_CO2 = vector(length=640);
Biomass_Gain_CO2 = vector(length=640);
Biomass_Loss_CO2 = vector(length=640);
Biomass_End_CO2 = vector(length=640);
Biomass_Change_CO2 = vector(length=640);

# Read in file with CO2 values
if(Scen == "45"){
	CO2_Data = read.csv("CO2 RCP45 2050 cap.csv") # CO2 time series representing RCP45 but then stabalising at 2050
} else {
	CO2_Data = read.csv("CO2 RCP26 2050 cap.csv") # CO2 time series representing RCP26 but then stabalising at 2050
}
CO2_Vals = CO2_Data[97:nrow(CO2_Data),2];

# Simple model - annual time step, corresponding to the CO2 values. 
# This is effectively how a '1st Generation' biosphere model would simulate the change
# in biomass. 

for (i in 1:640){
    Biomass_Start_CO2[i]=Biomass_Start;  
    NPP = NPP_0 * (1+Beta*log(CO2_Vals[i]/CO2_0));
    Biomass_Gain = NPP*Wood_Alloc;
    Biomass_Loss = k0*Biomass_Start;
    Biomass_Change = Biomass_Gain - Biomass_Loss;
    Biomass_End = Biomass_Start + Biomass_Change;
    Biomass_End_CO2[i]= Biomass_End;
    Biomass_Gain_CO2[i] = Biomass_Gain;
    Biomass_Loss_CO2[i] = Biomass_Loss;
    Biomass_Change_CO2[i] = Biomass_Change;
    Biomass_Start = Biomass_End;
}


par(mfrow=c(1,3))
plot(1861:2500,Biomass_End_CO2,type='l',ylab="Biomass",xlab="Years")
plot(1861:2500,Biomass_Change_CO2,type='l',col="blue")
plot(1861:2500,Biomass_Loss_CO2,'l',col="red")

yrs<-1861:2500
cng<-(Biomass_End_CO2[yrs==2499]-Biomass_End_CO2[yrs==2000])/Biomass_End_CO2[yrs==2000]
cng # This is the percentage change in biomass under the CO2 increase scenario 