


set.seed(1)
library(kableExtra)
library(randomForest)
library(ggplot2)
library(reshape2)
library(cowplot)
library(triangle)


setwd("C:/Users/Fangwei Cheng/Documents/GitHub/Cheng_LCNC")


action = data.frame(Action = paste0("Action",1:24), 
                    Scenarios = c(replicate(9,"S1 Hydrothermal treatment"),replicate(9,"S2 Pyrolysis"),replicate(3,"S3 Gasification"),replicate(3,"S4 Combustion")), 
                    Feedstocks = c( replicate(3, "Crop Residues"),replicate(3, "Woody Wastes"),replicate(3, "Biosolids"),replicate(3, "Crop Residues"),replicate(3, "Woody Wastes"),replicate(3, "Biosolids"),
                                    "Crop Residues","Woody Wastes","Biosolids","Crop Residues","Woody Wastes","Biosolids"), Temperature = c( 250, 300,350,250, 300,350,250, 300,350,400,550,700,400,550,700,400,550,700,900,900,900,900,900,900) )

##############################################################
### develop machine learning model for hydrothermal treatment 
df1 <- read.csv("HTT.data.csv", header = TRUE) #read HTT data file

# select required data to predict biocrude yield 
df <- data.frame(Ash = df1$Ash,Temperature = df1$Temperature, Time = df1$Time, Solid = df1$Solid, 
                 oil = df1$oil_yield, 
                 HC= df1$H.C, OC = df1$O.C) 

df <-na.omit(df)# delete data that does not have biocrude yield 

# fit model 
fit_HTT_oil <- randomForest(oil ~ Ash  + HC + OC+ Temperature +Time+ Solid, data = df,importance = TRUE)
fit_HTT_oil

# select required data to predict biocrude HHV
df <- data.frame( Ash = df1$Ash,Temperature = df1$Temperature, Time = df1$Time, Solid = df1$Solid, 
                  oil_hhv = df1$oil_HHV,
                  HC= df1$H.C, OC = df1$O.C)

df <-na.omit(df) # delete data that does not have biocrude HHV
# fit model 
fit_HHT_oil_hhv <- randomForest(oil_hhv ~ Ash   + HC + OC+ Temperature +Time+ Solid , data = df,importance = TRUE)
fit_HHT_oil_hhv


# select required data to predict biocrude carbon content
df <- data.frame( Ash = df1$Ash,Temperature = df1$Temperature, Time = df1$Time, Solid = df1$Solid, 
                  oil_c = df1$oil_C,
                  HC= df1$H.C, OC = df1$O.C)
df <-na.omit(df) # delete data that does not have biocrude carbon content
# fit model 
fit_HHT_oil_c <- randomForest(oil_c~Ash   + HC + OC+ Temperature +Time+ Solid , data = df,importance = TRUE)
fit_HHT_oil_c


# select required data to predict hydrochar yield
df <- data.frame( Ash = df1$Ash,Temperature = df1$Temperature, Time = df1$Time, Solid = df1$Solid, 
                  char = df1$char_yield,
                  HC= df1$H.C, OC = df1$O.C)

df <-na.omit(df)
# fit model 
fit_HHT_char <- randomForest(char~Ash   + HC + OC+ Temperature +Time+ Solid , data = df,importance = TRUE)
fit_HHT_char


# select required data to predict hydrochar HHV
df <- data.frame( Ash = df1$Ash,Temperature = df1$Temperature, Time = df1$Time, Solid = df1$Solid, 
                  char_hhv = df1$char_HHV,
                  HC= df1$H.C, OC = df1$O.C)

df <-na.omit(df)
# fit model 
fit_HHT_char_energy <- randomForest(char_hhv~ Ash+ HC + OC+ Temperature +Time+ Solid , data = df,importance = TRUE)
fit_HHT_char_energy


# select required data to predict hydrochar carbon content
df <- data.frame(Ash = df1$Ash,Temperature = df1$Temperature, Time = df1$Time, Solid = df1$Solid, 
                 char_C = df1$char_C,char = df1$char_yield,
                 HC= df1$H.C, OC = df1$O.C)

df <-na.omit(df)

fit_HHT_char_C <- randomForest(char_C ~Ash   + HC + OC + Temperature +Time+ Solid , data = df,importance = TRUE)
fit_HHT_char_C 


# select required data to predict gas yield
df <- data.frame(Ash = df1$Ash,Temperature = df1$Temperature, Time = df1$Time, Solid = df1$Solid, 
                 gas = df1$gas_yield,
                 HC= df1$H.C, OC = df1$O.C)

df <-na.omit(df)


fit_HHT_gas <- randomForest(gas ~ Ash   + HC + OC+ Temperature +Time+ Solid , data = df,importance = TRUE)
fit_HHT_gas


# load biomass data from Phyllis; each feedstock contains 20 data point
biomass_data <- read.csv("biomass_data.csv",header = TRUE)
# convert to H/C, O/C, and N/C atomic ratio
biomass_data$HC <- biomass_data$H/biomass_data$C*12
biomass_data$OC <- biomass_data$O/biomass_data$C/16*12
biomass_data$NC <- biomass_data$N/biomass_data$C/14*12

# creat a table that summarize feedstock parameters
feedstock_parameters <- data.frame(feedstock = c("Crop Residues", "Woody Wastes", "Biosolids"), Moisture = c(0.16,0.22,0.8), production_gwp = c(57,33,0), electricity_for_pre_pyrolysis = c(336,336,516), electricity_for_HTT = c(402,402,580), feedstock_cost = c(90,40,0))


# GWP factor for energy and materials
# for this scenario, GWP associated with heat and electricity were assumed to be zero
gwp_electricity <- 0
gwp_NG <- 0
gwp_hexane <- 35.35/1000 #kg CO2/kg
gwp_h2 <- 11.07 #kg CO2/kg
gwp_meoh <- 1.1 #kg CO2/kg
gwp_feso4 <- 0.11 #kg CO2/kg
gwp_mea <- 3.2 #kg CO2/kg

#CO2 capture rate
CO2_capture <- 0.9

# write a function that calculate the GWP of action 1-9 (hydrothermal treatment system); input is action 1-9
HTT_Action_GWP <- function(a){
  # get the name of feedstock
  feedstock =  as.character(action[action$Action == a,]$Feedstocks)
  # get the reaction temperature
  Temperature = action[action$Action == a,]$Temperature
  # create a dataframe
  df <- biomass_data[biomass_data$type == feedstock,]
  # include reaction conditions; assume reaction Time = 30 min and initial solid concentration = 25%
  df$Temperature <- Temperature
  df$Time <- 30
  df$Solid <- 25
  # predict product yield and characteristics at such condition
  df$biocrude <- predict(fit_HTT_oil,df)
  df$biochar <- predict(fit_HHT_char, df)
  df$gas <- predict(fit_HHT_gas,df)
  df$biocrude_HHV <- predict(fit_HHT_oil_hhv, df)
  df$biochar_C <- predict(fit_HHT_char_C, df)
  df$biochar_HHV <- predict(fit_HHT_char_energy, df)
  # add GWP of biomass production and electricity consumption
  df$production_GWP <- feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$production_gwp
  df$electricity_GWP = feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$electricity_for_HTT*gwp_electricity
  # calculate natural gas required for such action; assume heat recovery rate is 70% 
  E_HTL <- 1000*(((1/(df$Solid/100)-1)*(125*4.22+4.86*(df$Temperature-150))/1000+1*1.25/1000))*(1-0.7)
  # GWP for HTT unit is the product of required heat and its corresponding carbon intensity
  df$HTT_GWP <- gwp_NG*E_HTL
  # calculate GWP associated with solvent extraction 
  df$solvent_GWP <- 1000*(df$biocrude/100)*(0.003*gwp_hexane+(2.30*gwp_NG+0.08*gwp_electricity))
  # calculate GWP associated with upgrading
  df$H2_GWP <- df$biocrude/100*0.034*gwp_h2*1000
  # calcualte GWP associated with ACP treatment; different feedstocks were assumed to have different TN, TP, and COD
  ifelse(feedstock == "Biosolids",df$ACP_GWP<-(1/(df$Solid/100)-1)*(1.933*3.4*gwp_meoh+1.37*1.8*0.222*gwp_electricity+66.7*0.6*2.2*gwp_electricity), df$ACP_GWP  <- (1/(df$Solid/100)-1)*(0.048*3.4*gwp_meoh+1.07*0.027*0.222*gwp_electricity+23*0.6*2.2*gwp_electricity))
  # calculate how much CO2 is captured and stored
  df$CO2_out <-1000*(df$gas/100 + df$biochar_C/12*44)*CO2_capture 
  # calcualate the GWP associated with fossil fuel energy consumption
  df$CO2_in <- df$production_GWP + df$electricity_GWP + df$HTT_GWP + df$solvent_GWP + df$ACP_GWP
  # the net GWP = GWP associated with fossil fuel - sequestered CO2; negative values indicate negative carbon emissions
  df$GWP <- df$CO2_in -df$CO2_out
  return(df$GWP)
  
}


HTT_action = paste0("Action",1:9)
# create a dataframe that store the output
HTT_GWP = data.frame(Action = HTT_action, GWP_mean = c(1:9), GWP_min = c(1:9), GWP_max = c(1:9))

# store the average, minimum, and maximum GWP for action 1-9
for (i in c(1:9)){
  HTT_GWP[i,][2] <- mean(HTT_Action_GWP(HTT_action[i]))
  HTT_GWP[i,][3] <- min(HTT_Action_GWP(HTT_action[i]))
  HTT_GWP[i,][4] <-  max(HTT_Action_GWP(HTT_action[i]))
}

HTT_GWP

########################################
# get data for biomass pyrolysis
df1 <- read.csv("pyrolysis.data.csv", header = TRUE)

# get required data for biochar yield prediction
df <- data.frame(char = df1$char_db,Ash = as.numeric(df1$Ash),Temperature = df1$Temperature, heating_rate = df1$HR, time = df1$Time, OC= df1$O.C, HC= df1$H.C, NC = df1$N.C)
df <-na.omit(df)

#fit model
fit_char <- randomForest(char ~ Ash + OC +HC +NC + Temperature + heating_rate + time  , 
                         data = df, importance = TRUE, ntree = 8500, mtry = 4)
fit_char

# get required data for biochar carbon content prediction
df <- data.frame(char_c = df1$char_C,Ash = as.numeric(df1$Ash),Temperature = df1$Temperature, heating_rate = df1$HR, time = df1$Time, OC= df1$O.C, HC= df1$H.C, NC = df1$N.C)
df <-na.omit(df)
#fit model
fit_char_C <- randomForest(char_c ~ Ash + OC +HC +NC    + Temperature  + heating_rate + time , 
                           data = df, importance = TRUE,ntree = 2500, mtry = 4)

fit_char_C

# get required data for biochar HHV prediction
df <- data.frame(char_HHV = df1$char_HHV,Ash = as.numeric(df1$Ash),Temperature = df1$Temperature, heating_rate = df1$HR, time = df1$Time, OC= df1$O.C, HC= df1$H.C, NC = df1$N.C)
df <-na.omit(df)

fit_char_HHV <- randomForest(char_HHV ~ Ash +  OC +HC +NC   + Temperature + heating_rate + time  , 
                             data = df, importance = TRUE,ntree = 9500, mtry = 10)
fit_char_HHV

##############################
# get required data for biochar N prediction
df <- data.frame(char_N = df1$char_N,Ash = as.numeric(df1$Ash),Temperature = df1$Temperature, heating_rate = df1$HR, time = df1$Time, OC= df1$O.C, HC= df1$H.C, NC = df1$N.C)
df <-na.omit(df)
fit_char_N <- randomForest(char_N ~ Ash +  OC +HC +NC  + Temperature  + heating_rate + time  , 
                           data = df, importance = TRUE,ntree = 7500, mtry = 4)

fit_char_N



# write a function that calculate the GWP of action 10-18 (pyrolysis system); input is action 10-18
Pyrolysis_Action_GWP <- function(a){
  # get the name of feedstock
  feedstock =  as.character(action[action$Action == a,]$Feedstocks)
  # get the reaction temperature
  Temperature = action[action$Action == a,]$Temperature
  # create a data frame
  df <- biomass_data[biomass_data$type == feedstock,]
  # include reaction conditions; assume heating rate = 30 min/C and reaction time = 50 min
  df$Temperature <- Temperature
  df$heating_rate <- 30
  df$time <- 50
  # get the predicted biochar yield and characteristics
  df$biochar <- predict(fit_char, df)
  df$biochar_C <- predict(fit_char_C, df)
  df$biochar_HHV <- predict(fit_char_HHV, df)
  df$biochar_N <- predict(fit_char_N, df)
  # include the GWP associated with biomass production
  df$production_GWP <- feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$production_gwp
  # include the GWP associated with pretreatment
  df$pre_GWP = feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$electricity_for_pre*gwp_electricity
  # calculate the heat required for drying per FU; heat loss = 25%
  df$drying_heat <- 1000/(1-feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$Moisture)*2.28/0.75
  # get the energy content in oil and gas; oil and gas were combusted to provide heat, with efficiency of 90%
  df$gas_oil_heat = (df$HHV - df$biochar_HHV)*1000*0.85
  # if the heat created by  gas and oil is not sufficient, extra heat from natural gas should be used to supply for drying unit 
  ifelse(df$gas_oil_heat>df$drying_heat, df$drying_GWP <-0, df$drying_GWP <- (df$drying_heat - df$gas_oil_heat)*gwp_NG)
  # the stability of biochar is depdent on reaction temperature;stability = 0.65, 0.8, and 0.89 at 400, 550, and 600C
  stability <- ifelse(Temperature == 400, 0.65, ifelse(Temperature == 550, 0.8, 0.89))
  # negative emissions that could be achieved by burying biochar 
  df$biochar_GWP <- df$biochar_C*1000*stability/12*44
  # calculate net GWP
  df$GWP <-  df$pre_GWP + df$drying_GWP + df$production_GWP- df$biochar_GWP
  
  return(df$GWP)
  
}


pyrolysis_action = paste0("Action",10:18)

pyrolysis_GWP = data.frame(Action = pyrolysis_action, GWP_mean = c(1:9), GWP_min = c(1:9), GWP_max = c(1:9))


for (i in c(1:9)){
  pyrolysis_GWP[i,][2] <- mean(Pyrolysis_Action_GWP(pyrolysis_action[i]))
  pyrolysis_GWP[i,][3] <- min(Pyrolysis_Action_GWP(pyrolysis_action[i]))
  pyrolysis_GWP[i,][4] <- max(Pyrolysis_Action_GWP(pyrolysis_action[i]))
}
pyrolysis_GWP 




# write a function that calculate the GWP of action 19-21 (gasification system); input is action 19-21
Gasification_Action_GWP <- function(a){
  # get the name of feedstock
  feedstock =  as.character(action[action$Action == a,]$Feedstocks)
  # create a dataframe
  df <- biomass_data[biomass_data$type == feedstock,]
  # include the GWP associated biomass production and pretreatment
  df$production_GWP <- feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$production_gwp
  df$pre_GWP = feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$electricity_for_pre*gwp_electricity
  # calculate heat required for drying; heat loss = 25%
  df$E_drying <- (2260/1000 +4.2*75/1000)/(1-feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$Moisture)/0.75*1000
  # calcualte GWP associated with drying 
  df$drying_GWP <- df$E_drying*gwp_NG
  # 0.0015 kg MEA was consumed per capturing 1 kg CO2
  df$ccs_GWP <-   df$C/12*44*1000/100*CO2_capture*0.0015*gwp_mea
  # calculate GWP associated with fossil fuel consumption
  df$CO2_in <- df$production_GWP + df$pre_GWP +df$drying_GWP  + df$ccs_GWP
  # calculate Sequestered CO2
  df$CO2_out <-  df$C/12*44*1000/100*CO2_capture
  # get net GWP
  df$GWP <- df$CO2_in - df$CO2_out
  
  return(df$GWP)
  
}


Gasification_action = paste0("Action",19:21)

Gasification_GWP = data.frame(Action = Gasification_action, GWP_mean = c(1:3), GWP_min = c(1:3), GWP_max = c(1:3))

for (i in c(1:3)){
  Gasification_GWP[i,][2] <- mean(Gasification_Action_GWP(Gasification_action[i]))
  Gasification_GWP[i,][3] <- min(Gasification_Action_GWP(Gasification_action[i]))
  Gasification_GWP[i,][4] <-  max(Gasification_Action_GWP(Gasification_action[i]))
}
Gasification_GWP



# write a function that calculate the GWP of action 22-24 (combustion system); input is action 22-24
Combustion_Action_GWP <- function(a){
  # get the name of feedstock
  feedstock =  as.character(action[action$Action == a,]$Feedstocks)
  # create data frame
  df <- biomass_data[biomass_data$type == feedstock,]
  # include the GWP associated biomass production and pretreatment
  df$production_GWP <- feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$production_gwp
  df$pre_GWP = feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$electricity_for_pre*gwp_electricity
  # calculate heat required for drying unit
  df$E_drying <- (2260/1000 +4.2*75/1000)/(1-feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$Moisture)/0.75*1000
  # calculate the GWP associated with drying unit
  df$drying_GWP <- df$E_drying*gwp_NG
  # calculate GWP associated with ccs, 0.0015 kg MEA was consumed per capturing 1 kg CO2
  df$ccs_GWP <-   df$C/12*44*1000/100*CO2_capture*0.0015*gwp_mea
  # calculate GWP associated with fossil fuel consumption
  df$CO2_in <- df$production_GWP + df$pre_GWP +df$drying_GWP  + df$ccs_GWP
  # calculate Sequestered CO2
  df$CO2_out <-  df$C/12*44*1000/100*CO2_capture
  # net GWP
  df$GWP <- df$CO2_in - df$CO2_out
  
  
  return(df$GWP)
  
}



Combustion_action = paste0("Action",22:24)

Combustion_GWP = data.frame(Action =Combustion_action, GWP_mean = c(1:3), GWP_min = c(1:3), GWP_max = c(1:3))


for (i in c(1:3)){
  Combustion_GWP[i,][2] <- mean(Combustion_Action_GWP(Combustion_action[i]))
  Combustion_GWP[i,][3] <- min(Combustion_Action_GWP(Combustion_action[i]))
  Combustion_GWP[i,][4] <-  max(Combustion_Action_GWP(Combustion_action[i]))
}


Combustion_GWP






# combine all the GWP data
Action_summary <-rbind(HTT_GWP,pyrolysis_GWP)
Action_summary <-rbind(Action_summary ,Gasification_GWP)
Action_summary <- rbind(Action_summary,Combustion_GWP)
Action_summary$technology <- c(replicate(9,"HTT"),replicate(9,"Pyrolysis"),replicate(3,"Gasification"),replicate(3,"Combustion"))
Action_summary$technology <- factor(Action_summary$technology, levels = c( "HTT", "Pyrolysis", "Gasification","Combustion"))
Action_summary$Action <- factor(Action_summary$Action,paste0("Action",c(1:24)))

Action_summary

# plot the GWP of each action
library(ggplot2)
plot_GWP_zero <- ggplot() + geom_bar(data = Action_summary, aes(x = Action, y = GWP_mean, fill = technology),stat="identity")+
  geom_errorbar(data = Action_summary,
                aes(x=Action, ymin = GWP_min, ymax = GWP_max), width=0.2, size=0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(title = element_text(size =8,face="bold"),
        axis.text  = element_text(size =6,face="bold"),axis.title=element_text(size = 7,face="bold"))+
  theme(legend.title=element_blank(),legend.text = element_text(size = 5,face = "bold"))+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  ylab("GWP (kg CO2 eq/t biomass on dry weight)")+
  ggtitle("(b). GWP of each action, electricity and heat come from carbon netural resources")

plot_GWP_zero 

ggsave("GWP_ZERO.jpg", plot_GWP_zero,height = 3, width = 7.5)

# all the actions are carbon negative
Action_negative = c(paste0("Action",1:24))
Action_negative 

#######################################################
#Economic assessment
# Here are the financial parameters evaluated in this study
discount <- 0.1
crop_cost <- 90  #$/t
forest_cost <-30  #$/t
wood_cost <- 40 #$/t
NG_cost <- 0.004 #$/MJ
electricity_cost <- 0.06 #$/kwh
electricity_sale <- 0.06 #$/kwh
methanol_cost <- 500 # $/t
FeSO4_cost <- 120  # $/t
hexane_cost <- 1000  # $/t
h2_cost <- 2 #$/kg
oil_sale <- 2.35 # $/gal
biochar_sale <- 1137 #$/t
################################


# This function calculate the LCNC of hydrothermal treatment based on given action
HTT_Action_LCNC <- function(a){
  # Get the name of feedstock according to the action
  feedstock =  as.character(action[action$Action == a,]$Feedstocks)
  # Get the temperature according to the action
  Temperature = action[action$Action == a,]$Temperature
  df <- biomass_data[biomass_data$type == feedstock,]
  # put temperature, time, and solid concentration to the dataframe such that we could predict the yields and characteristics based on RF model
  df$Temperature <- Temperature
  df$Time <- 30
  df$Solid <- 25
  # make predictions based on feedstock properties and processing conditions
  df$biocrude <- predict(fit_HTT_oil,df)
  df$biochar <- predict(fit_HHT_char, df)
  df$gas <- predict(fit_HHT_gas,df)
  df$biocrude_HHV <- predict(fit_HHT_oil_hhv, df)
  df$biochar_C <- predict(fit_HHT_char_C, df)
  df$biochar_HHV <- predict(fit_HHT_char_energy, df)
  # calculate the cost of feedstocks 
  F_cost <- feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$feedstock_cost
  # The capital cost of HTT
  capital_cost <-rtriangle(n=1000,a=420*0.7,b=420*1.3,c=420)
  # Operating and maintenance cost is 19.4 M/yr
  OM_cost <- rtriangle(n=1000,a=19.4*0.7,b=19.4*1.3,c=19.4)
  # in 1 year, 1340 t/day * 330 day = 44200 t biomass was consumed
  feedstock_annual <- 1340*330
  # The annual cost of feedstock would be calculated as follow
  feedstock_cost <-rtriangle(n=1000,a=F_cost*0.7,b=F_cost*1.3,c=F_cost)*feedstock_annual/1000^2
  # The cost of preprocessing
  pre_cost <- feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$electricity_for_HTT*electricity_cost*feedstock_annual/1000^2
  # The energy required to run HTT  
  E_HTL <- 1000*(((1/(df$Solid/100)-1)*(125*4.22+4.86*(df$Temperature-150))/1000+1*1.25/1000))*(1-0.7)
  # electricity was used to supply the heat required to run HTT, so the cost of HTL unit was calculated as follow 
  HTL_cost = E_HTL*electricity_cost/3.6*feedstock_annual/1000^2
  # solvent required to separate biocrude from ACPs  
  solvent_cost <- (df$biocrude/100)*(0.003*hexane_cost+(2.30*electricity_cost/3.6+0.08*electricity_cost))*feedstock_annual/1000^2
  # Calculate the cost to treat ACP based on the type of feedstocks.
  ifelse(feedstock == "Biosolids",ACP_cost<-feedstock_annual*(1/(df$Solid/100)-1)*(1.933*3.4*methanol_cost/1000+1.37*1.8*0.222*FeSO4_cost/1000+66.7*0.6*2.2*electricity_cost)/1000^2, ACP_cost  <- feedstock_annual*(1/(df$Solid/100)-1)*(0.048*3.4*methanol_cost/1000+1.07*0.027*0.222*FeSO4_cost/1000+23*0.6*2.2*electricity_cost)/1000^2)
  # The commerial power plant of HTT-CCS was not estabilished, so we used $80/t as the cost to capture CO2 from HTT system. The CO2 capture efficiency was 85%
  ccs_cost <-feedstock_annual/1000^2*1000*(df$gas/100 + df$biochar_C/12*44)*CO2_capture/1000*80  # capture 1 t CO2 cost $80
  # Calculate the cost of hydrogen based on the biocrude yields. 
  hydrogen_cost <- h2_cost*df$biocrude/100* 0.034* 1000*feedstock_annual/1000^2
  # biocrude were upgraded to alternative to transportation oil with conversion rate to be 85%. the market price of produced oil, assume liquid fuel = $2.35/gal
  out_oil <- (df$biocrude/100* 0.85*1000)*0.264*oil_sale*feedstock_annual/1000^2 # 2.35/gal liquid fuel
  # Add the electricity, energy, material costs together as cost_in
  cost_in <- feedstock_cost + pre_cost + HTL_cost + solvent_cost + ACP_cost + ccs_cost+hydrogen_cost
  # hydrochar was burned to produce electircity, calculate the market price of generated electricity based on the HHV of hydrochar and the electricical efficiency (0.25)
  out_electricity <- df$biochar_HHV*1000*0.25*feedstock_annual/1000^2/3.6*electricity_cost
  # Get the GWP of this action based on previous function
  action_GWP = HTT_Action_GWP(a)
  # calculate the GWP of such systems annually by mutiplying the annual feedstock consumption (t/year)
  annual_GWP =  feedstock_annual/1000*action_GWP
  # discount rate 
  r <-0.1
  # plant life is 20 year
  plant_life = 30
  # get a function to calculate the sum of levelized cost, n =the nth year, m = the cost in the first year, r = discount rate  
  f <- function(n,m,r){
    x = 0 
    for (i in 1:n) x = x + m/(1+r)^i
    return(x)
  }
  # calculate the levelized cost of carbon
  NPV = -(capital_cost + f(plant_life, OM_cost, r)+ f(plant_life,cost_in,r) -  f(plant_life,out_oil,r)-f(plant_life,out_electricity,r))
  LCNC = NPV/(f(plant_life,annual_GWP,r))*1000^2
  
  return(list("NPV" = NPV, "LCNC" = LCNC))
  
}


# This function calculate the LCNC of pyrolysis based on given action
Pyrolysis_Action_LCNC <- function(a){
  # get the name of feedstock and reaction temperature
  feedstock <-  as.character(action[action$Action == a,]$Feedstocks)
  Temperature <- action[action$Action == a,]$Temperature
  # create data frame
  df <- biomass_data[biomass_data$type == feedstock,]
  # feed reaction conditions
  df$heating_rate <- 30
  df$time <- 50
  df$Temperature <- Temperature
  # predict biochar yield and characteristics
  df$biochar <- predict(fit_char, df)
  df$biochar_C <- predict(fit_char_C, df)
  df$biochar_HHV <- predict(fit_char_HHV, df)
  df$biochar_N <- predict(fit_char_N, df)
  # get capital cost 
  capital_cost <-rtriangle(n=1000,a=76.7*0.7,b=76.7*1.3,c=76.7)
  # get fixed Operating, Maintenance, and labor cost
  OM_cost <- rtriangle(n=1000,a=7.8*0.7,b=7.8*1.3,c=7.8)
  # get the cost of feedstocks 
  F_cost <- feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$feedstock_cost
  # feedstock annual consumption
  feedstock_annual <- 65700
  # calculate annual feedstock cost by mutiplying consumption and market price 
  feedstock_cost <-  rtriangle(n=1000,a=F_cost*0.7,b=F_cost*1.3,c=F_cost)*feedstock_annual/1000^2
  # calculate cost of preatment
  pre_cost <- feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$electricity_for_HTT*electricity_cost*feedstock_annual/1000^2
  # get the heat required for drying unit
  df$drying_heat <- 1000/(1-feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$Moisture)*2.28/0.75
  # get the energy content in oil and gas; oil and gas were combusted to provide heat, with efficiency of 90%
  df$gas_oil_heat <- (df$HHV - df$biochar_HHV)*1000*0.85
  # if the heat created by  gas and oil is not sufficient, extra heat should be used to supply for drying unit 
  ifelse(df$gas_oil_heat>df$drying_heat, drying_cost <-0, drying_cost <- (df$drying_heat - df$gas_oil_heat)*electricity_cost/3.6*feedstock_annual/1000^2)
  # calculate variable costs
  cost_in <- feedstock_cost + pre_cost +drying_cost
  # calculate exported heat
  out_heat <- ifelse(feedstock == "Biosolids", 0, ((df$HHV - df$biochar_HHV)*0.85*1000-df$drying_heat)*NG_cost*feedstock_annual/1000^2)
  # calculate the sale of biochar 
  out_biochar <- df$biochar/100*feedstock_annual/1000^2 *biochar_sale
  # get the GWP of this action; unit = kg CO2/1 t feedstock
  action_GWP = Pyrolysis_Action_GWP(a)
  # get the annual GWP by multiplying annual feedstock consumption and the GWP of 1 t feedstock
  annual_GWP =  feedstock_annual/1000*action_GWP
  # discount rate = 10%
  r = 0.1
  # plant lifetime = 20  year
  plant_life = 20
  # calculate discount cash flow, n = year, m = cost at that year, r = discount rate
  f <- function(n,m,r){
    x = 0 
    for (i in 1:n) x = x + m/(1+r)^i
    return(x)
  }
  NPV = -(capital_cost + f(plant_life, OM_cost, r)+ f(plant_life,cost_in,r) -  f(plant_life,out_heat,r)-f(plant_life, out_biochar,r))
  # Levelized cost of negative emissions
  LCNC = NPV/(f(plant_life,annual_GWP,r))*1000^2
  
  return(list("NPV" = NPV, "LCNC" = LCNC))
  
}

# This function calculate the LCNC of gasification based on given action
Gasification_Action_LCNC <- function(a){
  # electrical efficiency = 25%
  efficiency = 0.25
  # get the name of feedstock
  feedstock =  as.character(action[action$Action == a,]$Feedstocks)
  # create dataframe
  df <- biomass_data[biomass_data$type == feedstock,]
  # calculate heat required for drying 
  df$drying_heat <- 1000/(1-feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$Moisture)*2.28/0.75
  # capital cost
  capital_cost_IGCC <- rtriangle(n=1000,a=2655*0.7,b=2655*1.3,c=2655)
  # fixed O&M cost; F O &M = $170/kw year = 170*300*1000/1000^2 = 51 M/year
  OM_cost_IGCC <- rtriangle(n=1000,a=51*0.7,b=51*1.3,c=51)
  # vairable O&M cost; V O &M = $18/Mwh  = 18*300*7000/1000^2= 37.8 M/year
  variable_IGCC <- rtriangle(n=1000,a=37.8*0.7,b=37.8*1.3,c=37.8)
  # add up fixed OM and variable OM
  fix_variable <- OM_cost_IGCC +variable_IGCC
  # feedstock cost
  F_cost = feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$feedstock_cost
  # generated electricity per year
  generated_electricity = 7000*300 #MW
  # sale of generated electricity
  electricity_cost_IGCC = generated_electricity*1000*electricity_sale/1000^2  #M
  # feedstock consumption per year
  feedstock_t_IGCC <-  generated_electricity/efficiency/(df$HHV/3.6)
  # cost of feedstock per year
  feedstock_cost_IGCC <-  rtriangle(n=1000,a=F_cost*0.7,b=F_cost*1.3,c=F_cost)*feedstock_t_IGCC/1000^2
  # calculate cost for biomass drying
  drying_cost <- feedstock_t_IGCC*(df$drying_heat)*electricity_cost/3.6/1000^2
  # get GWP of handling 1 t feedstock
  action_GWP = Gasification_Action_GWP(a)
  # calculate the annual GWP of such action 
  annual_GWP =  feedstock_t_IGCC/1000*action_GWP
  # discount rate
  r = 0.1
  # plant life
  plant_life = 30
  # discounted cost benefit analysis
  f <- function(n,m,r){
    x = 0 
    for (i in 1:n) x = x + m/(1+r)^i
    return(x)
  }
  # get NPV
  NPV = -(capital_cost_IGCC + f(plant_life, drying_cost , r)+f(plant_life, fix_variable, r)+ f(plant_life,feedstock_cost_IGCC,r) -  f(plant_life,electricity_cost_IGCC,r))
  # get LCNC
  LCNC = NPV/(f(plant_life,annual_GWP,r))*1000^2
  return(list("NPV" = NPV, "LCNC" = LCNC))
  
}

# This function calculate the LCNC of combustion based on given action
Combustion_Action_LCNC <- function(a){
  # electrical efficiency = 18%
  efficiency = 0.18
  # get the name of feedstock
  feedstock =  as.character(action[action$Action == a,]$Feedstocks)
  # create dataframe
  df <- biomass_data[biomass_data$type == feedstock,]
  # calculate heat required for drying 
  df$drying_heat <- 1000/(1-feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$Moisture)*2.28/0.75
  # capital cost
  capital_cost_PC <- rtriangle(n=1000,a=2310*0.7,b=2310*1.3,c=2310)
  # fixed O&M cost; F O &M = $116/kw year = 116*300*1000/1000^2 = 34.8 M/year
  OM_cost_PC <- rtriangle(n=1000,a=34.8*0.7,b=34.8*1.3,c=34.8)
  # vairable O&M cost; V O &M = $13.4/Mwh  = 13.4*300*7000/1000^2= 28.14 M/year
  variable_PC <- rtriangle(n=1000,a=28.14*0.7,b=28.14*1.3,c=28.14)
  # add up fixed OM and variable OM
  fix_variable <- OM_cost_PC +variable_PC
  # feedstock cost
  F_cost = feedstock_parameters[feedstock_parameters$feedstock == feedstock,]$feedstock_cost
  # generated electricity per year
  generated_electricity = 7000*300 #MW
  # sale of generated electricity
  electricity_cost_PC = generated_electricity*1000*electricity_sale/1000^2 
  # feedstock consumption per year
  feedstock_t_PC <-  generated_electricity/efficiency/df$HHV*3.6
  # cost of feedstock per year
  feedstock_cost_PC <-  rtriangle(n=1000,a=F_cost*0.7,b=F_cost*1.3,c=F_cost)*feedstock_t_PC/1000^2
  # calculate cost for biomass drying
  drying_cost <- feedstock_t_PC*(df$drying_heat)*electricity_cost/3.6/1000^2
  # get GWP of handling 1 t feedstock
  action_GWP = Gasification_Action_GWP(a)
  # calculate the annual GWP of such action 
  annual_GWP =  feedstock_t_PC/1000*action_GWP
  # discount rate
  r = 0.1
  # plant life
  plant_life = 30
  # discounted cost benefit analysis
  f <- function(n,m,r){
    x = 0 
    for (i in 1:n) x = x + m/(1+r)^i
    return(x)
  }
  # get NPV
  NPV =  -(capital_cost_PC + f(plant_life, drying_cost , r)+f(plant_life, fix_variable, r)+ f(plant_life,feedstock_cost_PC,r) -  f(plant_life,electricity_cost_PC,r))
  # get LCNC
  LCNC = NPV/(f(plant_life,annual_GWP,r))*1000^2
  
  return(list("NPV" = NPV, "LCNC" = LCNC))
  
}



# calculate LCNC of each action
# HTT
action_all = paste0("Action",1:24)

for (i in c(1:9)){
  assign(paste0("Action_LCNC",i),as.numeric(unlist(HTT_Action_LCNC(action_all[i])[2])))
}

#pyrolysis
Pyrolysis_action = paste0("Action",10:18)

for (i in c(10:18)){
  assign(paste0("Action_LCNC",i),as.numeric(unlist(Pyrolysis_Action_LCNC(action_all[i])[2])))
}

#gasification
gasification_action = paste0("Action",19:21)

for (i in c(19:21)){
  assign(paste0("Action_LCNC",i),as.numeric(unlist(Gasification_Action_LCNC(action_all[i])[2])))
}

#combustion
Combustion_action = paste0("Action",22:24)

for (i in c(22:24)){
  assign(paste0("Action_LCNC",i),as.numeric(unlist(Combustion_Action_LCNC(action_all[i])[2])))
}

# only negatative emissions technologies would be used to assess LCNC
negative_action_summary = data.frame(Action1 = Action_LCNC1)
negative_action_summary$Action2 <- Action_LCNC2
negative_action_summary$Action3 <- Action_LCNC3
negative_action_summary$Action4 <- Action_LCNC4
negative_action_summary$Action5 <- Action_LCNC5
negative_action_summary$Action6 <- Action_LCNC6
negative_action_summary$Action7 <- Action_LCNC7
negative_action_summary$Action8 <- Action_LCNC8
negative_action_summary$Action9 <- Action_LCNC9
negative_action_summary$Action10 <- Action_LCNC10
negative_action_summary$Action11 <- Action_LCNC11
negative_action_summary$Action12 <- Action_LCNC12
negative_action_summary$Action13 <- Action_LCNC13
negative_action_summary$Action14 <- Action_LCNC14
negative_action_summary$Action15 <- Action_LCNC15
negative_action_summary$Action16 <- Action_LCNC16
negative_action_summary$Action17 <- Action_LCNC17
negative_action_summary$Action18 <- Action_LCNC18
negative_action_summary$Action19 <- Action_LCNC19
negative_action_summary$Action20 <- Action_LCNC20
negative_action_summary$Action21 <- Action_LCNC21
negative_action_summary$Action22 <- Action_LCNC22
negative_action_summary$Action23 <- Action_LCNC23
negative_action_summary$Action24 <- Action_LCNC24

library(reshape2)
kk <- melt(negative_action_summary)
means <- aggregate(value ~  variable, kk, mean)
means[2] <- round(means[2])

kk$action <- as.numeric(gsub("Action", "", kk$variable))

kk$technology <-ifelse(kk$action  %in%  c(1,2,3,4,5,6,7,8,9), "HTT", ifelse(kk$action %in% seq(10,18), "Pyrolysis", ifelse(kk$action %in% seq(19,21), "Gasification","Combustion")))

kk$technology <- factor(kk$technology, levels = c("HTT", "Pyrolysis", "Gasification","Combustion"))

# plot boxplot diagram
plot_LCNC_ZERO <- ggplot() + geom_boxplot(data = kk,fatten =1, aes(x = reorder(variable,value, FUN = mean), y = value, fill = technology))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylab("Levelized cost of carbon ($/t CO2)")+theme(axis.text.x = element_text(angle = 90))+xlab("Action")+  stat_summary(fun=mean, colour="darkred", geom="point", 
                                                                                                                                                                                                                                                                                                                                                                                                                                     shape=18, size=2)+theme(legend.position= "bottom")+
  geom_text(data = means, aes(label = value, x = variable, y = 1200), size = 3)+
  ylim(-300,1200)+
  ggtitle("(b). LCNC of each action; electricity and heat come from carbon netural resources")+
  theme(plot.title = element_text(size=10,face = "bold"), axis.title.y = element_text(size = 8, face = "bold"),
        axis.title.x = element_blank(),axis.text = element_text(size = 6,face = "bold"))


plot_LCNC_ZERO

ggsave("LCNC_ZERO.png",plot_LCNC_ZERO, height = 3.5, width = 7)

