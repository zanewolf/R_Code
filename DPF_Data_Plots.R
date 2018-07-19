########
# Name: DPF_Data_Plots.R
# Author: Zane Wolf
# Date Created: 12/1/17
# Purpose: To analyze and plot duo pneufish data. Complements DPF_Stats_Analysis.R

# Date Updated: 7/6/18

########

#housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("C:/users/zane/Documents/Research/Projects/DuoPneufish_Project/Analyzed_Data/")

# libraries
library(tidyverse)
#includes ggplot2, tibble, tidyr, readr, purrr, dplyr
library(plyr)
library(stringr)
library(lubridate)
library(readxl)
library(doBy)
library(rstan)
library(rstanarm)
library(rstantools)
library(arm)
library(shinystan)


########################################################################################################################

#                                                DATA IMPORT 

########################################################################################################################

data_csv <- read_csv("~/Research/Projects/DuoPneufish_Project/Analyzed_Data/Parameter_Sweep_Data_Comp_wAmp.csv")

colnames(data_csv)

data_csv <- as.data.frame(data_csv)

########################################################################################################################

#                                                DATA MANIPULATION 

########################################################################################################################

useless_cols <- names(data_csv) %in% c("Date", "RampTime", "Regulator A", "Regulator B","Biased")
data_A <- data_csv[!useless_cols]

data_A$StepTime[data_A$StepTime==200] <- 250
data_A$`Flow Speed (rpm)` <- mapvalues(data_A$`Flow Speed (rpm)`, from = c(0,50), to=c("0", "5.3"))

data_A$`Maxpress` <- mapvalues(data_A$`Maxpress`, from = c(50, 75, 100), to=c(0.5, 0.75, 1.0))

data_A <- data_A %>% mutate(freq = as.factor(`Frequency (Hz)`))
data_A <- data_A %>% mutate(Frequency = as.factor(`Frequency (Hz)`))
data_A <- data_A %>% mutate(minPress = as.factor(`Minpress Percentage`))
data_A <- data_A %>% mutate(maxPress = as.factor(`Maxpress`))
data_A <- data_A %>% mutate(flowSpeed = as.factor(`Flow Speed (rpm)`))

#data_C is all data represented in mN 
data_C <- data_A %>% mutate(ThrustmN=FxMean*1000)

# data_500 is all data for StepTime == 500ms
data_500 <- subset(data_C, StepTime==500)

#Create data sets for summarized result variables 

#Force_x
data_Fx_mN <- data_500 %>% 
  group_by(freq, Frequency,flowSpeed, `Foil Color`, maxPress, minPress) %>% 
  summarise_at(vars(ThrustmN), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

data_Fx_mN_nominPress <- data_500 %>% 
  group_by(freq, Frequency,flowSpeed, `Foil Color`, maxPress) %>% 
  summarise_at(vars(ThrustmN), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

data_Fx_mN_vs_FlowSpeed <- data_500 %>% 
  group_by(flowSpeed, `Foil Color`) %>% 
  summarise_at(vars(ThrustmN), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

data_Fx_mN_interaction <- data_500 %>% 
  group_by(`Foil Color`, freq) %>% 
  summarise_at(vars(ThrustmN), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

data_Fx_mN_vs_freq <- data_500 %>% 
  group_by(freq) %>% 
  summarise_at(vars(ThrustmN), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

#Torze_Z
data_Tz_mN <- data_500 %>% 
  group_by(freq, Frequency,flowSpeed, `Foil Color`, StepTime, maxPress, minPress) %>% 
  summarise_at(vars(TzMean), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

#Force_y
data_Fy_mN <- data_500 %>% 
  group_by(freq, Frequency,flowSpeed, `Foil Color`, StepTime, maxPress, minPress) %>% 
  summarise_at(vars(FyMean), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

# Subset for specific variables 

Fx_fs_0 <- subset(data_Fx_mN,flowSpeed=='0 cm/s')
Fx_fs_50 <- subset(data_Fx_mN, flowSpeed=='5.3 cm/s')

#Set up amplitude data

data_amp_analysis <- subset(data_500, Height >=0)
data_amp_analysis$`Foil Color`<-factor(data_amp_analysis$`Foil Color`, c("Black", "Yellow", "Orange"))

levels(data_amp_analysis$Frequency) <- c("0.25 Hz", "0.5 Hz", "0.75 Hz", "1.0 Hz")

# fs_0_Orange <- subset(fs_0, `Foil Color`=="Orange")
# fs_0_Yellow <- subset(fs_0, `Foil Color`=="Yellow")
# fs_0_Black <- subset(fs_0, `Foil Color`=="Black")

# levels(fs_0$Frequency) <- c("0.25 Hz", "0.5 Hz", "0.75 Hz", "1.0 Hz")

# fs_0_Max_1 <- subset(fs_0, maxPress==1.0)
# fs_0_max_1_min_0 <- subset(fs_0_Max_1, minPress=0)


###########################################################################################################################

 #                                                               FX GRAPHS

###########################################################################################################################


#___________________________________________________

#         MINIMUM PRESSURE RESULT GRAPHS
#___________________________________________________


#create different levels so that facet_wraps have units
data_Fx_mN_A <- data_Fx_mN
levels(data_Fx_mN_A$flowSpeed) <- c("0 cm/s", "5.3 cm/s")
levels(data_Fx_mN_A$maxPress) <- c("0.5 kPa", "0.75 kPa", "1.0 kPa")

# min press vs thrust, grouped by frequency, facet-grid for flow speed, max pressure, and foil color
ggplot(data_Fx_mN_A, aes(x=minPress, y=mean, group=Frequency, colour=Frequency))+geom_point()+geom_line()+facet_grid(`Foil Color`~flowSpeed+maxPress)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se)) + ylab("Net Thrust (mN)") + xlab("Minimum Pressure (%)")+scale_color_discrete(name="Frequency (Hz)")

#reduced
ggplot(data_Fx_mN, aes(x=minPress, y=mean, group=Frequency, color=Frequency))+facet_grid(flowSpeed~`Foil Color`)+geom_smooth(method=lm)+theme_bw(base_size=18)+xlab("Minimum Pressure (%)")+ylab("Net Thrust (mN)") + scale_color_discrete(name="Frequency (Hz)")

#boxplot: minpress vs thrust for all data 
ggplot(data_Fx_mN, aes(x=minPress, y=mean))+geom_boxplot()+theme_bw(base_size=18)+xlab("Minimum Pressure (%)")+ylab("Net Thrust (mN)")

# ~~~~~~~~~~~~~~~~~~~~~ ! ~~~~~~~~~~~~~~~~~~~~~~~~~~#

#boxplot: minpress vs thrust, wrapped by flow speed
ggplot(data_Fx_mN, aes(x=minPress, y=mean))+geom_boxplot()+facet_wrap(~flowSpeed)+theme_bw(base_size=18)+xlab("Minimum Pressure (%)")+ylab("Net Thrust (mN)")

# ~~~~~~~~~~~~~~~~~~~~~ ! ~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ggplot(data_Fx_mN, aes(x = minPress , y = mean, fill=as.factor(flowSpeed)))+facet_grid(`Foil Color`~maxPress+Frequency)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Frequency (hz)")+ labs(fill="Foil_Color")


#___________________________________________________

#         FREQUENCY PRESSURE RESULT GRAPHS
#___________________________________________________


#line graph: frequency vs net thrust, grouped by minPress, facet-grid for flow speed, maxPress, and foil - with smooth
ggplot(data_Fx_mN, aes(x=Frequency, y=mean, group=minPress, colour=minPress))+geom_point()+facet_grid(`Foil Color`~maxPress+flowSpeed) +   
  geom_smooth(data=data_Fx_mN[data_Fx_mN$minPress=="0",],aes(x=Frequency, y=mean), method="lm", se=FALSE, color="green")+
  geom_smooth(data=data_Fx_mN[data_Fx_mN$minPress=="0.05",], aes(x=Frequency, y=mean), method="lm", se=FALSE, color="red")+
  geom_smooth(data=data_Fx_mN[data_Fx_mN$minPress=="0.1",], aes(x=Frequency, y=mean), method="lm", se=FALSE, color="blue")+
  geom_smooth(data=data_Fx_mN[data_Fx_mN$minPress=="0.25",], aes(x=Frequency, y=mean), method="lm", se=FALSE, color="purple")+xlab("Frequency (Hz)")+ ylab("Thrust (mN)")+scale_color_manual(values=c("0" = "green", "0.05"= "red", "0.1"= "blue","0.25"="purple" ))
# theme(legend.position=c(0.93, 0.9), legend.title = element_blank())

#line graph: frequency vs net thrust, grouped by minPress, facet-grid for flow speed, maxPress, and foil - with geom_line
ggplot(data_Fx_mN_A, aes(x=Frequency, y=mean, group=minPress, colour=minPress))+geom_point()+facet_grid(`Foil Color`~maxPress+flowSpeed) + geom_line()+geom_errorbar(aes(ymin=mean-se, ymax=mean+se))+xlab("Frequency (Hz)")+ ylab("Net Thrust (mN)")+scale_color_discrete(name="Min. Pressure (%)") 

#^ without error bars
ggplot(data_Fx_mN_A, aes(x=Frequency, y=mean, group=minPress, colour=minPress))+geom_point()+facet_grid(`Foil Color`~maxPress+flowSpeed) + geom_line()+xlab("Frequency (Hz)")+ ylab("Net Thrust (mN)")+scale_color_discrete(name="Min. Pressure (%)") 

#boxplot graph: frequency vs Thrust, for all data
ggplot(data_Fx_mN, aes(x=freq, y=mean))+geom_boxplot()+theme_bw(base_size=18)+xlab("Frequency (Hz)")+ylab("Net Thrust (mN)")

# ~~~~~~~~~~~~~~~~~~~~~ ! ~~~~~~~~~~~~~~~~~~~~~~~~~~#

#boxplot graph: frequency vs Thrust, for all data, grouped by foil 
ggplot(data_Fx_mN, aes(x=freq, y=mean, fill=`Foil Color`))+geom_boxplot()+theme_bw(base_size=18)+xlab("Frequency (Hz)")+ylab("Net Thrust (mN)")+scale_fill_manual(values=c("Black" = "black", "Yellow"= "gold2", "Orange"= "darkorange2"))

#line graph: frequency vs thrust, for data_Fx_mN_interaction, grouped by foil - with geom_line
  #mN_interaction groups by 'Foil Color' and frequency only. MaxPress and MinPress and FlowSpeed are all obsorbed into the data
ggplot(data_Fx_mN_interaction, aes(x=freq, y=mean, group=`Foil Color`, color=`Foil Color`))+geom_point(size=3)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5)+theme_bw(base_size=18)+xlab("Frequency (Hz)")+ylab("Net Thrust (mN)")+scale_color_manual(values=c("Black" = "black", "Yellow"= "gold", "Orange"= "orange"))+geom_line()


# ~~~~~~~~~~~~~~~~~~~~~ ! ~~~~~~~~~~~~~~~~~~~~~~~~~~#




#___________________________________________________

#         MAXIMUM PRESSURE RESULT GRAPHS
#___________________________________________________

ggplot(data_Fx_mN, aes(x=maxPress, y=mean))+geom_boxplot(aes(fill=`Foil Color`))+facet_grid(Frequency~flowSpeed)+xlab("Maximum Pressure (kPa)")+ylab("Net Thrust (mN)")+scale_fill_manual(values=c("Black" = "black", "Yellow"= "gold", "Orange"= "orange"))+geom_line()

ggplot(data_Fx_mN, aes(x=maxPress, y=mean))+geom_boxplot(aes(fill=`Foil Color`))+facet_grid(Frequency~flowSpeed)+xlab("Maximum Pressure (kPa)")+ylab("Net Thrust (mN)")+scale_fill_manual(values=c("Black" = "black", "Yellow"= "gold2", "Orange"= "darkorange2"))+geom_line()

ggplot(data_Fx_mN_nominPress, aes(x=maxPress, y=mean, color=Frequency, group=Frequency))+facet_grid(flowSpeed~`Foil Color`)+geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1)

ggplot(data_Fx_mN_nominPress, aes(x=maxPress, y=mean, color=Frequency, group=Frequency))+facet_grid(flowSpeed~`Foil Color`)+geom_point()+geom_line(size=0.5)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5)


ggplot(data_Fx_mN_nominPress, aes(x=maxPress, y=mean, color=Frequency, group=Frequency))+facet_grid(flowSpeed~`Foil Color`)+geom_point()+geom_line()+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1)

ggplot(data_Fx_mN_nominPress, aes(x=maxPress, y=mean, color=Frequency, group=Frequency))+facet_grid(flowSpeed~`Foil Color`)+geom_point()+geom_line(size=0.5)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5)






ggplot(data_amp_analysis, aes(x=Height, y=FxMean, colour=`Foil Color`))+geom_point()+facet_wrap(~Frequency)+
  geom_smooth(data=data_amp_analysis[data_amp_analysis$`Foil Color`=="Black",],aes(x=Height, y=FxMean), method="lm", se=FALSE, color="black")+
  geom_smooth(data=data_amp_analysis[data_amp_analysis$`Foil Color`=="Yellow",], aes(x=Height, y=FxMean),method="lm", se=FALSE, color="yellow")+
  geom_smooth(data=data_amp_analysis[data_amp_analysis$`Foil Color`=="Orange",], aes(x=Height, y=FxMean), method="lm", se=FALSE, color="orange")+xlab("Peak to Peak Amplitude (cm)")+ ylab("Thrust (N)")+scale_color_manual(values=c("Black" = "black", "Yellow"= "yellow", "Orange"= "orange"))+theme(legend.position=c(0.93, 0.9), legend.title = element_blank())



#plot for result 1) flow speed decreases thrust
ggplot(data_Fx_mN_vs_FlowSpeed, aes(x=flowSpeed, y=mean, color=`Foil Color`))+geom_point(size=3)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5)+theme_bw(base_size=22)+xlab("Flow Speed (cm/s)")+ylab("Net Thrust (mN)")+scale_color_manual(values=c("Black" = "black", "Yellow"= "gold", "Orange"= "orange"))+theme(legend.position=c(0.85, 0.9), legend.title = element_blank())

#plot for result 1) flow speed decreases thrust
ggplot(data_Fx_mN_vs_FlowSpeed, aes(x=flowSpeed, y=mean))+geom_boxplot()+xlab("Flow Speed (cm/s)")+ylab("Net Thrust (mN)")+theme_bw(base_size=18)

#plot for result 2) increasing pressure increases thrust

  #interaction between foil and stiffness
ggplot(data_Fx_mN_vs_maxPress, aes(x=maxPress, y=mean, group=`Foil Color`, color=`Foil Color`))+geom_point(size=3)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5)+theme_bw(base_size=18)+xlab("Maximum Pressure (kPa)")+ylab("Net Thrust (mN)")+scale_color_manual(values=c("Black" = "black", "Yellow"= "gold", "Orange"= "orange"))+geom_line()

  #general pressure vs thrust 
ggplot(data_Fx_mN, aes(x=maxPress, y=mean))+geom_boxplot(aes(fill=`Foil Color`))+facet_grid(Frequency~flowSpeed)+xlab("Maximum Pressure (kPa)")+ylab("Net Thrust (mN)")

#plot for result 3) increasing minimum pressure has little effect
    #facet grid display of results


#plot for result 4) increasing frequency alone or stiffness alone doesn't matter that much


ggplot(data_Fx_mN, aes(x=`Foil Color`, y=mean))+geom_boxplot()+theme_bw(base_size=18)+xlab("Stiffness (Foil Color)")+ylab("Net Thrust (mN)")

#plot for result 5) interaction
ggplot(data_Fx_mN_interaction, aes(x=freq, y=mean, group=`Foil Color`, color=`Foil Color`))+geom_point(size=3)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5)+theme_bw(base_size=18)+xlab("Frequency (Hz)")+ylab("Net Thrust (mN)")+scale_color_manual(values=c("Black" = "black", "Yellow"= "gold", "Orange"= "orange"))+geom_line()


#minpress results 
data_B <- subset(data_Fx_analysis, StepTime==500)
# data_B <- subset(data_B,flowSpeed=='0 cm/s')
# data_B <- subset(data_B, flowSpeed=='5.3 cm/s')

data_B$`Foil Color`<-factor(data_B$`Foil Color`, c("Black", "Yellow", "Orange"))

# ggplot(data_B, aes(x = minPress , y = mean, fill=as.factor(`Foil Color`)))+facet_grid(maxPress~flowSpeed+freq)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Frequency (Hz)")+ labs(fill="Foil_Color")

#choose to compare graphs for both flow speeds, maxpress=1.0bar, and freq=0.5
mincomp <- subset(data_B, maxPress==1.0 & freq == 0.5)
# 
# ggplot(mincomp, aes(x = minPress , y = mean, fill=as.factor(`Foil Color`)))+facet_wrap(~flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Minimum Pressure (%)")+ labs(fill="Stiffness")+scale_fill_manual(values=c("Black" = "black", "Yellow"= "yellow", "Orange"= "orange"))+theme(legend.position=c(0.85, 0.8))

#amplitude data 
data_amp_analysis <- subset(data_500, Height >=0)
data_amp_analysis$`Foil Color`<-factor(data_amp_analysis$`Foil Color`, c("Black", "Yellow", "Orange"))

levels(data_amp_analysis$Frequency) <- c("0.25 Hz", "0.5 Hz", "0.75 Hz", "1.0 Hz")



ggplot(data_amp_analysis, aes(x = minPress , y = Height, fill=as.factor(Frequency)))+facet_grid(maxPress~`Foil Color`+flowSpeed)+ geom_point()+geom_line()+ylab("Peak to Peak Amplitude (cm)")+xlab("Minimum Pressure (%)")+ labs(fill="Frequency")

# ggplot(data_B, aes(x = minPress , y = mean, fill=as.factor(Frequency)))+facet_grid(maxPress~`Foil Color`+flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Minimum Pressure (%)")+ labs(fill="Frequency") 

#select for maxp=1

amp_maxp1 <- subset(data_Fx_mN, maxPress == 1.0)
thrust_maxp1 <- subset(data_Fx_mN, maxPress==1.0)

# ggplot(amp_maxp1, aes(x = minPress , y = Height, fill=as.factor(Frequency)))+facet_grid(`Foil Color`~flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Peak to Peak Amplitude (cm)")+xlab("Minimum Pressure (%)")+ labs(fill="Frequency") 
# 
# ggplot(amp_maxp1, aes(x = freq , y = Height, fill=as.factor(minPress)))+facet_grid(`Foil Color`~flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Freq")+ labs(fill="MinPress") 

# ggplot(thrust_maxp1, aes(x = freq , y = mean, fill=as.factor(minPress)))+facet_grid(`Foil Color`~flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Freq")+ labs(fill="MinPress") 

#select for yellow 

amp_maxp1_yellow <- subset(amp_maxp1, `Foil Color`=="Yellow")
thrust_maxp1_yellow <- subset(thrust_maxp1, `Foil Color`=="Yellow")

# amp <- ggplot(amp_maxp1_yellow, aes(x = freq , y = Height, fill=as.factor(minPress)))+facet_wrap(~flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Freq")+ labs(fill="MinPress") 

ggplot(thrust_maxp1_yellow, aes(x = freq , y = mean, color=as.factor(minPress)))+facet_wrap(`Foil Color`~flowSpeed)+ geom_line()+ylab("Thrust (mN)")+xlab("Freq")+ labs(fill="MinPress") 

grid.arrange(thrust, amp, ncol=1)

ggplot(data_amp_analysis, aes(x=minPress, y=Height, color=freq, group=freq))+facet_grid(maxPress~flowSpeed+`Foil Color`)+geom_point()+geom_line()

ggplot(data_Fx_mN, aes(x=minPress, y=mean, color=freq, group=freq))+facet_grid(flowSpeed~`Foil Color`)+geom_point()+geom_line()
#select yellow, maxp=0.75
amp1 <- subset(data_amp_analysis, maxPress == 0.75)
thrust1 <- subset(data_B, maxPress == 0.75)

ggplot(amp1, aes(x=minPress, y=Height, color=freq, group=freq))+facet_grid(flowSpeed~`Foil Color`)+geom_point()+geom_line()

ggplot(thrust1, aes(x=minPress, y=mean, color=freq, group=freq))+facet_grid(flowSpeed~`Foil Color`)+geom_point()+geom_line()

levels(data_amp_analysis$Frequency) <- c("0.25 Hz", "0.5 Hz", "0.75 Hz", "1.0 Hz")

ggplot(amp_yellow_data, aes(x = maxPress , y = Height, fill=as.factor(minPress)))+facet_grid(Frequency~flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Peak to Peak Amplitude (cm)")+xlab("Minimum Pressure (%)")+ labs(fill="Flow Speed") +theme(legend.position=c(0.9, 0.85))+labs(fill=FALSE) +theme(legend.position="none")

#collect the same param combos for thrust and plot them together 
ggplot(data_Fx_mN, aes(x = minPress , y = mean, fill=as.factor(flowSpeed)))+facet_grid(`Foil Color`~maxPress+Frequency)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Frequency (hz)")+ labs(fill="Foil_Color")

thrust_yellow_data <- subset(data_B, `Foil Color`=="Yellow" & maxPress == 0.75)

levels(thrust_yellow_data$Frequency) <- c("0.25 Hz", "0.5 Hz", "0.75 Hz", "1.0 Hz")

ggplot(thrust_yellow_data, aes(x = minPress , y = mean, fill=as.factor(Frequency)))+facet_wrap(~flowSpeed, ncol=4)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Minimum Pressure (%)")+ labs(fill="Flow Speed") +theme(legend.position=c(0.9, 0.85))


grid.arrange(thrust, amp, ncol=1)
# +scale_fill_manual(values=c("Black" = "black", "Yellow"= "yellow", "Orange"= "orange"))

model<- stan_glm(FxMean ~ `Foil Color`+`Frequency (Hz)`+`Foil Color`:`Frequency (Hz)`+Maxpress+`Minpress Percentage`+`Flow Speed (rpm)`, data=data_A)
model1 <- lm(FxMean ~ `Foil Color`+`Frequency (Hz)`+`Foil Color`:`Frequency (Hz)`+`Foil Color`:Maxpress+`Minpress Percentage`+`Flow Speed (rpm)`, data=data_A)
display(model1)
model2 <- lm(mean ~ `Foil Color`+freq+`Foil Color`:freq+maxPress+minPress+flowSpeed, data=data_Fx_analysis)
model3 <- lm(mean ~ `Foil Color`+freq+`Foil Color`:freq+maxPress+minPress+`Foil Color`:freq:flowSpeed+flowSpeed:maxPress+flowSpeed:freq+`Foil Color`:maxPress+flowSpeed, data=data_Thrust_mN)
display(model2)
display(model3)
model4 <- lm(mean ~ `Foil Color`+freq+maxPress+minPress+flowSpeed+`Foil Color`:freq+`Foil Color`:maxPress+`Foil Color`:flowSpeed+freq:maxPress+freq:minPress+freq:flowSpeed+maxPress:flowSpeed+`Foil Color`:freq:maxPress:flowSpeed, data=data_Thrust_mN)
 display(model4)#shiny stan time
 
 model_min <- lm(mean~minPress+freq:minPress+maxPress:minPress+flowSpeed:minPress+`Foil Color`:minPress+`Foil Color`:freq:maxPress:minPress, data=data_Thrust_mN)
display(model_min)
plot(model4)


# m1 <- stan_glm(mean ~ `Foil Color`+freq+maxPress+minPress+flowSpeed+`Foil Color`:freq+`Foil Color`:maxPress+`Foil Color`:flowSpeed+freq:maxPress+freq:minPress+freq:flowSpeed+maxPress:flowSpeed+`Foil Color`:freq:maxPress:flowSpeed, data=data_Thrust_mN)
launch_shinystan(m1)

m3 <- stan_glm(mean ~ `Foil Color`+freq+maxPress+minPress+flowSpeed+`Foil Color`:freq, data=data_Thrust_mN)
plot(m3)

m4 <- stan_glm(mean ~ `Foil Color`+freq+maxPress+minPress+flowSpeed+`Foil Color`:freq+`Foil Color`:maxPress+`Foil Color`:flowSpeed, data=data_Fx_mN)
plot(m4)

m5 <- stan_glm(mean ~ `Foil Color`+freq+maxPress+minPress+flowSpeed+`Foil Color`:freq+`Foil Color`:maxPress+`Foil Color`:flowSpeed+flowSpeed:minPress, data=data_Fx_mN)
plot(m5)

m5 <- stan_glm(mean ~ `Foil Color`+freq+maxPress+minPress+flowSpeed+`Foil Color`:freq+flowSpeed:minPress, data=data_Fx_mN)
plot(m5)

step(lm(mean ~ `Foil Color`+freq+maxPress+flowSpeed+`Foil Color`:freq+`Foil Color`:flowSpeed+flowSpeed:minPress+flowSpeed:maxPress+`Foil Color`:freq:maxPress, data=data_Fx_mN), direction="both")


launch_shinystan(m5)

mTz <- stan_glm(mean ~ `Foil Color`+freq+maxPress+minPress+flowSpeed+`Foil Color`:freq+`Foil `, data=data_ztorque_mN)
plot(mTz)
launch_shinystan(m2)

mFy <- stan_glm(mean ~ `Foil Color`+freq+maxPress+minPress+flowSpeed+`Foil Color`:freq, data=data_yforce_mN)
plot(mFy)


# m4 <- stan_glm(mean ~ `Foil Color`+freq+maxPress+maxPress:freq+flowSpeed:`Foil Color`:freq:maxPress, data=data_Thrust_mN)
# plot(m4)
# launch_shinystan(m2)
# 
# m2 <- stan_glm(mean ~ `Foil Color`+freq+maxPress+minPress+flowSpeed+`Foil Color`:freq-1, data=data_Thrust_mN)
# launch_shinystan(m2)
# 
# 
# mH <- stan_glm(Height ~ `Foil Color`+freq+maxPress+minPress+flowSpeed+`Foil Color`:freq, data=data_amp_analysis)
# plot(mH)
# launch_shinystan(m2)
# 
# mH3 <- stan_glm(FxMean ~ Height , data=data_amp_analysis)
# summary(mH3)

ggplot(data_C, aes(x=Frequency, y=mean, colour=as.factor(StepTime)))+geom_point()+facet_wrap(~maxPress)+      
  geom_smooth(data=data_C[data_C$`StepTime`=="250",],aes(x=Frequency, y=mean), method="lm", se=FALSE, color="blue")+
  geom_smooth(data=data_C[data_C$`StepTime`=="500",], aes(x=Frequency, y=mean), method="lm", se=FALSE, color="red")+xlab("Peak to Peak Amplitude (cm)")+ ylab("Thrust (N)")+theme(legend.position=c(0.93, 0.9), legend.title = element_blank())
  # geom_smooth(data=data_amp_analysis[data_amp_analysis$`Foil Color`=="Yellow",], aes(x=Height, y=FxMean),method="lm", se=FALSE, color="yellow")+
  # geom_smooth(data=data_amp_analysis[data_amp_analysis$`Foil Color`=="Orange",], aes(x=Height, y=FxMean), method="lm", se=FALSE, color="orange")+xlab("Peak to Peak Amplitude (cm)")+ ylab("Thrust (N)")+scale_color_manual(values=c("Black" = "black", "Yellow"= "yellow", "Orange"= "orange"))+theme(legend.position=c(0.93, 0.9), legend.title = element_blank())

# 
ggplot(data_amp_analysis, aes(x=Height, y=FxMean, colour=`Foil Color`))+geom_point()+facet_wrap(~Frequency)+
          geom_smooth(data=data_amp_analysis[data_amp_analysis$`Foil Color`=="Black",],aes(x=Height, y=FxMean), method="lm", se=FALSE, color="black")+
          geom_smooth(data=data_amp_analysis[data_amp_analysis$`Foil Color`=="Yellow",], aes(x=Height, y=FxMean),method="lm", se=FALSE, color="yellow")+
          geom_smooth(data=data_amp_analysis[data_amp_analysis$`Foil Color`=="Orange",], aes(x=Height, y=FxMean), method="lm", se=FALSE, color="orange")+xlab("Peak to Peak Amplitude (cm)")+ ylab("Thrust (N)")+scale_color_manual(values=c("Black" = "black", "Yellow"= "yellow", "Orange"= "orange"))+theme(legend.position=c(0.93, 0.9), legend.title = element_blank())


# ggplot(data_amp_analysis, aes(x=Frequency, y=FxMean, colour=`Foil Color`))+geom_point()+facet_grid(maxPress~flowSpeed)+        
#   geom_smooth(data=data_amp_analysis[data_amp_analysis$`Foil Color`=="Black",],aes(x=Frequency, y=FxMean), method="lm", se=FALSE, color="black")+
#   geom_smooth(data=data_amp_analysis[data_amp_analysis$`Foil Color`=="Yellow",], aes(x=Frequency, y=FxMean),method="lm", se=FALSE, color="yellow")+
#   geom_smooth(data=data_amp_analysis[data_amp_analysis$`Foil Color`=="Orange",], aes(x=Frequency, y=FxMean), method="lm", se=FALSE, color="orange")+xlab("Peak to Peak Amplitude (cm)")+ ylab("Thrust (N)")+scale_color_manual(values=c("Black" = "black", "Yellow"= "yellow", "Orange"= "orange"))+theme(legend.position=c(0.93, 0.9), legend.title = element_blank())


ab_hat <- coef(mH3)
data_amp_analysis$`Foil Color`=='Black'
abline(ab_hat[1], ab_hat[2])

ggplot(data = child, aes(x=momage, y=ppvt)) + geom_point(aes(colour=hs)) +
  geom_smooth(data=child[child$hs==0,], aes(x=momage, y=ppvt), method="lm", se=FALSE, color="black") +
  geom_smooth(data=child[child$hs==1,], aes(x=momage, y=ppvt), method="lm", se=FALSE, color="deepskyblue2")


bayes_R2 <- function(fit) {
  y <- get_y(fit)
  ypred <- posterior_linpred(fit)
  e <- -1 * sweep(ypred, 2, y)
  var_ypred <- apply(ypred, 1, var)
  var_e <- apply(e, 1, var)
  var_ypred / (var_ypred + var_e)
}
## Example
M1 <- stan_glm(y ~ x)
print(median(bayes_R2(mH3)))
