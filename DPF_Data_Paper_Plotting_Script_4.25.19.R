########
# Name: DPF_Data_Plots_3.R
# Author: Zane Wolf
# Date Created: 12/1/17
# Purpose: To analyze and plot duo pneufish data. Complements DPF_Stats_Analysis.R

# Date Updated: 8/24/18

########

#housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("C:/users/zane/Documents/Research/Projects/DuoPneufish_Project/Analyzed_Data/")

# libraries
library(tidyverse)
#includes ggplot2, tibble, tidyr, readr, purrr, dplyr
library(plyr,stringr)
library(readxl,doBy)
library(rstan)
library(arm)
library(rstanarm)
library(rstantools)
library(ggpubr,grid)
library(ggsignif)



########################################################################################################################

#                                                DATA IMPORT 

########################################################################################################################

# data_csv <- read_csv("~/Research/Projects/DuoPneufish_Project/Analyzed_Data/Parameter_Sweep_Data_Comp_wAmp.csv")
data_csv <- read_excel("~/Research/Projects/DuoPneufish_Project/Analyzed_Data/Duo_Pneufish_Parameter_Database_8.14.18.xlsx")

colnames(data_csv)

data_csv <- as.data.frame(data_csv)


plot_grid(plot_histo)

n=4
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
########################################################################################################################

#                                                DATA MANIPULATION 

########################################################################################################################

useless_cols <- names(data_csv) %in% c("Date", "RampTime", "Regulator A", "Regulator B","Biased")
data_A <- data_csv[!useless_cols]

data_A$StepTime[data_A$StepTime==200] <- 250
data_A$`Flow Speed (rpm)` <- mapvalues(data_A$`Flow Speed (rpm)`, from = c(0,50), to=c("0", "5.3"))
data_A$`Foil Color` <- mapvalues(data_A$`Foil Color`, from = c("Black", "Yellow", "Orange"), to=c("Flexible", "Medium", "Stiff"))

data_A$`Maxpress` <- mapvalues(data_A$`Maxpress`, from = c(50, 75, 100), to=c(0.5, 0.75, 1.0))

data_A <- data_A %>% mutate(freq = as.factor(`Frequency (Hz)`))
data_A <- data_A %>% mutate(Frequency = as.factor(`Frequency (Hz)`))
data_A <- data_A %>% mutate(minPress = as.factor(`Minpress Percentage`))
data_A <- data_A %>% mutate(maxPress = as.factor(`Maxpress`))
data_A <- data_A %>% mutate(flowSpeed = as.factor(`Flow Speed (rpm)`))
data_A <- data_A %>% mutate(orientation = as.factor(`Orientation`))
data_A <- data_A %>% mutate(stepTime = as.factor(`StepTime`))
data_A <- data_A %>% mutate(stiffness = as.factor(`Foil Color`))

data_B <- data_A %>% mutate(ThrustmN=FxMean*1000)

# Separate data into specific groups: just Original (for most parameter comparisons), just Yellow (for Orientation comparison)
#data_C is all data represented in mN 

#data with the Original Orientation
data_Orig <- subset(data_B, orientation == 'Original')

# data_500 is data with StepTime == 500ms
data_all_500 <- subset(data_B, StepTime==500)

# data_500 is made with only Original orientation data
data_Orig_500 <- subset(data_Orig, StepTime==500)

data_Yellow_500 <- subset(data_all_500, `stiffness`=='Medium')


#prep analyzed data used throughout scripts

data_Fx_mN <- data_Orig_500 %>% 
  group_by(freq, Frequency,flowSpeed, stiffness, maxPress, minPress) %>% 
  summarise_at(vars(ThrustmN), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

data_Fx_mN$stiffness <- factor(data_Fx_mN$stiffness,
                               levels = c("Flexible","Medium", "Stiff"),ordered = TRUE)
levels(data_Fx_mN$minPress) <- c("0%", "5%", "10%", "25%")
levels(data_Fx_mN$flowSpeed) <- c("0 cm/s", "5.3 cm/s")
levels(data_Fx_mN$maxPress) <- c("0.5 kPa", "0.75 kPa", "1.0 kPa")
levels(data_Fx_mN$freq) <- c("0.25 Hz", "0.5 Hz", "0.75 Hz", "1.0 Hz")

###########################################################################################################################

#                                                         GRAPHS

###########################################################################################################################

#___________________________________________________

#        FIGURE 3 RESULT GRAPHS
#___________________________________________________

#prep data groups
data_Fx_mN_orientation <- data_Yellow_500 %>% 
  group_by(freq, flowSpeed, maxPress, orientation) %>% 
  summarise_at(vars(ThrustmN), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

levels(data_Fx_mN_orientation$minPress) <- c("0%", "5%", "10%", "25%")
levels(data_Fx_mN_orientation$flowSpeed) <- c("0 cm/s", "5.3 cm/s")
levels(data_Fx_mN_orientation$maxPress) <- c("0.5 kPa", "0.75 kPa", "1.0 kPa")
levels(data_Fx_mN_orientation$freq) <- c("0.25 Hz", "0.5 Hz", "0.75 Hz", "1.0 Hz")

#generate graphs

##### FIGURE 3
#boxplot with significance between just Original and Reverse
ggplot(data_Fx_mN_orientation, aes(x=orientation, y=mean))+geom_boxplot(size=1.5)+
  labs(x="Orientation", y="Net Thrust (mN)",fill="Orientation")+
  theme_classic(base_size=34)+
  theme(legend.position=c(0.85, 0.9))+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("Original", "Reverse")), size=1,textsize=6)+theme(plot.title = element_text(hjust = 0.5))

#___________________________________________________

#        FIGURE 8 BOXPLOT STACK GRAPHS
#___________________________________________________


### FIGURE 8.A FLOW SPEED VS NET THRUST
data_Fx_mN_vs_FlowSpeed <- data_Orig_500 %>% 
  group_by(flowSpeed, stiffness, maxPress, minPress, freq) %>% 
  summarise_at(vars(ThrustmN), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

levels(data_Fx_mN_vs_FlowSpeed$flowSpeed) <- c("0", "5.3")

#plot for result 1) flow speed decreases thrust
ggplot(data_Fx_mN_vs_FlowSpeed, aes(x=flowSpeed, y=mean))+
  geom_boxplot(size=1.5)+
  labs(y="Net Thrust (mN)", x="Flow Speed (cm/s)")+
  theme_classic(base_size=48)+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("0", "5.3")), size=1,textsize=12)+
  theme(plot.title = element_text(hjust = 0.5))

### FIGURE 8.B 

levels(data_Fx_mN$freq) <- c("0.25", "0.5", "0.75", "1.0")

#boxplot frequency vs thrust with significance
#significant difference only between 0.25hz and 0.5hz
ggplot(data_Fx_mN, aes(x=freq, y=mean))+
  geom_boxplot(size=1.5)+
  theme_classic(base_size=48)+
  labs(x="Frequency (Hz)",y ="Net Thrust (mN)")+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("0.25", "0.5")), size=1,textsize=12)+
  theme(plot.title = element_text(hjust = 0.5))

### FIGURE 8.C _Stiffness_vs NETTHRUST
#boxplot stiffness vs thrust by frequency, with significance
ggplot(data_Fx_mN, aes(x=stiffness, y=mean, fill=stiffness))+
  geom_boxplot(size=1.5)+
  scale_fill_hue(l=65, c=90)+
  labs(x="Stiffness", y="Net Thrust (mN)")+
  theme_classic(base_size=48)+
  geom_signif(test="t.test", map_signif_level=c("***"=0.001,"**"=0.01, "*"=0.05), comparisons=list(c("Flexible", "Medium")), size=1, textsize=12)+
  geom_signif(test="t.test", map_signif_level=c("***"=0.001,"**"=0.01, "*"=0.05), comparisons=list(c("Flexible", "Stiff")), size=1, textsize=12, y=75)+
  geom_signif(test="t.test", map_signif_level=c("***"=0.001,"**"=0.01, "*"=0.05), comparisons=list(c("Medium", "Stiff")), size=1, textsize=12)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")
#___________________________________________________

#        FIGURE 2 STEPTIME GRAPHS
#___________________________________________________

### FIGUE 2 FREQ vs THRUST by STEPTIME 
#boxplot with significance - FINAL PAPER FIGURE
data_Orig_noflow <- subset(data_Orig, flowSpeed == "0")

data_Orig_noflow_Fx_mN_0 <- data_Orig_noflow %>% 
  group_by(freq, stepTime) %>% 
  summarise_at(vars(ThrustmN), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

data_Orig_noflow_Fx_mN_1 <- data_Orig_noflow %>% 
  group_by(`Foil Color`,freq, maxPress, minPress, stepTime) %>% 
  summarise_at(vars(ThrustmN), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

levels(data_Orig_noflow_Fx_mN$stepTime) <- c("250ms", "500ms")
levels(data_Orig_noflow_Fx_mN$freq) <- c("0.25 Hz", "0.5 Hz", "0.75 Hz", "1.0 Hz")
ggplot(data_Orig_noflow_Fx_mN_0, aes(x=freq, y=mean, fill=stepTime))+
  geom_boxplot(size=1.5)+xlab("Frequency (Hz)")+
  ylab("Net Thrust (mN)")+
  theme_classic(base_size=48)+
  geom_signif(annotation="*", y_position=64, xmin=1.81, xmax=2.19, tip_length = 0.01, size=1.5, textsize=12)+
  geom_signif(annotation="***", y_position=65, xmin=2.81, xmax=3.19, tip_length = 0.01, size=1.5, textsize=12)+
  geom_signif(annotation="***", y_position=52, xmin=3.81, xmax=4.19, tip_length = 0.01, size=1.5, textsize=12)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name = "Step Size", labels = c("250ms", "500ms"))

#___________________________________________________

#        FIGURE 9 BOXPLOT STACK GRAPHS
#___________________________________________________

###FIGURE 9 - TWO GRAPHS NEED TO COMBINE THE TWO

#Freq vs Thrust by Stiffness - VERSION WITH SIGNIF
ggplot(data_Fx_mN, aes(x=freq, y=mean))+geom_boxplot(size=1.5)+facet_wrap(~stiffness)+theme_classic(base_size=48)+labs(x="Frequency (Hz)",y ="Net Thrust (mN)")+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("0.25", "0.5")), size=1,textsize=12)+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("0.5", "0.75")), size=1,textsize=12)+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("0.25", "0.75")), size=1,textsize=12, y=75)+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("0.25", "1.0")), size=1,textsize=12, y=85)+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("0.5", "1.0")), size=1,textsize=12, y=95)+
  theme(plot.title = element_text(hjust = 0.5))
        
###FIGURE 9 Freq vs Thrust by Stiffness - VERSION WITH COLOR
ggplot(data_Fx_mN, aes(x=freq, y=mean, fill=stiffness))+geom_boxplot(size=1.5)+facet_wrap(~stiffness)+theme_classic(base_size=48)+labs(x="Frequency (Hz)",y ="Net Thrust (mN)")+
  scale_fill_hue(l=65, c=90, name="Stiffness")+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("0.25", "0.5")), size=1,textsize=12)+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("0.5", "0.75")), size=1,textsize=12)+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("0.25", "0.75")), size=1,textsize=12, y=75)+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("0.25", "1.0")), size=1,textsize=12, y=85)+
  geom_signif(test="t.test",map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05),comparisons = list(c("0.5", "1.0")), size=1,textsize=12, y=95)+
  theme(plot.title = element_text(hjust = 0.5))

#___________________________________________________

#        FIGURE 6 LineGraph
#___________________________________________________

data_amp_fs_5.3 <- subset(data_Orig_500, Height >=0 & flowSpeed=="5.3")

# data_amp_anal <- data_amp %>% 
#   group_by(freq, flowSpeed, stiffness, maxPress, minPress) %>% 
#   summarise_at(vars(ThrustmN), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

levels(data_amp_fs_5.3$maxPress) <- "0.75 kPa"

data_amp_anal_2 <- data_amp_fs_5.3 %>% 
  group_by(freq,stiffness, maxPress) %>% 
  summarise_at(vars(Height), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

# ggplot(data_Fx_mN_interaction, aes(x=freq, y=mean, group=`Foil Color`, color=`Foil Color`))+geom_point(size=3)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5)+theme_bw(base_size=18)+xlab("Frequency (Hz)")+ylab("Net Thrust (mN)")+scale_color_manual(values=c("Black" = "black", "Yellow"= "gold", "Orange"= "orange"))+geom_line()

ggplot(data_amp_anal_2, aes(x=freq, y=mean, group=stiffness, color=stiffness))+
  geom_point(size=4)+
  theme_classic(base_size=34)+
  xlab("Frequency (Hz)")+ylab("Peak-to-Peak Amplitude (cm)")+
  # scale_color_brewer(palette="Reds")+
  geom_line(size=1.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3,size=1.5)+
  guides(color=guide_legend(title="Stiffness"))+
  theme(legend.position=c(0.85, 0.9))

ggplot(data_amp_anal_2, aes(x=freq, y=mean, group=stiffness, color=stiffness))+
  geom_line(aes(x=freq, y=mean, color=stiffness), size=2)+
  theme_classic(base_size=28)+
  xlab("Frequency (Hz)")+ylab("Peak-to-Peak Amplitude (cm)")+
  # scale_color_brewer(palette="Reds")+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, x=freq, fill=stiffness), alpha=0.3)


ggplot(data_amp_anal_2, aes(x=freq, y=mean, group=stiffness, color=stiffness))+
  geom_line(aes(x=freq, y=mean, color=stiffness), size=2)+
  theme_classic(base_size=34)+
  xlab("Frequency (Hz)")+ylab("Peak-to-Peak Amplitude (cm)")+
  # scale_color_brewer(palette="Reds")+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, x=freq, fill=stiffness), alpha=0.3)+
  scale_color_hue(name="")+scale_fill_hue(name="")+
  theme(legend.position=c(0.85, 0.8))

#___________________________________________________

#        FIGURE 7 & Supplementary AIC Table 
#___________________________________________________

data_Fx_mN$stiffness <- as.factor(data_Fx_mN$stiffness)

step(lm(mean~stiffness+maxPress+flowSpeed+freq+stiffness:freq+maxPress+freq, data=data_Fx_mN), direction="backward")

step(lm(mean ~ stiffness+freq+maxPress+minPress+flowSpeed+stiffness:freq+stiffness:flowSpeed+maxPress:flowSpeed+freq:flowSpeed+flowSpeed:minPress+maxPress:freq+maxPress:stiffness+freq:minPress+stiffness:minPress, data=data_Fx_mN), direction="both")

m1 <- stan_glm(mean ~ stiffness+freq+maxPress+minPress+flowSpeed+stiffness:freq+stiffness:flowSpeed+maxPress:flowSpeed+flowSpeed:minPress+maxPress:freq+maxPress:stiffness, data=data_Fx_mN)
print(m1)
display(m1)
coefficients(m1)
plot(m1)

m2 <- stan_glm(mean ~ stiffness+freq+maxPress+minPress+flowSpeed+stiffness:freq+stiffness:flowSpeed+maxPress:flowSpeed+freq:flowSpeed+flowSpeed:minPress+maxPress:freq+maxPress:stiffness, data=data_Fx_mN)

# library(leaps)
# leaps <- regsubsets(mean ~ stiffness+freq+maxPress+flowSpeed+stiffness:freq+stiffness:flowSpeed+maxPress:flowSpeed+freq:flowSpeed+flowSpeed:minPress+maxPress:freq+maxPress:stiffness, data=data_Fx_mN,nbest=10)
# summary(leaps)

#try running models on unprocessed data

m3 <- stan_lm(mean ~ stiffness+freq+maxPress+minPress+flowSpeed+stiffness:freq+stiffness:flowSpeed+maxPress:flowSpeed+freq:flowSpeed+flowSpeed:minPress+maxPress:stiffness, data=data_Fx_mN)
plot(m3)
pp_check(m3, plotfun="error_binned")
summary(m3)
display(m3)


step(lm(mean ~ .^2, data=data_Fx_mN),direction="backward")

#result: mean ~ freq + flowSpeed + stiffness + maxPress + minPress + sd + 
# se + freq:flowSpeed + freq:stiffness + freq:maxPress + freq:sd + 
#   flowSpeed:stiffness + flowSpeed:maxPress + flowSpeed:minPress + 
#   flowSpeed:sd + stiffness:maxPress + stiffness:sd + maxPress:sd + 
#   minPress:sd + sd:se

m_final <- stan_glm(mean ~ freq + flowSpeed + stiffness + maxPress + minPress + freq:flowSpeed + freq:stiffness + freq:maxPress + flowSpeed:stiffness + flowSpeed:maxPress + flowSpeed:minPress + stiffness:maxPress , data=data_Fx_mN)
plot(m_final) #figure 7 coefficients graph 
summary(m_final)

stepAIC(lm(mean ~ freq + flowSpeed + stiffness + maxPress + minPress + freq:flowSpeed + freq:stiffness + freq:maxPress + flowSpeed:stiffness + flowSpeed:maxPress + flowSpeed:minPress + stiffness:maxPress , data=data_Fx_mN), difference = "both")


#___________________________________________________

#        SUPP ROC Figure 
#___________________________________________________

roc_data <- read_excel("~/Research/Projects/DuoPneufish_Project/Experiments/Spring_2017/Pneunet_Pressure_Characterization/Preliminary/Regulator_Curvature_Data_Summary_2.xlsx")

colnames(roc_data)

roc_data_2 <- roc_data
roc_data_2$roc_cm <- roc_data$`ROC`/100
roc_data_2$press <- roc_data$`Maxvalue`/100

roc_data_analyze <- roc_data_2 %>% 
  group_by(press) %>% 
  summarise_at(vars(roc_cm), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

ggplot(roc_data_analyze, aes(x=press, y=mean))+geom_line(size=1)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se, x=press), size=0.75, width=0.1)+ylim(0,250)+xlim(0, 1)+labs(y="Radius of Curvature (cm)", x="Pressure (kPa)")+theme_classic(base_size=34)
                                                                                                                                                                         
                                                                                                                                                                    