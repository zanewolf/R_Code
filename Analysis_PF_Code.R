########
  # Author: Zane Wolf
  # Date Created: 12/1/17
  # Purpose: To analyze duo pneufish data

  # Date Updated: 

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
library(gridExtra)


########################################################################################################################

#                                                DATA IMPORT 

########################################################################################################################

# 
# data <- read_excel("~/Research/Projects/Pneufish_Project/Analyzed_Data/ParameterSweep_Data_Summary.xlsx")
# View(data)

data_csv <- read_csv("~/Research/Projects/DuoPneufish_Project/Analyzed_Data/Parameter_Sweep_Data_Comp_wAmp.csv")

colnames(data_csv)

# #extract columns to their own variables
# foilColor <- data_csv$`Foil Color`
# flowSpeed <- data_csv$`Flow Speed (rpm)`
# freq <- data_csv$`Frequency (Hz)`
# stepTime <- data_csv$StepTime
# maxP <- data_csv$Maxpress
# minP <- data_csv$Minpress
# minPP <- data_csv$`Minpress Percentage`
# biased <- data_csv$Biased
# FxMean <- data_csv$FxMean
# FyMean <- data_csv$FyMean
# FzMean <- data_csv$FzMean
# TxMean <- data_csv$TxMean
# TyMean <- data_csv$`T Mean`
# TzMean <- data_csv$TzMean

# data_csv$`Frequency (Hz)` <- as.factor(as.numeric(data_csv$`Frequency (Hz)`))
# data_csv$`Foil Color`<- as.factor(data_csv$`Foil Color`)
# data_csv$`Flow Speed (rpm)` <- as.factor(data_csv$`Flow Speed (rpm)`)
# data_csv$StepTime <- as.factor(data_csv$StepTime)
# data_csv$Maxpress <- as.factor(data_csv$Maxpress)
# data_csv$`Minpress Percentage` <- as.factor(data_csv$`Minpress Percentage`)


data_csv <- as.data.frame(data_csv)


useless_cols <- names(data_csv) %in% c("Date", "RampTime", "Regulator A", "Regulator B","Biased")
data_A <- data_csv[!useless_cols]

data_A$StepTime[data_A$StepTime==200] <- 250
data_A$`Flow Speed (rpm)` <- mapvalues(data_A$`Flow Speed (rpm)`, from = c(0,50), to=c("0 cm/s", "5.3 cm/s"))

data_A$`Maxpress` <- mapvalues(data_A$`Maxpress`, from = c(50, 75, 100), to=c(0.5, 0.75, 1.0))



data_A <- data_A %>% mutate(freq = as.factor(`Frequency (Hz)`))
data_A <- data_A %>% mutate(Frequency = as.factor(`Frequency (Hz)`))
data_A <- data_A %>% mutate(minPress = as.factor(`Minpress Percentage`))
data_A <- data_A %>% mutate(maxPress = as.factor(`Maxpress`))
data_A <- data_A %>% mutate(flowSpeed = as.factor(`Flow Speed (rpm)`))

# write.csv(data_250[1:50,], "Data_subset.csv" )

data_Fx_analysis <- data_A %>% 
  group_by(freq, Frequency,flowSpeed, `Foil Color`, StepTime, maxPress, minPress) %>% 
  summarise_at(vars(FxMean), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

fs_0 <- subset(data_Fx_analysis,flowSpeed=='0 cm/s')
fs_50 <- subset(data_Fx_analysis, flowSpeed=='5.3 cm/s')



# fs_50$PercentDiff <- fs_50$mean-fs_0$mean
# 
fs_0_Orange <- subset(fs_0, `Foil Color`=="Orange")
fs_0_Yellow <- subset(fs_0, `Foil Color`=="Yellow")
fs_0_Black <- subset(fs_0, `Foil Color`=="Black")
# fs_0["Level"] <- NA

# for (i in 1:nrow(fs_0)){
#   if (fs_0$`Flow Speed (rpm)`[i]=="0 cm/s"){
#     fs_0$Level[i] <- 0
#   } else if ((fs_0$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (fs_0$`Minpress Percentage`[i]==0.00)){
#     fs_0$Level[i] <- 1
#   } else if ((fs_0$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (fs_0$`Minpress Percentage`[i]==0.05)){
#     fs_0$Level[i] <- 2
#   } else if ((fs_0$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (fs_0$`Minpress Percentage`[i]==0.10)){
#     fs_0$Level[i] <- 3
#   } else if ((fs_0$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (fs_0$`Minpress Percentage`[i]==0.25)){
#     fs_0$Level[i] <- 4
#   }
# }


# fs_0 <- fs_0 %>% mutate(freq = as.factor(`Frequency (Hz)`))
# fs_0 <- fs_0 %>% mutate(freq = as.factor(`Frequency (Hz)`))
# ggplot(data=fs_0, mapping=aes(x=freq, y=mean, group=Level, fill=Level))+geom_bar(stat='identity', position='dodge')+facet_wrap(~`StepTime`)+ylab("Thrust (N)")+xlab("Frequency (Hz)")

levels(fs_0$Frequency) <- c("0.25 Hz", "0.5 Hz", "0.75 Hz", "1.0 Hz")

fs_0_Max_1 <- subset(fs_0, maxPress==1.0)
fs_0_max_1_min_0 <- subset(fs_0_Max_1, minPress=0)

ggplot(fs_0, aes(x = maxPress , y = mean, fill=as.factor(StepTime)))+facet_wrap(~freq)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Maximum Pressure (kPa)")+ labs(fill="StepTime (ms)")

ggplot(fs_0, aes(x = freq , y = mean, fill=as.factor(StepTime)))+facet_grid(maxPress~minPress)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Frequency (Hz)")+ labs(fill="StepTime)")

ggplot(fs_0_max_1_min_0, aes(x = freq , y = mean, fill=as.factor(StepTime)))+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Frequency (Hz)")+ labs(fill="StepTime (ms)")+theme(legend.position=c(0.85, 0.9))

#minpress results 
data_B <- subset(data_Fx_analysis, StepTime==500)
# data_B <- subset(data_B,flowSpeed=='0 cm/s')
# data_B <- subset(data_B, flowSpeed=='5.3 cm/s')

data_B$`Foil Color`<-factor(data_B$`Foil Color`, c("Black", "Yellow", "Orange"))

ggplot(data_B, aes(x = minPress , y = mean, fill=as.factor(`Foil Color`)))+facet_grid(maxPress~flowSpeed+freq)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Frequency (Hz)")+ labs(fill="Foil_Color")

#choose to compare graphs for both flow speeds, maxpress=1.0bar, and freq=0.5
mincomp <- subset(data_B, maxPress==1.0 & freq == 0.5)

ggplot(mincomp, aes(x = minPress , y = mean, fill=as.factor(`Foil Color`)))+facet_wrap(~flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Minimum Pressure (%)")+ labs(fill="Stiffness")+scale_fill_manual(values=c("Black" = "black", "Yellow"= "yellow", "Orange"= "orange"))+theme(legend.position=c(0.85, 0.8))

#amplitude data 
data_amp_analysis <- subset(data_A, StepTime ==500 & Height >=0)
data_amp_analysis$`Foil Color`<-factor(data_amp_analysis$`Foil Color`, c("Black", "Yellow", "Orange"))

levels(data_amp_analysis$Frequency) <- c("0.25 Hz", "0.5 Hz", "0.75 Hz", "1.0 Hz")



ggplot(data_amp_analysis, aes(x = minPress , y = Height, fill=as.factor(Frequency)))+facet_grid(maxPress~`Foil Color`+flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Peak to Peak Amplitude (cm)")+xlab("Minimum Pressure (%)")+ labs(fill="Frequency") 

ggplot(data_B, aes(x = minPress , y = mean, fill=as.factor(Frequency)))+facet_grid(maxPress~`Foil Color`+flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Minimum Pressure (%)")+ labs(fill="Frequency") 

#select for maxp=1

amp_maxp1 <- subset(data_amp_analysis, maxPress == 1.0)
thrust_maxp1 <- subset(data_B, maxPress==1.0)

ggplot(amp_maxp1, aes(x = minPress , y = Height, fill=as.factor(Frequency)))+facet_grid(`Foil Color`~flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Peak to Peak Amplitude (cm)")+xlab("Minimum Pressure (%)")+ labs(fill="Frequency") 

ggplot(amp_maxp1, aes(x = freq , y = Height, fill=as.factor(minPress)))+facet_grid(`Foil Color`~flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Freq")+ labs(fill="MinPress") 

ggplot(thrust_maxp1, aes(x = freq , y = mean, fill=as.factor(minPress)))+facet_grid(`Foil Color`~flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Freq")+ labs(fill="MinPress") 

#select for yellow 

amp_maxp1_yellow <- subset(amp_maxp1, `Foil Color`=="Yellow")
thrust_maxp1_yellow <- subset(thrust_maxp1, `Foil Color`=="Yellow")

amp <- ggplot(amp_maxp1_yellow, aes(x = freq , y = Height, fill=as.factor(minPress)))+facet_wrap(~flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Freq")+ labs(fill="MinPress") 

ggplot(thrust_maxp1_yellow, aes(x = freq , y = mean, color=as.factor(minPress)))+facet_wrap(`Foil Color`~flowSpeed)+ geom_line()+ylab("Thrust (mN)")+xlab("Freq")+ labs(fill="MinPress") 

grid.arrange(thrust, amp, ncol=1)

ggplot(data_amp_analysis, aes(x=minPress, y=Height, color=freq, group=freq))+facet_grid(maxPress~flowSpeed+`Foil Color`)+geom_point()+geom_line()

ggplot(data_B, aes(x=minPress, y=mean, color=freq, group=freq))+facet_grid(flowSpeed~`Foil Color`)+geom_point()+geom_line()
#select yellow, maxp=0.75
amp1 <- subset(data_amp_analysis, maxPress == 0.75)
thrust1 <- subset(data_B, maxPress == 0.75)

ggplot(amp1, aes(x=minPress, y=Height, color=freq, group=freq))+facet_grid(flowSpeed~`Foil Color`)+geom_point()+geom_line()

ggplot(thrust1, aes(x=minPress, y=mean, color=freq, group=freq))+facet_grid(flowSpeed~`Foil Color`)+geom_point()+geom_line()

levels(amp_yellow_data$Frequency) <- c("0.25 Hz", "0.5 Hz", "0.75 Hz", "1.0 Hz")

ggplot(amp_yellow_data, aes(x = maxPress , y = Height, fill=as.factor(minPress)))+facet_grid(Frequency~flowSpeed)+ geom_bar(stat='identity', position='dodge')+ylab("Peak to Peak Amplitude (cm)")+xlab("Minimum Pressure (%)")+ labs(fill="Flow Speed") +theme(legend.position=c(0.9, 0.85))+labs(fill=FALSE) +theme(legend.position="none")

#collect the same param combos for thrust and plot them together 
ggplot(data_Fx_mN, aes(x = minPress , y = mean, fill=as.factor(flowSpeed)))+facet_grid(`Foil Color`~maxPress+Frequency)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Frequency (hz)")+ labs(fill="Foil_Color")

thrust_yellow_data <- subset(data_B, `Foil Color`=="Yellow" & maxPress == 0.75)

levels(thrust_yellow_data$Frequency) <- c("0.25 Hz", "0.5 Hz", "0.75 Hz", "1.0 Hz")

ggplot(thrust_yellow_data, aes(x = minPress , y = mean, fill=as.factor(Frequency)))+facet_wrap(~flowSpeed, ncol=4)+ geom_bar(stat='identity', position='dodge')+ylab("Thrust (mN)")+xlab("Minimum Pressure (%)")+ labs(fill="Flow Speed") +theme(legend.position=c(0.9, 0.85))


grid.arrange(thrust, amp, ncol=1)
# +scale_fill_manual(values=c("Black" = "black", "Yellow"= "yellow", "Orange"= "orange"))


 #####################################################################################

data_500 <- subset(data_summed, StepTime==500)
data_250 <- subset(data_summed, StepTime==250)
# what_500 <- data2%>% 
#   group_by(`Frequency (Hz)`, `Flow Speed (rpm)`, `Foil Color`, StepTime, Maxpress, `Minpress Percentage`) %>% 
#   summarise_at(vars(FxMean), funs(N = length, mean, sd, se = sd(.) / sqrt(N)))

# what <- data_500 %>%
#   group_by (Maxpress) %>%
#   summarize(
#     FxThrust=mean(FxMean, na.rm=TRUE), 
#     FxSD=sd(FxMean, na.rm=TRUE)
#   ) 

fs_0 <- subset(data_500, `Flow Speed (rpm)`=='0 cm/s')
fs_50 <- subset(data_500, `Flow Speed (rpm)`=='5.3 cm/s')



fs_50$PercentDiff <- fs_50$mean-fs_0$mean

data_Orange <- subset(data_500, `Foil Color`=="Orange")
data_Yellow <- subset(data_500, `Foil Color`=="Yellow")
data_Black <- subset(data_500, `Foil Color`=="Black")

data_Orange_min_0 <- subset(data_Orange, `Minpress Percentage` ==0.00)
data_Orange_fs_0_min_0 <- subset(data_Orange_min_0, `Flow Speed (rpm)` =="0 cm/s")



frequency <- c(0.25, 0.5, 0.75, 1.0)
ggplot(data=data_Orange_fs_0_min_0, mapping=aes(x=`Frequency (Hz)`, y=mean, group=as.factor(Maxpress), color=as.factor(Maxpress)))+geom_point(size=4)+geom_line(size=2)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1)+ylab("Thrust (N)")+guides(color=guide_legend("Pressure (Bar)"))+scale_color_manual(labels=c("0.5", "0.75", "1.0"), values=c("green3", "blue", "red"))+theme_bw(base_size=22)+scale_x_discrete(breaks=c(0.25, 0.5, 0.75, 1.0),labels=frequency)

ggplot(data=what_500, mapping=aes(x=what_500$`Frequency (Hz)`, y=what_500$FxMean, group=what_500$Maxpress, color=what_500$Maxpress))+geom_line()

#create bar graph showing average pressure for ORANGE ONLY
data_Orange <- subset(data_500, `Foil Color`=="Orange")
data_Orange_fs_0 <- subset(data_Orange, `Flow Speed (rpm)`=='0 cm/s')
data_Orange_fs_50 <- subset(data_Orange, `Flow Speed (rpm)`=='5.3 cm/s')
data_Orange_fs_0_min_0 <- subset(data_Orange_fs_0, `Minpress Percentage`==0.00)


comp_min <- rbind(data_Orange_fs_0_min_0, data_Orange_fs_50)
comp_min["Level"] <- NA

for (i in 1:nrow(comp_min)){
  if (comp_min$`Flow Speed (rpm)`[i]=="0 cm/s"){
    comp_min$Level[i] <- 0
  } else if ((comp_min$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (comp_min$`Minpress Percentage`[i]==0.00)){
    comp_min$Level[i] <- 1
  } else if ((comp_min$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (comp_min$`Minpress Percentage`[i]==0.05)){
    comp_min$Level[i] <- 2
  } else if ((comp_min$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (comp_min$`Minpress Percentage`[i]==0.10)){
    comp_min$Level[i] <- 3
  } else if ((comp_min$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (comp_min$`Minpress Percentage`[i]==0.25)){
    comp_min$Level[i] <- 4
  }
}



ggplot(data=comp_min, mapping=aes(x=Maxpress, y=mean, group=Level, fill=Level))+geom_bar(stat='identity', position='dodge')+facet_wrap(~`Frequency (Hz)`)+ylab("Thrust (N)")+guides(color=guide_legend("Minimum Pressure"))+scale_fill_discrete(name='Minimum Pressure & \nFlow Speed', breaks=c(0,1,2,3,4), labels=c("0%, 0 cm/s", "0%, 5.3 cm/s", "5%, 5.3 cm/s", "10%, 5.3 cm/s", "25%, 5.3 cm/s"))

# +scale_color_manual(labels=c("0%", "5%", "10%", "25%"))

#do the same but facet wrap around foil colors

# data_Orange <- subset(data_500, `Foil Color`=="Orange")
data_fs_0 <- subset(data_500, `Flow Speed (rpm)`=='0 cm/s')
data_fs_50 <- subset(data_500, `Flow Speed (rpm)`=='5.3 cm/s')
data_fs_0_min_0 <- subset(data_fs_0, `Minpress Percentage`==0.00)


comp_min2 <- rbind(data_fs_0_min_0, data_fs_50)
comp_min2["Level"] <- NA

for (i in 1:nrow(comp_min2)){
  if (comp_min2$`Flow Speed (rpm)`[i]=="0 cm/s"){
    comp_min2$Level[i] <- 0
  } else if ((comp_min2$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (comp_min2$`Minpress Percentage`[i]==0.00)){
    comp_min2$Level[i] <- 1
  } else if ((comp_min2$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (comp_min2$`Minpress Percentage`[i]==0.05)){
    comp_min2$Level[i] <- 2
  } else if ((comp_min2$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (comp_min2$`Minpress Percentage`[i]==0.10)){
    comp_min2$Level[i] <- 3
  } else if ((comp_min2$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (comp_min2$`Minpress Percentage`[i]==0.25)){
    comp_min2$Level[i] <- 4
  }
}



ggplot(data=comp_min2, mapping=aes(x=Maxpress, y=mean, group=as.factor(Level), fill=as.factor(Level)))+geom_bar(stat='identity', position='dodge')+facet_grid(`Foil Color`~`Frequency (Hz)`)+ylab("Thrust (N)")+xlab("Activation Pressure (Bar)")+guides(color=guide_legend("Minimum Pressure"))+scale_fill_discrete(name='Minimum Pressure & \nFlow Speed', breaks=c(0,1,2,3,4), labels=c("0%, 0 cm/s", "0%, 5.3 cm/s", "5%, 5.3 cm/s", "10%, 5.3 cm/s", "25%, 5.3 cm/s"))+theme_bw(base_size=22)
#######################
data_fs_0_2 <- data_fs_0
data_Yellow_fs_0 <- subset(fs_0, `Foil Color`=="Yellow")
data_Yellow_fs_0_0.5Hz <- subset(data_Yellow_fs_0, `Frequency (Hz)`==1.0)

data_Yellow_fs_0_0.5Hz["Level"] <- NA
for (i in 1:nrow(data_Yellow_fs_0_0.5Hz)){
  if ((data_Yellow_fs_0_0.5Hz$`Flow Speed (rpm)`[i]=="0 cm/s") && (data_Yellow_fs_0_0.5Hz$`Minpress Percentage`[i]==0.00)){
    data_Yellow_fs_0_0.5Hz$Level[i] <- 1
  } else if ((data_Yellow_fs_0_0.5Hz$`Flow Speed (rpm)`[i]=="0 cm/s") && (data_Yellow_fs_0_0.5Hz$`Minpress Percentage`[i]==0.05)){
    data_Yellow_fs_0_0.5Hz$Level[i] <- 2
  } else if ((data_Yellow_fs_0_0.5Hz$`Flow Speed (rpm)`[i]=="0 cm/s") && (data_Yellow_fs_0_0.5Hz$`Minpress Percentage`[i]==0.10)){
    data_Yellow_fs_0_0.5Hz$Level[i] <- 3
  } else if ((data_Yellow_fs_0_0.5Hz$`Flow Speed (rpm)`[i]=="0 cm/s") && (data_Yellow_fs_0_0.5Hz$`Minpress Percentage`[i]==0.25)){
    data_Yellow_fs_0_0.5Hz$Level[i] <- 4
  }
}

ggplot(data=data_Yellow_fs_0_0.5Hz, mapping=aes(x=Maxpress, y=mean, group=as.factor(Level), fill=as.factor(Level)))+geom_bar(stat='identity', position='dodge')+ylab("Thrust (N)")+xlab("Activation Pressure (Bar)")+guides(color=guide_legend("Minimum Pressure"))+scale_fill_discrete(name='Minimum Pressure', breaks=c(0,1,2,3,4), labels=c("0%", "0%", "5%", "10%", "25%"))+theme_bw(base_size=22)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge())

data_Yellow <- subset(data_500, `Foil Color`=="Yellow")
data_Yellow_1Hz <- subset(data_Yellow, `Frequency (Hz)`==1.0)
data_Yellow_fs_0 <- subset(data_Yellow_1Hz, `Flow Speed (rpm)`=='0 cm/s')
data_Yellow_fs_50 <- subset(data_Yellow_1Hz, `Flow Speed (rpm)`=='5.3 cm/s')
data_Yellow_fs_0_min_0 <- subset(data_Yellow_fs_0, `Minpress Percentage`==0.00)


comp_min3 <- rbind(data_Yellow_fs_0_min_0, data_Yellow_fs_50)
comp_min3["Level"] <- NA

for (i in 1:nrow(comp_min3)){
  if (comp_min3$`Flow Speed (rpm)`[i]=="0 cm/s"){
    comp_min3$Level[i] <- 0
  } else if ((comp_min3$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (comp_min3$`Minpress Percentage`[i]==0.00)){
    comp_min3$Level[i] <- 1
  } else if ((comp_min3$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (comp_min3$`Minpress Percentage`[i]==0.05)){
    comp_min3$Level[i] <- 2
  } else if ((comp_min3$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (comp_min3$`Minpress Percentage`[i]==0.10)){
    comp_min3$Level[i] <- 3
  } else if ((comp_min3$`Flow Speed (rpm)`[i]=="5.3 cm/s") && (comp_min3$`Minpress Percentage`[i]==0.25)){
    comp_min3$Level[i] <- 4
  }
}

ggplot(data=comp_min3, mapping=aes(x=as.factor(Maxpress), y=mean, group=as.factor(Level), fill=as.factor(Level)))+geom_bar(stat='identity', position=position_dodge())+ylab("Thrust (N)")+xlab("Activation Pressure (Bar)")+guides(color=guide_legend("Minimum Pressure"))+scale_fill_discrete(name='Minimum Pressure & \nFlow Speed', breaks=c(0,1,2,3,4), labels=c("0%, 0 cm/s", "0%, 5.3 cm/s", "5%, 5.3 cm/s", "10%, 5.3 cm/s", "25%, 5.3 cm/s"))+theme_bw(base_size=22)+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge())
########################################################################################################################

#                                                OLD CODE 

########################################################################################################################


colnames(data_csv)

head(data)#in order to plot means and std errors, I'm going to need to average some data

typeof(data_csv$`Frequency (Hz)`)

data_csv <- data2
fx_data2 <- ddply(data_csv, c(data_csv$`Frequency (Hz)`,data_csv$`Flow Speed (rpm)`, data_csv$StepTime, data_csv$Maxpress, data_csv$`Minpress Percentage`), summarise,
                 N    = length(data_csv$FxMean),
                 mean = mean(data_csv$FxMean),
sd   = sd(data_csv$FxMean),
                 se   = sd / sqrt(N)
)

fx_data3 <- summaryBy(FxMean ~freq + foilColor+maxP+minPP, data=data_csv, FUN=c(length, mean, sd))

# + data_csv$`Foil Color` + data_csv$`Flow Speed (rpm)` + data_csv$StepTime + data_csv$Maxpress + data_csv$`Minpress Percentage`

# 
# 
# grouped <- group_by(data2, `Frequency (Hz)`, Maxpress, `Minpress Percentage`, StepTime, `Foil Color`,`Flow Speed (rpm)`)
# summarise(grouped, mean=mean(FxMean), sd=sd(FxMean))
# test1 <- ddply(data2, c(data2$`Frequency (Hz)`, data2$Maxpress, data2$`Minpress Percentage`, data2$StepTime, data2$`Foil Color`,data2$`Flow Speed (rpm)`), summarize, 
#       N = length(data2$FxMean), 
#       mean=mean(data2$FxMean), 
#       sd=sd(data2$FxMean), 
#       se=sd/sqrt(N)
#       )
# data_x <- aggregate(FxMean ~ `Frequency (Hz)` + Maxpress + `Minpress Percentage`+StepTime+`Foil Color`+`Flow Speed (rpm)`, data2, FUN=mean)
# data2$Maxpress, data2$`Minpress Percentage`, data2$StepTime, data2$`Foil Color`,data2$`Flow Speed (rpm)`
# test2 <- data2 %>%
#             group_by(data2$`Frequency (Hz)` ) %>%
#             summarize(FxMean.mean = mean(FxMean), 
#                       FxMean.sd= sd(FxMean))
# 
# data1.sd <- aggregate(FxMean ~ `Frequency (Hz)` + Maxpress + `Minpress Percentage`+StepTime+`Foil Color`+`Flow Speed (rpm)`, data_csv, sd)
# 
# Fx_data <- merge(data1.mean, data1.sd$FxMean)

length(data_csv$FxMean)
length(data_csv$`Frequency (Hz)`)

# fx_data <- aggregate(data_csv$FxMean, by=data_csv[c(data_csv$`Frequency (Hz)`)], FUN=length)


#try to recreate some JMP plots
ggplot(data=dataC, mapping=aes(x=maxP, y=FxMean, group=as.factor(stepTime), color=as.factor(stepTime)))+geom_point()+theme_bw()


