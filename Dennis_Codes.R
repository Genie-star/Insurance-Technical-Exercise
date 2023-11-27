library(readxl)
library(ggplot2)
library(plotly)
library(tidyr)
library(tidyselect)
library(caTools)     #Split Data into Train and Test set

Data <- read_xlsx('TopTee\\Dataset.xlsx')
View(Data)
subset(Data_NWP, 2016>100)
############# NWP ##############
Data_NWP <- Data[,c(1:6)]
colnames(Data_NWP) <- c('Firm',2016,2017,2018,2019,2020)
Data_NWP <- Data_NWP[-1,]      #remove first row
View(Data_NWP)
#NWP_plot <- ggplot(NWP, aes(Year, Value)) + geom_point(aes(color=Year))
#NWP_plot
#plot(Data_NWP)
#Gather the years together
NWP <- gather(Data_NWP, key = Year, value = Value, 2:6)
NWP <- data.frame(NWP)
View(NWP)

############# GWP ##############
Data_GWP <- Data[, c(1,22:26)]
colnames(Data_GWP) <- c('Firm',2016,2017,2018,2019,2020)
Data_GWP <- Data_GWP[-1,]      #remove first row
View(Data_GWP)
#Gather the years together
GWP <- gather(Data_GWP, key = Year, value = Value, 2:6)
GWP <- data.frame(GWP)
View(GWP)
NWP_GWP <- cbind(NWP,GWP$Value)
colnames(NWP_GWP) <- c('Firm', 'Year', 'NWP', 'GWP')
View(NWP_GWP)
##### Computing the Risk
Risk <- as.numeric(NWP_GWP$NWP) / as.numeric(NWP_GWP$GWP)
Risk <- as.numeric(Risk)
NWP_GWP <- cbind(NWP_GWP, Risk)
#Replace nas with zeros
NWP_GWP <- replace(NWP_GWP, is.na(NWP_GWP), 0)

########## Firm's Risk ##########
Firm_Risk <- subset(NWP_GWP, Risk<0)
View(Firm_Risk)
class(Firm_Risk$Risk)

############# MODEL ################
library(caTools)
set.seed(101)
sample_risk <- sample.split(Firm_Risk$Risk, SplitRatio = 0.7)
tr_r <- subset(sample_risk, sample==T)
te_r <- subset(sample_risk, sample==F)
