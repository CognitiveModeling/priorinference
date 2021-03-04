### Creating a data frame with all data and model predictions for scatterplots (Experiment 1) ###
## Before you do it make sure that all the files with data are commented out in "X1_Code/04_sRSA_X1_ScatterPlot.R" ##

########################################################################################

x1pilotData <- read.csv("X1_data/x1pDataAugm_sRSA_indOpt_fixed00_and_fixed.20.csv") ###
source("X1_Code/04_sRSA_X1_ScatterPlot.R")

########################################################################################

# Simple rsa, Individually optimized, softness at 0, obedience at 0, no alpha

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- 0
data$obedience <- 0
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m1 <-data
m1$Nr <- 1
remove(data)

###########################################################################################################

x1pilotData <- read.csv("X1_data/x1pDataAugm_sRSA_indOpt_PrefandObedOpt_and_fixed.2.2.csv") ###
source("X1_Code/04_sRSA_X1_ScatterPlot.R")

###########################################################################################################

## Simple rsa, Individually optimized, softness individually optimized, obedience at 0, no alpha ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m5 <-data
m5$Nr <- 5
remove(data)



###########################################################################################################

x1pilotData <- read.csv("X1_data/x1pDataAugm_sRSA_crossVal_Opt1_and_Opt2.csv")
source("X1_Code/04_sRSA_X1_ScatterPlot.R")

###########################################################################################################

## Cross-validated
## Simple rsa, Individually optimized softness, individually optimized obedience, no alpha ##


data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "yes"
m8 <-data
m8$Nr <- 8
remove(data)


###########################################################################################################

x1pilotData <- read.csv("X1_data/x1pDataAugm_fRSA_indOpt_fixed001_and_OptPrefandAlphaObed0.csv")
source("X1_Code/04_sRSA_X1_ScatterPlot.R")

###########################################################################################################

## Cross-validated
## Individual optimization
## Full rsa, softness 0, obedience 0, alpha at 1 ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- 0
data$obedience <- 0
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m19 <-data
m19$Nr <- 19
remove(data)

## Big data set ####

full <- rbind(m1, m5, m8, m19)
write.csv(full,"X1_Data/for_ggplot_scatterplots.csv")
