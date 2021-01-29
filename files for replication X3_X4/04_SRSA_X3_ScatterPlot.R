source("CommonCode/RSA_StratUtt.R")
source("CommonCode/getConstCodeStratUtt.R")

# loading the augmented pilot data file

################### full RSA ########################

### full RSA - individually optimized
#x3pilotData <- read.csv("X3_Data/x3pDataAugm_RSAindOpt_fixed_and_PrefOnly.csv")
#x3pilotData <- read.csv("X3_Data/x3pDataAugm_RSAindOpt_KappaOnly_and_ObedAndKappa.csv")
#x3pilotData <- read.csv("X3_Data/x3pDataAugm_RSAindOpt_PrefAndKappa_and_PrefObedAndKappa.csv")

################### simple RSA ######################

### simple RSA - individually optimized
#x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAindOpt_fixed001_and_fixed000.csv")
#x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAindOpt_prefOnly_and_obedOnly.csv")
#x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAindOpt_kappaOnly_and_obedAndKappa.csv")
#x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAindOpt_prefAndKappa_and_prefObedAndKappa.csv")
#x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAindOpt_fixed00-1_and_uniform.csv")
#x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAindOpt_fixed001_and_Lambda.csv")

#x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAglobalOpt_prefOnly_and_obedOnly.csv")
#x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAglobalOpt_kappaOnly_and_obedAndKappa.csv")
#x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAglobalOpt_prefAndKappa_and_prefObedAndKappa.csv")


#x3pilotData <- read.csv("X3_Data/x3pDataAugm_SRSAcrossVal_KappaOnly_and_PrefAndKappa.csv")
#x3pilotData <- read.csv("X3_Data/x3pDataAugm_SRSAcrossVal_ObedAndKappa_and_PrefObedAndKappa.csv")

## adding the 1-27 target and object2 & object3 code.
o1 <- x3pilotData$obj1OC27
o2 <- x3pilotData$obj2OC27
o3 <- x3pilotData$obj3OC27
##

########### 
## filtering for only the present feature values for each feature.
###########
subjectGuessIndexM1 <- grep("^DPost_1", colnames(x3pilotData)) - 1
modelGuessIndex1M1 <- grep("^MPost1_1", colnames(x3pilotData)) - 1
modelGuessIndex2M1 <- grep("^MPost2_1", colnames(x3pilotData)) - 1

for(i in c(1:nrow(x3pilotData))) {
  currentObjects <- c(o1[i], o2[i], o3[i])
  validUtterances <- determineValidUtterances(currentObjects)
  for(j in c(1:3)) { # iterating over the three feature types
    relevantIndices <- which(validUtterances>(3*(j-1)) & validUtterances<(3*j + 1)) # relevant indices for a particular feature type
    valUttRel <- validUtterances[relevantIndices]
    # setting the non-represented values to NA
    for(v in c(1:3)) {
      if(length(which(valUttRel == ((j-1)*3) + v )) == 0) {
        x3pilotData[[subjectGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x3pilotData[[modelGuessIndex1M1 + ((j-1)*3) + v]][i] <- NA
        x3pilotData[[modelGuessIndex2M1 + ((j-1)*3) + v]][i] <- NA
      }
    }
  }
}


## now determining the constellation code. 
uniqueCCode <- rep(0, length(x3pilotData$X))
featureOrder <- matrix(0, length(x3pilotData$X), 3)
objectOrder <- matrix(0, length(x3pilotData$X), 3)
featureValueOrder <- list()
for(i in c(1:length(x3pilotData$X))) {
  objectConstellation <- c(o1[i],o2[i],o3[i])
  cc <- getUtteranceChoiceConstellationCode(objectConstellation)
  uniqueCCode[i] <- cc[[1]]
  featureOrder[i,] <- cc[[2]]
  objectOrder[i,] <- cc[[3]]
  featureValueOrder[[i]] <- cc[[4]]
}
# feature order specifies reordering of standard order (e.g. shape=1, texture=2, color=3)
# object order specifies reordering of presented object order
# featureValueOrder specifies how the present feature in an object constellation should be ordered.
subjectGuessIndex <- grep("^DPost_1", colnames(x3pilotData))
modelGuessIndex1 <- grep("^MPost1_1", colnames(x3pilotData))
modelGuessIndex2 <- grep("^MPost2_1", colnames(x3pilotData))

x3pilotData$CCode <- uniqueCCode
x3pilotData$featValOrder <- featureValueOrder

modelDataOrdered <- matrix(-1,nrow(x3pilotData),34)
for(i in c(1:length(x3pilotData$X))) {
  for(j in c(1:length(x3pilotData$featValOrder[[i]]))) {
    modelDataOrdered[i,7+j] <- x3pilotData[[subjectGuessIndexM1+x3pilotData$featValOrder[[i]][j]]][i] 
    modelDataOrdered[i,16+j] <- x3pilotData[[modelGuessIndex1M1+x3pilotData$featValOrder[[i]][j]]][i] 
    modelDataOrdered[i,25+j] <- x3pilotData[[modelGuessIndex2M1+x3pilotData$featValOrder[[i]][j]]][i] 
  }
}
modelDataOrdered[,1] <- uniqueCCode
modelDataOrdered[,2] <- x3pilotData$obj1
modelDataOrdered[,3] <- x3pilotData$obj2
modelDataOrdered[,4] <- x3pilotData$obj3
modelDataOrdered[,5] <- x3pilotData$obj1OC27
modelDataOrdered[,6] <- x3pilotData$obj2OC27
modelDataOrdered[,7] <- x3pilotData$obj3OC27
#write.csv(modelDataOrdered, "X3_Data/x3pilotDataModelOptimizedSorted.csv")
write.csv(modelDataOrdered, "X3_Data/x3pilotDataModelOptimizedSorted_global.csv")
################################################################################


x3pilotData <- x3pilotData[order(x3pilotData$CCode),]
myCCodes <- unique(x3pilotData$CCode)
avDataMatrix <- matrix(0,length(myCCodes),19)
dataPointIndex <- 0
workerData <- 0
rsaModel1 <- 0
rsaModel2 <- 0
runIndex <- 1
for(i in c(1:length(myCCodes))) {
  cc <- myCCodes[i]
  cases <- which(x3pilotData$CCode == cc)
  allPilotDataCases <- x3pilotData[cases,]
  workerMeans <- 0
  rsaModel1Means <- 0
  rsaModel2Means <- 0
  for(j in c(1:nrow(allPilotDataCases))) {
    workerMeans <-  workerMeans + allPilotDataCases[j,subjectGuessIndexM1+(allPilotDataCases$featValOrder[[j]])]
    rsaModel1Means <- rsaModel1Means + allPilotDataCases[j,modelGuessIndex1M1+(allPilotDataCases$featValOrder[[j]])]
    rsaModel2Means <- rsaModel2Means + allPilotDataCases[j,modelGuessIndex2M1+(allPilotDataCases$featValOrder[[j]])]
  }
  for(j in c(1:length(workerMeans))) {
    workerData[runIndex] <- workerMeans[[j]] / nrow(allPilotDataCases)
    rsaModel1[runIndex] <- rsaModel1Means[[j]] / nrow(allPilotDataCases)
    rsaModel2[runIndex] <- rsaModel2Means[[j]] / nrow(allPilotDataCases)
    runIndex <- runIndex+1
  }
}
  
### plot after Optimization ###
rsaModel1 <- as.array(rsaModel1)

plot(rsaModel1, workerData)
abline(lm(formula = rsaModel1~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel1,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel1~workerData)
summary(model)
confint(model)

### plot with default parameters ###
rsaModel2 <- as.array(rsaModel2)

plot(rsaModel2, workerData)
abline(lm(formula = rsaModel2~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel2,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel2~workerData)
summary(model)
confint(model)




