# loading the augmented pilot data file

################### simple RSA ######################

# NOTE: to generate these data files you need to run 03a, 03b and 03c depending on the model you are interested in.

### simple RSA - individually optimized
#x2pilotData <- read.csv("X2_data/x2pDataAugm_sRSAindOpt_fixed001_and_fixed000.csv")
#x2pilotData <- read.csv("X2_data/x2pDataAugm_sRSAindOpt_prefOnly_and_obedOnly.csv")
#x2pilotData <- read.csv("X2_data/x2pDataAugm_sRSAindOpt_lambdaOnly_and_obedAndLambda.csv")
#x2pilotData <- read.csv("X2_data/x2pDataAugm_sRSAindOpt_prefAndLambda_and_prefObedAndLambda.csv")
#x2pilotData <- read.csv("X2_data/x2pDataAugm_sRSAindOpt_fixed00-1_and_uniform.csv")
#x2pilotData <- read.csv("X2_data/x2pDataAugm_sRSAindOpt_fixed001_and_Lambda.csv")

#x2pilotData <- read.csv("X2_data/x2pDataAugm_sRSAglobalOpt_prefOnly_and_obedOnly.csv")
#x2pilotData <- read.csv("X2_data/x2pDataAugm_sRSAglobalOpt_lambdaOnly_and_obedAndLambda.csv")
#x2pilotData <- read.csv("X2_data/x2pDataAugm_sRSAglobalOpt_prefAndLambda_and_prefObedAndLambda.csv")

x2pilotData <- read.csv("X2_data/x2pDataAugm_sRSAcrossVal_LambdaOnly_and_PrefAndLambda.csv")
#x2pilotData <- read.csv("X2_data/x2pDataAugm_sRSAcrossVal_ObedAndLambda_and_PrefObedAndLambda.csv")

## adding the 1-27 target and object2 & object3 code.
o1 <- x2pilotData$obj1OC27
o2 <- x2pilotData$obj2OC27
o3 <- x2pilotData$obj3OC27
##

########### 
## filtering for only the present feature values for each feature.
###########
subjectGuessIndexM1 <- grep("^DPost_1", colnames(x2pilotData)) - 1
modelGuessIndex1M1 <- grep("^MPost1_1", colnames(x2pilotData)) - 1
modelGuessIndex2M1 <- grep("^MPost2_1", colnames(x2pilotData)) - 1

for(i in c(1:nrow(x2pilotData))) {
  currentObjects <- c(o1[i], o2[i], o3[i])
  validUtterances <- determineValidUtterances(currentObjects)
  for(j in c(1:3)) { # iterating over the three feature types
    relevantIndices <- which(validUtterances>(3*(j-1)) & validUtterances<(3*j + 1)) # relevant indices for a particular feature type
    valUttRel <- validUtterances[relevantIndices]
    # setting the non-represented values to NA
    for(v in c(1:3)) {
      if(length(which(valUttRel == ((j-1)*3) + v )) == 0) {
        x2pilotData[[subjectGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x2pilotData[[modelGuessIndex1M1 + ((j-1)*3) + v]][i] <- NA
        x2pilotData[[modelGuessIndex2M1 + ((j-1)*3) + v]][i] <- NA
      }
    }
  }
}


## now determining the constellation code. 
uniqueCCode <- rep(0, length(x2pilotData$X))
featureOrder <- matrix(0, length(x2pilotData$X), 3)
objectOrder <- matrix(0, length(x2pilotData$X), 3)
featureValueOrder <- list()
for(i in c(1:length(x2pilotData$X))) {
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
subjectGuessIndex <- grep("^DPost_1", colnames(x2pilotData))
modelGuessIndex1 <- grep("^MPost1_1", colnames(x2pilotData))
modelGuessIndex2 <- grep("^MPost2_1", colnames(x2pilotData))

x2pilotData$CCode <- uniqueCCode
x2pilotData$featValOrder <- featureValueOrder


################################################################################
# This code can be used to generate an order matrix of model and participant data for inspection: 
#
# modelDataOrdered <- matrix(-1,nrow(x2pilotData),34)
# for(i in c(1:length(x2pilotData$X))) {
#   for(j in c(1:length(x2pilotData$featValOrder[[i]]))) {
#     modelDataOrdered[i,7+j] <- x2pilotData[[subjectGuessIndexM1+x2pilotData$featValOrder[[i]][j]]][i] 
#     modelDataOrdered[i,16+j] <- x2pilotData[[modelGuessIndex1M1+x2pilotData$featValOrder[[i]][j]]][i] 
#     modelDataOrdered[i,25+j] <- x2pilotData[[modelGuessIndex2M1+x2pilotData$featValOrder[[i]][j]]][i] 
#   }
# }
# modelDataOrdered[,1] <- uniqueCCode
# modelDataOrdered[,2] <- x2pilotData$obj1
# modelDataOrdered[,3] <- x2pilotData$obj2
# modelDataOrdered[,4] <- x2pilotData$obj3
# modelDataOrdered[,5] <- x2pilotData$obj1OC27
# modelDataOrdered[,6] <- x2pilotData$obj2OC27
# modelDataOrdered[,7] <- x2pilotData$obj3OC27
# write.csv(modelDataOrdered, "X2_data/x2pilotDataModelOptimizedSorted.csv")
# write.csv(modelDataOrdered, "X2_data/x2pilotDataModelOptimizedSorted_global.csv")
################################################################################


x2pilotData <- x2pilotData[order(x2pilotData$CCode),]
myCCodes <- unique(x2pilotData$CCode)
avDataMatrix <- matrix(0,length(myCCodes),19)
dataPointIndex <- 0
workerData <- 0
rsaModel1 <- 0
rsaModel2 <- 0
runIndex <- 1
for(i in c(1:length(myCCodes))) {
  cc <- myCCodes[i]
  cases <- which(x2pilotData$CCode == cc)
  allPilotDataCases <- x2pilotData[cases,]
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

