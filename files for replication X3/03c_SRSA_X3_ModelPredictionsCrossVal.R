source("R/SRSA_StratUtt.R")
source("R/SRSA_UttChoiceOptimization.R")

# loading the raw pilot data (as Greg sent it on 2018/12/21)
x3pilotData <- read.csv("X3_Data/3-pilot-utterance-choice.csv")

## adding the 1-27 target and object2 & object3 code.
temp <- x3pilotData$obj1
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj1OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x3pilotData$obj1OC27 <- obj1OC27

temp <- x3pilotData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x3pilotData$obj2OC27 <- obj2OC27

temp <- x3pilotData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x3pilotData$obj3OC27 <- obj3OC27

## now identify the first column number of the turker sliders and response pairs
sliderIndex <- grep("^pref1", colnames(x3pilotData))
## and use that index to determine all slider identities and corresponding slider values.
sliderUtteranceTypes <- matrix(NA, nrow(x3pilotData), 9)
sliderSetValues <- matrix(NA,  nrow(x3pilotData), 9)
for(i in c(1:9)) {
  colIndex <- sliderIndex + (i-1) * 2
  relRows <- which(!is.na(x3pilotData[[colIndex]]))
  for(j in c(1:length(relRows) ) ) { 
    sliderUtteranceTypes[relRows[j], i] <- which(allUtterancesNew==x3pilotData[[colIndex]][relRows[j]])
    sliderSetValues[relRows[j], i] <- x3pilotData[[colIndex+1]][relRows[j]]
  }
}
### normalizing the turker estimates and setting them into the corresponding matrix.
bInfGainUttTurkers <- matrix(NA, nrow(x3pilotData), 9)
for(i in c(1:nrow(x3pilotData)) ) {
  s <- sum(sliderSetValues[i,c(1:x3pilotData$numFeatures[i])])
  if(s > 0) {
    sliderSetValues[i,c(1:x3pilotData$numFeatures[i])] <- sliderSetValues[i,c(1:x3pilotData$numFeatures[i])] / s
  }else{
    sliderSetValues[i,c(1:x3pilotData$numFeatures[i])] <- 1 / (x3pilotData$numFeatures[i])
  }
  bInfGainUttTurkers[i, sliderUtteranceTypes[i,c(1:(x3pilotData$numFeatures[i]) )] ] <- sliderSetValues[i,c(1:(x3pilotData$numFeatures[i]) )]
  for(j in c(1:x3pilotData$numFeatures[i])) {
    if(is.na(sliderSetValues[i,j])) {
      print("ERRor")
    }
  }
}


############
## Generating the Unique Item codes
# constellationCode <- matrix(0,length(x3pilotData$X),6)
uniqueCCode <- rep("", length(x3pilotData$X))
for(i in c(1:length(x3pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  uniqueCCode[i] <- getUtteranceChoiceConstellationCode(objectConstellation)[[1]]
}
x3pilotData$itemCode <- uniqueCCode

#######################
## Optimizing (i.e. minimzing) the KL Divergence values for each worker...
## starting with 1 parameter RSA model optimizations... 
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X(max 15):TurkerSliderValues]
###################################################################################
parOptType <- 34 ########  12 OR 34  ##############################################
###################################################################################

if(parOptType == 12) {
  paramsUttWorkers1 <- as.matrix(read.csv("X3_Data/x3Params_simpleRSA_KappaOpt_crossVal_2019_1011.csv"))
  paramsUttWorkers1 <- paramsUttWorkers1[ , 2:ncol(paramsUttWorkers1)]
  paramsUttWorkers2 <- as.matrix(read.csv("X3_Data/x3Params_simpleRSA_PrefKappaOpt_crossVal_2019_1011.csv"))
  paramsUttWorkers2 <- paramsUttWorkers2[ , 2:ncol(paramsUttWorkers2)]
}else if(parOptType == 34) {
  paramsUttWorkers3 <- as.matrix(read.csv("X3_Data/x3Params_simpleRSA_ObedKappaOpt_crossVal_2019_1011.csv"))
  paramsUttWorkers3 <- paramsUttWorkers3[ , 2:ncol(paramsUttWorkers3)]
  paramsUttWorkers4 <- as.matrix(read.csv("X3_Data/x3Params_simpleRSA_all3Opt_crossVal_2019_1011.csv"))
  paramsUttWorkers4 <- paramsUttWorkers4[ , 2:ncol(paramsUttWorkers4)]
}

#####################################################################################################
##############  TIME To determine and record the actual (optimized) Model Predictions ###############
#####################################################################################################
### 
# determining the model predictions after worker-specific model parameter optimization!
workerIDs <- x3pilotData$workerid
idMax <- max(workerIDs)
constellationCode <- matrix(0,length(x3pilotData$X),6)
uniqueCCode <- rep(0, length(x3pilotData$X))
postListMat1 <- matrix(0,length(x3pilotData$X),9)
postListMat2 <- matrix(0,length(x3pilotData$X),9)
klDivValues <- rep(0,length(x3pilotData$X))
workerID <- -1
workerIndex <- 0 ### starting with zero and -1 worker ID...
### as a result, workerIndex is increase by one  in the first for-loop iteration
trialIndex <- 1
for(i in c(1:length(x3pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  validUtterances <- determineValidUtterances(objectConstellation)
  if(workerID != x3pilotData$workerid[i]) {
    workerID <- x3pilotData$workerid[i]
    workerIndex <- workerIndex + 1
    trialIndex <- 1
  }
  if(parOptType==12) {
    params1 <- paramsUttWorkers1[workerIndex, trialIndex]
    postListMat1[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation,  
                                                               0, 0, params1[1])
    params2 <- paramsUttWorkers2[workerIndex, (((trialIndex-1)*2+1):((trialIndex-1)*2+2))]
    postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 
                                                               abs(params2[1]), 0, params2[2])
  }else if(parOptType==34) {
    params3 <- paramsUttWorkers3[workerIndex, (((trialIndex-1)*2+1):((trialIndex-1)*2+2))]
    postListMat1[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 
                                                               0, abs(params3[1]), params3[2])
    params4 <- paramsUttWorkers4[workerIndex, (((trialIndex-1)*3+1):((trialIndex-1)*3+3))]
    postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 
                                                           abs(params4[1]), abs(params4[2]), params4[3])
  }
}

###########
## adding all those values to the x3pilotData table.
subjectResponses <- round(bInfGainUttTurkers, digits=3)
colnames(subjectResponses) <- colnames(subjectResponses, do.NULL = FALSE, prefix = "DPost_")
x3pilotData <- data.frame(x3pilotData, as.data.frame(subjectResponses)) 

postListMat1 <- round(postListMat1, digits=3)
colnames(postListMat1) <- colnames(postListMat1, do.NULL = FALSE, prefix = "MPost1_")
x3pilotData <- data.frame(x3pilotData, as.data.frame(postListMat1)) 

postListMat2 <- round(postListMat2, digits=3)
colnames(postListMat2) <- colnames(postListMat2, do.NULL = FALSE, prefix = "MPost2_")
x3pilotData <- data.frame(x3pilotData, as.data.frame(postListMat2)) 

if(parOptType==12) {
  write.csv(x3pilotData, "X3_Data/x3pDataAugm_SRSAcrossVal_KappaOnly_and_PrefAndKappa.csv")
}else if(parOptType==34) {
  write.csv(x3pilotData, "X3_Data/x3pDataAugm_SRSAcrossVal_ObedAndKappa_and_PrefObedAndKappa.csv")
}























