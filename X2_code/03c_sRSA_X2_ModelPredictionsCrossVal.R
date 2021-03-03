source("X2_code/01_sRSA_X2_DataPreProcessing.R")

## Generating the Unique Item codes
# constellationCode <- matrix(0,length(x2pilotData$X),6)
uniqueCCode <- rep("", length(x2pilotData$X))
for(i in c(1:length(x2pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  uniqueCCode[i] <- getUtteranceChoiceConstellationCode(objectConstellation)[[1]]
}
x2pilotData$itemCode <- uniqueCCode

#######################
## Optimizing (i.e. minimzing) the KL Divergence values for each worker...
## starting with 1 parameter RSA model optimizations... 
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X(max 15):TurkerSliderValues]
###################################################################################
parOptType <- 12 ########  12 OR 34  ##############################################
###################################################################################

if(parOptType == 12) {
  paramsUttWorkers1 <- as.matrix(read.csv("X2_data/x2Params_sRSA_LambdaOpt_crossVal_2019_1011.csv"))
  paramsUttWorkers1 <- paramsUttWorkers1[ , 2:ncol(paramsUttWorkers1)]
  paramsUttWorkers2 <- as.matrix(read.csv("X2_data/x2Params_sRSA_PrefLambdaOpt_crossVal_2019_1011.csv"))
  paramsUttWorkers2 <- paramsUttWorkers2[ , 2:ncol(paramsUttWorkers2)]
}else if(parOptType == 34) {
  paramsUttWorkers3 <- as.matrix(read.csv("X2_data/x2Params_sRSA_ObedLambdaOpt_crossVal_2019_1011.csv"))
  paramsUttWorkers3 <- paramsUttWorkers3[ , 2:ncol(paramsUttWorkers3)]
  paramsUttWorkers4 <- as.matrix(read.csv("X2_data/x2Params_sRSA_all3Opt_crossVal_2019_1011.csv"))
  paramsUttWorkers4 <- paramsUttWorkers4[ , 2:ncol(paramsUttWorkers4)]
}

#####################################################################################################
##############  TIME To determine and record the actual (optimized) Model Predictions ###############
#####################################################################################################
### 
# determining the model predictions after worker-specific model parameter optimization!
workerIDs <- x2pilotData$workerid
idMax <- max(workerIDs)
constellationCode <- matrix(0,length(x2pilotData$X),6)
uniqueCCode <- rep(0, length(x2pilotData$X))
postListMat1 <- matrix(0,length(x2pilotData$X),9)
postListMat2 <- matrix(0,length(x2pilotData$X),9)
klDivValues <- rep(0,length(x2pilotData$X))
workerID <- -1
workerIndex <- 0 ### starting with zero and -1 worker ID...
### as a result, workerIndex is increase by one  in the first for-loop iteration
trialIndex <- 1
for(i in c(1:length(x2pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  validUtterances <- determineValidUtterances(objectConstellation)
  if(workerID != x2pilotData$workerid[i]) {
    workerID <- x2pilotData$workerid[i]
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
## adding all those values to the x2pilotData table.
subjectResponses <- round(bInfGainUttTurkers, digits=3)
colnames(subjectResponses) <- colnames(subjectResponses, do.NULL = FALSE, prefix = "DPost_")
x2pilotData <- data.frame(x2pilotData, as.data.frame(subjectResponses)) 

postListMat1 <- round(postListMat1, digits=3)
colnames(postListMat1) <- colnames(postListMat1, do.NULL = FALSE, prefix = "MPost1_")
x2pilotData <- data.frame(x2pilotData, as.data.frame(postListMat1)) 

postListMat2 <- round(postListMat2, digits=3)
colnames(postListMat2) <- colnames(postListMat2, do.NULL = FALSE, prefix = "MPost2_")
x2pilotData <- data.frame(x2pilotData, as.data.frame(postListMat2)) 

if(parOptType==12) {
  write.csv(x2pilotData, "X2_data/x2pDataAugm_sRSAcrossVal_LambdaOnly_and_PrefAndLambda.csv")
}else if(parOptType==34) {
  write.csv(x2pilotData, "X2_data/x2pDataAugm_sRSAcrossVal_ObedAndLambda_and_PrefObedAndLambda.csv")
}
