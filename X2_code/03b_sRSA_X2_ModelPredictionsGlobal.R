source("X2_code/01_sRSA_X2_DataPreProcessing.R")

## reloading optimization values
paramsUttWorkers <- as.matrix(read.csv("X2_data/x2Params_sRSA_globalOpt_2019_12_20.csv"))
paramsUttWorkers <- paramsUttWorkers[ , 2:ncol(paramsUttWorkers)]
#print(paramsUttWorkers)
#####################
procType <- 2
#####################

#####################################################################################################
##############  TIME To determine and record the actual (optimized) Model Predictions ###############
#####################################################################################################

# determining the model predictions after worker-specific model parameter optimization!
postListMat1 <- matrix(0,length(x2pilotData$X),9)
postListMat2 <- matrix(0,length(x2pilotData$X),9)
klDivValues <- matrix(NA,length(x2pilotData$X),3)
workerID <- -1
paramsA <- paramsUttWorkers[c(2)]
paramsB <- paramsUttWorkers[c(3)]
paramsD <- paramsUttWorkers[c(4)]
paramsBD <- paramsUttWorkers[c(5:6)]
paramsAD <- paramsUttWorkers[c(7:8)]
paramsABD <- paramsUttWorkers[c(9:11)]

for(i in c(1:length(x2pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  ##
  validUtterances <- determineValidUtterances(objectConstellation)

  ## determining the model predictions
  if(procType == 1) {
    postListMat1[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsA[1], 0, 1)
    postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, paramsB[1], 1)
  }else if(procType == 2) {
    postListMat1[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, 0, paramsD[1])
    postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, paramsBD[1], paramsBD[2])
  }else if(procType == 3) {
    postListMat1[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsAD[1], 0, paramsAD[2])
    postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsABD[1], paramsABD[2], paramsABD[3])
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

if(procType == 1) {
  write.csv(x2pilotData, "X2_data/x2pDataAugm_sRSAglobalOpt_prefOnly_and_obedOnly.csv")
}else if(procType == 2) {
  write.csv(x2pilotData, "X2_data/x2pDataAugm_sRSAglobalOpt_lambdaOnly_and_obedAndLambda.csv")
}else if(procType == 3) {
  write.csv(x2pilotData, "X2_data/x2pDataAugm_sRSAglobalOpt_prefAndLambda_and_prefObedAndLambda.csv")
} 
