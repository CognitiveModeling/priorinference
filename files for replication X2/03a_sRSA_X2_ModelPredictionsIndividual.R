## reloading optimization values
paramsUttWorkers <- as.matrix(read.csv("X2_data/x2Params_sRSA_indOpt_2019_10_11.csv"))
paramsUttWorkers <- paramsUttWorkers[ , 2:ncol(paramsUttWorkers)]

#####################
procType <- 6
#####################

#####################################################################################################
##############  TIME To determine and record the actual (optimized) Model Predictions ###############
#####################################################################################################

# determining the model predictions after worker-specific model parameter optimization!
postListMat1 <- matrix(0,length(x2pilotData$X),9)
postListMat2 <- matrix(0,length(x2pilotData$X),9)
klDivValues <- matrix(NA,length(x2pilotData$X),3)
workerID <- -1
for(i in c(1:length(x2pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  if(workerID != x2pilotData$workerid[i]) {
    workerID <- x2pilotData$workerid[i]
    paramsA <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(2)]
    paramsB <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(3)]
    paramsD <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(4)]
    paramsBD <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(5:6)]
    paramsAD <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(7:8)]
    paramsABD <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(9:11)]
    # print(params)
  }
  ##
  validUtterances <- determineValidUtterances(objectConstellation)

  ## determining the model predictions
  if(procType == 1) {
    postListMat1[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, 0, 1)
    postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, 0, 0)
  }else if(procType == 2) {
    postListMat1[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsA[1], 0, 1)
    postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, paramsB[1], 1)
  }else if(procType == 3) {
    postListMat1[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, 0, paramsD[1])
    postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, paramsBD[1], paramsBD[2])
  }else if(procType == 4) {
    postListMat1[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsAD[1], 0, paramsAD[2])
    postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, paramsABD[1], paramsABD[2], paramsABD[3])
  }else if(procType == 5) {
    postListMat1[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, 0, -1)
    postListMat2[i,validUtterances] <- rep(1./length(validUtterances), length(validUtterances) )
  } else if(procType == 6) {
    postListMat1[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, 0, 1)
    postListMat2[i,validUtterances] <- getSimpleBestInfGainUttPreferences(objectConstellation, 0, 0, paramsD[1])
  }
}

###########
## adding all those values to the x4pilotData table.
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
  write.csv(x2pilotData, "X2_data/x2pDataAugm_sRSAindOpt_fixed001_and_fixed000.csv")
}else if(procType == 2) {
  write.csv(x2pilotData, "X2_data/x2pDataAugm_sRSAindOpt_prefOnly_and_obedOnly.csv")
}else if(procType == 3) {
  write.csv(x2pilotData, "X2_data/x2pDataAugm_sRSAindOpt_lambdaOnly_and_obedAndLambda.csv")
}else if(procType == 4) {
  write.csv(x2pilotData, "X2_data/x2pDataAugm_sRSAindOpt_prefAndLambda_and_prefObedAndLambda.csv")
}else if(procType == 5) {
  write.csv(x2pilotData, "X2_data/x2pDataAugm_sRSAindOpt_fixed00-1_and_uniform.csv")
} else if(procType == 6) {
  write.csv(x2pilotData, "X2_data/x2pDataAugm_sRSAindOpt_fixed001_and_Lambda.csv")
}
