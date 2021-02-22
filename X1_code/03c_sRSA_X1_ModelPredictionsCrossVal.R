## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x1pilotData$workerid
idMax <- max(workerIDs)

##############################################################################
##############################################################################

## loading optimized trial-respective cross-validated parameter values 
#paramsWorkers1 <- as.matrix(read.csv("X1_data/x1CrossVal_sRSA_Params_1parOpt_2019_0430.csv"))
#paramsWorkers1 <- paramsWorkers1[,c(2:ncol(paramsWorkers1))]
#paramsWorkers1notObey.1 <- as.matrix(read.csv("X1_data/x1CrossVal_sRSA_Params_notObey.1_2019_0430.csv"))
#paramsWorkers1notObey.1 <- paramsWorkers1notObey.1[,c(2:ncol(paramsWorkers1notObey.1))]

paramsWorkers1notObey.1 <- as.matrix(read.csv("X1_data/x1Params_sRSA_crossVal_1parOptnotObej.1_2019_1009.csv"))
paramsWorkers1notObey.1 <- paramsWorkers1notObey.1[,c(2:ncol(paramsWorkers1notObey.1))]
paramsWorkers2 <- as.matrix(read.csv("X1_data/x1Params_sRSA_crossVal_2parOpt_2019_1009.csv"))
paramsWorkers2 <- paramsWorkers2[,c(2:ncol(paramsWorkers2))]


############################################################################################
procType <- 2   ###########################################################################
############################################################################################

### 
# determining the model predictions after worker-specific model parameter optimization!
constellationCode <- matrix(0,length(x1pilotData$X),6)
uniqueCCode <- rep(0, length(x1pilotData$X))
postListMat1 <- matrix(0,length(x1pilotData$X),9)
postListMat2 <- matrix(0,length(x1pilotData$X),9)
logLik <- rep(0,length(x1pilotData$X))
workerID <- -1
workerIndex <- 0 ### starting with zero and -1 worker ID...
### as a result, workerIndex is increase by one  in the first for-loop iteration
trialIndex <- 1
for(i in c(1:length(x1pilotData$X))) {
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  featChoice <- uttFeat[i]
  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
  uc <- 0
  for(j in c(1:6)) {
    uc <- (uc * 10) + constellationCode[i,j]
  }
  uniqueCCode[i] <- uc
  if(workerID != x1pilotData$workerid[i]) {
    workerID <- x1pilotData$workerid[i]
    workerIndex <- workerIndex + 1
    trialIndex <- 1
  }
  if(procType == 1) {
    params1 <- paramsWorkers1[workerIndex, trialIndex]
    postListMat1[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                               params1[1], 0) 
    params2 <- paramsWorkers2[workerIndex, (((trialIndex-1)*2+1):((trialIndex-1)*2+2))]
    postListMat2[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                               params2[1], params2[2]) 
  } else if(procType == 2) {
    params1nO.1 <- paramsWorkers1notObey.1[workerIndex, trialIndex]
    postListMat1[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                               params1nO.1[1], 0.1) 
    postListMat2[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                               0.1, 0.1) 
    
  } 
  trialIndex <- trialIndex + 1
}
# now determine expected log likelihoods given the subject responses and the optimized model values.
logLik <- rep(0,length(x1pilotData$X))
for(i in c(1:length(x1pilotData$X))) {
  for(j in 1:3) {  
    logLik[i] <- logLik[i] - subjectResponses[i,j] * 
      log(postListMat1[i, j+(q1Feat[i]-1)*3])
  }
  for(j in 1:3) {  
    logLik[i] <- logLik[i] - subjectResponses[i,3+j] * 
      log(postListMat1[i, j+(q2Feat[i]-1)*3])
  }
}

###########
## adding all those values to the x1pilotData table.
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)
colnames(subjectResponsesOrdered) <- colnames(subjectResponsesOrdered, do.NULL = FALSE, prefix = "DataPost_")
x1pilotData <- data.frame(x1pilotData, as.data.frame(subjectResponsesOrdered)) 

postListMat1 <- round(postListMat1, digits=5)
colnames(postListMat1) <- colnames(postListMat1, do.NULL = FALSE, prefix = "Post1_")
consCodeAndPosteriors <- data.frame(as.data.frame(postListMat1))
x1pilotData <- data.frame(x1pilotData, consCodeAndPosteriors) 

postListMat2 <- round(postListMat2, digits=5)
colnames(postListMat2) <- colnames(postListMat2, do.NULL = FALSE, prefix = "Post2_")
consCodeAndPosteriorsNO <- data.frame(as.data.frame(postListMat2))
x1pilotData <- data.frame(x1pilotData, consCodeAndPosteriorsNO) 

x1pilotData$CCode <- uniqueCCode
x1pilotData$logLik <- logLik

if(procType == 1) {
  write.csv(x1pilotData, "X1_data/x1pDataAugm_sRSA_crossVal_Opt1_and_Opt2.csv")
} else if(procType == 2) {
  write.csv(x1pilotData, "X1_data/x1pDataAugm_sRSA_crossVal_Opt1obed.1_and_fixed.1.1.csv")
} 


