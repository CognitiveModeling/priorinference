## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x1pilotData$workerid
idMax <- max(workerIDs)

#################################################
paramsWorkers12 <- as.matrix(read.csv("X1_data/x1Params_sRSA_globalOpt_2019_1009.csv"))
paramsWorkers12 <- paramsWorkers12[,c(2:ncol(paramsWorkers12))]

params1 <- paramsWorkers12[1,2]
params1notObey.1 <- paramsWorkers12[1,3]
params12 <- paramsWorkers12[1,c(4,5)]

############################################################################################
procType <- 2    ###########################################################################
############################################################################################

### 
# determining the model predictions after worker-specific model parameter optimization!
constellationCode <- matrix(0,length(x1pilotData$X),6)
uniqueCCode <- rep(0, length(x1pilotData$X))
postListMat1Opt <- matrix(0,length(x1pilotData$X),9)
postListMat2Opt <- matrix(0,length(x1pilotData$X),9)

###########################################
for(i in c(1:length(x1pilotData$X))) {
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  featChoice <- uttFeat[i]
  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
  uc <- 0
  for(j in c(1:6)) {
    uc <- (uc * 10) + constellationCode[i,j]
  }
  uniqueCCode[i] <- uc
  
  if(procType == 1) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                         0.1, 0.1)
    postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                         abs(params1notObey.1[1]), .1)
  }else if(procType == 2) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                         abs(params1[1]), 0)
    postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                         abs(params12[1]), abs(params12[2]))
  }
}


###########
## adding all those values to the x1pilotData table.
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)
colnames(subjectResponsesOrdered) <- colnames(subjectResponsesOrdered, do.NULL = FALSE, prefix = "DataPost_")
x1pilotData <- data.frame(x1pilotData, as.data.frame(subjectResponsesOrdered)) 

postListMat1Opt <- round(postListMat1Opt, digits=5)
colnames(postListMat1Opt) <- colnames(postListMat1Opt, do.NULL = FALSE, prefix = "Post1_")
consCodeAndPosteriors <- data.frame(as.data.frame(postListMat1Opt))
x1pilotData <- data.frame(x1pilotData, consCodeAndPosteriors) 

postListMat2Opt <- round(postListMat2Opt, digits=5)
colnames(postListMat2Opt) <- colnames(postListMat2Opt, do.NULL = FALSE, prefix = "Post2_")
consCodeAndPosteriorsNO <- data.frame(as.data.frame(postListMat2Opt))
x1pilotData <- data.frame(x1pilotData, consCodeAndPosteriorsNO) 

x1pilotData$CCode <- uniqueCCode

if(procType == 1) {
write.csv(x1pilotData, "X1_data/x1pDataAugm_sRSA_globaOpt_fixed.1.1_and_OptPrefobedFixed.1.csv")
}else if(procType == 2) {
  write.csv(x4pilotData, "X1_data/x1pDataAugm_sRSA_globalOpt_OptPrefObedFixed0_and_Opt12.csv")
}
