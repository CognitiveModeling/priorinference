source("X1_code/01_sRSA_X1_DataPreProcessing.R")
## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x1pilotData$workerid
idMax <- max(workerIDs)

#################################################
llWorkers12 <- as.matrix(read.csv("X1_data/x1KLDivs_sRSA_indOpt_2019_1010.csv"))
paramsWorkers12 <- as.matrix(read.csv("X1_data/x1Params_sRSA_indOpt_2019_1010.csv"))
llWorkers12 <- llWorkers12[,c(2:ncol(llWorkers12))]
paramsWorkers12 <- paramsWorkers12[,c(2:ncol(paramsWorkers12))]

############################################################################################
procType <- 5    ###########################################################################
############################################################################################

### 
# determining the model predictions after worker-specific model parameter optimization!
constellationCode <- matrix(0,length(x1pilotData$X),6)
uniqueCCode <- rep(0, length(x1pilotData$X))
postListMat1Opt <- matrix(0,length(x1pilotData$X),9)
postListMat2Opt <- matrix(0,length(x1pilotData$X),9)
postListMat1Opt <- matrix(0,length(x1pilotData$X),9)
postListMat2Opt <- matrix(0,length(x1pilotData$X),9)
logLik <- rep(0,length(x1pilotData$X))
workerID <- -1
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
    # all three optimized    params <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(11:13)]
    params1only <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(2)]
    params1only_obey.2 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(3)]
    params12 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(4:5)]
    # print(params)
  }
  ###############
  if(procType == 1) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  0, 0)
    postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  .2, 0)
  }else if(procType == 2) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  abs(params1only[1]), 0)
    postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  abs(params1only_obey.2[1]), .2)
  }else if(procType == 3) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  abs(params12[1]), abs(params12[2]))
    postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  .2, .2)
  }else if(procType == 4) {
  postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                100, 100)
  postListMat2Opt[i,] <- rep((1/3),9)
  } else if(procType == 5) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice, 
                                                                  0, 0)
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
x1pilotData$logLik <- logLik

if(procType == 1) {
write.csv(x1pilotData, "X1_data/x1pDataAugm_sRSA_indOpt_fixed00_and_fixed.20.csv")
}else if(procType == 2) {
  write.csv(x1pilotData, "X1_data/x1pDataAugm_sRSA_indOpt_PrefStrengthOpt_obed0_and_obed.2.csv")
}else if(procType == 3) {
  write.csv(x1pilotData, "X1_data/x1pDataAugm_sRSA_indOpt_PrefandObedOpt_and_fixed.2.2.csv")
}else if(procType == 4) {
  write.csv(x1pilotData, "X1_data/x1pDataAugm_sRSA_indOpt_PrefandObed100_and_uniform.csv")
} else if(procType == 5) {
  write.csv(x1pilotData, "X1_data/x1pDataAugm_sRSA_indOpt_fixed00_and_PrefandObedOpt.csv")
}
