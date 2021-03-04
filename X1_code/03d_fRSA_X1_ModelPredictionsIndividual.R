source("X1_code/01_sRSA_X1_DataPreProcessing.R")
## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x1pilotData$workerid
idMax <- max(workerIDs)

## sorting based on 2-parameter RSA optimized version. 
#llWorkers12 <- llWorkers12[order(llWorkers12[,4]),]
#llWorkers12[,2:7] <- llWorkers12[,2:7]*2
## writing out sorted table
paramsWorkers12 <- as.matrix(read.csv("x1_Data/x1OptParams_fRSA_indOpt_2019_1006.csv"))
paramsWorkers12 <- paramsWorkers12[,c(2:ncol(paramsWorkers12))]

#######################################
procType <- 2
#######################################


### 
# determining the model predictions after worker-specific model parameter optimization!
constellationCode <- matrix(0,length(x1pilotData$X),6)
uniqueCCode <- rep(0, length(x1pilotData$X))
postListMat1Opt <- matrix(0,length(x1pilotData$X),9)
postListMat2Opt <- matrix(0,length(x1pilotData$X),9)
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
    params13 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(7,8)]
    params12 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(9,10)]
    params123 <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(11:13)]
    # print(params)
  }
  if(procType == 1) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice,
                                                         0, 0, 1)
    postListMat2Opt[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice,
                                                         abs(params13[1]), 0, abs(params13[2]))
  }else if(procType == 2) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, 
                                                         abs(params12[1]), abs(params12[2]), 1)
    postListMat2Opt[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, 
                                                         abs(params123[1]), abs(params123[2]), abs(params123[3]))
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
  write.csv(x1pilotData, "X1_Data/x1pDataAugm_fRSA_indOpt_fixed001_and_OptPrefandAlphaObed0.csv")
}else if(procType == 2) {
  write.csv(x1pilotData, "X1_Data/x1pDataAugm_fRSA_indOpt_OptPrefAndObedAlpha1_and_OptAll3.csv")
}

