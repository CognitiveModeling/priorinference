source("X1_code/01_sRSA_X1_DataPreProcessing.R")

## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x1pilotData$workerid
idMax <- max(workerIDs)

## reading in parameters from optimization files 
paramsWorkers12 <- as.matrix(read.csv("X1_data/x1Params_fRSA_globalOpt_2019_1006.csv"))
paramsWorkers12 <- paramsWorkers12[,c(2:ncol(paramsWorkers12))]

params1 <- paramsWorkers12[1,2] 
params1notObey.1 <- paramsWorkers12[1,3]
params12 <- paramsWorkers12[1,4] # obedience optimized
#
params2obeyFixed <- paramsWorkers12[1,c(8,9)]
params2alphaFixed <- paramsWorkers12[1,c(10:11)]
#
params123 <- paramsWorkers12[1,c(12:14)]

###################################################################################
parOptType <- 1 ######## number of parameters to be optimized. #####################
###################################################################################

### 
# determining the model predictions after worker-specific model parameter optimization!
constellationCode <- matrix(0,length(x1pilotData$X),6)
uniqueCCode <- rep(0, length(x1pilotData$X))
postListMat1Opt <- matrix(0,length(x1pilotData$X),9)
postListMat2Opt <- matrix(0,length(x1pilotData$X),9)

for(i in c(1:length(x1pilotData$X))) {
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  featChoice <- uttFeat[i]
  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
  uc <- 0
  for(j in c(1:6)) {
    uc <- (uc * 10) + constellationCode[i,j]
  }
  uniqueCCode[i] <- uc

  if(parOptType == 1) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice,
                                                        abs(params1[1]), 0, 1)
    postListMat2Opt[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice,
                                                        abs(params1notObey.1[1]), .1, 1)
  }else if(parOptType == 2) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice,
                                                        0, abs(params12[1]), 1) 
    postListMat2Opt[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice,
                                                        abs(params2alphaFixed[1]), abs(params2alphaFixed[2]), 1)
  }else if(parOptType == 3) {
    postListMat1Opt[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, 
                                                      abs(params2obeyFixed[1]), 0, abs(params2obeyFixed[2]) )
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

if(parOptType == 1) {
  write.csv(x1pilotData, "X1_Data/x1pDataAugm_fRSAglobalOpt_Opt1_and__Opt1obed.1.csv")
}else if(parOptType == 2) {
  write.csv(x1pilotData, "X1_Data/x1pDataAugm_fRSAglobalOpt_Opt2_and__Opt12.csv")
}else if(parOptType == 3) {
  write.csv(x1pilotData, "X1_Data/x1pDataAugm_fRSAglobalOpt_Opt13_and__Opt123.csv")
}
