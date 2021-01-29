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

## reloading optimization values
paramsUttWorkers <- as.matrix(read.csv("X3_Data/x3Params_simpleRSA_indOpt_2019_10_11.csv"))
paramsUttWorkers <- paramsUttWorkers[ , 2:ncol(paramsUttWorkers)]

#####################
procType <- 6
#####################

#####################################################################################################
##############  TIME To determine and record the actual (optimized) Model Predictions ###############
#####################################################################################################

# determining the model predictions after worker-specific model parameter optimization!
postListMat1 <- matrix(0,length(x3pilotData$X),9)
postListMat2 <- matrix(0,length(x3pilotData$X),9)
klDivValues <- matrix(NA,length(x3pilotData$X),3)
workerID <- -1
for(i in c(1:length(x3pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  if(workerID != x3pilotData$workerid[i]) {
    workerID <- x3pilotData$workerid[i]
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
x3pilotData <- data.frame(x3pilotData, as.data.frame(subjectResponses)) 

postListMat1 <- round(postListMat1, digits=3)
colnames(postListMat1) <- colnames(postListMat1, do.NULL = FALSE, prefix = "MPost1_")
x3pilotData <- data.frame(x3pilotData, as.data.frame(postListMat1)) 

postListMat2 <- round(postListMat2, digits=3)
colnames(postListMat2) <- colnames(postListMat2, do.NULL = FALSE, prefix = "MPost2_")
x3pilotData <- data.frame(x3pilotData, as.data.frame(postListMat2)) 

if(procType == 1) {
  write.csv(x3pilotData, "X3_Data/x3pDataAugm_simpleRSAindOpt_fixed001_and_fixed000.csv")
}else if(procType == 2) {
  write.csv(x3pilotData, "X3_Data/x3pDataAugm_simpleRSAindOpt_prefOnly_and_obedOnly.csv")
}else if(procType == 3) {
  write.csv(x3pilotData, "X3_Data/x3pDataAugm_simpleRSAindOpt_kappaOnly_and_obedAndKappa.csv")
}else if(procType == 4) {
  write.csv(x3pilotData, "X3_Data/x3pDataAugm_simpleRSAindOpt_prefAndKappa_and_prefObedAndKappa.csv")
}else if(procType == 5) {
  write.csv(x3pilotData, "X3_Data/x3pDataAugm_simpleRSAindOpt_fixed00-1_and_uniform.csv")
} else if(procType == 6) {
  write.csv(x3pilotData, "X3_Data/x3pDataAugm_simpleRSAindOpt_fixed001_and_Lambda.csv")
}





















