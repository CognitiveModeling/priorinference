source("getStartedLocally.R")
source("X2_code/01_sRSA_X2_DataPreProcessing.R")

## Generating the Unique Item codes
# constellationCode <- matrix(0,length(x2pilotData$X),6)
uniqueCCode <- rep("", length(x2pilotData$X))
for(i in c(1:length(x2pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  uniqueCCode[i] <- getUtteranceChoiceConstellationCode(objectConstellation)[[1]]
}
x2pilotData$itemCode <- uniqueCCode

## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x2pilotData$workerid
idMax <- max(workerIDs)

###################################################################################

params1 <- as.matrix(read.csv("X2_data/x2Params_sRSA_LambdaOpt_crossVal_2019_1011.csv"))
params1 <- params1[ , 2:ncol(params1)]
params2 <- as.matrix(read.csv("X2_data/x2Params_sRSA_PrefLambdaOpt_crossVal_2019_1011.csv"))
params2 <- params2[ , 2:ncol(params2)]

zeroOffset <- 0.001

klDivCrossValValues1 <- matrix(0,length(unique(workerIDs)), 15)
klDivCrossValValues2 <- matrix(0,length(unique(workerIDs)), 15)

workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    workerItemCodes <- (x2pilotData$itemCode)[idICases]
    ## now moving through the trials and optimizing with respect to the other trials
    for(workerTrialIndex in c(1:length(idICases))) {
      trialDataIndex <- idICases[workerTrialIndex]
      dataTrialWorker <- matrix(0, 1, 13)
      dataTrialWorker[,1] <- obj1OC27[trialDataIndex]
      dataTrialWorker[,2] <- obj2OC27[trialDataIndex]
      dataTrialWorker[,3] <- obj3OC27[trialDataIndex]
      dataTrialWorker[,4] <- x2pilotData$numFeatures[trialDataIndex]
      dataTrialWorker[,5:13] <- bInfGainUttTurkers[trialDataIndex,]
      
      klDivCrossValValues1[workerIndex, workerTrialIndex] <-
        SimpleRSAModelUttKLDiv_3params(dataTrialWorker, 
                                       zeroOffset, 
                                       zeroOffset, 
                                       params1[workerIndex,workerTrialIndex])
      klDivCrossValValues2[workerIndex, workerTrialIndex] <-
        SimpleRSAModelUttKLDiv_3params(dataTrialWorker, 
                                       zeroOffset+params2[workerIndex,(workerTrialIndex-1)*2+1], 
                                       zeroOffset, 
                                       params2[workerIndex,(workerTrialIndex-1)*2+2])
    } # end of loop moving through the worker trials
    # done with that worker
    workerIndex <- workerIndex + 1
  }
}

print(sum(klDivCrossValValues1))
write.csv(klDivCrossValValues1, "X2_data/x2klDiv_Trials_sRSA_LambdaOpt_crossVal_adjusted.csv")
print(sum(klDivCrossValValues2))
write.csv(klDivCrossValValues2, "X2_data/x2klDiv_Trials_sRSA_PrefLambdaOpt_crossVal_adjusted.csv")

