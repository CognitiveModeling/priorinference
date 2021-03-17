source("getStartedLocally.R")
source("X1_code/01_sRSA_X1_DataPreProcessing.R")

## recording KL divergence and parameters (base model, 1 param, 2 params)
## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x1pilotData$workerid
idMax <- max(workerIDs)

##############################################################################
##############################################################################

klDivCrossValValues <- matrix(0,length(unique(workerIDs)), 15)

paramsWorkers2 <- as.matrix(read.csv("X1_data/x1Params_sRSA_crossVal_2parOpt_2019_1009.csv"))
paramsWorkers2 <- paramsWorkers2[,c(2:ncol(paramsWorkers2))]

zeroOffset <- 0.001
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    workerItemCodes <- (x1pilotData$itemCode)[idICases]
    ## now moving through the trials and optimizing with respect to the other trials
    for(workerTrialIndex in c(1:length(idICases))) {
      dataTrialWorker <- matrix(0, 1, 12)
      trialDataIndex <- idICases[workerTrialIndex]
      dataTrialWorker[,1] <- targetOC27[trialDataIndex]
      dataTrialWorker[,2] <- obj2OC27[trialDataIndex]
      dataTrialWorker[,3] <- obj3OC27[trialDataIndex]
      dataTrialWorker[,4] <- uttFeat[trialDataIndex]
      dataTrialWorker[,5] <- q1Feat[trialDataIndex]
      dataTrialWorker[,6] <- q2Feat[trialDataIndex]
      dataTrialWorker[,7:12] <- subjectResponses[trialDataIndex,1:6]
      
      klDivCrossValValues[workerIndex, workerTrialIndex] <- 
        RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(dataTrialWorker,        
                                                                           zeroOffset+paramsWorkers2[workerIndex,(workerTrialIndex-1)*2+1], 
                                                                           zeroOffset+paramsWorkers2[workerIndex,(workerTrialIndex-1)*2+2])
      
    } # end of loop moving through the worker trials
    # done with that worker
    workerIndex <- workerIndex + 1
  }
}

print(sum(klDivCrossValValues))
write.csv(klDivCrossValValues, "X1_data/x1klDiv_Trials_sRSA_2parOpt_crossVal_adjusted.csv")

