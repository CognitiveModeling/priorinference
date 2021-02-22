## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x1pilotData$workerid
idMax <- max(workerIDs)
uniqueItemCodes <- sort(unique(x1pilotData$itemCode))

#######################
## Optimizing wrt KL divergence criterion
# parameter-based Simple RSA model optimizations...
# leave one out parameter determination for each participant!

###################################################################################
parOptType <-2 ######## number of parameters to be optimized (1 or 2). ############
###################################################################################

llWorkers12 <- matrix(0,length(unique(workerIDs)), 15)
klDivCrossValValues <- matrix(0,length(unique(workerIDs)), 15)
if(parOptType==1 || parOptType==2) {
  paramsWorkers12 <- matrix(0,length(unique(workerIDs)), 15)
}else if(parOptType==3) {
  paramsWorkers12 <- matrix(0,length(unique(workerIDs)), 30)
}

workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    print(c("Optimizing cross. val. w.r.t. worker with ID: ",workerID))
    workerItemCodes <- (x1pilotData$itemCode)[idICases]
    ## now moving through the trials and optimizing with respect to the other trials
    for(workerTrialIndex in c(1:length(idICases))) {
      trialItemCode <- workerItemCodes[workerTrialIndex]
      filteredTrialIndices <- which(workerItemCodes != trialItemCode)
      consideredIdICases <- idICases[filteredTrialIndices]
      #      if(length(consideredIdICases) < 14) {
      #        print(c("Multiple trials of same case in worker: ",workerID," with a length of ",
      #                length(consideredIdICases)," of ",length(idICases)))
      #      }
      ## generating data matrix containing the considered cases
      dataWorker <- matrix(0, length(consideredIdICases), 12)
      dataWorker[,1] <- targetOC27[consideredIdICases]
      dataWorker[,2] <- obj2OC27[consideredIdICases]
      dataWorker[,3] <- obj3OC27[consideredIdICases]
      dataWorker[,4] <- uttFeat[consideredIdICases]
      dataWorker[,5] <- q1Feat[consideredIdICases]
      dataWorker[,6] <- q2Feat[consideredIdICases]
      dataWorker[,7:12] <- subjectResponses[consideredIdICases,1:6]
      
      if(parOptType==1) {
        optRes1 <- optimize(RSAModelLL1_1simpleRSA, c(0,1e+10), dataWorker)
        llWorkers12[workerIndex,workerTrialIndex] <- optRes1$objective
        paramsWorkers12[workerIndex,workerTrialIndex] <- optRes1$minimum
      }else if(parOptType==2) {
        optRes1notObey.1 <- optimize(RSAModelLL1_1simpleRSA_notObey.1, c(0,1e+10), dataWorker)
        llWorkers12[workerIndex,workerTrialIndex] <- optRes1notObey.1$objective
        paramsWorkers12[workerIndex,workerTrialIndex] <- optRes1notObey.1$minimum
      }else if(parOptType==3) {
        optRes2 <- optim(c(.2, .2), RSAModelLL2_simpleRSA, method="L-BFGS-B", gr=NULL, dataWorker,
                         lower = c(0, 0), upper = c(1e+10, 1e+10))
        llWorkers12[workerIndex,workerTrialIndex] <- optRes2$value
        paramsWorkers12[workerIndex,(workerTrialIndex-1)*2+1] <- optRes2$par[1]
        paramsWorkers12[workerIndex,(workerTrialIndex-1)*2+2] <- optRes2$par[2]
      }
      ## now determine the KLdiv value for the trial that was left out... 
      dataTrialWorker <- matrix(0, 1, 12)
      trialDataIndex <- idICases[workerTrialIndex]
      dataTrialWorker[,1] <- targetOC27[trialDataIndex]
      dataTrialWorker[,2] <- obj2OC27[trialDataIndex]
      dataTrialWorker[,3] <- obj3OC27[trialDataIndex]
      dataTrialWorker[,4] <- uttFeat[trialDataIndex]
      dataTrialWorker[,5] <- q1Feat[trialDataIndex]
      dataTrialWorker[,6] <- q2Feat[trialDataIndex]
      dataTrialWorker[,7:12] <- subjectResponses[trialDataIndex,1:6]
      
      if(parOptType==1) {
        klDivCrossValValues[workerIndex, workerTrialIndex] <-
          RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(dataTrialWorker, abs(optRes1$minimum), 0)
      }else if(parOptType==2) {
        klDivCrossValValues[workerIndex, workerTrialIndex] <-
          RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(dataTrialWorker, abs(optRes1notObey.1$minimum), 0.1)
      }else if(parOptType==3) {
        klDivCrossValValues[workerIndex, workerTrialIndex] <- 
          RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(dataTrialWorker, abs(optRes2$par[1]), abs(optRes2$par[2]))
      }
      
    } # end of loop moving through the worker trials
    
    # done with that worker
    workerIndex <- workerIndex + 1
  }
}

## 
## writing out tables
if(parOptType==1) {
  write.csv(llWorkers12, "X1_data/x1klDivOpt_sRSA_crossVal_1parOpt_2019_1009.csv")
  write.csv(paramsWorkers12, "X1_data/x1Params_sRSA_crossVal_1parOpt_2019_1009.csv")
  write.csv(klDivCrossValValues, "X1_data/x1klDiv_Trials_sRSA_1parOpt_crossVal_2019_1009.csv")
}else if(parOptType==2) {
  write.csv(llWorkers12, "X1_data/x1klDivOpt_sRSA_crossVal_1parOptnotObej.1_2019_1009.csv")
  write.csv(paramsWorkers12, "X1_data/x1Params_sRSA_crossVal_1parOptnotObej.1_2019_1009.csv")
  write.csv(klDivCrossValValues, "X1_data/x1klDiv_Trials_sRSA_1parOptnotObej.1_crossVal_2019_1009.csv")
}else if(parOptType==3) {
  write.csv(llWorkers12, "X1_data/x1klDivOpt_sRSA_crossVal_2parOpt_2019_1009.csv")
  write.csv(paramsWorkers12, "X1_data/x1Params_sRSA_crossVal_2parOpt_2019_1009.csv")
  write.csv(klDivCrossValValues, "X1_data/x1klDiv_Trials_sRSA_2parOpt_crossVal_2019_1009.csv")
}


