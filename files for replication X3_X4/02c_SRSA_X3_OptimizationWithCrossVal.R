source("CommonCode/SRSA_StratUtt.R")
source("CommonCode/SRSA_UttChoiceOptimization.R")

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


############
## Generating the Unique Item codes
# constellationCode <- matrix(0,length(x3pilotData$X),6)
uniqueCCode <- rep("", length(x3pilotData$X))
for(i in c(1:length(x3pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  uniqueCCode[i] <- getUtteranceChoiceConstellationCode(objectConstellation)[[1]]
}
x3pilotData$itemCode <- uniqueCCode

## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x3pilotData$workerid
idMax <- max(workerIDs)
#######################
## Optimizing (i.e. minimzing) the KL Divergence values for each worker...
## starting with 1 parameter RSA model optimizations... 
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X(max 15):TurkerSliderValues]
###################################################################################
parOptType <- 4 ######## 1, 2, 3, or 4                        #####################
###################################################################################

klDivUttWorkers <- matrix(0,length(unique(workerIDs)), 15)
klDivCrossValValues <- matrix(0,length(unique(workerIDs)), 15)
paramsUttWorkers <- matrix(0,length(unique(workerIDs)), 30)
if(parOptType==1) {
  paramsUttWorkers <- matrix(0,length(unique(workerIDs)), 15)
}else if(parOptType==4) {
  paramsUttWorkers <- matrix(0,length(unique(workerIDs)), 45)
}

workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0 & workerID != 54) {
    print(c("Optimizing Simple RSA cross. val. w.r.t. worker with ID: ",workerID))
    workerItemCodes <- (x3pilotData$itemCode)[idICases]
    ## now moving through the trials and optimizing with respect to the other trials
    for(workerTrialIndex in c(1:length(idICases))) {
      ## generating data matrix for the purpose of optimization
      trialItemCode <- workerItemCodes[workerTrialIndex]
      filteredTrialIndices <- which(workerItemCodes != trialItemCode)
      consideredIdICases <- idICases[filteredTrialIndices]
      dataWorker <- matrix(0, length(consideredIdICases), 13)
      dataWorker[,1] <- obj1OC27[consideredIdICases]
      dataWorker[,2] <- obj2OC27[consideredIdICases]
      dataWorker[,3] <- obj3OC27[consideredIdICases]
      dataWorker[,4] <- x3pilotData$numFeatures[consideredIdICases]
      dataWorker[,5:13] <- bInfGainUttTurkers[consideredIdICases,]
      # print(dataWorker)
      
      if(parOptType==1) {
        optRes1 <- optimize(SimpleRSAModelUttKLDivParamK, c(-10,10), dataWorker)
        klDivUttWorkers[workerIndex,workerTrialIndex] <- optRes1$objective
        paramsUttWorkers[workerIndex,workerTrialIndex] <- optRes1$minimum
      }else if(parOptType==2) {
        optRes2 <- optim(c(.2, 0), SimpleRSAModelUttKLDivParamAK, method="L-BFGS-B", gr=NULL, dataWorker,
                         lower = c(0,-10), upper = c(1e+10,10))
        klDivUttWorkers[workerIndex,workerTrialIndex] <- optRes2$value
        paramsUttWorkers[workerIndex,(workerTrialIndex-1)*2+1] <- optRes2$par[1]
        paramsUttWorkers[workerIndex,(workerTrialIndex-1)*2+2] <- optRes2$par[2]
      }else if(parOptType==3) {
        optRes3 <-  optim(c(.2, .2), SimpleRSAModelUttKLDivParamBK, method="L-BFGS-B", gr=NULL, dataWorker,
                          lower = c(0,-10), upper = c(1e+10,10))
        klDivUttWorkers[workerIndex,workerTrialIndex] <- optRes3$value
        paramsUttWorkers[workerIndex,(workerTrialIndex-1)*2+1] <- optRes3$par[1]
        paramsUttWorkers[workerIndex,(workerTrialIndex-1)*2+2] <- optRes3$par[2]
      }else if(parOptType==4) {
        optRes4 <-  optim(c(.2, .2, 0), SimpleRSAModelUttKLDivParamABK, method="L-BFGS-B", gr=NULL, dataWorker,
                          lower = c(0,0,-10), upper = c(1e+10,1e+10,10))
        klDivUttWorkers[workerIndex,workerTrialIndex] <- optRes4$value
        paramsUttWorkers[workerIndex,(workerTrialIndex-1)*3+1] <- optRes4$par[1]
        paramsUttWorkers[workerIndex,(workerTrialIndex-1)*3+2] <- optRes4$par[2]
        paramsUttWorkers[workerIndex,(workerTrialIndex-1)*3+3] <- optRes4$par[2]
      }
      ## now determine the KLdiv value for the trial that was left out... 
      trialDataIndex <- idICases[workerTrialIndex]
      dataTrialWorker <- matrix(0, length(consideredIdICases), 13)
      dataTrialWorker[,1] <- obj1OC27[trialDataIndex]
      dataTrialWorker[,2] <- obj2OC27[trialDataIndex]
      dataTrialWorker[,3] <- obj3OC27[trialDataIndex]
      dataTrialWorker[,4] <- x3pilotData$numFeatures[trialDataIndex]
      dataTrialWorker[,5:13] <- bInfGainUttTurkers[trialDataIndex,]
      
      if(parOptType==1) {
        klDivCrossValValues[workerIndex, workerTrialIndex] <-
          SimpleRSAModelUttKLDiv_3params(dataTrialWorker, 0, 0, optRes1$minimum)
      }else if(parOptType==2) {
        klDivCrossValValues[workerIndex, workerTrialIndex] <- 
          SimpleRSAModelUttKLDiv_3params(dataTrialWorker, optRes2$par[1], 0, optRes2$par[2])
      }else if(parOptType==3) {
        klDivCrossValValues[workerIndex, workerTrialIndex] <-
          SimpleRSAModelUttKLDiv_3params(dataTrialWorker, 0, optRes3$par[1], optRes3$par[2])
      }else if(parOptType==4) {
        klDivCrossValValues[workerIndex, workerTrialIndex] <-
          SimpleRSAModelUttKLDiv_3params(dataTrialWorker, optRes4$par[1], optRes4$par[2], optRes4$par[3])
      }
    } # end of loop moving through the worker trials
    # done with that worker
    workerIndex <- workerIndex + 1
  }
}

## 
## writing out tables
if(parOptType==1) {
  write.csv(klDivUttWorkers, "X3_Data/x3klDivOpt_simpleRSA_KappaOpt_crossVal_2019_1011.csv")
  write.csv(paramsUttWorkers, "X3_Data/x3Params_simpleRSA_KappaOpt_crossVal_2019_1011.csv")
  write.csv(klDivCrossValValues, "X3_Data/x3klDiv_Trials_simpleRSA_KappaOpt_crossVal_2019_1011.csv")
}else if(parOptType==2) {
  write.csv(klDivUttWorkers, "X3_Data/x3klDivOpt_simpleRSA_PrefKappaOpt_crossVal_2019_1011.csv")
  write.csv(paramsUttWorkers, "X3_Data/x3Params_simpleRSA_PrefKappaOpt_crossVal_2019_1011.csv")
  write.csv(klDivCrossValValues, "X3_Data/x3klDiv_Trials_simpleRSA_PrefKappaOpt_crossVal_2019_1011.csv")
}else if(parOptType==3) {
  write.csv(klDivUttWorkers, "X3_Data/x3klDivOpt_simpleRSA_ObedKappaOpt_crossVal_2019_1011.csv")
  write.csv(paramsUttWorkers, "X3_Data/x3Params_simpleRSA_ObedKappaOpt_crossVal_2019_1011.csv")
  write.csv(klDivCrossValValues, "X3_Data/x3klDiv_Trials_simpleRSA_ObedKappaOpt_crossVal_2019_1011.csv")
}else if(parOptType==4) {
  write.csv(klDivUttWorkers, "X3_Data/x3klDivOpt_simpleRSA_all3Opt_crossVal_2019_1011.csv")
  write.csv(paramsUttWorkers, "X3_Data/x3Params_simpleRSA_all3Opt_crossVal_2019_1011.csv")
  write.csv(klDivCrossValValues, "X3_Data/x3klDiv_Trials_simpleRSA_all3Opt_crossVal_2019_1011.csv")
}

