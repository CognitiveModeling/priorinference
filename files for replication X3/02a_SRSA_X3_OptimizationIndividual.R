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

## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x3pilotData$workerid
idMax <- max(workerIDs)
klDivUttWorkers <- matrix(0,length(unique(workerIDs)), 8)
paramsUttWorkers <- matrix(0,length(unique(workerIDs)), 11)

#######################################################
## Starting with simple base model determination:    ##
#######################################################
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    klDivUttWorkers[workerIndex,1] <- workerID
    paramsUttWorkers[workerIndex,1] <- workerID
    ## based model -> no change in preferences!
    klDivUttWorkers[workerIndex,2] <- 0
    for(i in c(1:length(idICases))) {
      len <- x3pilotData$numFeatures[idICases[i]]
      for(j in c(1:len) ) {
        klDivUttWorkers[workerIndex, 2] <- klDivUttWorkers[workerIndex, 2] + 
          sliderSetValues[idICases[i],j] * 
          (log(sliderSetValues[idICases[i],j] + 1e-100) - log(1/len))
        if(is.na(klDivUttWorkers[workerIndex, 2])) {
          print("Is NA!???")
          print(c(sliderSetValues[idICases[i],j], log(1/len), i, j, len))
          j <- 10
          i <- length(idICases) +1 
        }
      }
    }
    ## done with this worker -> proceed
    workerIndex <- workerIndex + 1
  }
}

#######################
## Optimizing (i.e. minimzing) the KL Divergence values for each worker...
## starting with 1 parameter RSA model optimizations... 
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X(max 15):TurkerSliderValues]
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    ## generating data matrix for the purpose of optimization
    dataWorker <- matrix(0, length(idICases), 13)
    dataWorker[,1] <- obj1OC27[idICases]
    dataWorker[,2] <- obj2OC27[idICases]
    dataWorker[,3] <- obj3OC27[idICases]
    dataWorker[,4] <- x3pilotData$numFeatures[idICases]
    dataWorker[,5:13] <- bInfGainUttTurkers[idICases,]
    # print(dataWorker)
    # now optimize for one parameter... 
    optRes1 <- optimize(SimpleRSAModelUttKLDivParamA, c(0,1e+10), dataWorker)
    optRes2 <- optimize(SimpleRSAModelUttKLDivParamB, c(0,1e+10), dataWorker)   
    optRes3 <- optimize(SimpleRSAModelUttKLDivParamK, c(-10,10), dataWorker)   

    ## 1 param RSA Utt model
    klDivUttWorkers[workerIndex,3] <- optRes1$objective
    klDivUttWorkers[workerIndex,4] <- optRes2$objective
    klDivUttWorkers[workerIndex,5] <- optRes3$objective
    ## resulting parameter choice
    paramsUttWorkers[workerIndex,2] <- optRes1$minimum
    paramsUttWorkers[workerIndex,3] <- optRes2$minimum
    paramsUttWorkers[workerIndex,4] <- optRes3$minimum
    ####
    optRes2n1 <- optim(c(.2, .2), SimpleRSAModelUttKLDivParamBK, method="L-BFGS-B", gr=NULL, dataWorker,
                       lower = c(0,-10), upper = c(1e+10,10))
    optRes2n2 <- optim(c(.2, .2), SimpleRSAModelUttKLDivParamAK, method="L-BFGS-B", gr=NULL, dataWorker,
                       lower = c(0,-10), upper = c(1e+10,10))
    optRes3 <- optim(c(.2, .2, 1), SimpleRSAModelUttKLDivParamABK, method="L-BFGS-B", gr=NULL, dataWorker,
                     lower = c(0,0,-10), upper = c(1e+10,1e+10,10))
    ## 2 and 3 param RSA model2
    ## max likelihood parameter choice
    klDivUttWorkers[workerIndex,6] <- optRes2n1$value
    klDivUttWorkers[workerIndex,7] <- optRes2n2$value
    klDivUttWorkers[workerIndex,8] <- optRes3$value
    ## max likelihood parameter choice
    paramsUttWorkers[workerIndex,5] <- optRes2n1$par[1]
    paramsUttWorkers[workerIndex,6] <- optRes2n1$par[2]
    paramsUttWorkers[workerIndex,7] <- optRes2n2$par[1]
    paramsUttWorkers[workerIndex,8] <- optRes2n2$par[2]
    paramsUttWorkers[workerIndex,9] <- optRes3$par[1]
    paramsUttWorkers[workerIndex,10] <- optRes3$par[2]
    paramsUttWorkers[workerIndex,11] <- optRes3$par[3]
    ##    
    print(c("Done with worker ",workerIndex," with worder ID ", workerID))
    print(c(klDivUttWorkers[workerIndex,], paramsUttWorkers[workerIndex,]))
    ####
    workerIndex <- workerIndex + 1
  }
}

write.csv(klDivUttWorkers, "X3_Data/x3KLDivs_simpleRSA_indOpt_2019_10_11.csv")
write.csv(paramsUttWorkers, "X3_Data/x3Params_simpleRSA_indOpt_2019_10_11.csv")


