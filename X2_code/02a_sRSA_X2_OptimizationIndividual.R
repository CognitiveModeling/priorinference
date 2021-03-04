source("X2_code/01_sRSA_X2_DataPreProcessing.R")

## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x2pilotData$workerid
idMax <- max(workerIDs)
klDivUttWorkers <- matrix(0,length(unique(workerIDs)), 8)
paramsUttWorkers <- matrix(0,length(unique(workerIDs)), 11)

#######################################################
## Starting with simple base model determination:    ##
## Note that the KL values here do NOT filter out those feature values that are NOT in the objects.
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
      len <- x2pilotData$numFeatures[idICases[i]]
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
    dataWorker[,4] <- x2pilotData$numFeatures[idICases]
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
    print(klDivUttWorkers[workerIndex,])
    print(paramsUttWorkers[workerIndex,])
    ####
    workerIndex <- workerIndex + 1
  }
}

write.csv(klDivUttWorkers, "X2_data/x2KLDivs_sRSA_indOpt_2021_03_01.csv")
write.csv(paramsUttWorkers, "X2_data/x2Params_sRSA_indOpt_2021_03_01.csv")


