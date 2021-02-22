## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x1pilotData$workerid
idMax <- max(workerIDs)
llWorkers12 <- matrix(0,length(unique(workerIDs)), 5)
paramsWorkers12 <- matrix(0,length(unique(workerIDs)), 5)

##########
## Starting with simple base model determination:
##
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    llWorkers12[workerIndex,1] <- workerID
    paramsWorkers12[workerIndex,1] <- workerID
    ## based model -> no change in preferences!
    llWorkers12[workerIndex,2] <- 0 # -2 * length(idICases) * log(1/3)
    for(i in c(1:length(idICases))) {
      for(j in c(1:6)) {
        llWorkers12[workerIndex, 2] <- llWorkers12[workerIndex, 2] + 
                                        subjectResponses[idICases[i],j] * 
                                          (log(subjectResponses[idICases[i],j]) - log(1/3))
      }
    }
    ## done with this worker -> proceed
    workerIndex <- workerIndex + 1
  }
}

#######################
## Optimizing the log likelihoods (maximum log likelihoods for each worker...)
##    1 parameter RSA model optimizations... 
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    ## generating data matrix for the purpose of optimization
    dataWorker <- matrix(0, length(idICases), 12)
    dataWorker[,1] <- targetOC27[idICases]
    dataWorker[,2] <- obj2OC27[idICases]
    dataWorker[,3] <- obj3OC27[idICases]
    dataWorker[,4] <- uttFeat[idICases]
    dataWorker[,5] <- q1Feat[idICases]
    dataWorker[,6] <- q2Feat[idICases]
    dataWorker[,7:12] <- subjectResponses[idICases,1:6]
    
    # before optimization:         llWorkers12[workerIndex,3] <- RSAModelLL1(c(.2), dataWorker)
    optRes1 <- optimize(RSAModelLL1_1simpleRSA, c(0,1e+10), dataWorker)   
    optRes2 <- optimize(RSAModelLL1_1simpleRSA_notObey.2, c(0,1e+10), dataWorker)   
    ## 1 param RSA model
    llWorkers12[workerIndex,3] <- optRes1$objective
    llWorkers12[workerIndex,4] <- optRes2$objective
    ## resulting parameter choice
    paramsWorkers12[workerIndex,2] <- optRes1$minimum
    paramsWorkers12[workerIndex,3] <- optRes2$minimum
    ####
    print(llWorkers12[workerIndex,])
    print(paramsWorkers12[workerIndex,])
    ####
    workerIndex <- workerIndex + 1
  }
}

##########
## Optimizing the log likelihoods (maximum log likelihoods for each worker...)
# 2 & 3 parameters 
print("Starting optimization with two free parameters Simple RSA model... ")
workerIDs <- x1pilotData$workerid
idMax <- max(workerIDs)

# llWorkers12 <- matrix(0,length(unique(workerIDs)), 2)
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    ## generating data matrix for the purpose of optimization
    dataWorker <- matrix(0, length(idICases), 12)
    dataWorker[,1] <- targetOC27[idICases]
    dataWorker[,2] <- obj2OC27[idICases]
    dataWorker[,3] <- obj3OC27[idICases]
    dataWorker[,4] <- uttFeat[idICases]
    dataWorker[,5] <- q1Feat[idICases]
    dataWorker[,6] <- q2Feat[idICases]
    dataWorker[,7:12] <- subjectResponses[idICases,1:6]
    
# before optimization:     llWorkers12[workerIndex,7] <- RSAModelLL2(c(.2,.2), dataWorker)
    optRes2n1 <- optim(c(.2, .2), RSAModelLL2_simpleRSA, method="L-BFGS-B", gr=NULL, dataWorker,
                       lower = c(0,0), upper = c(1e+10,1e+10))
    # print(optRes)
    ## 2 and 3 param RSA model2
    ## max likelihood parameter choice
    llWorkers12[workerIndex,5] <- optRes2n1$value
    ## max likelihood parameter choice
    paramsWorkers12[workerIndex,4] <- optRes2n1$par[1]
    paramsWorkers12[workerIndex,5] <- optRes2n1$par[2]
    ##
    print(llWorkers12[workerIndex,])
    print(paramsWorkers12[workerIndex,])
    workerIndex <- workerIndex + 1
  }
}



## writing out result tables
write.csv(llWorkers12, "X1_data/x1KLDivs_sRSA_indOpt_2019_1010")
write.csv(paramsWorkers12, "X1_data/x1Params_sRSA_indOpt_2019_1010.csv")
