## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x1pilotData$workerid
idMax <- max(workerIDs)
klDivWorkers12 <- matrix(0,length(unique(workerIDs)), 10)
paramsWorkers12 <- matrix(0,length(unique(workerIDs)), 14)


##########
## Starting with simple base model determination:
## Note that the KL values here do NOT filter out those feature values that are NOT in the objects.
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    klDivWorkers12[workerIndex,1] <- workerID
    paramsWorkers12[workerIndex,1] <- workerID
    ## based model -> no change in preferences!
    klDivWorkers12[workerIndex,2] <- 0 # -2 * length(idICases) * log(1/3)
    for(i in c(1:length(idICases))) {
      for(j in c(1:6)) {
        klDivWorkers12[workerIndex, 2] <- klDivWorkers12[workerIndex, 2] + 
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
# 1 parameter RSA model optimizations... 

allIndices <- c(1:length(workerIDs))
## generating data matrix for the purpose of optimization
dataWorker <- matrix(0, length(allIndices), 12)
dataWorker[,1] <- targetOC27[allIndices]
dataWorker[,2] <- obj2OC27[allIndices]
dataWorker[,3] <- obj3OC27[allIndices]
dataWorker[,4] <- uttFeat[allIndices]
dataWorker[,5] <- q1Feat[allIndices]
dataWorker[,6] <- q2Feat[allIndices]
dataWorker[,7:12] <- subjectResponses[allIndices,1:6]

# before optimization:         klDivWorkers12[1,3] <- RSAModelLL1(c(.2), dataWorker)
optRes1 <- optimize(RSAModelLL1_1simpleRSA, c(0, 1e+10), dataWorker)
optRes1notObey.1 <- optimize(RSAModelLL1_1simpleRSA_notObey.1, c(0, 1e+10), dataWorker)
optRes2 <- optim(c(.2, .2), RSAModelLL2_simpleRSA, method="L-BFGS-B", gr=NULL, dataWorker,
                   lower = c(0, 0), upper = c(1e+10, 1e+10))
#optRes3 <- optimize(RSAModelLL1_3, c(.01,100), dataWorker)   
#print(optRes)
## 1 param RSA model
klDivWorkers12[1,3] <- optRes1$objective
klDivWorkers12[1,4] <- optRes1notObey.1$objective
klDivWorkers12[1,5] <- optRes2$value
#klDivWorkers12[1,6] <- optRes3$objective
## resulting parameter choice
paramsWorkers12[1,2] <- optRes1$minimum
paramsWorkers12[1,3] <- optRes1notObey.1$minimum
paramsWorkers12[1,4] <- optRes2$par[1]
paramsWorkers12[1,5] <- optRes2$par[2]
#paramsWorkers12[1,5] <- optRes3$minimum
####
print(klDivWorkers12[1,])
print(paramsWorkers12[1,])
####

## writing out table
write.csv(klDivWorkers12, "X1_data/x1KLDivs_sRSA_globalOpt_2019_1009.csv")
write.csv(paramsWorkers12, "X1_data/x1Params_sRSA_globalOpt_2019_1009.csv")

