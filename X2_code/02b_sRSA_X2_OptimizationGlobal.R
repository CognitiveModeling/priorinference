## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x2pilotData$workerid
idMax <- max(workerIDs)
klDivUttWorkers <- matrix(0, 1, 8)
paramsUttWorkers <- matrix(0, 1, 11)

#####################################################################################
## Starting with simple base model determination - global KLDivUttWorkers value    ##
## Note that the KL values here do NOT filter out those feature values that are NOT in the objects.
#####################################################################################
for(i in c(1:length(workerIDs))) {
  len <- x2pilotData$numFeatures[i]
  for(j in c(1:len) ) {
    klDivUttWorkers[1, 2] <- klDivUttWorkers[1, 2] + 
      sliderSetValues[i,j] * 
      (log(sliderSetValues[i,j] + 1e-100) - log(1/len))
    if(is.na(klDivUttWorkers[1, 2])) {
      print("Is NA!???")
      print(c(sliderSetValues[i,j], log(1/len), i, j, len))
      j <- 10
      i <- length(idICases) +1 
    }
  }
}


#######################
## Optimizing (i.e. minimzing) the KL Divergence values for each worker...
## starting with 1 parameter RSA model optimizations... 
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X(max 15):TurkerSliderValues]

## generating data matrix for the purpose of optimization
allIndices <- c(1:length(workerIDs))
dataWorker <- matrix(0, length(workerIDs), 13)
dataWorker[,1] <- obj1OC27[allIndices]
dataWorker[,2] <- obj2OC27[allIndices]
dataWorker[,3] <- obj3OC27[allIndices]
dataWorker[,4] <- x2pilotData$numFeatures[allIndices]
dataWorker[,5:13] <- bInfGainUttTurkers[allIndices,]
# print(dataWorker)
# now optimize for one parameter... 
optRes1 <- optimize(SimpleRSAModelUttKLDivParamA, c(0,1e+10), dataWorker)
optRes2 <- optimize(SimpleRSAModelUttKLDivParamB, c(0,1e+10), dataWorker)   
optRes3 <- optimize(SimpleRSAModelUttKLDivParamK, c(-10,10), dataWorker)   

## 1 param RSA Utt model
klDivUttWorkers[1,3] <- optRes1$objective
klDivUttWorkers[1,4] <- optRes2$objective
klDivUttWorkers[1,5] <- optRes3$objective
## resulting parameter choice
paramsUttWorkers[1,2] <- optRes1$minimum
paramsUttWorkers[1,3] <- optRes2$minimum
paramsUttWorkers[1,4] <- optRes3$minimum
####
optRes2n1 <- optim(c(.2, .2), SimpleRSAModelUttKLDivParamBK, method="L-BFGS-B", gr=NULL, dataWorker,
                   lower = c(0,-10), upper = c(1e+10,10))
optRes2n2 <- optim(c(.2, .2), SimpleRSAModelUttKLDivParamAK, method="L-BFGS-B", gr=NULL, dataWorker,
                   lower = c(0,-10), upper = c(1e+10,10))
optRes3 <- optim(c(.2, .2, 1), SimpleRSAModelUttKLDivParamABK, method="L-BFGS-B", gr=NULL, dataWorker,
                 lower = c(0,0,-10), upper = c(1e+10,1e+10,10))
## 2 and 3 param RSA model2
## max likelihood parameter choice
klDivUttWorkers[1,6] <- optRes2n1$value
klDivUttWorkers[1,7] <- optRes2n2$value
klDivUttWorkers[1,8] <- optRes3$value
## max likelihood parameter choice
paramsUttWorkers[1,5] <- optRes2n1$par[1]
paramsUttWorkers[1,6] <- optRes2n1$par[2]
paramsUttWorkers[1,7] <- optRes2n2$par[1]
paramsUttWorkers[1,8] <- optRes2n2$par[2]
paramsUttWorkers[1,9] <- optRes3$par[1]
paramsUttWorkers[1,10] <- optRes3$par[2]
paramsUttWorkers[1,11] <- optRes3$par[3]
##    
print(c("Done with global optimization"))
print(c(klDivUttWorkers[1,], paramsUttWorkers[1,]))
####
write.csv(klDivUttWorkers, "X2_data/x2KLDivs_sRSA_globalOpt_2019_12_20.csv")
write.csv(paramsUttWorkers, "X2_data/x2Params_sRSA_globalOpt_2019_12_20.csv")


