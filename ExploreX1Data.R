source("getStartedLocally.R")
source("X1_code/01_sRSA_X1_DataPreProcessing.R")

workerIDs <- x1pilotData$workerid

##
workerID <- 23 

idICases <- which(workerIDs == workerID)

dataWorker <- matrix(0, length(idICases), 12)
dataWorker[,1] <- targetOC27[idICases]
dataWorker[,2] <- obj2OC27[idICases]
dataWorker[,3] <- obj3OC27[idICases]
dataWorker[,4] <- uttFeat[idICases]
dataWorker[,5] <- q1Feat[idICases]
dataWorker[,6] <- q2Feat[idICases]
dataWorker[,7:12] <- subjectResponses[idICases,1:6]

### Looking at Blocks of Five....
for(trial in c(1:15)) {
  print(paste0("Trial: ", trial, " Utterance: ",allUtterances[allObjectsToUtterancesMappings[dataWorker[trial,1],dataWorker[trial,4]] ] ) )
  print(allObjects[dataWorker[trial,c(1:3)],])
  print(allUtterances[c( ((dataWorker[trial,5]-1)*3+1):(dataWorker[trial,5]*3), ((dataWorker[trial,6]-1)*3+1):(dataWorker[trial,6]*3))])
  print(round(dataWorker[trial,c(7:12)], 6))
  print("")
}
