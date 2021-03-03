source("getStartedLocally.R")
source("X2_code/01_sRSA_X2_DataPreProcessing.R")

workerIDs <- x2pilotData$workerid

## Some workers preferring non-ambiguous statements.
#workerID <- 11 # choose most non-ambiguous utterances - with some small glitches.
#workerID <- 35 # chooses most non-ambiguous utterances with full certainty - always makes one choice.
#workerID <- 36 # same as 35 but focusses on "pattern" from the second trial onwards and on shape in trials 11-13 LOL

## Information-gain-oriented workers.
workerID <- 23 # 46 (extreme values, consistently choosing most ambiguous terms regardless of context)
#workerID <- 53 # , 53 (somewhat looses concentration in the second half), 
#workerID <- 57 # (one of the best workers)

idICases <- which(workerIDs == workerID)

dataWorker <- matrix(0, length(idICases), 13)
dataWorker[,1] <- obj1OC27[idICases]
dataWorker[,2] <- obj2OC27[idICases]
dataWorker[,3] <- obj3OC27[idICases]
dataWorker[,4] <- x2pilotData$numFeatures[idICases]
dataWorker[,5:13] <- bInfGainUttTurkers[idICases,]

### Looking at Blocks of Five....
dataWorker[,c(5:13)]
allUtterances
allObjects[dataWorker[1,c(1:3)],]
allObjects[dataWorker[2,c(1:3)],]
allObjects[dataWorker[3,c(1:3)],]
allObjects[dataWorker[4,c(1:3)],]
allObjects[dataWorker[5,c(1:3)],]
allUtterances
### Looking at Blocks of Five....
dataWorker[,c(5:13)]
allUtterances
allObjects[dataWorker[6,c(1:3)],]
allObjects[dataWorker[7,c(1:3)],]
allObjects[dataWorker[8,c(1:3)],]
allObjects[dataWorker[9,c(1:3)],]
allObjects[dataWorker[10,c(1:3)],]
allUtterances
### Looking at Blocks of Five....
dataWorker[,c(5:13)]
allUtterances
allObjects[dataWorker[11,c(1:3)],]
allObjects[dataWorker[12,c(1:3)],]
allObjects[dataWorker[13,c(1:3)],]
allObjects[dataWorker[14,c(1:3)],]
allObjects[dataWorker[15,c(1:3)],]
allUtterances
dataWorker[,c(5:13)]
