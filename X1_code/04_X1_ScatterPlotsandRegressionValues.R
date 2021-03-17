# simple RSA with individual optimization
# x1pilotData <- read.csv("X1_data/x1pDataAugm_sRSA_indOpt_fixed00_and_fixed.20.csv")
# x1pilotData <- read.csv("X1_data/x1pDataAugm_sRSA_indOpt_PrefStrengthOpt_obed0_and_obed.2.csv")
# x1pilotData <- read.csv("X1_data/x1pDataAugm_sRSA_indOpt_PrefandObedOpt_and_fixed.2.2.csv")
############# For barplots ####################
x1pilotData <- read.csv("X1_data/x1pDataAugm_sRSA_indOpt_fixed00_and_PrefandObedOpt.csv")

# simple RSA global optimization
#x1pilotData <- read.csv("X1_data/x1pDataAugm_sRSA_globaOpt_fixed.1.1_and_OptPrefobedFixed.1.csv")
#x1pilotData <- read.csv("X1_data/x1pDataAugm_sRSA_globalOpt_OptPrefObedFixed_and_Opt12.csv")

# simple RSA with individual crossvalidation (leave-one-out) 
#x1pilotData <- read.csv("X1_data/x1pDataAugm_sRSA_crossVal_Opt1_and_Opt2.csv")
#x1pilotData <- read.csv("X1_data/x1pDataAugm_sRSA_crossVal_Opt1obed.1_and_fixed.1.1.csv")

######################################## Full RSA ###############################################
# x1pilotData <- read.csv("X1_Data/x1pDataAugm_fRSA_indOpt_fixed001_and_OptPrefandAlphaObed0.csv")
# x1pilotData <- read.csv("X1_Data/x1pDataAugm_fRSA_indOpt_OptPrefAndObedAlpha1_and_OptAll3.csv")

# full RSA global optimization

# x1pilotData <- read.csv("X1_Data/x1pDataAugm_fRSAglobalOpt_Opt1_and__Opt1obed.1.csv")
# x1pilotData <- read.csv("X1_Data/x1pDataAugm_fRSAglobalOpt_Opt13_and__Opt123.csv")


# adding feature property codes (which feature was uttereed, which features were questioned)
uttFeat <- ifelse(x1pilotData$utterance=="green" | x1pilotData$utterance=="red" | x1pilotData$utterance=="blue", 3,
                  ifelse(x1pilotData$utterance=="solid" | x1pilotData$utterance=="striped" | x1pilotData$utterance=="polka-dotted", 2, 1))
x1pilotData$uttFeat <- uttFeat

q1Feat <- ifelse(x1pilotData$pref1=="green things" | x1pilotData$pref1=="red things" | x1pilotData$pref1=="blue things", 3,
                 ifelse(x1pilotData$pref1=="solid things" | x1pilotData$pref1=="striped things" | x1pilotData$pref1=="polka-dotted things", 2, 
                        ifelse(x1pilotData$pref1=="clouds" | x1pilotData$pref1=="circles" | x1pilotData$pref1=="squares", 1,
                               -1 ) ))
x1pilotData$q1Feat <- q1Feat

q2Feat <- ifelse(x1pilotData$pref4=="green things" | x1pilotData$pref4=="red things" | x1pilotData$pref4=="blue things", 3,
                 ifelse(x1pilotData$pref4=="solid things" | x1pilotData$pref4=="striped things" | x1pilotData$pref4=="polka-dotted things", 2, 
                        ifelse(x1pilotData$pref4=="clouds" | x1pilotData$pref4=="circles" | x1pilotData$pref4=="squares", 1,
                               -1 ) ))
x1pilotData$q2Feat <- q2Feat

## adding the 1-27 target and object2 & object3 code.
temp <- x1pilotData$target
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
targetOC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x1pilotData$targetOC27 <- targetOC27

temp <- x1pilotData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x1pilotData$obj2OC27 <- obj2OC27

temp <- x1pilotData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x1pilotData$obj3OC27 <- obj3OC27


#### 
## filtering for only the present feature values for each feature.
###########
subjectGuessIndexM1 <- grep("^DataPost_1", colnames(x1pilotData)) - 1
modelGuessIndexM1 <- grep("^Post1_1", colnames(x1pilotData)) - 1
modelGuessIndexM2 <- grep("^Post2_1", colnames(x1pilotData)) - 1
for(i in c(1:nrow(x1pilotData))) {
  currentObjects <- c(targetOC27[i], obj2OC27[i], obj3OC27[i])
  validUtterances <- determineValidUtterances(currentObjects)

  for(j in c(1:3)) { # iterating over the three feature types
    relevantIndices <- which(validUtterances>(3*(j-1)) & validUtterances<(3*j + 1)) # relevant indices for a particular feature type
    valUttRel <- validUtterances[relevantIndices]
    if(length(relevantIndices)>1){
      sumSG <- 0
      sumMG <- 0
      sumMG2 <- 0
      for(x in c(1:length(valUttRel))) {
        sumSG <- sumSG + x1pilotData[[valUttRel[x]+subjectGuessIndexM1]][i]
        sumMG <- sumMG + x1pilotData[[valUttRel[x]+modelGuessIndexM1]][i]
        sumMG2 <- sumMG2 + x1pilotData[[valUttRel[x]+modelGuessIndexM2]][i]
      }
      if(!is.na(sumSG)) {
        for(x in c(1:length(valUttRel))) {
          x1pilotData[[valUttRel[x]+subjectGuessIndexM1]][i] <- x1pilotData[[valUttRel[x]+subjectGuessIndexM1]][i] /
            (sumSG + 1e-100)
        }
      }
      for(x in c(1:length(valUttRel))) {
        x1pilotData[[valUttRel[x]+modelGuessIndexM1]][i] <- x1pilotData[[valUttRel[x]+modelGuessIndexM1]][i] /
          (sumMG + 1e-100)
        x1pilotData[[valUttRel[x]+modelGuessIndexM2]][i] <- x1pilotData[[valUttRel[x]+modelGuessIndexM2]][i] /
          (sumMG2 + 1e-100)
      }
    }else{# set single feature value case to NA
      x1pilotData[[subjectGuessIndexM1 + valUttRel[1]]][i] <- NA
      x1pilotData[[modelGuessIndexM1 + valUttRel[1]]][i] <- NA
      x1pilotData[[modelGuessIndexM2 + valUttRel[1]]][i] <- NA
    }
    
    # setting the non-represented values to NA
    for(v in c(1:3)) {
      if(length(which(valUttRel == ((j-1)*3) + v )) == 0) {
        x1pilotData[[subjectGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x1pilotData[[modelGuessIndexM1 + ((j-1)*3) + v]][i] <- NA
        x1pilotData[[modelGuessIndexM2 + ((j-1)*3) + v]][i] <- NA
      }
    }
  }
}


## now determining the constellation code. 
constellationCode <- matrix(0,length(x1pilotData$X),6)
uniqueCCode <- rep(0, length(x1pilotData$X))
featureOrder <- matrix(0, length(x1pilotData$X), 3)
objectOrder <- matrix(0, length(x1pilotData$X), 3)
for(i in c(1:length(x1pilotData$X))) {
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  featChoice <- uttFeat[i]
  cc <- getConstellationCode(objectConstellation, featChoice)
  constellationCode[i,] <- cc[[1]]
  featureOrder[i,] <- cc[[2]]
  objectOrder[i,] <- cc[[3]]
  ## one-column code
  uc <- 0
  for(j in c(1:6)) {
    uc <- (uc * 10) + constellationCode[i,j]
  }
  uniqueCCode[i] <- uc
}
# feature order specified reordering of standard order (i.e. shape=1, texture=2, color=3)
# object order specifies reordering of presented object order
subjectGuessIndex <- grep("^DataPost_1", colnames(x1pilotData))
modelGuessIndex1 <- grep("^Post1_1", colnames(x1pilotData))
modelGuessIndex4 <- grep("^Post2_1", colnames(x1pilotData))

for(i in c(1:length(x1pilotData$X))) {
  # reordering the feature order
  x1pilotData[i,] <- replace(x1pilotData[i,], c(subjectGuessIndex:(subjectGuessIndex+8)),  
                             x1pilotData[i, c( (subjectGuessIndex + (featureOrder[i,1]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,1]-1)*3),
                                               (subjectGuessIndex+ (featureOrder[i,2]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,2]-1)*3),
                                               (subjectGuessIndex+ (featureOrder[i,3]-1)*3) : (subjectGuessIndex+2+(featureOrder[i,3]-1)*3) )])
  x1pilotData[i,] <- replace(x1pilotData[i,], c(modelGuessIndex1:(modelGuessIndex1+8)),  
                             x1pilotData[i, c( (modelGuessIndex1 + (featureOrder[i,1]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,1]-1)*3),
                                               (modelGuessIndex1+ (featureOrder[i,2]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,2]-1)*3),
                                               (modelGuessIndex1+ (featureOrder[i,3]-1)*3) : (modelGuessIndex1+2+(featureOrder[i,3]-1)*3) )])
  x1pilotData[i,] <- replace(x1pilotData[i,], c(modelGuessIndex4:(modelGuessIndex4+8)),  
                             x1pilotData[i, c( (modelGuessIndex4 + (featureOrder[i,1]-1)*3) : (modelGuessIndex4+2+(featureOrder[i,1]-1)*3),
                                               (modelGuessIndex4+ (featureOrder[i,2]-1)*3) : (modelGuessIndex4+2+(featureOrder[i,2]-1)*3),
                                               (modelGuessIndex4+ (featureOrder[i,3]-1)*3) : (modelGuessIndex4+2+(featureOrder[i,3]-1)*3) )])
  ## now rearranging the individual feature values dependent on the object order (first object is the chosen one!)
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  objectCReordered <- replace(objectConstellation, c(1:3), objectConstellation[objectOrder[i,]])
  
  for(j in c(1:3)) {
    featValOrder <- rep(0,3)
    targetFeatureValue <- allObjectsToUtterancesMappings[objectCReordered[1],featureOrder[i,j]]
    featValOrder[1] <-  targetFeatureValue # target feature value comes first
    featValIndex <- 2
    for(k in c(2:3)) {
      objectFeatureValue <- allObjectsToUtterancesMappings[objectCReordered[k],featureOrder[i,j]]
      if(length(which(featValOrder==objectFeatureValue))==0) { # feature not included yet
        featValOrder[featValIndex] <- objectFeatureValue
        featValIndex <- featValIndex + 1
      }
    }
    if(featValIndex < 4) { # not all feature values assigned, yet -> fill up
      for(featVal in c(((featureOrder[i,j]-1)*3+1):((featureOrder[i,j]-1)*3+3))) {
        if(length(which(featValOrder==featVal))==0) { # feature not included yet
          featValOrder[featValIndex] <- featVal
          featValIndex <- featValIndex + 1
        }
      }
    }
    featValOrder <- featValOrder - (featureOrder[i,j]-1)*3 - 1
    ### now featValOrder specifies the feature value reordering for order feature with index j
    # reordering the feature value order of ordered feature j 
    x1pilotData[i,] <- replace(x1pilotData[i,], c(((j-1)*3 + subjectGuessIndex):(2+((j-1)*3 + subjectGuessIndex))),  
                               x1pilotData[i, subjectGuessIndex + ((j-1)*3 + featValOrder)]) 
    x1pilotData[i,] <- replace(x1pilotData[i,], c(((j-1)*3 + modelGuessIndex1):(2+((j-1)*3 + modelGuessIndex1))),  
                               x1pilotData[i, modelGuessIndex1 + ((j-1)*3 + featValOrder)]) 
    x1pilotData[i,] <- replace(x1pilotData[i,], c(((j-1)*3 + modelGuessIndex4):(2+((j-1)*3 + modelGuessIndex4))),  
                               x1pilotData[i, modelGuessIndex4 + ((j-1)*3 + featValOrder)]) 
  }
}
x1pilotData$CCode <- uniqueCCode

# Saves sorted data for barplots

#write.csv(x1pilotData, "X1_data/x1pilotDataModelOptimizedSorted.csv")

x1pilotData <- x1pilotData[order(x1pilotData$CCode),]
myCCodes <- unique(x1pilotData$CCode)
avDataMatrix <- matrix(0,length(myCCodes),19)
dataPointIndex <- 0
workerData <- 0
rsaModel <- 0
rsaModel2 <- 0
for(i in c(1:length(myCCodes))) {
  cc <- myCCodes[i]
  cases <- which(x1pilotData$CCode == cc)
  allPilotDataCases <- x1pilotData[cases,]
  for(j in c(1:9)) {
    specCases <- which(is.na(allPilotDataCases[,subjectGuessIndex-1+j]) == FALSE)
    if(length(specCases) > 0) {
      #      if(mean(allPilotDataCases[specCases,(modelGuessIndex4-1+j)]) < 1/3 - 1e-5
      #         | mean(allPilotDataCases[specCases,(modelGuessIndex4-1+j)]) > 1/3 + 1e-5) {
      dataPointIndex <- dataPointIndex + 1
      workerData[dataPointIndex] <- mean(allPilotDataCases[specCases,(subjectGuessIndex-1+j)])
      rsaModel[dataPointIndex] <- mean(allPilotDataCases[specCases,(modelGuessIndex1-1+j)])
      rsaModel2[dataPointIndex] <- mean(allPilotDataCases[specCases,(modelGuessIndex4-1+j)])
      #      }
    }
  }
}
### plot after Optimization ###


plot(rsaModel, workerData)
abline(lm(formula = rsaModel~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel~workerData)
summary(model)
confint(model)

### plot with default parameters ###

plot(rsaModel2, workerData)
abline(lm(formula = rsaModel2~workerData), col="red") # regression line (y~x)
lines(lowess(rsaModel2,workerData), col="blue") # lowess line (x,y)

model <- lm(formula = rsaModel2~workerData)
summary(model)
confint(model)

