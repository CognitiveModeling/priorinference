# loading the raw pilot data (as Greg sent it on 2018/12/21)
x2pilotData <- read.csv("X2_data/x2-pilot-utterance-choice.csv")

## adding the 1-27 target and object2 & object3 code.
temp <- x2pilotData$obj1
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj1OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x2pilotData$obj1OC27 <- obj1OC27

temp <- x2pilotData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x2pilotData$obj2OC27 <- obj2OC27

temp <- x2pilotData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x2pilotData$obj3OC27 <- obj3OC27

## now identify the first column number of the turker sliders and response pairs
sliderIndex <- grep("^pref1", colnames(x2pilotData))
## and use that index to determine all slider identities and corresponding slider values.
sliderUtteranceTypes <- matrix(NA, nrow(x2pilotData), 9)
sliderSetValues <- matrix(NA,  nrow(x2pilotData), 9)
for(i in c(1:9)) {
  colIndex <- sliderIndex + (i-1) * 2
  relRows <- which(!is.na(x2pilotData[[colIndex]]))
  for(j in c(1:length(relRows) ) ) { 
    sliderUtteranceTypes[relRows[j], i] <- which(allUtterancesNew==x2pilotData[[colIndex]][relRows[j]])
    sliderSetValues[relRows[j], i] <- x2pilotData[[colIndex+1]][relRows[j]]
  }
}
### normalizing the turker estimates and setting them into the corresponding matrix.
bInfGainUttTurkers <- matrix(NA, nrow(x2pilotData), 9)
for(i in c(1:nrow(x2pilotData)) ) {
  s <- sum(sliderSetValues[i,c(1:x2pilotData$numFeatures[i])])
  if(s > 0) {
    sliderSetValues[i,c(1:x2pilotData$numFeatures[i])] <- sliderSetValues[i,c(1:x2pilotData$numFeatures[i])] / s
  }else{
    sliderSetValues[i,c(1:x2pilotData$numFeatures[i])] <- 1 / (x2pilotData$numFeatures[i])
  }
  bInfGainUttTurkers[i, sliderUtteranceTypes[i,c(1:(x2pilotData$numFeatures[i]) )] ] <- sliderSetValues[i,c(1:(x2pilotData$numFeatures[i]) )]
  for(j in c(1:x2pilotData$numFeatures[i])) {
    if(is.na(sliderSetValues[i,j])) {
      print("ERRor")
    }
  }
}