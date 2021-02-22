# loading the raw pilot data (as Greg sent it on 2018/12/21)
x1pilotData <- read.csv("X1_data/x1-pilot-training.csv")

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

## now determining the recorded subject responses 
subjectResponses <- matrix(0,length(x1pilotData$X),6)
# postListMat <- matrix(0,length(pilotData$X),9)
# logLik <- rep(0,length(pilotData$X))
for(i in c(1:length(x1pilotData$X))) {
  # objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  # featChoice <- uttFeat[i]
  # postListMat[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, .1, 0)
  # print(c(objectConstellation, featChoice))
  #
  subjectResponses[i,1] <- x1pilotData$response1[i] + 1e-100
  subjectResponses[i,2] <- x1pilotData$response2[i] + 1e-100
  subjectResponses[i,3] <- x1pilotData$response3[i] + 1e-100
  subjectResponses[i,4] <- x1pilotData$response4[i] + 1e-100
  subjectResponses[i,5] <- x1pilotData$response5[i] + 1e-100
  subjectResponses[i,6] <- x1pilotData$response6[i] + 1e-100
  subjectResponses[i,1:3] <- subjectResponses[i,1:3] / sum(subjectResponses[i,1:3])
  subjectResponses[i,4:6] <- subjectResponses[i,4:6] / sum(subjectResponses[i,4:6])
}

## ordering the recorded subject responses such that they can be compared directly 
#   to the model predictions 
##             (particularly for visual comparison in the table) 
subjectResponsesOrdered <- matrix(NA ,length(x1pilotData$X),9)
for(i in c(1:length(x1pilotData$X))) {
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q1Feat[i]-1)*3)] <- subjectResponses[i,j]
  }
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q2Feat[i]-1)*3)] <- subjectResponses[i,3+j]
  }
}
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)
