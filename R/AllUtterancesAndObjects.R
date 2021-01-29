
## All Utterances
# All possible utterances (i.e. object features) that can be handled.
# Here, we assume a 3x3 matrix (three feature types with three expressions each)

#' All utterances
#'
#' @description
#' A vector that contains all feature values:
#'
#' ('cloud', 'circle', 'square', 'solid', 'striped', 'dotted', 'blue', 'red', 'green')
#'
#' @details
#' The difference between this matrix and the \code{\link{allUtterancesNew}} is that
#' the 'dotted' pattern is called \emph{polka-dotted}.
#'
#' @export
  allUtterances <- c('cloud', 'circle', 'square', 'solid', 'striped', 'dotted', 'blue', 'red', 'green')


#' All utterances (new)
#'
#' @description
#' A vector that contains all feature values:
#'
#' ('cloud', 'circle', 'square', 'solid', 'striped', 'polka-dotted', 'blue', 'red', 'green')
#'
#' @details
#' The difference between this vector and the \code{\link{allUtterances}} is that
#' the 'polka-dotted' pattern is called \emph{dotted}.
#' @export
  allUtterancesNew <- c('cloud', 'circle', 'square', 'solid', 'striped', 'polka-dotted', 'blue', 'red', 'green')

#' All feature types
#'
#' @description
#' A vector of all object features: ('shape','pattern','color')
#'
#' @export
  allFeatureTypesNew <- c('shape','pattern','color')



#' All objects
#'
#' @description
#'
#' The matrix contains \code{3^3} types of objects.
#'
#' It essentially specifies the 3 feature expressions for each object
#' thus, the matrix maps objects to matching utterances.
#' The matrix contains strings of all possible feature values of the objects.
#'
#' The index mapping of the strings are contained in the matrix
#' \code{\link{allObjectsToUtterancesMappings}}.
#'
#' @examples
#' \dontrun{
#' output:
#'      shape   pattern  color
#' [1,] cloud    solid   blue
#' [2,] circle   solid   blue
#' [3,] square   solid   blue
#' [4,] cloud   striped  blue
#' [5,] circle  striped  blue
#' [6,] square  striped  blue
#' [7,] cloud   dotted   blue
#' [8,] circle  dotted   blue
#' [9,] square  dotted   blue
#'[10,] cloud    solid    red
#'[11,] circle   solid    red
#'[12,] square   solid    red
#'[13,] cloud   striped   red
#'[14,] circle  striped   red
#'[15,] square  striped   red
#'[16,] cloud   dotted    red
#'[17,] circle  dotted    red
#'[18,] square  dotted    red
#'[19,] cloud    solid    green
#'[20,] circle   solid    green
#'[21,] square   solid    green
#'[22,] cloud   striped   green
#'[23,] circle  striped   green
#'[24,] square  striped   green
#'[25,] cloud   dotted    green
#'[26,] circle  dotted    green
#'[27,] square  dotted    green
#'}
#' @export
  allObjects <- matrix('',27,3)
  allUttMatrix <- matrix(allUtterances, ncol=3, byrow=TRUE)
  #allObjectsToUtterancesMappings <- matrix(0,27,3)
  for(index in c(1:27)) {
    allObjects[index,1] <- allUttMatrix[1,1+((index-1)%%3)]
    allObjects[index,2] <- allUttMatrix[2,1+floor(((index-1)%%9)/3)]
    allObjects[index,3] <- allUttMatrix[3,1+floor((index-1)/9)]
    #allObjectsToUtterancesMappings[index,1] <- 1+((index-1)%%3)
    #allObjectsToUtterancesMappings[index,2] <- 4+floor(((index-1)%%9)/3)
    #allObjectsToUtterancesMappings[index,3] <- 7+floor((index-1)/9)
  }
  colnames(allObjects) <- c("shape", "pattern", "color")


#' All objects to utterances mappings
#'
#' @description
#' The matrix contains \code{3^3} types of objects.
#'
#' It essentially specifies the 3 feature expressions for each object
#' thus, the matrix maps objects to matching utterances.
#' The matrix contains the index mapping of all possible feature values:
#'
#' cloud = 1, circle = 2, square = 3
#'
#' solid = 4, striped = 5, dotted = 6
#'
#' blue = 7, red = 8, green = 9
#'
#' The strings of the index mapping are contained in the matrix
#' \code{\link{allObjects}}.
#
#'
#' @examples
#' \dontrun{
#' output:
#'
#'       [,1] [,2] [,3]
#' [1,]    1    4    7
#' [2,]    2    4    7
#' [3,]    3    4    7
#' [4,]    1    5    7
#' [5,]    2    5    7
#' [6,]    3    5    7
#' [7,]    1    6    7
#' [8,]    2    6    7
#' [9,]    3    6    7
#'[10,]    1    4    8
#'[11,]    2    4    8
#'[12,]    3    4    8
#'[13,]    1    5    8
#'[14,]    2    5    8
#'[15,]    3    5    8
#'[16,]    1    6    8
#'[17,]    2    6    8
#'[18,]    3    6    8
#'[19,]    1    4    9
#'[20,]    2    4    9
#'[21,]    3    4    9
#'[22,]    1    5    9
#'[23,]    2    5    9
#'[24,]    3    5    9
#'[25,]    1    6    9
#'[26,]    2    6    9
#'[27,]    3    6    9
#'}
#'
#' @export
allObjectsToUtterancesMappings <- matrix(0,27,3)
  for(index in c(1:27)) {
    allObjectsToUtterancesMappings[index,1] <- 1+((index-1)%%3)
    allObjectsToUtterancesMappings[index,2] <- 4+floor(((index-1)%%9)/3)
    allObjectsToUtterancesMappings[index,3] <- 7+floor((index-1)/9)
  }


#' Determine valid utterances
#'
#' @description
#' The relevant utterances are determined based on the features of the current objects present in the scene.
#' @param currentObjects A vector of three values in range \code{{1,...,27}} specifying the target and the other two objects.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#' @return A vector of utterances that correspond to all feature values present in the current objects in the scene.
#'
#' @examples
#' \donttest{currentObjects <- c(1,2,3)}
#'
#' \donttest{determineValidUtterances(currentObjects)}
#'
#' output: [1] 1 2 3 4 7
#'
#' To see which objects are present in the scene run:
#' \donttest{allObjects[currentObjects,]}
#'
#' output:
#'
#'       shape    pattern color
#' [1,] "cloud"  "solid" "blue"
#' [2,] "circle" "solid" "blue"
#' [3,] "square" "solid" "blue"
#'
#' Based on this output you can see that the indices in the validUtterances vector refer to:
#' cloud = 1, circle = 2, square = 3, solid = 4 and blue = 7.
#'
#' @export
determineValidUtterances <- function(currentObjects) {
  validUtterances <- c()
  for(i in c(1:length(currentObjects))) {
    validUtterances <- c(validUtterances, allObjectsToUtterancesMappings[currentObjects[i],])
  }
  validUtterances <- sort(unique(validUtterances))
  return(validUtterances)
}

###
#Hania: not included in package as deprecated (16.10.20)
# this function is only used in X9

## No preference is encoded with 4, whereas a specific feature expression preference is encoded
# by the respective index value
# @description
# Get feature respective priors
#
# returns general feature respective priors for all 3 features
# deprecated (not used currently!)

getFeatureRespectivePriors <- function(softAddProb) {
  featureRespectivePriors <- list()
  for(i in c(1:3)) { ## for all three features generate a preference matrix
    m <- matrix(0,4,3)
    for(fPref in c(1:3)) {
      m[fPref,fPref] <- 1
      m[fPref,] <- m[fPref,] + softAddProb
      m[fPref,] <- m[fPref,] / sum(m[fPref,])
    }
    m[4,] <- 1/3
    featureRespectivePriors[[i]] <- m
  }
  return(featureRespectivePriors)
}

##

# this function is only used in X9
#' Map objects to utterances
#'
#' @description
#' Determining the specific mapping of objects to utterances that applies given the present objects in the scene.
#' @param currentObjects A vector of three values in range \code{{1,...,27}} specifying the target and the other two objects.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#' @return A 3x3 matrix. Values in the matrix are in range \code{{1-9}}.
#'
#' \strong{rows:} Objects; \strong{columns:} Feature values of the objects
#' @examples
#' \donttest{currentObjects <- c(1,2,3)}
#' \donttest{mapObjectToUtterances(currentObjects)}
#'
#' output:
#'       [,1] [,2] [,3]
#' [1,]    1    4    7
#' [2,]    2    4    7
#' [3,]    3    4    7
#'
#' To see which objects are present in the scene run:
#' \donttest{allObjects[currentObjects,]}
#'
#' output:
#'
#'       shape   pattern  color
#' [1,] "cloud"  "solid" "blue"
#' [2,] "circle" "solid" "blue"
#' [3,] "square" "solid" "blue"
#'
#' Based on this output you can see that the indices output matrix vector refer
#' to the following feature values:
#' cloud = 1, circle = 2, square = 3, solid = 4 and blue = 7.
#'
#' @details
#' This function is only used in X9
#' @export
# mapping current objects to utterances
mapObjectToUtterances <- function(currentObjects) {
  mapObjToUtt <- matrix(0, length(currentObjects), 3)
  for(i in c(1:length(currentObjects))) {
    mapObjToUtt[i,] <- allObjectsToUtterancesMappings[currentObjects[i],]
  }
  return(mapObjToUtt)
}

##
# this function is only used in X9
#' Determine utterances to object probabilities
#'
#' @description
#' This function determines maps the valid utterances to the corresponding objects in the scene.
#'
#' It then determines the probability of choosing an object based on the valid utterances in the scene.
#'
#' @param validUtterances A vector of utterances that correspond to all feature values present
#' in the current objects in the scene.
#'
#' For example, it only makes sense to utter \emph{"red"} in a scene if there are \emph{red} objects present.
#'
#' @param currentObjects A vector of three values in range \code{{1,...,27}} specifying
#' the target and the other two objects in the scene.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#'
#' @param mapObjToUtt A 3x3 matrix. Values in the matrix are in range \code{{1-9}}.
#'
#' \strong{rows:} The current objects in the scene
#'
#' \strong{columns:} Features of the objects
#'
#' @param notObeyInst Determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.
#' @return A matrix. The rows map each possible utterance that corresponds to each present feature value
#' of the current objects. The columns represent the three objects in the scene.
#'
#' This reflects the obedience-parameter and which objects match the respective utterance.
#' The matrix shows the probability that a certain object is chosen following a certain utterance, that is valid in the scene.
#' The number of rows of the matrix match the length of the validUtterances vector.
#' @examples
#' \donttest{determineUttToObjectProbs(validUtterances, currentObjects, mappedObjToUtt, notObeyInst)}
#'
#' output:
#'      [,1] [,2] [,3]
#' [1,] 1.00 0.00 0.00
#' [2,] 0.00 1.00 0.00
#' [3,] 0.00 0.00 1.00
#' [4,] 0.33 0.33 0.33
#' [5,] 0.33 0.33 0.33
#'
#' Example:
#' 
#' To see which objects are present in the scene run:
#' \donttest{allObjects[currentObjects,]}
#'
#' output:
#'
#'       shape   pattern  color
#' [1,] "cloud"  "solid" "blue"
#' [2,] "circle" "solid" "blue"
#' [3,] "square" "solid" "blue"
#'
#'
#' The columns correspond to the three objects in the scene:
#' A solid blue cloud, a solid blue circle and a solid blue square.
#' If the utterance is circle the probability of choosing circle is 1,
#' while the probability for the cloud and the square is 0.
#' If the utterance is blue the probability of choosing the cloud,
#' the circle or the square is 1/3, since all three objects are blue.
#' The same goes for the utterance solid.
#'
#' @details
#' This function is only used in X9
#' @export
determineUttToObjectProbs <- function(validUtterances, currentObjects,
                                                    mapObjToUtt, notObeyInst) {
  mapUttToObj <- list()
  # fill the matrix with the value of notObeyInst
  uttToObjProbs <- matrix(notObeyInst, length(validUtterances), length(currentObjects))
  for(utt in rep(1:length(validUtterances)) ) {
    #  determine array of all objects that match the utterance:
    #  take utterance index utt,
    #  return the indices of those rows of mapObjToUtt (=indices of objects) that have the feature denoted by utt
    mapUttToObj[[utt]] = ((which(mapObjToUtt[,] == validUtterances[utt])-1)%%nrow(mapObjToUtt))+1

    # Hania: -1 for the modulo to work; +1 to change the indices to what they were before we substracted 1.

    for(i in rep(1:length(mapUttToObj[[utt]]))) {
      uttToObjProbs[utt,mapUttToObj[[utt]][i]] <- uttToObjProbs[utt,mapUttToObj[[utt]][i]] + 1; #Hania: +1 adding probability
     }

    # normalize probabilities
    uttToObjProbs[utt,] <- uttToObjProbs[utt,] / sum(uttToObjProbs[utt,])# length(mapUttToObj[[utt]])
  }
  return(uttToObjProbs)
}

##

#' Get priors of object preferences of the listener
#'
#' @description
#' Determine the priors of object preferences of the listener.
#'
#' These are automatically derived from the valid utterances in the scene
#' (i.e. derived from all features of the current objects).
#' @param validUtterances A vector of utterances that correspond to all feature values present
#' in the current objects in the scene.
#'
#' For example, it only makes sense to utter \emph{"red"} in a scene if there are \emph{red} objects present.
#' @param currentObjects A vector of three values in range \code{{1,...,27}} specifying the target and the other two objectsin the scene.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#'
#' @param type Is set to \code{0:} Hard priors or \code{ >0:} Soft priors with specified softness
#'
#' @param uttToObjProbs A matrix. The rows map each possible utterance that corresponds to each present feature value
#' of the current objects. The columns represent the three objects in the scene.
#'
#' This reflects the obedience-parameter and which objects match the respective utterance.
#' The matrix shows the probability that a certain object is chosen following a certain utterance,
#' that is valid in the scene.
#' The number of rows of the matrix match the length of the validUtterances vector.
#'
#' @return A list of preference priors for all valid utterances based on the objects in the scene.
#'
#' The list has as many rows as the length of the validUtterances vector + 1.
#'
#' Each row in the list contains a vector of length 3, as there are three objects in the scene.
#'
#' The extra row is for the case of no preferences whatsoever, i.e. uniform prior over all three objects in the scene.
#' @examples
#' \donttest{getObjectPreferencePriors(validUtterances, currentObjects, type, uttToObjProbs)}
#'
#' output:
#'[[1]]
#'[1] 0.85 0.077 0.077
#'
#'[[2]]
#'[1] 0.077 0.85 0.077
#'
#'[[3]]
#'[1] 0.077 0.077 0.85
#'
#'[[4]]
#'[1] 0.33 0.33 0.33
#'
#'[[5]]
#'[1] 0.33 0.33 0.33
#'
#'[[6]]
#'[1] 0.33 0.33 0.33
#'
#'
#' Example:
#' 
#' To see which objects are present in the scene run:
#' \donttest{allObjects[currentObjects,]}
#'
#' output:
#'
#'       shape   pattern  color
#' [1,] "cloud"  "solid" "blue"
#' [2,] "circle" "solid" "blue"
#' [3,] "square" "solid" "blue"
#'
#' The validUtterances vector: [1] 1 2 3 4 7
#'
#' The rows of the list 1-6 refer to the valid utterances in the scene.
#' 
#' Example: 
#' 
#' If we take a look at row 4 of the list:
#' [[4]]
#' [1] 0.33 0.33 0.33
#' We can see that the preference prior of the listener is 1/3 for
#' each of the objects, if the utterance is "solid".
#' The last row (6) in the list contains a uniform prior over the present
#' objects in the scene.
#'
#' @details
#' This function is only used in X9
#' @export
getObjectPreferencePriors <- function(validUtterances, currentObjects, type, uttToObjProbs) {
  objectPreferenceHardPriors <- list()
  for(utt in rep(1:length(validUtterances)) ) {
    objectPreferenceHardPriors[[utt]] <- uttToObjProbs[utt,]
  }
  objectPreferenceHardPriors[[length(validUtterances)+1]] = # Adding an extra row with flat prior over objects
    rep(1/length(currentObjects), length(currentObjects) )
  # soft preferences with uniform choice fusion.
  softAddProb <- type
  objectPreferenceSoftPriors <- list()
  for(utt in rep(1:(length(validUtterances)+1)) ) {
    objectPreferenceSoftPriors[[utt]] <- objectPreferenceHardPriors[[utt]] + softAddProb
    objectPreferenceSoftPriors[[utt]] <- objectPreferenceSoftPriors[[utt]] / sum(objectPreferenceSoftPriors[[utt]])
  }
  return(objectPreferenceSoftPriors)
}


######## From Ella's Code:
# Hania: look at Ella's functions separately (09.10.20)
#Hania: not included in package (16.10.20)

# Get preferences for the utterances
#
# @description
# from Ella's Code
# gives a full random matrix of all feature value preferences given a certain ratio i.e. c(0, 1/3, 2/3)
# @param ratio .
# @return allUtterancePref full random matrix of all feature value preferences.
# @export
# @examples
# ratio <- c(0, 1/3, 2/3)
# getAllUtterancePref(ratio)
getAllUtterancePref <- function(ratio) {
  allUttNum <- c(1:9)
  allUtterancePref <- matrix(allUttNum, length(allUttNum), 3)
  sampledRatio <- c(sample(ratio), sample(ratio), sample(ratio))
  repRatio <- c(ratio, ratio, ratio)
  for(utt in rep(1:length(allUtterances))){
    allUtterancePref[utt,1] <- utt
    allUtterancePref[utt,2] <- allUtterances[utt]
    # allUtterancePref[utt,3] <- as.numeric(sampledRatio[utt])
    allUtterancePref[utt,3] <- as.numeric(repRatio[utt])
  }
  return(allUtterancePref)
}


#Hania: not included in package (16.10.20)

# Determines all feature values
# @description
# from Ella's Code
# @param currentObjects Vector of three values in \code{{1,...,27}} specifying the target and the other two objects.
#
# The target is the first object in the vector \code{(index = 1)}.
# @return allFeaUtterances .
# @examples
# @export
determineAllFeaValues <- function(currentObjects) {
  allFeaUtterances <- c()
  for(i in c(1:length(currentObjects))) {
    allFeaUtterances <- c(allFeaUtterances, allObjectsToUtterancesMappings[currentObjects[i],])
  }
  return(allFeaUtterances)
}

#Hania: not included in package as deprecated (16.10.20)

# Get mapped utterances to preferences
#
# @description
# from Ella's Code
# get a matrix of all preferences for the validUtterances
# @param validUtterances Vector of all present feature values (blue, red,green, cloud, circle, square, dotted, solid…).
# @param allObjects matrix 27 rows, 3 columns; made out of words.
# @param allUtterancePref.
# @return mapUttToPref matrix of all preferences.
# @examples
# @export
getMapUttToPref <- function(validUtterances, allObjects, allUtterancePref){
  mapUttToPref <- allUtterancePref
  allUtteranceNum <- c(1:9)
  notRelevantUtt <- setdiff(allUtteranceNum, validUtterances)
  for (rowNum in rep(1:length(notRelevantUtt))){
    mapUttToPref <- mapUttToPref[-which(mapUttToPref[,1] == notRelevantUtt[rowNum]),]
  }
  return (mapUttToPref)
}


#Hania: not included in package  (16.10.20)
# Get object preferences' priors with utterances to preferences
#
# @description
# from Ella's Code
# @param validUtterances .
# @param currentObjects Vector of three values in \code{{1,...,27}} specifying the target and the other two objects.
#
# The target is the first object in the vector \code{(index = 1)}.
# @param type 0: hard priors; > 0: soft prior with specified softness (Same as SoftAddProb).
# @param uttToObjProbs Matrix where the rows map each possible utterance, which corresponds to each present feature value,
# to the objects that may be chosen (reflecting the "obedience" parameter and which objects match the respective utterance)
#
# probability that object is chosen following an utterances.
# @param mapUttToPref .
# @return objectPreferenceSoftPriors a list of preference priors for all considered features,
# i.e. utterances, as well as for "no preference" whatsoever, i.e., uniform prior over all three objects..
# @examples
# @export
getObjectPreferencePriorsWithUttToPref <- function(validUtterances, currentObjects, type, uttToObjProbs, mapUttToPref) {
  objectPreferenceHardPriors <- list()
  for(utt in rep(1:length(validUtterances)) ) {
    objectPreferenceHardPriors[[utt]] <- uttToObjProbs[utt,]#*as.numeric(mapUttToPref[utt,3])
  }
  #nopreference case
  objectPreferenceHardPriors[[length(validUtterances)+1]] = rep(1/length(currentObjects), length(currentObjects) )

  #  soft preferences with uniform choice fusion.
  softAddProb <- type
  objectPreferenceSoftPriors <- list()
  for(utt in rep(1:(length(validUtterances)+1)) ) {
    objectPreferenceSoftPriors[[utt]] <- objectPreferenceHardPriors[[utt]] + softAddProb
    objectPreferenceSoftPriors[[utt]] <- objectPreferenceSoftPriors[[utt]] / sum(objectPreferenceSoftPriors[[utt]])
  }
  return(objectPreferenceSoftPriors)
}

# Hania: we don't really know what this does (09.10.20)
# Get mapped utterances to object to preferences
#
# @description
# from Ella's Code
# creates a matrix containing all preferences for the object choice of the listener,
# depending on the present objects and the target feature and the preferences of the listener
# @param currentObjects Vector of three values in \code{{1,...,27}} specifying the target and the other two objects.
#
# The target is the first object in the vector \code{(index = 1)}.
# @param targetfeature is a value between 1 and 3, specifying which feature  type is considered (for preferences).
# @param validUtterances Vector of all present feature values (blue, red,green, cloud, circle, square, dotted, solid…).
# @param allUtterancePref .
# @param allObjects matrix 27 rows, 3 columns, made out of words.
# @param mapUttToPref .
# @return mapUttToObjToPref .
# @examples
# @export
# +1 row without preferences
getMapUttToObjToPref <- function(currentObjects, targetFeature, validUtterances, allUtterancePref, allObjects, mapUttToPref){
  isUnique <- matrix(FALSE, nrow = length(validUtterances)+1, ncol = length(currentObjects))
  mapUttToObjToPref <- matrix(0, nrow = length(validUtterances)+1, ncol = length(currentObjects))
  objectSpecific <- matrix("", nrow= 3, ncol = 3)
  for(obj in rep(1:3)){
    objectSpecific[obj,] <- allObjects[currentObjects[obj],]
  }
  countedUttObj <- table(objectSpecific)
  relevantUttWords <- validUtterances
  for(utt in rep(1:length(relevantUttWords))){
    relevantUttWords[utt] <- mapUttToPref[utt,2]
    index <- which(objectSpecific == relevantUttWords[utt], arr.ind=TRUE)[,"row"]
    for(ind in rep(1:length(index))){
      isUnique[utt,index[ind]] <- TRUE
    }
  }
  # cat(print(relevantUttWords))
  # cat(print(isUnique))
  for(row in rep(1:length(validUtterances))){
    for(col in rep(1:length(currentObjects))){
      if(isUnique[row,col]){
        targetFeatureValue <- objectSpecific[col,targetFeature]
        targetFeatureValuePref <- mapUttToPref[which(mapUttToPref[,2]==targetFeatureValue),3]
        if(length(targetFeatureValuePref)==0) {
          print(c("length is zero!?",targetFeatureValue,targetFeatureValuePref))
          print(allUtterances[validUtterances])
          print(allObjects[currentObjects,])
        }
        mapUttToObjToPref[row,col] <- targetFeatureValuePref
      }
    }
  }
  # cat(print(isUnique))
  mapUttToObjToPref[length(validUtterances)+1,]<- 0.33
  if(targetFeature == "shape" || targetFeature == 1){
    notRelevantUtt <- c(1, 2, 3)
  } else  if(targetFeature == "pattern" || targetFeature == 2){
    notRelevantUtt <- c(4, 5, 6)
  } else  if(targetFeature == "color" || targetFeature == 3){
    notRelevantUtt <- c(7, 8, 9)
  }
  currentNotRelevantUtt <- which(as.numeric(mapUttToPref[,1]) %in% notRelevantUtt)
  for (row in rep(1:length(currentNotRelevantUtt))){
    mapUttToObjToPref[currentNotRelevantUtt[row],] <- 0
  }
  return (mapUttToObjToPref)
}

