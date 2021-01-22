#' Determine speaker's inference of the posterior listener preferences
#'
#' @description
#' Simple RSA
#'
#' This function calculates the speaker's posterior guess of the feature value preferences of the listener.
#' That means how the speaker infers the preferences of the listener based on the object choice.
#'
#' @param currentObjects A vector of three values in \code{{1,...,27}} specifying the target and the other two objects in the scene.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#' @param featureUtt One of the values \code{{1,2,3}} specifying which feature is uttered (i.e. shape = 1 / texture = 2 / or color = 3).
#' @param softPrefValue A parameter value between \code{[0,infinity)} (The larger the higher the tendency towards uniform liking).
#'
#' Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.
#' @param notObeyInst Determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.
#' @return A vector of length 9. It contains the speaker's inference of the feature value preferences of the listener.
#' @examples 
#' \donttest{determineSpeakerPostListPrefsSimpleRSA(currentObjects, featureUtt,
#' softPrefValue, notObeyInst)}
#'
#' \dontrun{output: [1] 1.00 0.00 0.00 0.33 0.33 0.33 0.33 0.33 0.33}
#'
#' This output means that the speaker inferred that the listener prefers circles
#' over squares and clouds and that the doesn't have a preference for color or
#' texture, since they all uniform.
#' @export
determineSpeakerPostListPrefsSimpleRSA <- function(currentObjects, featureUtt,
                                          softPrefValue, notObeyInst) {
  validUtterances <- determineValidUtterances(currentObjects)
  mapObjToUtt <- mapObjectToUtterances(currentObjects)
  uttToObjProbs <- determineUttToObjectProbs(validUtterances,
                                                              currentObjects,
                                                              mapObjToUtt, notObeyInst)
  mapUttToObjDeterministic <- determineUttToObjectProbs(validUtterances,
                                                              currentObjects,
                                                              mapObjToUtt, 0)
  #  print(uttToObjProbs)
  objectPreferenceSoftPriors <- getObjectPreferencePriors(validUtterances, currentObjects,
                                                          softPrefValue, mapUttToObjDeterministic)
  postListPrefs = rep(0,9)
  for(f in c(1:3)) {
    prefPrior <- rep(0, length(validUtterances+1)) # determine preference prior over all valid utterances plus no preference
    relevantIndices <- which(validUtterances>(3*(f-1)) & validUtterances<(3*f+1)) # relevant indices for a particular feature type
    prefPrior[relevantIndices] <- 1/length(relevantIndices) # prior over the indices
    prefPost <- simplePragmaticSpeaker(which(validUtterances==
                                  allObjectsToUtterancesMappings[currentObjects[1],featureUtt]),
                          1, prefPrior, validUtterances, currentObjects,
                          uttToObjProbs, objectPreferenceSoftPriors)
    for(i in c((1+(f-1)*3):(f*3))) {
      postListPrefs[i] <- 1/3
    }
    for(i in c(1:length(relevantIndices))) {
      postListPrefs[validUtterances[relevantIndices[i]]] <- prefPost[relevantIndices[i]] * length(relevantIndices) / 3 # posterior back into vector of 3x3 values
    }
  }
  return(postListPrefs)
}

####
#' Cost function for one parameter optimization.
#' Optimizing softness.
#' Non-obedience fixed at 0.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; The softness parameter is optimized.
#'
#' The non-obedience parameter is fixed.
#'
#' @param params One value vector specifying one of the two parameters to be optimized:
#' \enumerate{
#' \item{softPrefValue parameter is optimized, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' \item{non-obedience parameter is fixed at 0, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#'   (0 = full obedience, infinity = full instruction ignorance)}.
#' }
#'
#' @param data A Matrix with data rows.
#'
#' column structure:
#'
#' [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat]
#'
#' [7:Q1AnswerV1,V2,V3, 10:Q2AnswerV1,V2,V3]
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:UUFeat} Uttered feature. A number between 1 and 3. (1: shape, 2: pattern, 3: color)
#'
#' \strong{5:Q1Feat} Questioned feature 1. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{6:Q2Feat} Questioned feature 2. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{7:Q1AnswerV1, V2, V3} The columns 7-9 contain the participants' slider values for the first questioned feature.
#'
#' \strong{10:Q2AnswerV1, V2, C3} The columns 10-12 contain the participants' slider values for the second questioned feature.
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA}}.
#' @export
RSAModelLL1_1simpleRSA <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(data, abs(params[1]), 0))
}

#' Cost function for one parameter optimization.
#' Optimizing softness.
#' Non-obedience fixed at 0.1.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; The softness parameter is optimized.
#'
#' The non-obedience parameter is fixed.
#'
#' @param params One value vector specifying one of the two parameters to be optimized:
#' \enumerate{
#' \item{softPrefValue parameter is optimized, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' \item{non-obedience is fixed at 0.1, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance)}
#' }
#' @param data A matrix with data rows.
#'
#' column structure:
#'
#' [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat]
#'
#' [7:Q1AnswerV1,V2,V3, 10:Q2AnswerV1,V2,V3]
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:UUFeat} Uttered feature. A number between 1 and 3. (1: shape, 2: pattern, 3: color)
#'
#' \strong{5:Q1Feat} Questioned feature 1. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{6:Q2Feat} Questioned feature 2. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{7:Q1AnswerV1, V2, V3} The columns 7-9 contain the participants' slider values for the first questioned feature.
#'
#' \strong{10:Q2AnswerV1, V2, C3} The columns 10-12 contain the participants' slider values for the second questioned feature.
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA}}.
#' @export
RSAModelLL1_1simpleRSA_notObey.1 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(data, abs(params[1]), .1))
}

#' Cost function for one parameter optimization.
#' Optimizing softness.
#' Non-obedience fixed at 0.2.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; The softness parameter is optimized.
#'
#' The non-obedience parameter is fixed.
#'
#' @param params One value vector specifying one of the two parameters to be optimized:
#' \enumerate{
#' \item{softPrefValue parameter is optimized, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' \item{non-obedience is fixed at 0.2, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance)}
#' }
#'
#' @param data A matrix with data rows.
#'
#' column structure:
#'
#' [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat]
#'
#' [7:Q1AnswerV1,V2,V3, 10:Q2AnswerV1,V2,V3]
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:UUFeat} Uttered feature. A number between 1 and 3. (1: shape, 2: pattern, 3: color)
#'
#' \strong{5:Q1Feat} Questioned feature 1. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{6:Q2Feat} Questioned feature 2. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{7:Q1AnswerV1, V2, V3} The columns 7-9 contain the participants' slider values for the first questioned feature.
#'
#' \strong{10:Q2AnswerV1, V2, C3} The columns 10-12 contain the participants' slider values for the second questioned feature.
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA}}.
#' @export
RSAModelLL1_1simpleRSA_notObey.2 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(data, abs(params[1]), .2))
}


#' Cost function for one parameter optimization.
#' Optimizing non-obedience.
#' Softness is fixed at 0.2.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; The non-obedience parameter is optimized.
#'
#' The softness parameter is fixed.
#'
#' @param params One value vector specifying one of the two parameters to be optimized:
#' \enumerate{
#' \item{softPrefValue parameter is fixed at 0, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' \item{non-obedience parameter is optimized, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' }
#' @param data A matrix with data rows.
#'
#' column structure:
#'
#' [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat]
#'
#' [7:Q1AnswerV1,V2,V3, 10:Q2AnswerV1,V2,V3]
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:UUFeat} Uttered feature. A number between 1 and 3. (1: shape, 2: pattern, 3: color)
#'
#' \strong{5:Q1Feat} Questioned feature 1. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{6:Q2Feat} Questioned feature 2. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{7:Q1AnswerV1, V2, V3} The columns 7-9 contain the participants' slider values for the first questioned feature.
#'
#' \strong{10:Q2AnswerV1, V2, C3} The columns 10-12 contain the participants' slider values for the second questioned feature.
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA}}.
#' @export
RSAModelLL1_2simpleRSA <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(data, 0, abs(params[1])))
}

#' Cost function for one parameter optimization.
#' Optimizing non-obedience.
#' Softness is fixed at 0.2.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; The non-obedience parameter is optimized.
#'
#' The softness parameter is fixed.
#' @param params One value vector specifying one of the two parameters to be optimized:
#' \enumerate{
#' \item{softPrefValue is fixed at 0.2, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' \item{non-obedience parameter is optimized, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' }
#' @param data A matrix with data rows.
#'
#' column structure:
#'
#' [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat]
#'
#' [7:Q1AnswerV1,V2,V3, 10:Q2AnswerV1,V2,V3]
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:UUFeat} Uttered feature. A number between 1 and 3. (1: shape, 2: pattern, 3: color)
#'
#' \strong{5:Q1Feat} Questioned feature 1. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{6:Q2Feat} Questioned feature 2. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{7:Q1AnswerV1, V2, V3} The columns 7-9 contain the participants' slider values for the first questioned feature.
#'
#' \strong{10:Q2AnswerV1, V2, C3} The columns 10-12 contain the participants' slider values for the second questioned feature.
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA}}.
#' @export
RSAModelLL1_2simpleRSA_pref2 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(data, 0.2, abs(params[1])))
}


####
#' Cost function for two parameter optimization.
#' Optimizing softness and non-obedience.
#' @description
#' Simple RSA
#'
#' 2 parameter optimization; The softness and non-obedience parameter are optimized.
#'
#' @param params Two value vector specifying two of the two parameters to be optimized:
#' \enumerate{
#' \item{softPrefValue parameter is optimized, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking).}
#' \item{non-obedience parameter is fixed at 0.1, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' }
#
#' @param data A matrix with data rows.
#'
#' column structure:
#'
#' [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat]
#'
#' [7:Q1AnswerV1,V2,V3, 10:Q2AnswerV1,V2,V3]
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:UUFeat} Uttered feature. A number between 1 and 3. (1: shape, 2: pattern, 3: color)
#'
#' \strong{5:Q1Feat} Questioned feature 1. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{6:Q2Feat} Questioned feature 2. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{7:Q1AnswerV1, V2, V3} The columns 7-9 contain the participants' slider values for the first questioned feature.
#'
#' \strong{10:Q2AnswerV1, V2, C3} The columns 10-12 contain the participants' slider values for the second questioned feature.
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA}}.
#' @export
RSAModelLL2_simpleRSA <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA(data, abs(params[1]), abs(params[2])))
}

#' Simple RSA model Kullback-Leibler divergence determination
#' (all feature values considered)
#'
#' @description
#' Simple RSA
#'
#' The function calculates the optimal parameter values of the free parameters by estimating the log-likelihood of the
#' RSA model given model parameters and data. It also determines the actual RSA model Kullback-Leibler divergence.
#'
#' 3 parameter optimization considering all feature values (also the ones not present in the scene), i.e. feature values of shape, texture and color.
#'
#' @param data A matrix with data rows.
#'
#' column structure:
#'
#' [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat]
#'
#' [7:Q1AnswerV1,V2,V3, 10:Q2AnswerV1,V2,V3]
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:UUFeat} Uttered feature. A number between 1 and 3. (1: shape, 2: pattern, 3: color)
#'
#' \strong{5:Q1Feat} Questioned feature 1. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{6:Q2Feat} Questioned feature 2. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{7:Q1AnswerV1, V2, V3} The columns 7-9 contain the participants' slider values for the first questioned feature.
#'
#' \strong{10:Q2AnswerV1, V2, C3} The columns 10-12 contain the participants' slider values for the second questioned feature.
#'
#' @param par1 \describe{
#' \item{softness parameter}{The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' }
#' @param par2 \describe{
#' \item{non-obedience parameter}{The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' }
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @export
RSAModelKLDiv3paramsAllValuesConsidered_simpleRSA <- function(data, par1, par2) {
  #  print(params)
  llRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    ## determining the model predictions
    probModelRes <- determineSpeakerPostListPrefsSimpleRSA(currentObjects, uttFeat, par1, par2)
    ## adding the negative log likelihoods
    for(j in c(1:3)) {
      llRes <- llRes + data[i, 6+j] *
        ( log(data[i, 6+j] + 1e-100) - log( probModelRes[j + (data[i, 5]-1)*3] + 1e-100) )
    }
    for(j in c(1:3)) {
      llRes <- llRes + data[i, 9+j] *
        ( log(data[i, 9+j] + 1e-100) - log( probModelRes[j + (data[i, 6]-1)*3] + 1e-100) )
    }
  }
  return(llRes)
}


#' Simple RSA model Kullback-Leibler divergence determination
#' (available feature values only)
#'
#' @description
#' Simple RSA
#'
#' The function calculates the optimal parameter values of the free parameters by estimating the log-likelihood of the
#' RSA model given model parameters and data. It also determines the actual RSA model Kullback-Leibler divergence.
#'
#' 3 parameter optimization considering only the available feature values present in the scene, i.e. feature values of shape, texture and color.
#' @param data A matrix with data rows.
#'
#' column structure:
#'
#' [1:OC1,OC2,OC3,4:UUFeat, 5:Q1Feat,6:Q2Feat]
#'
#' [7:Q1AnswerV1,V2,V3, 10:Q2AnswerV1,V2,V3]
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:UUFeat} Uttered feature. A number between 1 and 3. (1: shape, 2: pattern, 3: color)
#'
#' \strong{5:Q1Feat} Questioned feature 1. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{6:Q2Feat} Questioned feature 2. A number between 1 and 3. (1: shape, 2: pattern, 3: color).
#'
#' Example: If you utter "blue" (feature: color), then you can learn something about shape and texture preferences.
#'
#' \strong{7:Q1AnswerV1, V2, V3} The columns 7-9 contain the participants' slider values for the first questioned feature.
#'
#' \strong{10:Q2AnswerV1, V2, C3} The columns 10-12 contain the participants' slider values for the second questioned feature.
#' @param par1 \describe{
#' \item{softness parameter}{The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' }
#' @param par2 \describe{
#' \item{non-obedience parameter}{The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' }
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function is used in \code{\link{RSAModelLL1_1simpleRSA}},
#' \code{\link{RSAModelLL1_1simpleRSA_notObey.1}},
#'
#' \code{\link{RSAModelLL1_1simpleRSA_notObey.2}},
#'\code{\link{RSAModelLL1_2simpleRSA}},
#'
#' \code{\link{RSAModelLL1_2simpleRSA_pref2}},
#' \code{\link{RSAModelLL2_simpleRSA}}.
#' @export
RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered_simpleRSA <- function(data, par1, par2) {
  #  print(params)
  llRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    ##
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    probModelRes <- determineSpeakerPostListPrefsSimpleRSA(currentObjects, uttFeat, abs(par1), abs(par2))
    ## adding the KL Divergence terms of the relevant feature values for the two sets of answers.
    ##
    ## answer set 1 with feature type data[i,5]
    relevantIndices <- which(validUtterances>(3*(data[i, 5]-1)) & validUtterances<(3*data[i, 5] + 1)) # relevant indices for a particular feature type
    # normalizing to one.
    probModelRes[validUtterances[relevantIndices]] <- probModelRes[validUtterances[relevantIndices]] /
                                                                    (sum(probModelRes[validUtterances[relevantIndices]]) +  1e-100)
    relIndicesRel <- validUtterances[relevantIndices] - ((data[i,5]-1)*3)
    data[i,6+relIndicesRel] <- data[i,6+relIndicesRel] / (sum(data[i,6+relIndicesRel]) + 1e-100)
    # determining respective KL divergence values
    for(j in c(1:length(relevantIndices))) {
      llRes <- llRes + data[i, 6+relIndicesRel[j]] *
        ( log(data[i, 6+relIndicesRel[j]] + 1e-100) - log(probModelRes[validUtterances[relevantIndices[j]]] + 1e-100) )
    }
    ##
    ## answer set 2 with feature type data[i,6]
    relevantIndices <- which(validUtterances>(3*(data[i, 6]-1)) & validUtterances<(3*data[i, 6] + 1)) # relevant indices for a particular feature type
    # normalizing to one.
    probModelRes[validUtterances[relevantIndices]] <- probModelRes[validUtterances[relevantIndices]] /
                                                                     (sum(probModelRes[validUtterances[relevantIndices]]) +  1e-100)
    relIndicesRel <- validUtterances[relevantIndices] - (3*(data[i, 6]-1))
    data[i,9+relIndicesRel] <- data[i,9+relIndicesRel] / (sum(data[i,9+relIndicesRel]) + 1e-100)
    # determining respective KL divergence values
    for(j in c(1:length(relevantIndices))) {
      llRes <- llRes + data[i, 9+relIndicesRel[j] ] *
        ( log(data[i, 9+relIndicesRel[j] ] + 1e-100) - log(probModelRes[validUtterances[relevantIndices[j]]] + 1e-100) )
    }
  }
  return(llRes)
}


###
# Hania: not included in Rpackage as only used in X9 (16.10.2020)
# Posterior listener preferences for all constellations (simple RSA)
#
# Simple RSA
# Get matrix of all posteriors of all object constellations and object choices possible.
# @param softPrefValue fixed at 0; in \code{[0,infinity)} (the larger the higher the tendency towards uniform liking).
#
# Value reflects how categorical the listener's preferences are:
#
# 0: always pick the preferred object. If the listener prefers red objects, she will always pick the red object.
# infinity: It is as likely for the listener to pick green, blue or red objects.
# @param nonObedience fixed at 0; determines if the instruction does not need to be obeyed (0 = full obedience: -> infty  = full instruction ignorance).
# @param alpha fixed at 1; Exponential scaling of the speaker uttering that value that maximizes the chance of getting the target object right.
#
# Value between 0 and 1.
# @return resultMat matrix of all posteriors of all object constellations and object choices possible.
# @examples
# @export
getPostListPrefsForAllConstellations_simpleRSA <- function(softPrefValue=0, nonObedience=0, alpha=1) {
  resultMat <- matrix(0, 1 + (27*27*27*3), 28)
  for(o1 in c(1:27)) {
    print(o1)
    for(o2 in c(1:27)) {
      print(c(o1,o2))
      for(o3 in c(1:27)) {
        for(featChoice in c(1:3)) {
          row = 3 * (27 * ((o1-1) * 27 + (o2-1) ) + (o3-1) ) + featChoice
          objectConstellation <- c(o1,o2,o3)
          resultMat[row,1:3] <- allObjects[o1,]
          resultMat[row,4:6] <- allObjects[o2,]
          resultMat[row,7:9] <- allObjects[o3,]
          resultMat[row,10] <- o1
          resultMat[row,11] <- o2
          resultMat[row,12] <- o3
          resultMat[row,13] <- featChoice
          resultMat[row,14:19] <- getConstellationCode(objectConstellation, featChoice)
          resultMat[row,20:28] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
                                                                softPrefValue, nonObedience, alpha)
        }
      }
    }
  }
  return(resultMat)
}

