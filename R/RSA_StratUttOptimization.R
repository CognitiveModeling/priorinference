###
#' Determine speaker's inference of the posterior listener preferences
#'
#' @description
#' Full-RSA
#'
#' This function calculates the speaker's posterior guess of the feature value preferences of the listener.
#' That means how the speaker infers the preferences of the listener based on the object choice.
#'
#' @param currentObjects A vector of three values in \code{{1,...,27}} specifying the target and the other two objects in the scene.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#' @param featureUtt One of the values \code{{1,2,3}} specifying which feature is uttered (i.e. shape = 1 / texture = 2 / or color = 3).
#'
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
#' @param alpha A parameter value between 0 and 1.
#'
#' Exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.
#' @return A vector of length 9. It contains the speaker's inference of the feature value preferences of the listener.
#' 
#' @examples 
#' \donttest{determineSpeakerPostListPrefs(currentObjects, featureUtt, 
#' softPrefValue, notObeyInst, alpha)}
#' 
#' \dontrun{output: [1] 1.00 0.00 0.00 0.33 0.33 0.33 0.33 0.33 0.33}
#'
#' This output means that the speaker inferred that the listener prefers
#' circles over squares and clouds and that the doesn't have a preference
#' for color or texture, since they all uniform.
#' @export
determineSpeakerPostListPrefs <- function(currentObjects, featureUtt,
                                          softPrefValue, notObeyInst, alpha) {
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
    print(prefPrior)
    prefPost <- pragmaticSpeaker(which(validUtterances==
                                  allObjectsToUtterancesMappings[currentObjects[1],featureUtt]),
                          1, prefPrior, validUtterances, currentObjects,
                          uttToObjProbs, objectPreferenceSoftPriors, alpha)
    # fill all with 1/3
    for(i in c((1+(f-1)*3):(f*3))) {
      postListPrefs[i] <- 1/3
    }
    # overwrite the relevant indices: this is why you still get 1/3 for the feature values that were not relevant in the scene
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
#' Full-RSA
#'
#' 1 parameter optimization; The softness parameter is optimized.
#'
#' The non-obedience parameter and the alpha parameter are fixed.
#'
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered}}.
#'
#' @param params One value vector, which specifies one of three parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue parameter is optimized, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience parameter is fixed at 0, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#'   (0 = full obedience, infinity = full instruction ignorance)}
#'   \item{alpha parameter is fixed at 1, i.e. An exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}}
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
#' @export
RSAModelLL1_1 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, abs(params[1]), 0, 1))
}

#' Cost function for one parameter optimization.
#' Optimizing softness.
#' Non-obedience fixed at 0.1.
#'
#' @description
#' Full-RSA
#'
#' 1 parameter optimization; The softness parameter is optimized.
#'
#' The non-obedience parameter and the alpha parameter are fixed.
#'
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered}}.
#'
#' @param params One value vector specifying one of the three parameters to be optimized:
#' \enumerate{
#' \item{softPrefValue parameter is optimized, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' \item{non-obedience parameter is fixed at 0.1, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance)}
#' \item{alpha parameter is fixed at 1, i.e. An exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
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
#' @export
RSAModelLL1_1_notObey.1 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, abs(params[1]), .1, 1))
}


#' Cost function for one parameter optimization.
#' Optimizing non-obedience.
#' Softness fixed at 0.
#'
#' @description
#' Full-RSA
#'
#' 1 parameter optimization; The non-obedience parameter is optimized.
#'
#' The softness parameter and the alpha parameter are set to certain values.
#'
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered}}.
#'
#' @param params One value vector specifying one of the three parameters to be optimized:
#' \enumerate{
#' \item{softPrefValue parameter is fixed at 0, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' \item{non-obedience parameter is optimized, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' \item{alpha parameter is fixed at 1, i.e. An exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
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
#' @export
RSAModelLL1_2 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, 0, abs(params[1]), 1))
}

#' Cost function for one parameter optimization.
#' Optimizing alpha.
#' Softness and non-obedience fixed at 0.
#'
#' @description
#' Full-RSA
#'
#' 1 parameter optimization; The alpha parameter is optimized.
#'
#' The softness parameter and the non-obedience parameter are fixed.
#'
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered}}.
#'
#' @param params One value vector specifying one of the three parameters to be optimized:
#' \enumerate{
#' \item{softPrefValue parameter is fixed at 0, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' \item{non-obedience parameter is fixed at 0, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' \item{alpha parameter is optimized, i.e. An exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
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
#' @export
RSAModelLL1_3 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, 0, 0, abs(params[1])))
}


####
#' Cost function for two parameter optimization.
#' Optimizing non-obedience and alpha.
#' Softness fixed at 0.
#'
#' @description
#' Full-RSA
#'
#' 2 parameter optimization; The non-obedience and alpha parameters are optimized.
#'
#' The softness parameter is fixed.
#'
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered}}.
#'
#' @param params Two value vector, which specifies two of three parameters (n1=not the first, n2=not the second, n3= not the third) to be optimized:
#' \enumerate{
#' \item{softPrefValueparameter fixed at 0, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' \item{non-obedience parameter is optimized, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' \item{alpha parameter is optimized, i.e. An exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
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
#'
#' @export
RSAModelLL2_n1 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, 0, abs(params[1]), abs(params[2])))
}

#' Cost function for two parameter optimization.
#' Optimizing softness and alpha.
#' Non-obedience fixed at 0.
#'
#' @description
#' Full-RSA
#'
#' 2 parameter optimization; The softness and alpha parameters are optimized.
#'
#' The non-obedience parameter is fixed.
#'
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered}}.
#'
#' @param params Two value vector, which specifies two of three parameters (n1 = not the first, n2 = not the second, n3 = not the third) to be optimized:
#' \enumerate{
#'   \item{softPrefValue parameter is optimized, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience parameter is fixed at 0, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#'   \item{alpha parameter is optimized, i.e. An exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
#'}
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
#' @export
RSAModelLL2_n2 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, abs(params[1]), 0, abs(params[2])))
}

#' Cost function for two parameter optimization.
#' Optimizing softness and non-obedience.
#' Alpha fixed at 1.
#'
#' @description
#' Full-RSA
#'
#' 2 parameter optimization; The softness parameter and the non-obedience parameter are optimized.
#'
#' The alpha parameter is fixed.
#'
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered}}.
#'
#' @param params Two value vector, which specifies two of three parameters (n1 = not the first, n2 = not the second, n3 = not the third) to be optimized:
#' \enumerate{
#' \item{softPrefValue parameter is optimized, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' \item{non-obedience parameter is optimized, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' \item{alpha parameter is fixed at 1, i.e. An exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
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
#' @export
RSAModelLL2_n3 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, abs(params[1]), abs(params[2]), 1))
}

####
#' Cost function for three parameter optimization.
#' Optimizing softness, non-obedience and alpha.
#'
#' @description
#' Full-RSA
#'
#' 3 parameter optimization; The softness parameter, the non-obedience and the alpha parameter are optimized.
#'
#' @details
#' This function uses \code{\link{RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered}}.
#'
#' @param params Three value vector, which specifies all three model parameters to be optimized:
#' \enumerate{
#' \item{softPrefValue parameter is optimized, i.e., The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#' \item{non-obedience parameter is optimized, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' \item{alpha parameter is optimized, i.e. An exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
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
#'
#' @export
RSAModelLL3 <- function(params,  data) {
  return(RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered(data, abs(params[1]), abs(params[2]), abs(params[3])))
}


#' RSA model Kullback-Leibler divergence determination
#' (all feature values considered)
#'
#' @description
#' Full-RSA
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
#' \item{softness parameter}{Specifies how much actual feature priorities come into play in the object choice (The larger the higher the tendency towards uniform liking).}
#' }
#' @param par2 \describe{
#' \item{non-obedience parameter}{The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' }
#' @param par3 \describe{
#' \item{alpha parameter}{An exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
#' }
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @export
RSAModelKLDiv3paramsAllValuesConsidered <- function(data, par1, par2, par3) {
  #  print(params)
  klDiv <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    ## determining the model predictions
    probModelRes <- determineSpeakerPostListPrefs(currentObjects, uttFeat, par1, par2, par3)
    ## adding the negative log likelihoods
    for(j in c(1:3)) {
      klDiv <- klDiv + data[i, 6+j] *
        ( log(data[i, 6+j] + 1e-100) - log( probModelRes[j + (data[i, 5]-1)*3] + 1e-100) )
    }
    for(j in c(1:3)) {
      klDiv <- klDiv + data[i, 9+j] *
        ( log(data[i, 9+j] + 1e-100) - log( probModelRes[j + (data[i, 6]-1)*3] + 1e-100) )
    }
  }
  return(klDiv)
}


#' RSA model Kullback-Leibler divergence determination
#' (available feature values only)
#'
#' @description
#' Full-RSA
#'
#' The function calculates the optimal parameter values of the free parameters by estimating the log-likelihood of the
#' RSA model given model parameters and data. it also determines the actual RSA model Kullback-Leibler divergence.
#'
#' 3 parameter optimization considering only the available feature values present in the scene, i.e. feature values of shape, texture and color.
#'
#' @details
#' This function is used in the functions \code{\link{RSAModelLL1_1}}, \code{\link{RSAModelLL1_1_notObey.1}}, \code{\link{RSAModelLL1_2}}, \code{\link{RSAModelLL1_3}}, \code{\link{RSAModelLL2_n1}}, \code{\link{RSAModelLL2_n2}}, \code{\link{RSAModelLL2_n3}}, \code{\link{RSAModelLL3}}.
#'
#' @param data  A matrix with data rows.
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
#' \item{softness parameter}{Specifies how much actual feature priorities come into play in the object choice (The larger the higher the tendency towards uniform liking).}
#' }
#' @param par2 \describe{
#' \item{non-obedience parameter}{The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' }
#' @param par3 \describe{
#' \item{alpha parameter}{An exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
#' }
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @export
RSAModelKLDiv3paramsOnlyAvailableFeatureValuesConsidered <- function(data, par1, par2, par3) {
  #  print(params)
  klDiv <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    ##
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    probModelRes <- determineSpeakerPostListPrefs(currentObjects, uttFeat, abs(par1), abs(par2), par3)
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
      klDiv <- klDiv + data[i, 6+relIndicesRel[j]] *
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
      klDiv <- klDiv + data[i, 9+relIndicesRel[j] ] *
        ( log(data[i, 9+relIndicesRel[j] ] + 1e-100) - log(probModelRes[validUtterances[relevantIndices[j]]] + 1e-100) )
    }
  }
  return(klDiv)
}


#' Uniform Model Kullback-Leibler divergence determination
#'
#' @description
#' Full-RSA
#'
#' Divergence of the observed distribution from the uniform distribution over preferences.
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
#'
#' @export
UniformModelKLDiv <- function(data) {
  klDiv <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    ##
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
#    probModelRes <- determineSpeakerPostListPrefs(currentObjects, uttFeat, par1, par2, par3)
    ## adding the KL Divergence terms of the relevant feature values for the two sets of answers.
    ##
    ## answer set 1 with feature type data[i,5]
    relevantIndices <- which(validUtterances>(3*(data[i, 5]-1)) & validUtterances<(3*data[i, 5] + 1)) # relevant indices for a particular feature type
    # normalizing to one.
    relIndicesRel <- validUtterances[relevantIndices] - ((data[i,5]-1)*3)
    data[i,6+relIndicesRel] <- data[i,6+relIndicesRel] / (sum(data[i,6+relIndicesRel]) + 1e-100)
    # determining respective KL divergence values
    for(j in c(1:length(relevantIndices))) {
      klDiv <- klDiv + data[i, 6+relIndicesRel[j]] *
        ( log(data[i, 6+relIndicesRel[j]] + 1e-100) - log( 1 / length(relevantIndices)) )
    }
    ##
    ## answer set 2 with feature type data[i,6]
    relevantIndices <- which(validUtterances>(3*(data[i, 6]-1)) & validUtterances<(3*data[i, 6] + 1)) # relevant indices for a particular feature type
    # normalizing to one.
    relIndicesRel <- validUtterances[relevantIndices] - (3*(data[i, 6]-1))
    data[i,9+relIndicesRel] <- data[i,9+relIndicesRel] / (sum(data[i,9+relIndicesRel]) + 1e-100)
    # determining respective KL divergence values
    for(j in c(1:length(relevantIndices))) {
      klDiv <- klDiv + data[i, 9+relIndicesRel[j] ] *
        ( log(data[i, 9+relIndicesRel[j] ] + 1e-100) - log( 1 / length(relevantIndices) ) )
    }
  }
  return(klDiv)
}



###
#Hania: Not included in the package, X9 only, Ella (16.10.20)

# Posteriors of all object constellations and object choices possible
# @description
# Full-RSA
#
# Get matrix of all posteriors of all object constellations and object choices possible.
# @param softPrefValue is set to 0; in \code{[0,infinity)} (the larger the higher the tendency towards uniform liking).
#
# Value reflects how categorical the listener's preferences are:
#
# 0: always pick the preferred object. If the listener prefers red objects, she will always pick the red object.
# infinity: It is as likely for the listener to pick green, blue or red objects.
# @param nonObedience is set to 0; determines if the instruction does not need to be obeyed (0 = full obedience: -> infty  = full instruction ignorance).
# @param alpha set to 1; Exponential scaling of the speaker uttering that value that maximizes the chance of getting the target object right.
#
# Value between 0 and 1.
# @return resultMat .
# @examples
# @export
getPostListPrefsForAllConstellations <- function(softPrefValue=0, nonObedience=0, alpha=1) {
  resultMat <- matrix(0, 1+ (27*27*27*3), 28)
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
          resultMat[row,20:28] <- determineSpeakerPostListPrefs(objectConstellation, featChoice,
                                                                softPrefValue, nonObedience, alpha)
        }
      }
    }
  }
  return(resultMat)
}

# ## this can be flexibly set to any number of objects under consideration
# currentObjects <- c(1,2,3)
# print(c("Objects:",currentObjects))
# determineSpeakerPostListPrefs(currentObjects, 1, 0, 0, 1)
# determineSpeakerPostListPrefs(currentObjects, 2, 0, 0, 1)
# determineSpeakerPostListPrefs(currentObjects, 3, 0, 0, 1)
# currentObjects <- c(1,4,16)
# print(c("Objects:",allObjects[currentObjects,]))
# determineSpeakerPostListPrefs(currentObjects, 1, 0, 0, 1)
# determineSpeakerPostListPrefs(currentObjects, 2, 0, 0, 1)
# determineSpeakerPostListPrefs(currentObjects, 3, 0, 0, 1)
# currentObjects <- c(1,4,7)
# print(c("Objects:",allObjects[currentObjects,]))
# determineSpeakerPostListPrefs(currentObjects, 1, 0, 0, 1)
# determineSpeakerPostListPrefs(currentObjects, 2, 0, 0, 1)
# determineSpeakerPostListPrefs(currentObjects, 3, 0, 0, 1)

#Generating the big table...
# bigTablePostListPrefs <- getPostListPrefsForAllConstellations()

