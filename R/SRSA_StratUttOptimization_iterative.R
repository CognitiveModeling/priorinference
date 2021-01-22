#source("R/AllUtterancesAndObjects.R")
#source("R/getConstCodeStratUtt.R")
#source("R/SRSA_StratUtt.R")

###
#' Determine speaker's inference of the posterior listener preferences
#' (iterative setting)
#'
#' @description
#' Simple RSA
#'
#' This function calculates the speaker's posterior guess of the feature value preferences of the listener in the iterative setting.
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
#'
#' @param priorPrefAll A vector of length 9.
#'
#' Probability mass over all feature values.
#'
#' Gives a prior preferences distribution over all (nine) feature values.
#'
#' @return A vector of length 9. It contains the speaker's inference of the feature value preferences of the listener.
#' @examples
#' \donttest{determineSpeakerPostListPrefsSimpleRSAWithPriorPref(currentObjects,
#' featureUtt, softPrefValue, notObeyInst, priorPrefAll)}
#'
#' output:
#' [1] 0.33 0.00 0.00 0.11 0.11 0.11 0.11 0.11 0.11
#'
#' @details
#' This is the iterative version of the function \code{\link{determineSpeakerPostListPrefsSimpleRSA}}
#' @export
determineSpeakerPostListPrefsSimpleRSAWithPriorPref <- function(currentObjects, featureUtt,
                                                   softPrefValue, notObeyInst, priorPrefAll) {
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
  prefPostAll <- simplePragmaticSpeakerWithPrefPriorAll(which(validUtterances==
                                               allObjectsToUtterancesMappings[currentObjects[1],featureUtt]),
                                       1, priorPrefAll, validUtterances, currentObjects,
                                       uttToObjProbs, objectPreferenceSoftPriors)
  return(prefPostAll)
}


####
#' Cost function for one parameter optimization (iterative setting).
#' Optimizing softness.
#' Non-obedience fixed at 0.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; The softness parameter is optimized.
#'
#' The non-obedience parameter is fixed.
#' @param params One value vector, which specifies one of two parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue is optimized,i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience (default = 0), i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#'   (0 = full obedience, infinity = full instruction ignorance)}
#'}
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
#' This function uses \code{\link{RSAModelKLDiv3params_simpleRSA4TrialsIterative}}.
#' @export
RSAModelLL1_1simpleRSA4TrialsIterative <- function(params,  data) {
  return(RSAModelKLDiv3params_simpleRSA4TrialsIterative(data, abs(params[1]), 0))
}


#' Cost function for one parameter optimization (iterative setting).
#' Optimizing softness.
#' Non-obedience fixed at 0.1.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; The softness parameter is optimized.
#'
#' The non-obedience parameter is fixed.
#' @param params One value vector, which specifies one of two parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue is optimized, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience is fixed at 0.1, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#'    (0 = full obedience, infinity = full instruction ignorance)}
#'}
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
#' This function uses \code{\link{RSAModelKLDiv3params_simpleRSA4TrialsIterative}}.
#' @export
RSAModelLL1_1simpleRSA4TrialsIterative_notObey.1 <- function(params,  data) {
  return(RSAModelKLDiv3params_simpleRSA4TrialsIterative(data, abs(params[1]), .1))
}


#' Cost function for one parameter optimization (iterative setting).
#' Optimizing softness.
#' Non-obedience fixed at at 0.2
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; The softness parameter is optimized.
#'
#' The non-obedience parameter is fixed.
#' @param params One value vector, which specifies one of two parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue is optimized, i.e. The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience is fixed at 0.2, i.e The extent to which the instruction of the speaker is obeyed by the listener.
#'    (0 = full obedience, infinity = full instruction ignorance)}
#'}
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
#' This function uses \code{\link{RSAModelKLDiv3params_simpleRSA4TrialsIterative}}.
#' @export
RSAModelLL1_1simpleRSA4TrialsIterative_notObey.2 <- function(params,  data) {
  return(RSAModelKLDiv3params_simpleRSA4TrialsIterative(data, abs(params[1]), .2))
}

#' Cost function for one parameter optimization (iterative setting).
#' Optimizing non-obedience.
#' Softness fixed at at 0.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; non-obedience parameter is optimized.
#'
#' The softness parameter is fixed.
#' @param params One value vector, which specifies one of two parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue is fixed at 0, i.e.The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience is optimized, i.e. The extent to which the instruction of the speaker is obeyed by the listener. #'   (0 = full obedience, infinity = full instruction ignorance)}
#'}
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
#' This function uses \code{\link{RSAModelKLDiv3params_simpleRSA4TrialsIterative}}.
#' @export
RSAModelLL1_2simpleRSA4TrialsIterative <- function(params,  data) {
  return(RSAModelKLDiv3params_simpleRSA4TrialsIterative(data, 0, abs(params[1])))
}

#' Cost function for one parameter optimization (iterative setting).
#' Optimizing non-obedience.
#' Softness fixed at at 0.2.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; non-obedience parameter is optimized.
#'
#' The softness parameter is fixed.
#'
#' @param params One value vector, which specifies one of two parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue is fixed at 0.2, i.e.The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience is optimized, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#'    (0 = full obedience, infinity = full instruction ignorance)}
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
#' @details
#' This function uses \code{\link{RSAModelKLDiv3params_simpleRSA4TrialsIterative}}.
#' @export
RSAModelLL1_2simpleRSA4TrialsIterative_pref.2 <- function(params,  data) {
  return(RSAModelKLDiv3params_simpleRSA4TrialsIterative(data, 0.2, abs(params[1])))
}


####
#' Cost function for one parameter optimization (iterative setting).
#' Optimizing softness and non-obedience.
#'
#' @description
#' Simple RSA
#'
#' 2 parameter optimization; softPrefValue and non-obedience are optimized.
#'
#' @param params Two value vector, which specifies two of two parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue is optimized, i.e.The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience is optimized, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#'    (0 = full obedience, infinity = full instruction ignorance)}
#'}
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
#' This function uses \code{\link{RSAModelKLDiv3params_simpleRSA4TrialsIterative}}.
#' @export
RSAModelLL2_simpleRSA4TrialsIterative <- function(params,  data) {
  return(RSAModelKLDiv3params_simpleRSA4TrialsIterative(data, abs(params[1]), abs(params[2])))
}

#' Simple RSA model Kullback-Leibler divergence determination
#' (iterative setting)
#'
#' @description
#' Simple RSA
#'
#' The function calculates the optimal parameter values of the free parameters by estimating the log-likelihood of the
#' RSA model given model parameters and data. It also determines the actual RSA model Kullback-Leibler divergence.
#'
#' 2 parameter optimization considering only the available feature values present in the scene, i.e. feature values of shape, texture and color.
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
#' @details
#' This function is used in \code{\link{RSAModelLL1_1simpleRSA4TrialsIterative}},
#'
#' \code{\link{RSAModelLL1_1simpleRSA4TrialsIterative_notObey.1}},
#'
#' \code{\link{RSAModelLL1_1simpleRSA4TrialsIterative_notObey.2}},
#'
#' \code{\link{RSAModelLL1_2simpleRSA4TrialsIterative}},
#'
#' \code{\link{RSAModelLL1_2simpleRSA4TrialsIterative_pref.2}},
#'
#' \code{\link{RSAModelLL2_simpleRSA4TrialsIterative}}.
#' @export
RSAModelKLDiv3params_simpleRSA4TrialsIterative<- function(data, par1, par2) {
  #  print(params)
  llRes <- 0
  for(i in c(1:nrow(data))) {
    if( (i-1)%%4 == 0) {
      preferencesPriorAll <- getPreferencesPrior(data[i,5]) # focussing on the feature type in question.
    }
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    ##
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    prefPostAll <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(currentObjects, uttFeat, abs(par1), abs(par2), preferencesPriorAll)
    ## adding the KL Divergence terms of the relevant feature values for the two sets of answers.
    ##
    ## adding the negative log likelihoods
    for(j in c(1:3)) {
      llRes <- llRes + data[i, 5+j] *
        ( log(data[i, 5+j] + 1e-100) - log( prefPostAll[j + (data[i, 5]-1)*3] + 1e-100) )
    }
    preferencesPriorAll <- prefPostAll
  }
  return(llRes)
}



####
#' Cost function for one parameter optimization (for independent trials).
#' Optimizing softness.
#' Non-obedience is fixed at 0.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; The softness parameter is optimized.
#'
#' The non-obedience parameter is fixed.
#' @param params One value vector, which specifies one of two parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue is optimized, i.e.The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience is fixed at 0, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#'    (0 = full obedience, infinity = full instruction ignorance)}
#'}
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
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelKLDiv3params_simpleRSA4TrialsIndependent}}.
#' @export
RSAModelLL1_1simpleRSA4TrialsIndependent <- function(params,  data) {
  return(RSAModelKLDiv3params_simpleRSA4TrialsIndependent(data, abs(params[1]), 0))
}


#' Cost function for one parameter optimization (for independent trials).
#' Optimizing softness.
#' Non-obedience is fixed at 0.1.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; The softness parameter is optimized.
#'
#' The non-obedience parameter is fixed.
#'
#' @param params One value vector, which specifies one of two parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue is optimized, i.e.The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience is fixed at 0.1, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#'   (0 = full obedience, infinity = full instruction ignorance)}
#'}
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
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelKLDiv3params_simpleRSA4TrialsIndependent}}.
#' @export
RSAModelLL1_1simpleRSA4TrialsIndependent_notObey.1 <- function(params,  data) {
  return(RSAModelKLDiv3params_simpleRSA4TrialsIndependent(data, abs(params[1]), .1))
}

#' Cost function for one parameter optimization (for independent trials).
#' Optimizing softness.
#' Non-obedience is fixed at 0.2.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; The softness parameter is optimized.
#'
#' The non-obedience parameter is fixed.
#' @param params One value vector, which specifies one of two parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue is optimized, i.e.The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience is fixed at 0.2, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#'    (0 = full obedience, infinity = full instruction ignorance)}
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
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelKLDiv3params_simpleRSA4TrialsIndependent}}.
#' @export
RSAModelLL1_1simpleRSA4TrialsIndependent_notObey.2 <- function(params,  data) {
  return(RSAModelKLDiv3params_simpleRSA4TrialsIndependent(data, abs(params[1]), .2))
}


#' Cost function for one parameter optimization (for independent trials).
#' Optimizing non-obedience.
#' Softness is fixed at 0.
#'
#' @description
#' Simple RSA
#'
#' 1 parameter optimization; The non-obedience parameter is optimized.
#'
#' The softness parameter is fixed.
#'
#' @param params One value vector, which specifies one of two parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue is is fixed at 0, i.e.The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience is optimized, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#'   (0 = full obedience, infinity = full instruction ignorance)}
#'}.
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
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelKLDiv3params_simpleRSA4TrialsIndependent}}.
#' @export
RSAModelLL1_2simpleRSA4TrialsIndependent <- function(params,  data) {
  return(RSAModelKLDiv3params_simpleRSA4TrialsIndependent(data, 0, abs(params[1])))
}

#' Cost function for one parameter optimization (for independent trials).
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
#' @param params One value vector, which specifies one of two parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue is is fixed at 0.2, i.e.The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience is optimized, The extent to which the instruction of the speaker is obeyed by the listener. #'   (0 = full obedience, infinity = full instruction ignorance)}
#'}.
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
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelKLDiv3params_simpleRSA4TrialsIndependent}}.
#' @export
RSAModelLL1_2simpleRSA4TrialsIndependent_pref.2 <- function(params,  data) {
  return(RSAModelKLDiv3params_simpleRSA4TrialsIndependent(data, 0.2, abs(params[1])))
}


####
#' Cost function for two parameter optimization (for independent trials).
#' Optimizing softness and non-obedience.
#'
#' @description
#' Simple RSA
#'
#' 2 parameter optimization; The softness and non-obedience parameter are optimized.
#'
#' @param params Two value vector, which specifies two of two parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue is optimized, i.e.The strength of "preferring one entity over others". (The larger the value the higher the tendency towards uniform liking)}
#'   \item{non-obedience is optimized, i.e. The extent to which the instruction of the speaker is obeyed by the listener.
#'    (0 = full obedience, infinity = full instruction ignorance)}
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
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelKLDiv3params_simpleRSA4TrialsIndependent}}.
#' @export
RSAModelLL2_simpleRSA4TrialsIndependent <- function(params,  data) {
  return(RSAModelKLDiv3params_simpleRSA4TrialsIndependent(data, abs(params[1]), abs(params[2])))
}

#' Simple RSA model Kullback-Leibler divergence determination
#' (for independent trials)
#'
#' @description
#' Simple RSA
#'
#' The function calculates the optimal parameter values of the free parameters by estimating the log-likelihood of the
#' RSA model given model parameters and data. It also determines the actual RSA model Kullback-Leibler divergence.
#'
#' 2 parameter optimization considering only the available feature values present in the scene, i.e. feature values of shape, texture and color.
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
#'
#' @details
#' This function is used in \code{\link{RSAModelLL1_1simpleRSA4TrialsIndependent}},
#'
#' \code{\link{RSAModelLL1_1simpleRSA4TrialsIndependent_notObey.1}},
#'
#' \code{\link{RSAModelLL1_1simpleRSA4TrialsIndependent_notObey.2}},
#'
#' \code{\link{RSAModelLL1_2simpleRSA4TrialsIndependent}},
#'
#' \code{\link{RSAModelLL1_2simpleRSA4TrialsIndependent_pref.2}},
#'
#' \code{\link{RSAModelLL2_simpleRSA4TrialsIndependent}}.
#' @export
RSAModelKLDiv3params_simpleRSA4TrialsIndependent<- function(data, par1, par2) {
  #  print(params)
  llRes <- 0
  for(i in c(1:nrow(data))) {
    preferencesPriorAll <- getPreferencesPrior(data[i,5]) # focussing on the feature type in question.
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    ##
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    prefPostAll <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(currentObjects, uttFeat, abs(par1), abs(par2), preferencesPriorAll)
    ## adding the KL Divergence terms of the relevant feature values for the two sets of answers.
    ##
    ## adding the negative log likelihoods
    for(j in c(1:3)) {
      llRes <- llRes + data[i, 5+j] *
        ( log(data[i, 5+j] + 1e-100) - log( prefPostAll[j + (data[i, 5]-1)*3] + 1e-100) )
    }
    preferencesPriorAll <- prefPostAll
  }
  return(llRes)
}

###
# Hania: not included in the package because it's deprecated (16.10.20)
# Listener's posterior preferences for all object constellations
#
# Simple RSA
# get matrix of all posteriors of all object constellations and object choices possible.
# @param softPrefValue is fixed at 0;in \code{[0,infinity)} (the larger the higher the tendency towards uniform liking).
#
# Value reflects how categorical the listener's preferences are:
#
# 0: always pick the preferred object. If the listener prefers red objects, she will always pick the red object.
# infinity: It is as likely for the listener to pick green, blue or red objects.
# @param nonObedience is fixed at 0; determines if the instruction does not need to be obeyed (0 = full obedience: -> infty  = full instruction ignorance).
# @param Exponential scaling of speaker to utter that value that maximizes the chance of getting the target object right.
# Value between 0 and 1.
# @return resultMat .
# @examples
# @export
getPostListPrefsForAllConstellations_simpleRSADEPRECATED <- function(softPrefValue=0, nonObedience=0, alpha=1) {
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
          resultMat[row,20:28] <- determineSpeakerPostListPrefsSimpleRSA(objectConstellation, featChoice,
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
# determineSpeakerPostListPrefsSimpleRSA(currentObjects, 1, 0, 0, 1)
# determineSpeakerPostListPrefsSimpleRSA(currentObjects, 2, 0, 0, 1)
# determineSpeakerPostListPrefsSimpleRSA(currentObjects, 3, 0, 0, 1)

# currentObjects <- c(1,4,16)
# print(c("Objects:",allObjects[currentObjects,]))
# determineSpeakerPostListPrefsSimpleRSA(currentObjects, 1, 0, 0, 1)
# determineSpeakerPostListPrefsSimpleRSA(currentObjects, 2, 0, 0, 1)
# determineSpeakerPostListPrefsSimpleRSA(currentObjects, 3, 0, 0, 1)
# currentObjects <- c(1,4,7)
# print(c("Objects:",allObjects[currentObjects,]))
# determineSpeakerPostListPrefsSimpleRSA(currentObjects, 1, 0, 0, 1)
# determineSpeakerPostListPrefsSimpleRSA(currentObjects, 2, 0, 0, 1)
# determineSpeakerPostListPrefsSimpleRSA(currentObjects, 3, 0, 0, 1)

#Generating the big table...
# bigTablePostListPrefs <- getPostListPrefsForAllConstellations()


