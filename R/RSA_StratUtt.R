#source("R/AllUtterancesAndObjects.R")

#' Calculate Kullback-Leibler divergence
#'
#' @description
#' Divergence measure.
#'
#' Simple KL divergence function- with small offset to tolerate \code{ p = 0 / q = 0}
#'
#' @param p Probability distribution.
#'
#' @param q Probability distribution.
#'
#' @return A scalar value (The divergence between p and q).
#'
#' @details
#' Full-RSA and Simple-RSA
#' @export
KLdivergence <- function(p, q) {
  toleranceOffset <- 1e-20
  return(max(0, sum(p * (log( (toleranceOffset + p) / (toleranceOffset + q) ) ) ) ) )
}

#
# Modeling speaker and listener
#' Literal listener function
#'
#' @description
#' Full-RSA
#'
#' Literal listener function according to assigned listener's object preferences.
#' @param utterance The uttered word by the speaker that the listener hears.
#'
#' An index referring to one of the values in the vector validUtterances.
#'
#' @param listenerObjectPreferences One of the rows of the list of preference priors
#' for all valid utterances based on the object in the scene.
#'
#' The list has as many rows as the length of the validUtterances vector + 1.
#'
#' Each row in the list contains a vector of length 3, as there are three objects in the scene.
#'
#' The extra row is for the case of no feature preferences whatsoever, i.e. uniform prior over all three objects in the scene.
#'
#' @param uttToObjProbs A matrix. The rows map each possible utterance that corresponds to each present feature value
#' of the current objects. The columns represent the three objects in the scene.
#'
#' This reflects the obedience-parameter and which objects match the respective utterance.
#' The matrix shows the probability that a certain object is chosen following a certain utterance, that is valid in the scene.
#' The number of rows of the matrix match the length of the validUtterances vector.
#'
#' @return P(obj | utt, listener's object preferences)
#'
#' A Vector of length 3.
#' It includes the normalized probability of choosing each of the three objects in the scene,
#' given the utterance and the listener's object preferences.
#'
#' @examples
#' listenerObjectPreferences <- objectPreferenceSoftPriors[[3]]
#' \donttest{literalListener(utterance, listenerObjectPreferences, uttToObjProbs)}
#'
#' output:
#' [1] 1 0 0
#' @export
literalListener <- function(utterance, listenerObjectPreferences, uttToObjProbs) {
  objPosterior <- uttToObjProbs[utterance,] # * (listenerObjectPreferences + 1e-100)
  if(sum(objPosterior)==0) {
    return(objPosterior)
  }
  return(objPosterior / sum(objPosterior))
}


#' Get the speaker's utterances' priors
#'
#' @description
#' Full-RSA
#'
#' This function determines the prior utterance preferences of the speaker.
#'
#' @param validUtterances A vector of utterances that correspond to all feature values present
#' in the current objects in the scene.
#'
#' For example, it only makes sense to utter \emph{"red"} in a scene if there are \emph{red} objects present.
#'
#' @return A vector of priors as numeric values.
#' @examples
#' \donttest{getSpeakerUtterancePriors(validUtterances)}
#'
#' output:
#' [1] 0.2 0.2 0.2 0.2 0.2
#' @export
getSpeakerUtterancePriors <- function(validUtterances) {
  return(rep(1./length(validUtterances), length(validUtterances) ) )
}


#' Speaker function
#'
#' @description
#' Full-RSA
#'
#' This function determines the utterances that increase the likelihood of the listener
#' picking the object the speaker is referring to.
#' @param obj A vector of three values in \code{{1,...,27}} specifying the target and the other two objects in the scene.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#' @param listenerObjectPreferences One of the rows of the list of preference priors
#' for all valid utterances based on the object in the scene.
#'
#' The list has as many rows as the length of the validUtterances vector + 1.
#'
#' Each row in the list contains a vector of length 3, as there are three objects in the scene.
#'
#' The extra row is for the case of no feature preferences whatsoever, i.e. uniform prior over all three objects in the scene.
#'
#'
#' @param validUtterances A vector of utterances that correspond to all feature values present
#' in the current objects in the scene.
#'
#' For example, it only makes sense to utter \emph{"red"} in a scene if there are \emph{red} objects present.
#'
#' @param uttToObjProbs A matrix. The rows map each possible utterance that corresponds to each present feature value
#' of the current objects. The columns represent the three objects in the scene.
#'
#' This reflects the obedience-parameter and which objects match the respective utterance.
#' The matrix shows the probability that a certain object is chosen following a certain utterance, that is valid in the scene.
#' The number of rows of the matrix match the length of the validUtterances vector.
#' @param alpha A parameter between 0 and 1.
#'
#' Exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.
#' @return P(utt | obj, listener's object preferences)
#'
#' A vector of normalized probability for a certain utterance given object
#' and listener's object preferences.
#'
#' The vector has the same length as the validUtterances vector.
#' @examples
#' listenerObjectPreferences <- objectPreferenceSoftPriors[[3]]
#' \donttest{speaker(obj, listenerObjectPreferences, validUtterances, uttToObjProbs, alpha)}
#'
#' output:
#' [1] 0.6 0.0 0.0 0.2 0.2
#'
#' @export
speaker <- function(obj, listenerObjectPreferences, validUtterances, uttToObjProbs, alpha) {
  priors <- getSpeakerUtterancePriors(validUtterances)  # prior over speaker utterances
  pugobj = rep(0, length(validUtterances))
  for (i in c(1:length(validUtterances))) { # considering all possible utterances
    #
    ll <- literalListener(i, listenerObjectPreferences, uttToObjProbs) # P_L0(obj | utterance i)
    #
    if(ll[obj]==0) {
      pugobj[i] <- 0
    }else{
      pugobj[i] <- exp(alpha * log(ll[obj]) - 0 ) * priors[i]
    }
    #print(c(i, psugs))
  }
  if(sum(pugobj) == 0) {
    return(pugobj)
  }
  return(pugobj / sum(pugobj) )
}

#' Pragmatic listener function samples over the speaker
#'
#' @description
#' Full-RSA
#'
#' Determines the probability of the listener choosing a certain object given
#' the speaker's utterance and her own object preferences.
#'
#' P(obj | utt, listener's object preferences).
#'
#' It essentially infers which object it should pick under a certain literal listener preference.
#' @param utterance The uttered word by the speaker that the listener hears.
#'
#' An index referring to one of the values in the vector validUtterances.
#' @param listenerObjectPreferences One of the rows of the list of preference priors
#' for all valid utterances based on the object in the scene.
#'
#' The list has as many rows as the length of the validUtterances vector + 1.
#'
#' Each row in the list contains a vector of length 3, as there are three objects in the scene.
#'
#' The extra row is for the case of no feature preferences whatsoever, i.e. uniform prior over all three objects in the scene.
#'
#' @param validUtterances A vector of utterances that correspond to all feature values present
#' in the current objects in the scene.
#'
#' For example, it only makes sense to utter \emph{"red"} in a scene if there are \emph{red} objects present.
#' @param currentObjects Vector of three values in \code{{1,...,27}} specifying the target and the other two objects.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#'
#' @param uttToObjProbs A matrix. The rows map each possible utterance that corresponds to each present feature value
#' of the current objects. The columns represent the three objects in the scene.
#'
#' This reflects the obedience-parameter and which objects match the respective utterance.
#' The matrix shows the probability that a certain object is chosen following a certain utterance, that is valid in the scene.
#' The number of rows of the matrix match the length of the validUtterances vector.
#'
#' @param alpha A parameter between 0 and 1.
#'
#' Exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.
#' @return A vector of the normalized probability of the listener choosing a certain object given
#' the speaker's utterance and her own object preferences.
#' @examples
#' \donttest{listenerObjectPreferences <- objectPreferenceSoftPriors[[2]]}
#' \donttest{pragmaticListener(utterance, listenerObjectPreferences,
#' validUtterances, currentObjects, uttToObjProbs, alpha)}
#'
#' output:
#' [1] 1 0 0
#'
#' To see which object are in the object constellation run:
#' \donttest{allObjects[currentObjects,]}
#'
#' output:
#'       [,1]     [,2]    [,3]
#'[1,] "cloud"  "solid" "blue"
#'[2,] "circle" "solid" "blue"
#'[3,] "square" "solid" "blue"
#' @export
pragmaticListener <- function(utterance, listenerObjectPreferences,
                              validUtterances, currentObjects, uttToObjProbs, alpha) {
  probObjGivenUtt <- rep(0, length(currentObjects))
  for(i in c(1:length(currentObjects))) {
    #    print(c("speaker with utt:", utterance, speaker(i, listenerObjectPreferences, validUtterances)))
    probObjGivenUtt[i] <- speaker(i, listenerObjectPreferences, validUtterances, uttToObjProbs, alpha)[utterance] *
      (listenerObjectPreferences[i] + 1e-100) ## slight offset to prevent 0 cases
  }
  #  print(c("PL:", probObjGivenUtt, "LOP:", listenerObjectPreferences))
  if(sum(probObjGivenUtt)==0) {
    return(probObjGivenUtt)
  }
  return (probObjGivenUtt / sum(probObjGivenUtt))
}

#' Pragmatic Speaker function
#'
#' @description
#' Full-RSA
#'
#' This pragmatic speaker considers all "imaginable" (i.e. implemented)
#' preference distributions - over objects - of the listener.
#'
#' It starts with a prior assumption over the possible preferences of the listener.
#' Then it infers the posterior over these preferences given an object choice of the listener
#' i.e. P(listener's feature value preferences | utterance, object choice by the listener, prior over preferences)
#' @param utterance The uttered word by the speaker that the listener hears.
#'
#' An index referring to one of the values in the vector validUtterances.
#'
#' @param obj The object chosen by the listener. A value referring to the index 1,2 or 3.
#' @param preferencesPrior A vector of length 9.
#'
#' Probability mass over all feature values present in the scenario plus a "no preference" case.
#'
#' Gives a prior preferences distribution over all (nine) feature values.
#' @param validUtterances A vector of utterances that correspond to all feature values present
#' in the current objects in the scene.
#'
#' For example, it only makes sense to utter \emph{"red"} in a scene if there are \emph{red} objects present.
#' @param currentObjects A vector of three values in \code{{1,...,27}} specifying the target and the other two objects in the scene.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#' @param uttToObjProbs A matrix. The rows map each possible utterance that corresponds to each present feature value
#' of the current objects. The columns represent the three objects in the scene.
#'
#' This reflects the obedience-parameter and which objects match the respective utterance.
#' The matrix shows the probability that a certain object is chosen following a certain utterance, that is valid in the scene.
#' The number of rows of the matrix match the length of the validUtterances vector.
#'
#' @param objectPreferenceSoftPriors A list of preference priors for all valid utterances based on the object in the scene.
#'
#' The list has as many rows as the length of the validUtterances vector + 1.
#'
#' Each row in the list contains a vector of length 3, as there are three objects in the scene.
#'
#' The extra row is for the case of no feature preferences whatsoever, i.e. uniform prior over all three objects in the scene.
#'
#' @param alpha A parameter between 0 and 1.
#'
#' Exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.
#' @return A vector with the same as the validUtterances vector.
#'
#' Normalized posterior probability over preferences- given the utterance, the object choice by the listener, and prior over preferences of the listener.
#' @examples
#'
#' \donttest{pragmaticSpeaker(utterance, obj, preferencesPrior, validUtterances,
#' currentObjects, uttToObjProbs, objectPreferenceSoftPriors, alpha)}
#'
#' output:
#' [1] 0.17 0.17 0.17 0.17 0.17 0.17
#'
#' @export
pragmaticSpeaker <- function(utterance, obj, preferencesPrior,
                             validUtterances, currentObjects, uttToObjProbs,
                             objectPreferenceSoftPriors, alpha) {
  prefPost <- rep(0, length(validUtterances)+1) # +1 because of uniform
  for(pref in c(1:length(preferencesPrior))) { # prior over the preferences the speaker is interested in
    if(preferencesPrior[pref] > 0) {
      #      print(c(utterance, objectPreferenceSoftPriors[[pref]]))
      pp <- pragmaticListener(utterance, objectPreferenceSoftPriors[[pref]],
                                          validUtterances, currentObjects, uttToObjProbs, alpha)
      prefPost[pref] <- pp[obj] * preferencesPrior[pref]
#      print(c(pref, pp, preferencesPrior))
    }
    #    print(c("PrefPost[]:", pref, prefPost[pref]))
  }
#  print(c("PrefPost:",prefPost))
#  print(c(utterance, obj, preferencesPrior, "rel",
#          validUtterances,currentObjects))
#  print(uttToObjProbs)
#  print(objectPreferenceSoftPriors)
  if(sum(prefPost) == 0) { # no evidence for any preferences... -> keep believing in the prior!
    return(prefPost)
  }
  return(prefPost / sum(prefPost))
}

#' Best information gain utterances
#'
#' @description
#' Full-RSA
#'
#' The ultimate function that determines the utterance preferences of a rather
#' "informed", "pragmatic" speaker considering all possible scenarios.
#' That is, hypothetically, all utterances are considered. Additionally, the resulting
#' inferred listener's object preferences are computed assuming the listener
#' picks a certain object and has certain object preferences.
#'
#' U(utt | listener's object preference priors) is computed.
#' The utility is determined as the information gain between prior and posterior of the
#' determined listener's object preferences.
#' @param preferencesPrior A vector of the length the validUtterances vector + 1.
#'
#' It constructed as such:
#'
#' \code{preferencesPrior <- rep(1/(length(validUtterances)+1), length(validUtterances)+1).}
#'
#' The vector contains the probability mass over all feature values present in the scenario plus a "no preference" case.
#'
#' Gives a prior preferences distribution over the feature values in the scene.
#' @param validUtterances A vector of utterances that correspond to all feature values present
#' in the current objects in the scene.
#'
#' For example, it only makes sense to utter \emph{"red"} in a scene if there are \emph{red} objects present.
#' @param currentObjects A vector of three values in \code{{1,...,27}} specifying the target and the other two objects in the scene.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#' @param uttToObjProbs A matrix.
#'
#' The rows map each possible utterance that corresponds to each present feature value
#' of the current objects.
#' The number of rows of the matrix match the length of the validUtterances vector.
#'
#' The columns represent the three objects in the scene.
#'
#' This reflects the obedience-parameter and which objects match the respective utterance.
#' The matrix shows the probability that a certain object is chosen following a certain utterance,
#' that is valid in the scene.
#'
#' @param objectPreferenceSoftPriors A list of preference priors for all valid utterances based on the object in the scene.
#'
#' The list has as many rows as the length of the validUtterances vector + 1.
#'
#' Each row in the list contains a vector of length 3, as there are three objects in the scene.
#'
#' The extra row is for the case of no preferences whatsoever, i.e. uniform prior over all three objects in the scene.
#' @param alpha A parameter between 0 and 1. (Here it's set to = 1)
#'
#' Exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.
#'
#' @param klValueFactor A parameter that can be negative, 0 or positive (Here it is set to = 1):
#' \describe{
#' \item{zero}{Don't care about learning about the feature preferences of the listener}
#' \item{positive}{Care about learning about the feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }
#' @return A vector containing the normalized probability over utterances given the listener's object preference priors.
#'
#' The utterance with the highest probability is the one that maximizes the information gain for the speaker.
#'
#' The vector has the same length as the validUtterances vector.
#' @examples
#' \donttest{currentObjects <- c(1,2,3)}
#' \donttest{allObjects[currentObjects,]}
#'
#' output:
#'
#'       shape    pattern color
#' [1,] "cloud"  "solid" "blue"
#' [2,] "circle" "solid" "blue"
#' [3,] "square" "solid" "blue"
#'
#'
#' \donttest{bestInfGainUtterance(preferencesPrior, validUtterances,
#'
#' currentObjects,uttToObjProbs, objectPreferenceSoftPriors,
#'
#' alpha=1, klValueFactor=1)}
#'
#' output:
#' [1] 0.0 0.0 0.0 0.5
#'
#' Since the all the objects present in the scene are solid and blue,
#' uttering solid or blue, would be optimal to learn something
#' about the shape preferences of the listener.
#' This means the speaker would have the best information gain.
#'
#' @export
bestInfGainUtterance <- function(preferencesPrior, validUtterances, currentObjects,
                                 uttToObjProbs, objectPreferenceSoftPriors, alpha=1,
                                 klValueFactor=1) {
  InfGainUttPosterior <- rep(0, length(validUtterances))
  utterancePrior <- getSpeakerUtterancePriors(validUtterances) # prior over speaker utterances
  #
  for(utt in c(1:length(validUtterances))) { # evaluating the usage of a particular utterance utt
    prefPostAll <- rep(0, length(preferencesPrior))
    for(pref in c(1:length(preferencesPrior))) { # prior over the preferences the speaker is interested in
      ### What is the likelihood that this particular preference prior is the correct one?
      prefPost <- 0
      for(obj in c(1:length(currentObjects)) ) {
        if(uttToObjProbs[utt,obj] > 0) {
          if(preferencesPrior[pref] > 0) { # only pay attention to preferences with non-zero probability
            objPrefPosterior <- pragmaticSpeaker(utt, obj, preferencesPrior,
                                                 validUtterances, currentObjects,
                                                 uttToObjProbs, objectPreferenceSoftPriors, alpha)
            # print(objPrefPosterior)
            KLvalue <- KLdivergence(preferencesPrior, objPrefPosterior)
 #           if(KLvalue > 0) {
              # log-likelihood interpretation of KLvalue:
              prefPost <- prefPost + uttToObjProbs[utt,obj] * utterancePrior[utt] *
                                              preferencesPrior[pref] * objectPreferenceSoftPriors[[pref]][obj] *
                                     exp(klValueFactor * KLvalue)
#              prefPost <- prefPost + exp( log(uttToObjProbs[utt,obj] * utterancePrior[utt] *
#                                                preferencesPrior[pref] * objectPreferenceSoftPriors[[pref]][obj]) +
#                                            klValueFactor * KLvalue)

              #              print(c(utt,KLvalue, exp(klValueFactor * KLvalue), prefPost))
              # direct likelihood interpretation of KLvalue
#                            prefPost<- prefPost + uttToObjProbs[utt,obj] * utterancePrior[utt] *
#                              preferencesPrior[pref] * objectPreferenceSoftPriors[[pref]][obj] * klValueFactor * KLvalue
            }
            #            prefPost <- prefPost + KLvalue #utterancePrior[utt] *
            #              objPrefPosterior[pref] *
            #             objectPreferenceSoftPriors[[pref]][obj]
 #         }
        }
      }
      if(prefPost > 0) {
        prefPostAll[pref] <- prefPost
      }
    }
    InfGainUttPosterior[utt] <- sum(prefPostAll)
  }
  if(sum(InfGainUttPosterior) == 0) # no gain from any utterance...
    return( rep(1/length(validUtterances), length(validUtterances)) )
  return(InfGainUttPosterior / sum(InfGainUttPosterior))
}

# ## Tests 1:
# notObeyInst <- 1e-10
# softPrefValue <- 0.1
# alpha <- .1
# currentObjects <- c(1,2,3)
# validUtterances <- determineValidUtterances(currentObjects)
# mapObjToUtt <- mapObjectToUtterances(currentObjects)
# uttToObjProbs <- determineUttToObjectProbs(validUtterances,
#                                                             currentObjects,
#                                                             mapObjToUtt, notObeyInst)
# objectPreferenceSoftPriors <- getObjectPreferencePriors(validUtterances, currentObjects,
#                                                         softPrefValue, uttToObjProbs)
# #pragmaticSpeaker <- function(utterance, obj, preferencesPrior,
# #                             validUtterances, currentObjects, uttToObjProbs,
# #                             objectPreferenceSoftPriors, alpha) {
# pragmaticSpeaker(4, 1, c(0, 0, 0, 0, 0, 1), validUtterances, currentObjects,
#                  uttToObjProbs, objectPreferenceSoftPriors, alpha) # sanity check - definite prior, no inf. gain possible
# pragmaticSpeaker(4, 1, c(.2, .2, .2, .2, .2, 0), validUtterances, currentObjects,
#                  uttToObjProbs, objectPreferenceSoftPriors, alpha) # NON compliant listener...
#
## Tests 2:
# notObeyInst <- 0
# softPrefValue <- .01
# alpha <- 1
# currentObjects <- c(1,2,6)
# validUtterances <- determineValidUtterances(currentObjects)
# mapObjToUtt <- mapObjectToUtterances(currentObjects)
# uttToObjProbs <- determineUttToObjectProbs(validUtterances,
#                                                             currentObjects,
#                                                             mapObjToUtt, notObeyInst)
# objectPreferenceSoftPriors <- getObjectPreferencePriors(validUtterances, currentObjects,
#                                                         softPrefValue, uttToObjProbs)
#bestInfGainUtterance <- function(preferencesPrior, validUtterances, currentObjects,
#                                 uttToObjProbs, objectPreferenceSoftPriors, alpha)
#bestInfGainUtterance(c(0, 0, 0, 0, 0, 0, 1), validUtterances, currentObjects,
#                     uttToObjProbs, objectPreferenceSoftPriors, alpha) # sanity check - definite prior, no inf. gain possible
#round(bestInfGainUtterance(c(.2, .2, .2, .2, .2, .2, 0), validUtterances, currentObjects,
#                     uttToObjProbs, objectPreferenceSoftPriors, alpha, -10), 3) # sanity check - definite prior, no inf. gain possible
#
# kldFact <- (c(0:200)-100)/2
# kldRes <- matrix(0,length(kldFact),6)
# for(i in c(1:length(kldFact))) {
#   kldRes[i,] <- round(bestInfGainUtterance(c(.1666, .1666, .1666, .1666, .1666, .1666, 0), validUtterances, currentObjects,
#                              uttToObjProbs, objectPreferenceSoftPriors, alpha, kldFact[i]), 3) # sanity check - definite prior, no inf. gain possible
# }
# plot(kldFact, kldRes[,1], ylim = c(0:1))
# lines(kldFact, kldRes[,2], col="black")
# lines(kldFact, kldRes[,3], col="grey")
# lines(kldFact, kldRes[,4], col="yellow")
# lines(kldFact, kldRes[,5], col="orange")
# lines(kldFact, kldRes[,6], col="blue")
#
# bestInfGainUtterance(c(.1666, .1666, .1666, .1666, .1666, .1666, 0), validUtterances, currentObjects,
#                      uttToObjProbs, objectPreferenceSoftPriors, alpha, kldFact[i])
#
# round(pragmaticSpeaker(4, 1, c(.1666, .1666, .1666, .1666, .1666, .1666, 0),
#                              validUtterances, currentObjects, uttToObjProbs,
#                              objectPreferenceSoftPriors, alpha), 3)
#

