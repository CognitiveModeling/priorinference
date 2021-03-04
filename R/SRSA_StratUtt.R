#source("R/AllUtterancesAndObjects.R")

# not included, because it is included already in the package through RSA_StratUtt.R
# Kullback-Leibler divergence
#
# Simple RSA
#
#Simple KL divergence function- with small offset to tolerate p=0/q=0
# @param p probability distribution.
# @param q probability distribution.
# @return a scalar value.
# @examples
# KLdivergence(p,q)
# @export
KLdivergence <-
  function(p, q) {
    toleranceOffset <- 1e-20
    return(max(0, sum(p * (log(
      (toleranceOffset + p) / (toleranceOffset + q)
    )))))
  }

#' Simple Listener function
#'
#' @description
#' Simple RSA
#'
#' The simple listener function determines the listener's object choice given the
#' present objects in the scene and her preferences.
#'
#' P(obj | utt, listener's object preferences)
#' @param utterance The uttered word by the speaker that the listener hears.
#'
#' An index referring to one of the values in the vector validUtterances.
#' @param uttToObjProbs A matrix. The rows map each possible utterance that corresponds to each present feature value
#' of the current objects. The columns represent the three objects in the scene.
#'
#' This reflects the obedience-parameter and which objects match the respective utterance.
#' The matrix shows the probability that a certain object is chosen following a certain utterance, that is valid in the scene.
#' The number of rows of the matrix match the length of the validUtterances vector.
#' @param listenerObjectPreferences One of the rows of the list of preference priors
#' for all valid utterances based on the object in the scene.
#'
#' The list has as many rows as the length of the validUtterances vector + 1.
#'
#' Each row in the list contains a vector of length 3, as there are three objects in the scene.
#'
#' The extra row is for the case of no feature preferences whatsoever, i.e. uniform prior over all three objects in the scene.
#' @return A Vector of length 3.
#' It includes the normalized probability of choosing each of the three objects in the scene,
#' given the utterance and the listener's object preferences.
#'
#' @examples
#' \donttest{simpleListener(utterance, uttToObjProbs, listenerObjectPreferences)}
#'
#' output:
#' [1] 1 0 0
#' @export
simpleListener <-
  function(utterance,
           uttToObjProbs,
           listenerObjectPreferences) {
    objPosterior <-
      uttToObjProbs[utterance, ] * (listenerObjectPreferences + 1e-100)
    if (sum(objPosterior) == 0) {
      return(objPosterior)
    }
    return(objPosterior / sum(objPosterior))
  }

#' Simple pragmatic speaker function
#'
#' @description
#' Simple RSA
#'
#' The simple pragmatic speaker considers all "imaginable" (i.e. implemented)
#' preference distributions over objects of the listener.
#' It starts with a prior assumption over the possible listener's preferences.
#' It then infers the posterior over these preferences given the listener makes a particular object choice.
#' P(listener's feature value preferences | utterance, object choice by the listener,
#'  prior over listener's feature value preferences)
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
#' @param objectPreferenceSoftPriors A list of preference priors for all valid utterances based on the object in the scene.
#'
#' The list has as many rows as the length of the validUtterances vector + 1.
#'
#' Each row in the list contains a vector of length 3, as there are three objects in the scene.
#'
#' The extra row is for the case of no feature preferences whatsoever, i.e. uniform prior over all three objects in the scene.
#' @return A vector with the same as the validUtterances vector.
#'
#' Normalized posterior probability density over preferences- given the utterance, the object choice by the listener, and prior over preferences of the listener.
#' @examples
#' \donttest{simplePragmaticSpeaker(utterance, obj, preferencesPrior,
#' validUtterances, currentObjects, uttToObjProbs, objectPreferenceSoftPriors)}
#'
#' output:
#' [1] 0.17 0.17 0.17 0.17 0.17 0.17
#'
#' @export
simplePragmaticSpeaker <-
  function(utterance,
           obj,
           preferencesPrior,
           validUtterances,
           currentObjects,
           uttToObjProbs,
           objectPreferenceSoftPriors) {
    prefPost <- rep(0, length(preferencesPrior)) # NOTE: length(preferencesPrior == length(validUtterances) + 1
    for (pref in c(1:length(preferencesPrior))) {
      # prior over the preferences the speaker is interested in
      if (preferencesPrior[pref] > 0) {
        pp <-
          simpleListener(utterance,
                         uttToObjProbs,
                         objectPreferenceSoftPriors[[pref]])
        prefPost[pref] <- pp[obj] * preferencesPrior[pref]
      } # else{} # prior preference for this perference is zero
    } # done with considering all possible preferences.
    if (sum(prefPost) == 0) {
      # no evidence for any preferences... -> no inference
      return(preferencesPrior)
    }
    return(prefPost / sum(prefPost))
  }

#' Simple pragmatic speaker with all prior preferences
#'
#' @description
#' Simple RSA
#'
#' The simple pragmatic speaker considers all "imaginable" (i.e. implemented)
#' preference distributions over objects of the listener.
#'
#' Starting with a prior assumption over the possible listener's preferences.
#' It then infers the posterior over these preferences given the listener makes a particular object choice.
#' P(listener's feature value preferences | utterance, object choice by the listener,
#' prior over listener's feature value preferences).
#' @param utterance The uttered word by the speaker that the listener hears.
#'
#' An index referring to one of the values in the vector validUtterances.
#' @param obj The object chosen by the listener. A value referring to the index 1,2 or 3.
#'
#' @param preferencesPriorAll A vector of length 9.
#'
#' Probability mass over all feature values.
#'
#' Gives a prior preferences distribution over all (nine) feature values.
#'
#' \code{preferencesPriorAll <- rep(1/9, 9)}
#' @param validUtterances A vector of utterances that correspond to all feature values present
#'  in the current objects in the scene.
#'
#' For example, it only makes sense to utter \emph{"red"} in a scene if there are \emph{red} objects present.
#' @param currentObjects Vector of three values in \code{{1,...,27}} specifying the target and the other two objects.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#' @param uttToObjProbs A matrix. The rows map each possible utterance that corresponds to each present feature value
#' of the current objects. The columns represent the three objects in the scene.
#'
#' This reflects the obedience-parameter and which objects match the respective utterance.
#' The matrix shows the probability that a certain object is chosen following a certain utterance, that is valid in the scene.
#' The number of rows of the matrix match the length of the validUtterances vector.
#' @param objectPreferenceSoftPriors A list of preference priors for all valid utterances based on the object in the scene.
#'
#' The list has as many rows as the length of the validUtterances vector + 1.
#'
#' Each row in the list contains a vector of length 3, as there are three objects in the scene.
#'
#' The extra row is for the case of no feature preferences whatsoever, i.e. uniform prior over all three objects in the scene.
#'
#' @return A vector of length 9. It contains the normalized probability over preferences (priors).
#'
#' @examples
#' \donttest{simplePragmaticSpeakerWithPrefPriorAll(utterance, obj,
#' preferencesPriorAll, validUtterances,
#' currentObjects, uttToObjProbs, objectPreferenceSoftPriors)}
#'
#' output:
#' [1] 0.12  0.12  0.12  0.12  0.12  0.12 0.12  0.12  0.12
#' @export
simplePragmaticSpeakerWithPrefPriorAll <-
  function(utterance,
           obj,
           preferencesPriorAll,
           validUtterances,
           currentObjects,
           uttToObjProbs,
           objectPreferenceSoftPriors) {
    #cat("preferencesPriorAll", preferencesPriorAll, "\n")
    preferencesPrior <- preferencesPriorAll[validUtterances]
    prefPost <- rep(0, length(validUtterances))
    for (pref in c(1:length(validUtterances))) {
      if (preferencesPrior[pref] > 0) {
        pp <-
          simpleListener(utterance,
                         uttToObjProbs,
                         objectPreferenceSoftPriors[[pref]])
        prefPost[pref] <- pp[obj] * preferencesPrior[pref]
      } # else{} # prior preference for this perference is zero
    } # done with considering all possible preferences.
    if (sum(prefPost) == 0) { # no evidence for any preferences... -> no inference
      return(preferencesPriorAll)
    }
    # normalizing relevant posterior preferences such that the sum is equal to their prior probability mass
    #   sum(preferencesPrior) is the probability mass of the full prior that we are "entitled" to redistribute 
    #           because it concerns the features present in the trial
    #   prefPost / sum(prefPost) is the normalized posterior, so that the updated vector sums up to 1
    prefPost <- sum(preferencesPrior) * prefPost / sum(prefPost)
    # replacing the relevant old prior preferences values in preferencesPriorAll with their posteriors (which become the new priors)
    preferencesPriorAll[validUtterances] <- prefPost
    #
    return(preferencesPriorAll / sum(preferencesPriorAll)) # Sidenote: the renormalization here should not be really necessary because it is taken care of by a scaled insertion of new values in the two commands above.
  }

#' Speaker's uniform priors for utterances
#'
#' @description
#' Simple RSA
#'
#' Determines the prior utterance preferences of the speaker.
#' @param validUtterances A vector of utterances that correspond to all feature values present
#'  in the current objects in the scene.
#'
#' For example, it only makes sense to utter \emph{"red"} in a scene if there are \emph{red} objects present.
#' @return A vector of the same length as the validUtterances vector.
#'
#' It contains numeric values of the prior utterance preferences of the speaker.
#' @examples
#' \donttest{getSpeakerUtteranceUniformPrior(validUtterances)}
#'
#' output:
#'  [1] 0.17 0.17 0.17 0.17 0.17 0.17
#'  
#'  @details
#'  This function is used in X9.
#' @export
getSpeakerUtteranceUniformPrior <- function(validUtterances) {
  return(rep(1. / length(validUtterances), length(validUtterances)))
}

#' Get prior preferences of the listener
#'
#' @description
#' Simple RSA
#'
#' @param targetFeature A value between 1 and 3, specifying which feature type- color, shape, or pattern- is considered (for preferences).
#' @return A vector of length 9. It contains a uniform prior over the three features of the specified feature type and zeros for the other feature values.
#'
#' @examples
#' \donttest{getPreferencesPrior(targetFeature)}
#'
#' output:
#' [1] 0.33 0.33 0.33 0.00 0.00 0.00 0.00 0.00 0.00
#'
#'  @details
#'  This function is used in X9.
#' @export
getPreferencesPrior <- function(targetFeature) {
  preferencesPrior <- c(rep(0, 9))
  index <- targetFeature * 3
  indices <- c(index-2, index - 1, index)
  preferencesPrior[indices] <- 1
  return(preferencesPrior / sum(preferencesPrior))
}


#' Best information gain utterances
#'
#' @description
#' Simple RSA
#'
#' The ultimate function that determines the utterance preferences of a
#' speaker, who wants to learn about the listener's preferences.
#' The speaker considers all relevant utterances given the currentObjects.
#' He also considers all prior feature value preferences (of the listener) and all possible object choices.
#'
#' NOTE: This can be manipulated to make the speaker focus on one particular feature type preference
#' by setting the other feature value preferences to zero!
#'
#'  The function infers the resulting posterior feature value preferences of the listener in the particular scenario.
#' It computes the Kullback-Leibler divergence between the expected prior and inferred posterior feature value preferences
#' and finally determines the utility value for the considered utterance in the imagined scenario,
#'  adding this utility to all scenarios for each considered utterance.
#'
#' The utility is determined as the expected information gain between prior and posterior of the
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
#' @param validUtterances  A vector of utterances that correspond to all feature values present
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
#' @param objectPreferenceSoftPriors A list of preference priors for all valid utterances based on the object in the scene.
#'
#' The list has as many rows as the length of the validUtterances vector + 1.
#'
#' Each row in the list contains a vector of length 3, as there are three objects in the scene.
#'
#' The extra row is for the case of no feature preferences whatsoever, i.e. uniform prior over all three objects in the scene.
#' @param klValueFactor (here set to = 1) can be negative, 0 or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }
#' @return A vector containing the normalized probability over utterances given the listener's object preference priors.
#'
#' The utterance with the highest probability is the one that maximizes the information gain for the speaker.
#'
#' The vector has the same length as the validUtterances vector.
#' @examples
#' \donttest{allObjects[currentObjects,]}
#'      shape   pattern  color
#'[1,] "cloud"  "solid" "blue"
#'[2,] "circle" "solid" "blue"
#'[3,] "square" "solid" "blue"
#'
#' \donttest{simpleBestInfGainUtterance(preferencesPrior, validUtterances, currentObjects,
#' uttToObjProbs, objectPreferenceSoftPriors, klValueFactor = 1)}
#'
#' output:
#' [1] 0.0 0.0 0.0 0.5
#'
#' Since the all the objects present in the scene are solid and blue,
#' uttering solid or blue, would be optimal to learn something
#' about the shape preferences of the listener.
#' This means the speaker would have the best information gain.
#' @export
simpleBestInfGainUtterance <-
  function(preferencesPrior,
           validUtterances,
           currentObjects,
           uttToObjProbs,
           objectPreferenceSoftPriors,
           klValueFactor = 1) {
    InfGainUttPosterior <- rep(0, length(validUtterances))
    utterancePrior <-
      getSpeakerUtteranceUniformPrior(validUtterances) # prior over speaker utterances
    #
    for (utt in c(1:length(validUtterances))) {
      # evaluating the usage of a particular utterance utt
      prefPostAll <- rep(0, length(preferencesPrior))
      for (pref in c(1:length(preferencesPrior))) {
        if (preferencesPrior[pref] > 0) {
          # only pay attention to preferences with non-zero probability
          ### What is the likelihood that this particular preference prior is the correct one?
          prefPost <- 0
          for (obj in c(1:length(currentObjects))) {
            if (uttToObjProbs[utt, obj] > 0) {
              featurePrefsPosterior <-
                simplePragmaticSpeaker(
                  utt,
                  obj,
                  preferencesPrior,
                  validUtterances,
                  currentObjects,
                  uttToObjProbs,
                  objectPreferenceSoftPriors
                )
              # determine KL Value.
              KLvalue <-
                KLdivergence(preferencesPrior, featurePrefsPosterior)
              
              # likelihood interpretation of KLvalue:
              prefPost <- prefPost +  uttToObjProbs[utt, obj] *
                objectPreferenceSoftPriors[[pref]][obj] *
                utterancePrior[utt] *  preferencesPrior[pref] *
                exp(klValueFactor * KLvalue)
            } # else{} # imagined object choice is not valid (due to incompatible utterance)
          } # done with considering all possible object choices.
          if (prefPost > 0) {
            prefPostAll[pref] <- prefPost
          }
        } # else{} # prior preference for this perference is zero
      } # done with considering all possible preferences.
      InfGainUttPosterior[utt] <- sum(prefPostAll)
    }
    if (sum(InfGainUttPosterior) == 0)
      # no gain from any utterance...
      return(rep(1 / length(validUtterances), length(validUtterances)))
    return(InfGainUttPosterior / sum(InfGainUttPosterior))
  }


######### Iterative utterance choice function ###########
#' Iterative utterance choice function.
#' Utterance preferences of a speaker, who wants to learn about the listener's preferences
#'
#' @description
#' Simple RSA
#'
#' This function calculates the utility of the utterances. The utterance with the highest utility delivers the best information gain for the speaker
#' about the feature preferences of the listener.
#'
#' This function is used in the iterative scenarios.
#' @details
#' iterative-version of \code{\link{simpleBestInfGainUtterance}}
#'
#' @param preferencesPriorAll A vector of length 9.
#
#  Probability mass over all feature values.
#
#  Gives a prior preferences distribution over all (nine) feature values.
#'
#' @param validUtterances A vector of utterances that correspond to all feature values present
#' in the current objects in the scene.
#'
#' For example, it only makes sense to utter \emph{"red"} in a scene if there are \emph{red} objects present.
#'
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
#' @param objectPreferenceSoftPriors A list of preference priors for all valid utterances based on the object in the scene.
#'
#' The list has as many rows as the length of the validUtterances vector + 1.
#'
#' Each row in the list contains a vector of length 3, as there are three objects in the scene.
#'
#' The extra row is for the case of no feature preferences whatsoever, i.e. uniform prior over all three objects in the scene.
#' @param klValueFactor (here set to = 1) can be negative, 0 or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }
#'
#' @param targetFeature A value between 1 and 3, specifying which feature type- color, shape, or pattern- is considered (for preferences).
#'
#' @param utterancePrior A vector of the same length of the validUtterances vector. It contains zeros.
#'
#' \code{utterancePrior <- rep(0,length(validUtterances))}
#' @return posterior preferences over feature values: 3 dimensional array for simulated preferences.
#'
#'  \strong{rows:} utterances, \strong{columns:} preferences, \strong{blocks}: objects.
#'
#' It contains the normalized probability over utterances given the listener's object preference priors.
#'
#' U(utterances | listener's object preference priors).
#' @examples
#' \donttest{simpleBestInfGainUtteranceWithPrefPriorAll(preferencesPriorAll,
#' validUtterances, currentObjects, uttToObjProbs,
#' objectPreferenceSoftPriors, klValueFactor = 1, targetFeature, utterancePrior)}
#'
#' output:
#' [[1]]
#' [1] 0  0  0  0.26  0.088  0.65
#'
#' [[2]]
#' , , 1
#'
#'       [,1]   [,2]  [,3]  [,4] [,5] [,6] [,7] [,8] [,9]
#' [1,]   0       0     0     0    0    0    0    0    0
#' [2,]   0       0     0     0    0    0    0    0    0
#' [3,]   0       0     0     0    0    0    0    0    0
#' [4,] 0.66   0.0065  0.33   0    0    0    0    0    0
#' [5,]   0       0     0     0    0    0    0    0    0
#' [6,] 0.98    0.01   0.01   0    0    0    0    0    0
#'
#' , , 2
#'
#'       [,1]   [,2]  [,3]  [,4] [,5] [,6] [,7] [,8] [,9]
#' [1,]   0       0     0     0    0    0    0    0    0
#' [2,]   0       0     0     0    0    0    0    0    0
#' [3,]   0       0     0     0    0    0    0    0    0
#' [4,] 0.065   0.66   0.33   0    0    0    0    0    0
#' [5,]   0       0     0     0    0    0    0    0    0
#' [6,]  0.01   0.98   0.01   0    0    0    0    0    0
#'
#' , , 3
#'
#'      [,1]    [,2]  [,3]  [,4] [,5] [,6] [,7] [,8] [,9]
#' [1,]   0       0     0     0    0    0    0    0    0
#' [2,]   0       0     0     0    0    0    0    0    0
#' [3,]   0       0     0     0    0    0    0    0    0
#' [4,]   0       0     0     0    0    0    0    0    0
#' [5,]  0.33   0.33   0.33   0    0    0    0    0    0
#' [6,] 0.0097 0.0097  0.98   0    0    0    0    0    0
#'
#'
#' @export
simpleBestInfGainUtteranceWithPrefPriorAll <-
  function(preferencesPriorAll,
           validUtterances,
           currentObjects,
           uttToObjProbs,
           objectPreferenceSoftPriors,
           klValueFactor = 1,
           targetFeature,
           utterancePrior) {
    InfGainUttPosterior <- rep(0, length(validUtterances))
    preferencesPrior <- preferencesPriorAll[validUtterances]
    
    # posterior preferences over feature values: 3 dimensional array for simulated preferences
    #     rows: utterances -- columns: preferences -- blocks:(hypothetical) object choice
    featurePrefsPosteriorAll <- array(0, c(length(validUtterances), length(preferencesPriorAll),3))
    # utterancePrior <-
    #   getSpeakerUtteranceUniformPrior(validUtterances) # prior over speaker utterances
    #
    for (utt in c(1:length(validUtterances))) {
      # evaluating the usage of a particular utterance utt
      if (utterancePrior[utt] > 0){
        prefPostAll <- rep(0, length(preferencesPrior))
        for (pref in c(1:length(preferencesPrior))) {
          # prior over the preferences the speaker is interested in
          if (preferencesPrior[pref] > 0) {
            # only pay attention to preferences with non-zero probability
            ### What is the likelihood that this particular preference prior is the correct one?
            prefPost <- 0
            for (obj in c(1:length(currentObjects))) {
              if (uttToObjProbs[utt, obj] > 0) {
                featurePrefsPosterior <-
                  simplePragmaticSpeakerWithPrefPriorAll(
                    utt, obj, preferencesPriorAll, validUtterances,
                    currentObjects, uttToObjProbs, objectPreferenceSoftPriors
                  )
                KLvalue <-
                  KLdivergence(preferencesPriorAll, featurePrefsPosterior)
                featurePrefsPosteriorAll[utt,,obj] <- featurePrefsPosterior
                #
                # likelihood interpretation of KLvalue:
                prefPost <- prefPost +  uttToObjProbs[utt, obj] *
                  objectPreferenceSoftPriors[[pref]][obj] *
                  utterancePrior[utt] *  preferencesPrior[pref] *
                  exp(klValueFactor * KLvalue)
              } # else{} # imagined object choice is not valid (due to incompatible utterance)
            } # done with considering all possible object choices.
            if (prefPost > 0) {
              prefPostAll[pref] <- prefPost
            }
          } # else{} # prior preference for this perference is zero
        } # done with considering all possible preferences.
        InfGainUttPosterior[utt] <- sum(prefPostAll)
      } # else{} # utterance prior is zero 
    } # done with considering all possible utterances.
    
    ## Defining returns ##
    output1 <- list(utterancePrior,  featurePrefsPosteriorAll)
    posterior <- InfGainUttPosterior / sum(InfGainUttPosterior)
    output2 <- list(posterior, featurePrefsPosteriorAll)
    #    return(rep(1 / length(validUtterances), length(validUtterances)))
    #    if (sum(InfGainUttPosterior) == 0){# no gain from any utterance...
    #     return(output1)} # if no learning occurs, use uniform prior over available utterances.
    # Available utterances correspond to present feature values excluding utterances for target feature
    # If the target feature is shape, 'square', 'circle', and 'cloud' are not available
    #return(InfGainUttPosterior / sum(InfGainUttPosterior))
    return(output2)
  }


###################################################

#
# # Test:
notObeyInst <- 1
softPrefValue <- 1
currentObjects <- c(25,13,16)
allObjects[currentObjects,]
targetFeature <- 1
klValueFactor <- 1
relevantUtterances <- determineValidUtterances(currentObjects)
mapObjToUtt <- mapObjectToUtterances(currentObjects)
uttToObjProbs <- determineUttToObjectProbs(relevantUtterances,
                                          currentObjects,
                                          mapObjToUtt, notObeyInst)
mapUttToObjDeterministic <- determineUttToObjectProbs(relevantUtterances,
                                                      currentObjects,
                                                      mapObjToUtt, 0)
objectPreferenceSoftPriors <- getObjectPreferencePriors(relevantUtterances, currentObjects,
                                                         softPrefValue, mapUttToObjDeterministic)
preferencesPriorAll <- getPreferencesPrior(1)
simpleListener(4,uttToObjProbs, objectPreferenceSoftPriors[[2]])
# 
# 
# ## Setting the utterance Prior uniformly over the allowed utterances 
# ##    "allowed" means those that are preset and do not concern the feature values one is supposed to learn about
# utterancePrior <- rep(0,length(relevantUtterances))
# irrelevantIndices <- which(relevantUtterances>(3*(targetFeature-1)) & relevantUtterances<(3*targetFeature + 1))
# validUtterances <- relevantUtterances[-irrelevantIndices]
# utterancePriorShort <- rep (1/length(validUtterances),length(validUtterances))
# utterancePrior[-irrelevantIndices] <- utterancePriorShort
# 
# # # simpleBestInfGainUtterance <- function(preferencesPrior, relevantUtterances, currentObjects,
# # #                                 uttToObjProbs, objectPreferenceSoftPriors)
# simpleBestInfGainUtterance(c(0, 0, 0, 0, 0, 0, 1), relevantUtterances, currentObjects,
#                            uttToObjProbs, objectPreferenceSoftPriors) # sanity check - definite prior, no inf. gain possible
# round(simpleBestInfGainUtterance(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6, 0), relevantUtterances, currentObjects,
#                                  uttToObjProbs, objectPreferenceSoftPriors), 3) # actual scenario played out.
# 
# # ### Testing iterative utterance choice function ###
# simpleBestInfGainUtterance(c(1/3, 1/3, 1/3, 0, 0, 0, 0), relevantUtterances, currentObjects,
#                            uttToObjProbs, objectPreferenceSoftPriors)
# 
# simpleBestInfGainUtteranceWithPrefPriorAll(preferencesPriorAll, relevantUtterances,
#                                            currentObjects, uttToObjProbs, objectPreferenceSoftPriors,
#                                            klValueFactor, targetFeature, utterancePrior) #

