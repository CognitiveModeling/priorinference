#source("R/AllUtterancesAndObjects.R")
#source("R/getConstCodeStratUtt.R")

#' Get the utterances for the speaker's best information gain
#' (iterative setting)
#'
#' @description
#' Simple RSA
#'
#' Determines the optimal utterance for the best information gain.
#'
#' These are based on the valid utterances determined from the current objects in the scene.
#'
#' The inferred listener's object preferences are computed assuming the listener
#' picks a certain object and has certain object preferences.
#'
#' @param preferencesPriorAll A vector of length 9.
#'
#' Probability mass over all feature values.
#'
#' Gives a prior preferences distribution over all (nine) feature values.
#'
#' @param currentObjects A vector of three values in \code{{1,...,27}} specifying the target and the other two objects in the scene.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#'
#' @param softPrefValue A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#' Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.
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
#' @param klValueFactor A parameter that can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }
#' @param targetFeature A value between 1 and 3, specifying which feature type- color, shape, or pattern- is considered (for preferences).
#' @return A vector containing the normalized probability over utterances given the listener's object preference priors.
#'
#' The utterance with the highest probability is the one that maximizes the information gain for the speaker.
#'
#' posterior preferences over feature values: 3 dimensional array for simulated preferences.
#'
#' \strong{rows:} utterances, \strong{columns:} preferences, \strong{blocks}: objects.
#'
#' @details
#' This function is only used in X9.
#'
#' This is the iterative version of \code{\link{getSimpleBestInfGainUttPreferences}}
#'
#' @examples
#' \donttest{getSimpleBestInfGainUttPreferencesIterative(preferencesPriorAll, currentObjects, softPrefValue,
#' notObeyInst, klValueFactor, targetFeature)}
#'
#' output:
#'[[1]]
#' [1] 0.25 0.25 0.25 0.00 0.00 0.25

#'[[2]]
#' , , 1
#'
#'       [,1] [,2]  [,3]    [,4]       [,5]      [,6]       [,7] [,8] [,9]
#' [1,]    0    0    0    0.3334166   0.33325   0.3333333    0    0    0
#' [2,]    0    0    0    0.3334166   0.33325   0.3333333    0    0    0
#' [3,]    0    0    0    0.3334167   0.33325   0.3333333    0    0    0
#' [4,]    0    0    0    0.0000000   0.00000   0.0000000    0    0    0
#' [5,]    0    0    0    0.0000000   0.00000   0.0000000    0    0    0
#' [6,]    0    0    0    0.3334166   0.33325   0.3333333    0    0    0
#'
#' , , 2
#'
#'        [,1] [,2] [,3]    [,4]        [,5]      [,6]     [,7] [,8] [,9]
#' [1,]    0    0    0    0.3334166   0.33325   0.3333333    0    0    0
#' [2,]    0    0    0    0.3334166   0.33325   0.3333333    0    0    0
#' [3,]    0    0    0    0.3334167   0.33325   0.3333333    0    0    0
#' [4,]    0    0    0    0.0000000   0.00000   0.0000000    0    0    0
#' [5,]    0    0    0    0.0000000   0.00000   0.0000000    0    0    0
#' [6,]    0    0    0    0.3334166   0.33325   0.3333333    0    0    0
#'
#' , , 3
#'
#'        [,1] [,2] [,3]      [,4]      [,5]      [,6]      [,7] [,8] [,9]
#' [1,]    0    0    0    0.3331667  0.3334999  0.3333333    0    0    0
#' [2,]    0    0    0    0.3331667  0.3334999  0.3333333    0    0    0
#' [3,]    0    0    0    0.3331668  0.3334999  0.3333333    0    0    0
#' [4,]    0    0    0    0.0000000  0.0000000  0.0000000    0    0    0
#' [5,]    0    0    0    0.0000000  0.0000000  0.0000000    0    0    0
#' [6,]    0    0    0    0.3331667  0.3334999  0.3333333    0    0    0
#' @export
getSimpleBestInfGainUttPreferencesIterative <- function(preferencesPriorAll,
                                                        currentObjects, softPrefValue,
                                                        notObeyInst, klValueFactor, targetFeature) {
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

  preferencesPrior <- preferencesPriorAll[relevantUtterances]

  # Define utterance prior excluding utterances of target feature
  utterancePrior <- rep(0,length(relevantUtterances))
  irrelevantIndices <- which(relevantUtterances>(3*(targetFeature-1)) & relevantUtterances<(3*targetFeature + 1))
  validUtterances <- relevantUtterances[-irrelevantIndices]
  utterancePriorShort <- rep (1/length(validUtterances),length(validUtterances))
  utterancePrior[-irrelevantIndices] <- utterancePriorShort

#  preferencesPrior <- rep(1/(length(relevantUtterances)), length(relevantUtterances)+1)
#  preferencesPrior[length(relevantUtterances)+1] = 0
  return( simpleBestInfGainUtteranceWithPrefPriorAll(preferencesPriorAll, relevantUtterances, currentObjects,
                               uttToObjProbs, objectPreferenceSoftPriors,
                               klValueFactor, targetFeature, utterancePrior) )
}


#' Main cost function for three parameter optimization (iterative setting).
#' Optimizing softness, non-obedience and klValueFactor.
#' @description
#' Simple RSA
#'
#' Actual RSA model Kullback-Leibler divergence determination for the utterance choice experiments.
#'
#' Softness, non-obedience and klValueFactor are optimized.
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @param par1 \describe{
#' \item{softness parameter}{A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#' Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#' }
#' @param par2 \describe{
#' \item{non-obedience parameter}{Determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#' }
#' @param par3 \describe{
#' \item{klValueFactor}{A parameter that can be negative, 0 or positive:}
#' \describe{
#' \item{\strong{zero}}{Don't care about learning about the feature preferences of the listener}
#' \item{\strong{positive}}{Care about learning about the feature preferences of the listener}
#' \item{\strong{negative}}{Trying to pick non-ambiguous utterances}
#' }}
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#'
#' This function is used in
#'
#' \code{\link{SimpleRSAModelUttKLDivParamA_iterative}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamB_iterative}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamK_iterative}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamBK_iterative}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamAK_iterative}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamAK.2_iterative}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamA.2_iterative}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamK.2.2_iterative}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamK.2.0_iterative}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamBK.2_iterative}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamAB_iterative}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamABK_iterative}}.
#' @export
SimpleRSAModelUttKLDiv_3params_iterative <- function(data, par1, par2, par3) {
  #   print(c(par1, par2, par3, data))
  logLik <- 0
  for(i in c(1:nrow(data))) {
    if( (i-1)%%4 == 0) {
      preferencesPriorAll <- getPreferencesPrior(data[i,5]) # focussing on the feature type in question.
    }
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    uttFeat <- data[i,4]
    targetFeat <- data[i, 5]
    pickedUtterance <- data[i, 6]
    relevantUtterances <- determineValidUtterances(currentObjects)
    irrelevantIndices <- which(relevantUtterances>(3*(data[i,5]-1)) &
                                 relevantUtterances<(3*data[i,5] + 1))
    validUtterances <- relevantUtterances[-irrelevantIndices]
    ## determining the model predictions
    bInfGainUttModel <- rep(NA, 9)
    output <- getSimpleBestInfGainUttPreferencesIterative(preferencesPriorAll,currentObjects, abs(par1),
                                                          abs(par2), par3, targetFeat)
    bInfGainUttModel[relevantUtterances] <- output[[1]]
    preferencesPriorAll <- output[[2]][pickedUtterance,,1]
    ## adding the negative log likelihoods
    logLik <- logLik - log(bInfGainUttModel[relevantUtterances[pickedUtterance]] + 1e-100)
#    print(logLik)
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(logLik)
}

#' Cost function for one parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing softness.
#' Non-obedience and klValueFactor are fixed.
#' @description
#' Simple RSA
#'
#' Softness is optimized.
#'
#' Non-obedience is fixed at 0.
#' klValueFactor is fixed at 1.
#' @param par One value vector, which specifies one of three parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#'  Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#'}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamA}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamA_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(par[1]), 0, 1))
}

#' Cost function for one parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing non-obedience.
#' Softness and klValueFactor are fixed.
#'
#' @description
#' Simple RSA
#'
#' Non-obedience is optimized.
#'
#' Softness is fixed at 0.
#' klValueFactor is fixed at 1.
#' @param par One value vector, which specifies one of three parameters to be optimized:
#' \enumerate{
#'   \item{non-obedience, This parameter determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#'  }
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamB}}
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamB_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0, abs(par[1]), 1))
}


#' Cost function for one parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing klValueFactor.
#' Softness and non-obedience are fixed.
#' @description
#' Simple RSA
#'
#' klValueFactor is optimized.
#'
#' Softness and non-obedience are fixed at 0.
#' @param par One value vector, which specifies one of three  parameters to be optimized:
#' \enumerate{
#' \item{klValueFactor: A parameter that can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamK}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamK_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0, 0, par[1]))
}

#' Cost function for two parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing non-obedience and klValueFactor.
#' Softness is fixed.
#' @description
#' Simple RSA
#'
#' Non-obedience and klValueFactor are optimized.
#'
#' Softness is fixed at 0.
#' @param params Two value vector specifying the two parameters to be optimized.
#' \enumerate{
#' \item{non-obedience: This parameter determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamBK}}
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamBK_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0, abs(params[1]), params[2]))
}

#' Cost function for two parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing softness and klValueFactor.
#' Non-obedience is fixed.
#' @description
#' Simple RSA
#'
#' Softness and klValueFactor are optimized.
#'
#' Non-obedience is fixed at 0.
#' @param params Two value vector specifying the two parameters to be optimized.
#' \enumerate{
#' \item{softPrefValue, i.e., the strength of preferring one entity over others.}
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamAK}}
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamAK_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(params[1]), 0, params[2]))
}

#' Cost function for two parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing softness and klValueFactor.
#' Non-obedience is fixed.
#'
#' @description
#' Simple RSA
#'
#' Softness and klValueFactor are optimized.
#'
#' Non-obedience is fixed at 0.2.
#' @param params Two value vector specifying the two parameters to be optimized.
#' \enumerate{
#' \item{softPrefValue: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#' Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamAK.2}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamAK.2_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(params[1]), 0.2, params[2]))
}


#' Cost function for one parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing softness.
#' Non-obedience and klValueFactor are fixed.
#' @description
#' Simple RSA
#'
#' Softness is optimized.
#'
#' Non-obedience is fixed at 0.2.
#' klValueFactor is fixed at 1.
#' @param par One value vector specifying one of the three parameters to be optimized.
#' \enumerate{
#' \item{softPrefValue: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#'  Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#'}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamA.2}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamA.2_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(par[1]), 0.2, 1))
}

#' Cost function for one parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing non-obedience.
#' Softness and klValueFactor are fixed.
#' @description
#' Simple RSA
#'
#' Non-obedience is optimized.
#'
#' Softness is fixed at 0.2
#' klValueFactor is fixed at 1.
#' @param par One value vector specifying one of the three parameters to be optimized.
#' \enumerate{
#' \item{non-obedience: This parameter determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#' }
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamB.2}}
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamB.2_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0.2, abs(par[1]), 1))
}

#' Cost function for one parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing klValueFactor.
#' Softness and non-obedience are fixed.
#'
#' @description
#' Simple RSA
#'
#' klValueFactor is optimized.
#'
#' Softness and non-obedience are fixed at 0.2.
#' @param par One value vector specifying one of the three parameters to be optimized.
#' \enumerate{
#' \item{klValueFactor: A parameter that can be negative, 0 or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamK.2.2}}
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamK.2.2_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0.2, 0.2, par[1]))
}

#' Cost function for one parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing klValueFactor.
#' Softness and non-obedience are fixed.
#'
#' @description
#' Simple RSA
#'
#' klValueFactor is optimized.
#'
#' Softness is fixed at 0.2.
#' Non-obedience is fixed at 0.
#' @param par One value vector specifying one of the three parameters to be optimized.
#' \enumerate{
#' \item{klValueFactor: A parameter that can be negative, 0 or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamK.2.0}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamK.2.0_iterative <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0.2, 0, par[1]))
}

#' Cost function for two parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing non-obedience and klValueFactor.
#' Softness is fixed.
#' @description
#' Simple RSA
#'
#' Non-obedience and klValueFactor are optimized.
#'
#' Softness is fixed at 0.2.
#' @param params Two value vector specifying two of the three parameters to be optimized.
#' \enumerate{
#' \item{non-obedience: Determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#' \item{klValueFactor: A parameter that can be negative, 0 or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamBK.2}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamBK.2_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, 0.2, abs(params[1]), params[2]))
}

#' Cost function for two parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing softness and non-obedience.
#' klValueFactor is fixed.
#' @description
#' Simple RSA
#'
#' Softness and non-obedience are optimized.
#'
#' klValueFactor is fixed at 1.
#' @param params Two value vector specifying two of the three parameters to be optimized.
#' \enumerate{
#' \item{softPrefValue: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#'  Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#' \item{non-obedience: This parameter determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#'  }
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamAB}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamAB_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(params[1]), abs(params[2]), 1))
}

#' Cost function for three parameter optimization for the utterance choice experiments. (iterative setting)
#' Optimizing softness, non-obedience and klValueFactor.

#' @description
#' Simple RSA
#'
#' Softness, non-obedience and klValueFactor are optimized.
#'
#' @param params Three value vector specifying three of the three parameters to be optimized.
#' \enumerate{
#' \item{softPrefValue: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#'  Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#' \item{non-obedience:This parameter determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#'
#' \item{klValueFactor: A parameter that can be negative, 0 or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the iterative version of \code{\link{SimpleRSAModelUttKLDivParamABK}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_iterative}}.
#' @export
SimpleRSAModelUttKLDivParamABK_iterative <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_iterative(data, abs(params[1]), abs(params[2]), params[3]))
}


#' Main cost function for three parameter optimization for the utterance choice experiments.
#' Optimizing softness, non-obedience and klValueFactor. (independent and iterative setting)
#' @description
#' Simple RSA
#'
#' Actual RSA model Kullback-Leibler divergence determination for utterance choice experiments.
#'
#' Softness, non-obedience and klValueFactor are optimized.
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,6: picked utterance]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{6: picked utterance} The utterance picked by the participant.
#' @param par1 \describe{
#' \item{softness parameter}{Specifies how much actual feature priorities come into play in the object choice (The larger the higher the tendency towards uniform liking).}
#' }
#' @param par2 \describe{
#' \item{non-obedience parameter}{The extent to which the instruction of the speaker is obeyed by the listener.
#' (0 = full obedience, infinity = full instruction ignorance).}
#' }
#' @param par3 \describe{
#' \item{klValueFactor}{A parameter that can be negative, 0 or positive:}
#' \describe{
#' \item{\strong{zero}}{Don't care about learning about the feature preferences of the listener}
#' \item{\strong{positive}}{Care about learning about the feature preferences of the listener}
#' \item{\strong{negative}}{Trying to pick non-ambiguous utterances}
#' }}
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{getSimpleBestInfGainUttPreferencesIterative}}.
#'
#' This function is used in
#'
#' \code{\link{SimpleRSAModelUttKLDivParamA_independent}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamB_independent}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamK_independent}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamBK_independent}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamAK_independent}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamAK.2_independent}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamA.2_independent}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamB.2_independent}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamK.2.2_independent}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamK.2.0_independent}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamBK.2_independent}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamAB_independent}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamABK_independent}}.
#' @export
SimpleRSAModelUttKLDiv_3params_independent <- function(data, par1, par2, par3) {
  #   print(c(par1, par2, par3, data))
  logLik <- 0
  for(i in c(1:nrow(data))) {
    preferencesPriorAll <- getPreferencesPrior(data[i,5]) # focussing on the feature type in question.
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    #    numUtterances <- data[i,4]
    uttFeat <- data[i,4]
    targetFeat <- data[i, 5]
    pickedUtterance <- data[i, 6]
    relevantUtterances <- determineValidUtterances(currentObjects)
    irrelevantIndices <- which(relevantUtterances>(3*(data[i,5]-1)) &
                                 relevantUtterances<(3*data[i,5] + 1))
    validUtterances <- relevantUtterances[-irrelevantIndices]
    ## determining the model predictions
    #################      Code below not edited ############
    bInfGainUttModel <- rep(NA, 9)
    output <- getSimpleBestInfGainUttPreferencesIterative(preferencesPriorAll,currentObjects, abs(par1),
                                                          abs(par2), par3, targetFeat)
    bInfGainUttModel[relevantUtterances] <- output[[1]]
 #   preferencesPriorAll <- output[[2]][pickedUtterance,,1]
    ## adding the negative log likelihoods
    logLik <- logLik - log(bInfGainUttModel[relevantUtterances[pickedUtterance]] + 1e-100)
   if (bInfGainUttModel[relevantUtterances[pickedUtterance]] == 0){
      print('#####################')
      print(bInfGainUttModel)
      print(relevantUtterances)
      print(validUtterances)
      print(pickedUtterance)
      print(currentObjects)
      print()
      print(allObjects[currentObjects,])
    }

    #    print(logLik)
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(logLik)
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing softness.
#' Non-obedience and klValueFactor are fixed.
#'
#' @description
#' Simple RSA
#'
#' Softness is optimized.
#'
#' Non-obedience is fixed at 0.
#' klValueFactor is fixed at 1.
#' @param par One value vector, which specifies one of three parameters to be optimized:
#' \enumerate{
#'   \item{softPrefValue: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#'  Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#'}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamA_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(par[1]), 0, 1))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing non-obedience.
#' Softness and klValueFactor are fixed.
#' @description
#' Simple RSA
#'
#' Non-obedience is optimized.
#'
#' Softness is fixed at 0.
#' klValueFactor is fixed at 1.
#' @param par One value vector, which specifies one of three parameters to be optimized:
#' \enumerate{
#'   \item{non-obedience: This parameter determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#'  }
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamB_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0, abs(par[1]), 1))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing klValueFactor.
#' Softness and non-obedience are fixed.
#' @description
#' Simple RSA
#'
#' klValueFactor is optimized.
#'
#' Softness and non-obedience are fixed at 0.
#' @param par One value vector specifying one of the three parameters to be optimized:
#' \enumerate{
#' \item{klValueFactor: A parameter that can be negative, 0 or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamK_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0, 0, par[1]))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing non-obedience and klValueFactor.
#' Softness is fixed.
#' @description
#' Simple RSA
#'
#' Non-obedience and klValueFactor is optimized.
#'
#' Softness is fixed at 0.
#'
#' @param params Two value vector specifying the two parameters to be optimized.
#' \enumerate{
#' \item{non-obedience:} This parameter determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.
#' 
#' \item{klValueFactor:} A parameter that can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamBK_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0, abs(params[1]), params[2]))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing softness and klValueFactor.
#' Non-obedience is fixed.
#' @description
#' Simple RSA
#'
#' Softness and klValueFactor are optimized.
#'
#' Non-obedience is fixed at 0.
#' @param params Two value vector specifying the two parameters to be optimized.
#' \enumerate{
#' \item{softPrefValue: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#' Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#' \item{klValueFactor: A parameter that can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamAK_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(params[1]), 0, params[2]))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing softness and klValueFactor.
#' Non-obedience is fixed.
#' @description
#' Simple RSA
#'
#' Softness and klValueFactor are optimized.
#'
#' Non-obedience is fixed at 0.2.
#' @param params Two value vector specifying the two parameters to be optimized.
#' \enumerate{
#' \item{softPrefValue: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#' Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#' \item{klValueFactor: A parameter that can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamAK.2_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(params[1]), 0.2, params[2]))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing softness.
#' Non-obedience and klValueFactor are fixed.
#'
#' @description
#' Simple RSA
#'
#' Softness is optimized.
#'
#' Non-obedience is fixed at 0.2.
#' klValueFactor is fixed at 1.
#' @param par One value vector specifying one of the three parameters to be optimized.
#' \enumerate{
#' \item{softPrefValue: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#'  Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#'}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamA.2_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(par[1]), 0.2, 1))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing non-obedience.
#' Softness and klValueFactor are fixed.
#'
#' @description
#' Simple RSA
#'
#' Non-obedience is optimized.
#'
#' Softness is fixed at 0.2.
#' klValueFactor is fixed at 1.
#' @param par One value vector specifying one of the three parameters to be optimized.
#' \enumerate{
#' \item{non-obedience: This parameter determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#' }
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamB.2_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0.2, abs(par[1]), 1))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing klValueFactor.
#' Softness and non-obedience are fixed.
#' @description
#' Simple RSA
#'
#' klValueFactor is optimized.
#'
#' Softness and non-obedience are fixed at 0.2.
#' @param par One value vector specifying one of the three parameters to be optimized.
#' \enumerate{
#' \item{klValueFactor: A parameter that can be negative, 0 or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamK.2.2_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0.2, 0.2, par[1]))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing klValueFactor.
#' Softness and non-obedience are fixed.
#'
#' @description
#' Simple RSA
#'
#' klValueFactor is optimized.
#'
#' Softness is fixed at 0.2.
#' Non-obedience is fixed at 0.
#' @param par One value vector specifying one of the three parameters to be optimized:
#' \enumerate{
#' \item{klValueFactor: A parameter that can be negative, 0 or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamK.2.0_independent <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0.2, 0, par[1]))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing non-obedience and klValueFactor.
#' Softness is fixed.
#'
#' @description
#' Simple RSA
#'
#' Non-obedience and klValueFactor are optimized.
#'
#' Softness is fixed at 0.2.
#' @param params Two value vector specifying two of the three parameters to be optimized.
#' \enumerate{
#' \item{non-obedience: Determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#' \item{klValueFactor: A parameter that can be negative, 0 or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamBK.2_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, 0.2, abs(params[1]), params[2]))
}


#' Cost function for two parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing softness and non-obedience.
#' klValueFactor is fixed.
#' @description
#' Simple RSA
#'
#' Softness and non-obedience are optimized.
#'
#' klValueFactor is fixed at 1.
#' @param params Two value vector specifying two of the three parameters to be optimized.
#' \enumerate{
#' \item{softPrefValue: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#'  Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#' \item{non-obedience: This parameter determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#'  }
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamAB_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(params[1]), abs(params[2]), 1))
}


#' Cost function for three parameter optimization for the utterance choice experiments.
#' (independent and iterative setting)
#' Optimizing softness, non-obedience and klValueFactor.
#'
#' @description
#' Simple RSA
#'
#' Softness, non-obedience and klValueFactor are optimized.
#'
#' @param params Three value vector specifying three of the three parameters to be optimized.
#' \enumerate{
#' \item{softPrefValue: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#'  Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#' \item{non-obedience: This parameter determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#'
#' \item{klValueFactor: A parameter that can be negative, 0 or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}}
#' @param data A matrix with data rows.
#'
#' column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
#'
#' \strong{1:OC1} Object 1. A value between 1 and 27.
#'
#' \strong{2:OC2} Object 2. A value between 1 and 27.
#'
#' \strong{3:OC3} Object 3. A value between 1 and 27.
#'
#' \strong{4:numUttOptions} The number of valid utterances in the scene.
#'
#' \strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params_independent}}.
#' @export
SimpleRSAModelUttKLDivParamABK_independent <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params_independent(data, abs(params[1]), abs(params[2]), params[3]))
}


#Testing optimization function
currentObjects <- c(1,2,6)
#currentObjects <- c(26,20,23)
notObeyInst <- 1000
klValueFactor <- 1
softPrefValue <- 1000
targetFeature <- 2
trial <- 1
utt <- 5
obj <- 1
if (trial-1%%4 == 0){
  preferencesPriorAll <- getPreferencesPrior(targetFeature)
}

output <-  getSimpleBestInfGainUttPreferencesIterative(
  preferencesPriorAll, currentObjects,
  softPrefValue, notObeyInst, klValueFactor, targetFeature)
posteriorUtterances <- round(output[[1]],3)
preferencesPriorAll <- round(output[[2]][utt,,obj],3)
