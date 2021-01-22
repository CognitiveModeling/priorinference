#' Feature Type Focus:
#' Get the utterances for the speaker's best information gain
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
#' Utterances of the feature type focus are filtered out (e.g. if you are interested in color preferences - so you are not allowed to utter colors!)
#' validUtterancesFTF correspond to all features present in the current objects except those of the featureTypeFocus!
#' @param currentObjects A vector of three values in \code{{1,...,27}} specifying the target and the other two objects in the scene.
#'
#' The target is the first object in the vector \code{(index = 1)}.
#'
#' @param featureTypeFocus A value between 1-3. It specifies the feature the speaker is interested to know more about and is not allowed to utter.
#' (shape =  1, pattern =  2, color = 3).
#' @param softPrefValue A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
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
#' @param klValueFactor A parameter that can be negative, 0 or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }
#' @return A vector containing the normalized probability over utterances given the listener's object preference priors.
#'
#' The utterance with the highest probability is the one that maximizes the information gain for the speaker.
#'
#' The vector has the same length as the validUtterancesFTF vector.
#' @examples
#' In the case of these objects being in a scene:
#'
#'      [shape] [pattern] [color]
#' [1,] "cloud"  "solid" "blue"
#' [2,] "circle" "solid" "blue"
#' [3,] "square" "solid" "blue"
#'
#' and these being the indices for the valid utterances:
#'
#' [1] 1 2 3 4 7 (cloud, circle, square, solid, blue)
#'
#' Since you are interested in shape (featureTypeFocus = 1) you are only
#' allowed to utter "solid" (index = 4) or "blue" (index = 7):
#'
#' validUtterancesFTF:
#'
#' [1] 4 7
#'
#' Then uttering solid or blue would be best in order to gain
#' information about the shape preferences of the listener:
#' \donttest{getSimpleBestInfGainUttPreferencesFTF (currentObjects, featureTypeFocus,
#' softPrefValue, notObeyInst, klValueFactor)}
#'
#' output:
#' [1] 0.0 0.0 0.0 0.5 0.5
#'
#' You can see here that the indices with the highest probability, namely 4 and 5,
#' correspond to the indices in the validUtterance vector for the feature
#' values 4 (solid) (index = 4) and 7 (blue) (index = 5).
#'
#' @export
getSimpleBestInfGainUttPreferencesFTF <- function(currentObjects, featureTypeFocus, softPrefValue, notObeyInst, klValueFactor) {
  validUtterances <- determineValidUtterances(currentObjects)

  validUtterancesFTF <-   validUtterances[which(validUtterances <= ((featureTypeFocus-1)*3) |
                                                  validUtterances > (featureTypeFocus*3))]
  invalidUtteranceIndices <-   which(validUtterances > ((featureTypeFocus-1)*3) &
                                                  validUtterances <= (featureTypeFocus*3))

  mapObjToUtt <- mapObjectToUtterances(currentObjects)
  uttToObjProbs <- determineUttToObjectProbs(validUtterances,
                                                              currentObjects,
                                                              mapObjToUtt, notObeyInst)
  mapUttToObjDeterministic <- determineUttToObjectProbs(validUtterances,
                                                                      currentObjects,
                                                                      mapObjToUtt, 0)
  objectPreferenceSoftPriors <- getObjectPreferencePriors(validUtterances, currentObjects,
                                                          softPrefValue, mapUttToObjDeterministic)

  preferencesPrior <- rep(0, length(validUtterances)+1)
  preferencesPrior[invalidUtteranceIndices] <- rep(1/(length(invalidUtteranceIndices)), length(invalidUtteranceIndices) )
  return( simpleBestInfGainUtterance(preferencesPrior, validUtterances, currentObjects,
                               uttToObjProbs, objectPreferenceSoftPriors,
                               klValueFactor) )
}

#' FeatureTypeFocus: Main cost function for three parameter optimization for the utterance choice experiments.
#' Optimizing softness, non-obedience and klValueFactor.
#'
#' @description
#' Simple RSA
#'
#' Actual RSA model Kullback-Leibler divergence determination for the utterance choice experiments.
#'
#' Softness, non-obedience and klValueFactor are optimized.
#'
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
#' \item{klValueFactor}{A parameter that can be negative, 0 or positive:
#' \describe{
#' \item{\strong{zero}}{Don't care about learning about the feature preferences of the listener}
#' \item{\strong{positive}}{Care about learning about the feature preferences of the listener}
#' \item{\strong{negative}}{Trying to pick non-ambiguous utterances}
#' }}}
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{getSimpleBestInfGainUttPreferencesFTF}}.
#'
#' This function is used in
#' \code{\link{SimpleRSAModelUttKLDivFTFParamA}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamB}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamK}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamBK}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamAK}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamAK.2}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamA.2}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamB.2}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamK.2.2}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamK.2.0}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamBK.2}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamAK.2}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamAB}},
#'
#' \code{\link{SimpleRSAModelUttKLDivFTFParamABK}}.
#'
#' @export
SimpleRSAModelUttKLDiv_3paramsFTF <- function(data, par1, par2, par3) {
  #   print(c(par1, par2, par3, data))
  klRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    numUtterances <- data[i,4]
    featureTypeFocus <- data[i,5]
    validUtterances <- determineValidUtterances(currentObjects)

    validUtterancesFTF <-   validUtterances[which(validUtterances <= ((featureTypeFocus-1)*3) |
                                                      validUtterances > (featureTypeFocus*3))]
    ## determining the model predictions
    bInfGainUttModel <- rep(NA, 9)
    bInfGainUttModel[validUtterances] <- getSimpleBestInfGainUttPreferencesFTF(currentObjects, featureTypeFocus,
                                                                               par1, par2, par3)
    bInfGainUttModel[validUtterancesFTF] <- bInfGainUttModel[validUtterancesFTF] / (sum(bInfGainUttModel[validUtterancesFTF]))
    ## adding the negative log likelihoods
    for(j in c(1:length(validUtterancesFTF))) {
      klRes <- klRes + data[i, 5+validUtterancesFTF[j]] *
        (log(data[i, 5+validUtterancesFTF[j]] + 1e-100) - log(bInfGainUttModel[validUtterancesFTF[j]] + 1e-100) )
    }
    #    print(c(data[i, 5+validUtterances],9999,bInfGainUttModel[validUtterances],8888))
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(klRes)
}

#' Feature Type Focus: Cost function for one parameter optimization for the utterance choice experiments.
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
#'
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#' @export
SimpleRSAModelUttKLDivFTFParamA <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(par[1]), 0, 1))
}

#' Feature Type Focus: Cost function for one parameter optimization for the utterance choice experiments.
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
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#' @export
SimpleRSAModelUttKLDivFTFParamB <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0, abs(par[1]), 1))
}

#' Feature Type Focus: Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing klValueFactor.
#' Softness and non-obedience are fixed.
#'
#' @description
#' Simple RSA
#'
#' klValueFactor is optimized.
#'
#' Softness and non-obedience are fixed at 0.
#' @param par One value vector, which specifies one of three parameters to be optimized:
#' \enumerate{
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#' @export
SimpleRSAModelUttKLDivFTFParamK <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0, 0, par[1]))
}


#' Feature Type Focus: Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing non-obedience and klValueFactor.
#' Softness is fixed.
#'
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#' @export
SimpleRSAModelUttKLDivFTFParamBK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0, abs(params[1]), params[2]))
}

#' Feature Type Focus: Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing softness and klValueFactor.
#' Non-obedience is fixed.
#'
#' @description
#' Simple RSA
#'
#' Softness and klValueFacto are optimized.
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
#' \item{klValueFactor can be negative, zero or positive:
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#' @export
SimpleRSAModelUttKLDivFTFParamAK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(params[1]), 0, params[2]))
}


#' Feature Type Focus: Cost function for two parameter optimization for the utterance choice experiments.
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
#' \item{klValueFactor: A parameter can be negative, zero or positive:
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#'
#' @export
SimpleRSAModelUttKLDivFTFParamAK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(params[1]), 0.2, params[2]))
}

#' Feature Type Focus: Cost function for one parameter optimization for the utterance choice experiments.
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
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#' @export
SimpleRSAModelUttKLDivFTFParamA.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(par[1]), 0.2, 1))
}

#' Feature Type Focus: Cost function for one parameter optimization for the utterance choice experiments.
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
#'
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#' @export
SimpleRSAModelUttKLDivFTFParamB.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0.2, abs(par[1]), 1))
}


#' Feature Type Focus: Cost function for one parameter optimization for the utterance choice experiments.
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#' @export
SimpleRSAModelUttKLDivFTFParamK.2.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0.2, 0.2, par[1]))
}


#' Feature Type Focus: Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing klValueFactor.
#' Softness and non-obedience are fixed.
#' @description
#' Simple RSA
#'
#' klValueFactor is optimized.
#'
#' Softness is fixed at 0.2.
#' Non-obedience is fixed at 0.
#'
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#' @export
SimpleRSAModelUttKLDivFTFParamK.2.0 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0.2, 0, par[1]))
}


#' Feature Type Focus: Cost function for two parameter optimization for the utterance choice experiments.
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#' @export
SimpleRSAModelUttKLDivFTFParamBK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, 0.2, abs(params[1]), params[2]))
}


#' Feature Type Focus: Cost function for two parameter optimization for the utterance choice experiments.
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#' @export
SimpleRSAModelUttKLDivFTFParamAB <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(params[1]), abs(params[2]), 1))
}


#' Feature Type Focus: Cost function for two parameter optimization for the utterance choice experiments.
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
#' @export
SimpleRSAModelUttKLDivFTFParamABK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3paramsFTF(data, abs(params[1]), abs(params[2]), params[3]))
}
