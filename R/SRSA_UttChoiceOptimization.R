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
#' @return A vector containing the normalized probability over utterances given the listener's object preference priors.
#'
#' The utterance with the highest probability is the one that maximizes the information gain for the speaker.
#'
#' The vector has the same length as the validUtterances vector.
#' @details
#' This function uses the function \code{\link{simpleBestInfGainUtterance}}.
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
#' Then uttering solid or blue would be best in order to gain
#' information about the shape preferences of the listener:
#' \donttest{getSimpleBestInfGainUttPreferences(currentObjects, softPrefValue,
#' notObeyInst, klValueFactor)}
#'
#' output:
#' [1] 0.0 0.0 0.0 0.5 0.5
#'
#' You can see here that the indices with the highest probability, namely 4 and 5,
#' correspond to the indices in the validUtterance vector for the feature
#' values 4 (solid) (index = 4) and 7 (blue) (index = 5).
#'
#' @export
getSimpleBestInfGainUttPreferences <- function(currentObjects, softPrefValue, notObeyInst, klValueFactor) {
  validUtterances <- determineValidUtterances(currentObjects)
  mapObjToUtt <- mapObjectToUtterances(currentObjects)
  uttToObjProbs <- determineUttToObjectProbs(validUtterances,
                                                              currentObjects,
                                                              mapObjToUtt, notObeyInst)
  mapUttToObjDeterministic <- determineUttToObjectProbs(validUtterances,
                                                                      currentObjects,
                                                                      mapObjToUtt, 0)
  objectPreferenceSoftPriors <- getObjectPreferencePriors(validUtterances, currentObjects,
                                                          softPrefValue, mapUttToObjDeterministic)

  preferencesPrior <- rep(1/(length(validUtterances)+1), length(validUtterances)+1)
#  preferencesPrior <- rep(1/(length(validUtterances)), length(validUtterances)+1)
#  preferencesPrior[length(validUtterances)+1] = 0
  return( simpleBestInfGainUtterance(preferencesPrior, validUtterances, currentObjects,
                               uttToObjProbs, objectPreferenceSoftPriors,
                               klValueFactor) )
}

#' Main cost function for three parameter optimization for the utterance choice experiments.
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
#' column structure:
#'
#' \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}
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
#' \item{klValueFactor}{A parameter that can be negative, 0 or positive:}
#' \describe{
#' \item{\strong{zero}}{Don't care about learning about the feature preferences of the listener}
#' \item{\strong{positive}}{Care about learning about the feature preferences of the listener}
#' \item{\strong{negative}}{Trying to pick non-ambiguous utterances}
#' }}
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function is used in
#'
#' \code{\link{SimpleRSAModelUttKLDivParamA}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamB}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamK}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamBK}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamAK}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamAK.2}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamA.2}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamB.2}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamK.2.2}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamK.2.0}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamBK.2}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamAB}},
#'
#' \code{\link{SimpleRSAModelUttKLDivParamABK}}.
#'
#' @export
SimpleRSAModelUttKLDiv_3params <- function(data, par1, par2, par3) {
  #   print(c(par1, par2, par3, data))
  klRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    numUtterances <- data[i,4]
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    bInfGainUttModel <- rep(NA, 9)
    bInfGainUttModel[validUtterances] <- getSimpleBestInfGainUttPreferences(currentObjects, par1, par2, par3)
    ## adding the negative log likelihoods
    for(j in c(1:length(validUtterances))) {
      klRes <- klRes + data[i, 4+validUtterances[j]] *
        (log(data[i, 4+validUtterances[j]] + 1e-100) - log(bInfGainUttModel[validUtterances[j]] + 1e-100) )
    }
    #    print(c(data[i, 4+validUtterances],9999,bInfGainUttModel[validUtterances],8888))
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(klRes)
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing softness.
#' Non-obedience and klValueFactor are fixed.
#'
#' @description
#' Simple RSA
#'
#' Softness is optimized.
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
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamA_iterative}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamA <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(par[1]), 0, 1))
}


#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing non-obedience.
#' Non-obedience and klValueFactor are fixed.
#'
#' @description
#' Simple RSA
#'
#' Non-obedience is optimized.
#'
#' softness parameter is fixed at 0.
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
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamB_iterative}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamB <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0, abs(par[1]), 1))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing klValueFactor.
#' Softness and non-obedience are fixed.
#'
#' @description
#' Simple RSA
#'
#' klValueFactor is optimized.
#'
#' Softness and non-obedience parameter are fixed at 0.
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
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamK_iterative}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamK <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0, 0, par[1]))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing non-obedience and klValueFactor.
#' Softness is fixed.
#'
#' @description
#' Simple RSA
#'
#' Non-obedience and klValueFactor are optimized.
#'
#' Softness parameter is fixed at 0.
#'
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
#' @details
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamBK_iterative}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamBK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0, abs(params[1]), params[2]))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing Softness and klValueFactor.
#' Non-obedience is fixed.
#'
#' @description
#' Simple RSA
#'
#' Softness and klValueFactor are optimized.
#'
#' Non-obedience parameter is fixed at 0.
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamAK_iterative}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamAK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), 0, params[2]))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing softness and klValueFactor.
#' Non-obedience is fixed.
#'
#' @description
#' Simple RSA
#'
#' Optimizing softness and klValueFactor.
#'
#' Non-obedience is fixed at 0.2.
#'
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
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamAK.2_iterative}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamAK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), 0.2, params[2]))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing softness.
#' Non-obedience and klValueFactor are fixed.
#'
#' @description
#' Simple RSA
#'
#' Softness is optimized.
#'
#' Non-obedience is fixed at 0.2.
#'
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
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamA.2_iterative}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamA.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(par[1]), 0.2, 1))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing non-obedience.
#' Softness and klValueFactor are fixed.
#'
#' @description
#' Simple RSA
#'
#' Non-obedience is optimized.
#'
#' Softness is fixed at 0.2.
#'
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamB.2_iterative}}
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamB.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0.2, abs(par[1]), 1))
}


#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing klValueFactor.
#' Softness and non-obedience are fixed.
#'
#' @description
#' Simple RSA
#'
#' klValueFactor is optimized.
#'
#' Softness and klValueFactor are fixed at 0.2.
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
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamK.2.2_iterative}}
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamK.2.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0.2, 0.2, par[1]))
}


#' Cost function for one parameter optimization for the utterance choice experiments.
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
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamK.2.0_iterative}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamK.2.0 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0.2, 0, par[1]))
}


#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing non-obedience and klValueFactor.
#' Softness is fixed.
#'
#' @description
#' Simple RSA
#'
#' Non-obedience and klValueFactor are optimized.
#' Softness is fixed at 0.2.
#'
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
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamBK.2_iterative}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamBK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0.2, abs(params[1]), params[2]))
}


#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing softness and non-obedience.
#' klValueFactor is fixed.
#'
#' @description
#' Simple RSA
#'
#' Softness and non-obedience are optimized.
#'
#' klValueFactor parameter is fixed at 1.
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
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamAB_iterative}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamAB <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), abs(params[2]), 1))
}


#' Cost function for three parameter optimization for the utterance choice experiments.
#' Optimizing softness and non-obedience and klValueFactor.
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamABK_iterative}}.
#'
#' This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
#' @export
SimpleRSAModelUttKLDivParamABK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), abs(params[2]), params[3]))
}

