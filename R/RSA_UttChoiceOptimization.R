#' Get the utterances for the speaker's best information gain
#'
#' @description
#' Full-RSA
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
#'
#' @param alpha A parameter value between 0 and 1.
#'
#' Exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.
#'
#' @param klValueFactor A parameter that can be negative, 0 or positive:
#' \describe{
#' \item{\strong{zero}}{Don't care about learning about the feature preferences of the listener}
#' \item{\strong{positive}}{Care about learning about the feature preferences of the listener}
#' \item{\strong{negative}}{Trying to pick non-ambiguous utterances}
#' }
#' @return A vector containing the normalized probability over utterances given the listener's object preference priors.
#'
#' The utterance with the highest probability is the one that maximizes the information gain for the speaker.
#'
#' The vector has the same length as the validUtterances vector.
#'
#' @details
#' This function uses the function \code{{bestInfGainUtterance}}.
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
#' Then uttering solid or blue would be best in order to gain information
#' about the shape preferences of the listener:
#' \donttest{getBestInfGainUttPreferences(currentObjects, softPrefValue,
#' notObeyInst, alpha, klValueFactor)}
#'
#' output:
#' [1] 0.0 0.0 0.0 0.5 0.5
#'
#' You can see here that the indices with the highest probability, namely 4 and 5,
#' correspond to the indices in the validUtterance vector for the feature
#' values 4 (solid) (index = 4) and 7 (blue) (index = 5).
#' @export
getBestInfGainUttPreferences <- function(currentObjects, softPrefValue, notObeyInst, alpha, klValueFactor) {
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
  return( bestInfGainUtterance(preferencesPrior, validUtterances, currentObjects,
                               uttToObjProbs, objectPreferenceSoftPriors,
                               alpha, klValueFactor) )
}


#' Main cost function for three parameter optimization for the utterance choice experiments.
#' Optimizing softness, non-obedience and klValueFactor.
#'
#' @description
#' Full-RSA
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
#'
#' @param par1 \describe{
#' \item{softness parameter}{A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking). 
#'
#' Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, 
#' \emph{blue} or \emph{red} objects.}
#' }
#' 
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
#' 
#' @param par3 \describe{
#' \item{klValueFactor}{A parameter that can be negative, 0 or positive (Here it is set to = 1):}
#' \describe{
#' \item{zero}{Don't care about learning about the feature preferences of the listener}
#' \item{positive}{Care about learning about the feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#'
#' @details
#' This function uses \code{\link{getBestInfGainUttPreferences}}.
#'
#' And is used by \code{\link{RSAModelUttKLDivParamA}},
#'
#' \code{\link{RSAModelUttKLDivParamB}},
#'
#' \code{\link{RSAModelUttKLDivParamC}},
#'
#' \code{\link{RSAModelUttKLDivParamBC}},
#'
#' \code{\link{RSAModelUttKLDivParamAC}},
#'
#' \code{\link{RSAModelUttKLDivParamAB}},
#'
#' \code{\link{RSAModelUttKLDivParamABC}}.
#' @export
RSAModelUttKLDiv_3params <- function(data, par1, par2, par3) {
  #   print(c(par1, par2, par3, data))
  llRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    numUtterances <- data[i,4]
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    bInfGainUttModel <- rep(NA, 9)
    bInfGainUttModel[validUtterances] <- getBestInfGainUttPreferences(currentObjects, par1, par2, 1, par3)
    ## adding the negative log likelihoods
    for(j in c(1:length(validUtterances))) {
      llRes <- llRes + data[i, 4+validUtterances[j]] *
        (log(data[i, 4+validUtterances[j]] + 1e-100) - log(bInfGainUttModel[validUtterances[j]] + 1e-100) )
    }
    #    print(c(data[i, 4+validUtterances],9999,bInfGainUttModel[validUtterances],8888))
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(llRes)
}


#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing softness.
#' Non-obedience and klValueFactor are fixed.
#'
#' @description
#' Full-RSA
#'
#' Softness is optimized.
#'
#' Non-obedience is fixed at 0 and klValueFactor at 1.
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
#' @param data  A matrix with data rows.
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
#' This function uses \code{\link{RSAModelUttKLDiv_3params}}.
#'
#' @export
RSAModelUttKLDivParamA <- function(par, data) {
  return(RSAModelUttKLDiv_3params(data, abs(par[1]), 0, 1))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing non-obedience.
#' Softness and klValueFactor are fixed.
#'
#' @description
#' Full-RSA
#'
#' Non-obedience is optimized.
#'
#' Softness is fixed at 0 and klValueFactor at 1.
#'
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{RSAModelUttKLDiv_3params}}.
#'
#' @export
RSAModelUttKLDivParamB <- function(par, data) {
  return(RSAModelUttKLDiv_3params(data, 0, abs(par[1]), 1))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing klValueFactor.
#' Softness and non-obedience are fixed.
#'
#' @description
#' Full-RSA
#'
#' klValueFactor is optimized.
#'
#' softness and non-obedience parameter are fixed at 0.
#'
#' @param par One value vector, which specifies one of three parameters to be optimized:
#' \enumerate{
#'    \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#'
#' @details
#' This function uses \code{\link{RSAModelUttKLDiv_3params}}.
#'
#' @export
RSAModelUttKLDivParamC <- function(par, data) {
  return(RSAModelUttKLDiv_3params(data, 0, 0, abs(par[1])))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing non-obedience and klValueFactor.
#' Softness is fixed.
#'
#' @description
#' Full-RSA
#'
#' Non-obedience and klValueFactor are optimized.
#'
#' Softness is fixed at 0.
#' @param params Two value vector specifying the two parameters to be optimized.
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
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#'
#' @details
#' This function uses \code{\link{RSAModelUttKLDiv_3params}}.
#' @export
RSAModelUttKLDivParamBC <- function(params, data) {
  return(RSAModelUttKLDiv_3params(data, 0, abs(params[1]), abs(params[2])))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing softness and klValueFactor.
#' Non-obedience is fixed.
#'
#' @description
#' Full-RSA
#'
#' Softness and klValueFactor are optimized.
#'
#' Non-obedience is fixed at 0.
#' @param params Two value vector specifying the two parameters to be optimized.
#' \enumerate{
#' \item{softness: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#' Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#' \item{klValueFactor: A parameter that can be negative, 0 or positive (Here it is set to = 1):
#' \describe{
#' \item{\strong{zero}}{Don't care about learning about the feature preferences of the listener}
#' \item{\strong{positive}}{Care about learning about the feature preferences of the listener}
#' \item{\strong{negative}}{Trying to pick non-ambiguous utterances}
#' }}
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
#'
#' @details
#' This function uses \code{\link{RSAModelUttKLDiv_3params}}.
#' @export
RSAModelUttKLDivParamAC <- function(params, data) {
  return(RSAModelUttKLDiv_3params(data, abs(params[1]), 0, abs(params[2])))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing softness and non-obedience.
#' klValueFactor is fixed.
#'
#' @description
#' Full-RSA
#'
#' Softness and non-obedience are optimized.
#'
#' klValueFactor is fixed at 1.
#' @param params Two value vector specifying the two parameters to be optimized.
#' \enumerate{
#' \item{softness A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#' Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#' \item{non-obedience: Determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#' }
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{RSAModelUttKLDiv_3params}}.
#' @export
RSAModelUttKLDivParamAB <- function(params, data) {
  return(RSAModelUttKLDiv_3params(data, abs(params[1]), abs(params[2]), 1))
}

#' Cost function for three parameter optimization for the utterance choice experiments.
#' Optimizing softness, non-obedience and klValueFactor.
#'
#' @description
#' Full-RSA
#'
#' Softness, non-obedience and klValueFactor are optimized.
#'
#' @param params Three value vector specifying the three parameters to be optimized.
#' \enumerate{
#' \item{softness: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#' Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
#' \item{non-obedience: Determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#' \item{klValueFactor: A parameter that can be negative, 0 or positive (Here it is set to = 1):
#' \describe{
#' \item{\strong{zero}}{Don't care about learning about the feature preferences of the listener}
#' \item{\strong{positive}}{Care about learning about the feature preferences of the listener}
#' \item{\strong{negative}}{Trying to pick non-ambiguous utterances}
#' }}
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
#'
#' @details
#' This function uses \code{\link{RSAModelUttKLDiv_3params}}.
#' @export
RSAModelUttKLDivParamABC <- function(params, data) {
  return(RSAModelUttKLDiv_3params(data, abs(params[1]), abs(params[2]), abs(params[3])))
}

#' Main cost function for four parameter optimization for the utterance choice experiments.
#' Optimizing softness, non-obedience, alpha and klValueFactor.
#'
#' @description
#' Full-RSA
#'
#' The actual RSA model Kullback-Leibler divergence determination for utterance choice experiments.
#'
#' Softness, non-obedience, alpha and klValueFactor are optimized.
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
#'
#' @param par1 \describe{
#' \item{softness parameter}{A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).
#'
#' Value reflects how categorical the listener's preferences are:
#'
#' \strong{0:} The listener always picks her preferred object.
#'
#' If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.
#'
#' \strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}}
#' @param par2 \describe{
#' \item{non-obedience parameter}{Determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}}
#'
#' @param par3 \describe{
#' \item{alpha parameter}{A parameter value between 0 and 1.
#'
#' This parameter is an exponential scaling of the speaker choosing the
#' utterance that maximizes the chance of the listener getting the target object right.}
#' }
#'
#' @param par4 klValueFactor can be negative, 0 or positive:
#' \describe{
#' \item{\strong{zero}}{Don't care about learning about the feature preferences of the listener}
#' \item{\strong{positive}}{Care about learning about the feature preferences of the listener}
#' \item{\strong{negative}}{Trying to pick non-ambiguous utterances}
#' }
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#'
#' @details
#' This function uses \code{\link{getBestInfGainUttPreferences}}.
#'
#' And is used by
#'
#' \code{\link{RSAModelUttKLDivParamD4}},
#'
#' \code{\link{RSAModelUttKLDivParamD4A.2}},
#'
#' \code{\link{RSAModelUttKLDivParamD4B.2}},
#'
#' \code{\link{RSAModelUttKLDivParamD4AB.2}},
#'
#' \code{\link{RSAModelUttKLDivParamAD4}},
#'
#' \code{\link{RSAModelUttKLDivParamBD4}},
#'
#' \code{\link{RSAModelUttKLDivParamAD4B.2}},
#'
#' \code{\link{RSAModelUttKLDivParamBD4A.2}},
#'
#' \code{\link{RSAModelUttKLDivParamCD4}},
#'
#' \code{\link{RSAModelUttKLDivParamBCD4}},
#'
#' \code{\link{RSAModelUttKLDivParamACD4}},
#'
#' \code{\link{RSAModelUttKLDivParamABD4}},
#'
#' \code{\link{RSAModelUttKLDivParamABCD4}}.
#' @export
RSAModelUttKLDiv_4params <- function(data, par1, par2, par3, par4) {
  #   print(c(par1, par2, par3, data))
  llRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    numUtterances <- data[i,4]
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    bInfGainUttModel <- rep(NA, 9)
    bInfGainUttModel[validUtterances] <- getBestInfGainUttPreferences(currentObjects, par1, par2, par3, par4)
    ## adding the negative log likelihoods
    for(j in c(1:length(validUtterances))) {
      llRes <- llRes + data[i, 4+validUtterances[j]] *
        (log(data[i, 4+validUtterances[j]] + 1e-100) - log(bInfGainUttModel[validUtterances[j]] + 1e-100) )
    }
    #    print(c(data[i, 4+validUtterances],9999,bInfGainUttModel[validUtterances],8888))
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(llRes)
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing klValueFactor.
#' Softness, non-obedience and alpha are fixed.
#'
#' @description
#' Full-RSA
#'
#' klValueFactor is optimized.
#'
#' Softness and non-obedience are set to 0.
#' Alpha is fixed at 1.
#'
#' @param params One value vector specifying the parameter to be optimized.
#' \enumerate{
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, 0, 1, params[1]))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing klValueFactor.
#' Softness, non-obedience and alpha are fixed.
#'
#' @description
#' Full-RSA
#'
#' Softness is fixed at 0.2.
#'
#' non-obedience is fixed at 0.
#'
#' Alpha is  fixed at to 1.
#'
#' @param params One value vector specifying the parameter to be optimized.
#' \enumerate{
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamD4A.2 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, .2, 0, 1, params[1]))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing klValueFactor.
#' Softness, non-obedience and alpha are fixed.
#'
#' @description
#' Full-RSA
#'
#' klValueFactor is optimized.
#'
#' Softness is fixed at 0.
#'
#' Non-obedience is fixed at 0.2.
#'
#' Alpha is fixed at 1.
#' @param params One value vector specifying the parameter to be optimized.
#' \enumerate{
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamD4B.2 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, .2, 1, params[1]))
}

#' Cost function for one parameter optimization for the utterance choice experiments.
#' Optimizing klValueFactor.
#' Softness, non-obedience and alpha are fixed.
#'
#' @description
#' Full-RSA
#'
#' klValueFactor is optimized.
#'
#' Softness and non-obedience are fixed at 0.2.
#'
#' Alpha is fixed at 1.
#'
#' @param params One value vector specifying the parameter to be optimized.
#' \enumerate{
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamD4AB.2 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, .2, .2, 1, params[1]))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing Softness and klValueFactor.
#' Non-obedience and alpha are fixed.
#'
#' @description
#' Full-RSA
#'
#' Softness and klValueFactor are optimized.
#'
#' Non-obedience is set to 0.
#'
#' Alpha is set to 1.
#'
#' @param params Two value vector specifying two of the four parameters to be optimized:
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
#' }}
#' }
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
#'
#' @return Minimized Kullback-Leibler divergence and the optimal parameters.
#' @details
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamAD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), 0, 1, params[2]))
}


#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing non-obedience and klValueFactor.
#' Softness and alpha are fixed.
#'
#' @description
#' Full-RSA
#'
#' Non-obedience and klValueFactor are optimized.
#'
#' Softness is fixed at 0.
#'
#' Alpha is fixed at 1.
#' @param params Two value vector specifying two of the four parameters to be optimized:
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
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamBD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, abs(params[1]), 1, params[2]))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing softness and klValueFactor.
#' Non-obedience and alpha are fixed.
#'
#' @description
#' Full-RSA
#'
#' Softness and klValueFactor are optimized.
#'
#' Non-obedience is fixed at 0.2.
#'
#' Alpha is fixed at 1.
#' @param params Two value vector specifying two of the four parameters to be optimized:
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
#'
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamAD4B.2 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), .2, 1, params[2]))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing non-obedience and klValueFactor.
#' Softness and alpha are fixed.
#'
#' @description
#' Full-RSA
#'
#' Non-obedience and klValueFactor are optimized.
#'
#' Softness is fixed at 0.2.
#'
#' Alpha is fixed at 1.
#' @param params Two value vector specifying two of the four parameters to be optimized:
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
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamBD4A.2 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, .2, abs(params[1]), 1, params[2]))
}

#' Cost function for two parameter optimization for the utterance choice experiments.
#' Optimizing alpha and klValueFactor.
#' Softness and non-obedience are fixed.
#'
#' @description
#' Full-RSA
#'
#' Alpha and klValueFactor are optimized.
#'
#' Softness and non-obedience are fixed at 0.
#' @param params Two value vector specifying two of the four parameters to be optimized:
#' \enumerate{
#' \item{alpha: A parameter value between 0 and 1.
#'
#' Exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
#' \item{klValueFactor: can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamCD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, 0, abs(params[1]), params[2]))
}

#' Cost function for three parameter optimization for the utterance choice experiments.
#' Optimizing non-obedience, alpha and klValueFactor.
#' Softness is fixed.
#'
#' @description
#' Full-RSA
#'
#' Non-obedience, alpha and klValueFactor are optimized.
#'
#' Softness is fixed at 0.
#' @param params Three value vector specifying three of the four parameters to be optimized:
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
#' \item{alpha: A parameter value between 0 and 1.
#'
#' Exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#' @details
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamBCD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, abs(params[1]), abs(params[2]), params[3]))
}

#' Cost function for three parameter optimization for the utterance choice experiments.
#' Optimizing softness, alpha and klValueFactor.
#' Non-obedience is fixed.
#'
#' @description
#' Full-RSA
#'
#' Softness, alpha and klValueFactor are optimized.
#'
#' Non-obedience is fixed at 0.
#' @param params Three value vector specifying three of the four parameters to be optimized:
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
#' \item{alpha: A parameter value between 0 and 1.
#'
#' Exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#' @details
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamACD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), 0, abs(params[2]), params[3]))
}

#' Cost function for three parameter optimization for the utterance choice experiments.
#' Optimizing softness, non-obedience and klValueFactor.
#' Alpha is fixed.
#'
#' @description
#' Full-RSA
#'
#' Softness, non-obedience and klValueFactora re optimized.
#'
#' Alpha is fixed at 1.
#' @param params Three value vector specifying three of the four parameters to be optimized:
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
#' \item{non-obedience: Determines the extent to which the instruction of the speaker is obeyed by the listener.
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
#' }}
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
#' @details
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamABD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), abs(params[2]), 1, params[3]))
}

#' Cost function for four parameter optimization for the utterance choice experiments.
#' Optimizing softness, non-obedience, alpha and klValueFactor.
#'
#' @description
#' Full-RSA
#'
#' Softness, non-obedience, alpha and klValueFactor are optimized.
#'
#' @param params Four value vector specifying four of the four parameters to be optimized:
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
#' \item{non-obedience: Determines the extent to which the instruction of the speaker is obeyed by the listener.
#'
#' (0 = full obedience, infinity = full instruction ignorance).
#'
#' \strong{Example:}
#'
#' \strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.
#'
#' \strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
#' \item{alpha: A parameter value between 0 and 1.
#'
#' Exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
#' \item{klValueFactor can be negative, zero or positive:
#' \describe{
#' \item{zero}{Don't care about learning about feature preferences of the listener}
#' \item{positive}{Care about learning about feature preferences of the listener}
#' \item{negative}{Trying to pick non-ambiguous utterances}
#' }}
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
#' @details
#' This function uses \code{\link{RSAModelUttKLDiv_4params}}.
#' @export
RSAModelUttKLDivParamABCD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), abs(params[2]), abs(params[3]), params[4]))
}

