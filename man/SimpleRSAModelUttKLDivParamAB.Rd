% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SRSA_UttChoiceOptimization.R
\name{SimpleRSAModelUttKLDivParamAB}
\alias{SimpleRSAModelUttKLDivParamAB}
\title{Cost function for two parameter optimization for the utterance choice experiments.
Optimizing softness and non-obedience.
klValueFactor is fixed.}
\usage{
SimpleRSAModelUttKLDivParamAB(params, data)
}
\arguments{
\item{params}{Two value vector specifying two of the three parameters to be optimized.
\enumerate{
\item{softPrefValue: A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).

 Value reflects how categorical the listener's preferences are:

\strong{0:} The listener always picks her preferred object.

If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.

\strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
\item{non-obedience: This parameter determines the extent to which the instruction of the speaker is obeyed by the listener.

(0 = full obedience, infinity = full instruction ignorance).

\strong{Example:}

\strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.

\strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
 }}

\item{data}{A matrix with data rows.

column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}

\strong{1:OC1} Object 1. A value between 1 and 27.

\strong{2:OC2} Object 2. A value between 1 and 27.

\strong{3:OC3} Object 3. A value between 1 and 27.

\strong{4:numUttOptions} The number of valid utterances in the scene.

\strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.}
}
\value{
Minimized Kullback-Leibler divergence and the optimal parameters.
}
\description{
Simple RSA

Softness and non-obedience are optimized.

klValueFactor parameter is fixed at 1.
}
\details{
This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamAB_iterative}}.

This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
}
