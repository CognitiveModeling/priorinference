% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SRSA_UttChoiceOptimization.R
\name{SimpleRSAModelUttKLDivParamB}
\alias{SimpleRSAModelUttKLDivParamB}
\title{Cost function for one parameter optimization for the utterance choice experiments.
Optimizing non-obedience.
Non-obedience and klValueFactor are fixed.}
\usage{
SimpleRSAModelUttKLDivParamB(par, data)
}
\arguments{
\item{par}{One value vector, which specifies one of three parameters to be optimized:
\enumerate{
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

Non-obedience is optimized.

softness parameter is fixed at 0.
klValueFactor is fixed at 1.
}
\details{
This is the non-iterative version of \code{\link{SimpleRSAModelUttKLDivParamB_iterative}}.

This function uses \code{\link{SimpleRSAModelUttKLDiv_3params}}.
}
