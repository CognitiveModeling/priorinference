% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SRSA_UttChoiceOptimization_FeatureTypeFocus.R
\name{SimpleRSAModelUttKLDivFTFParamK.2.0}
\alias{SimpleRSAModelUttKLDivFTFParamK.2.0}
\title{Feature Type Focus: Cost function for one parameter optimization for the utterance choice experiments.
Optimizing klValueFactor.
Softness and non-obedience are fixed.}
\usage{
SimpleRSAModelUttKLDivFTFParamK.2.0(par, data)
}
\arguments{
\item{par}{One value vector specifying one of the three parameters to be optimized.
\enumerate{
\item{klValueFactor: A parameter that can be negative, 0 or positive:
\describe{
\item{zero}{Don't care about learning about feature preferences of the listener}
\item{positive}{Care about learning about feature preferences of the listener}
\item{negative}{Trying to pick non-ambiguous utterances}
}}}}

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

klValueFactor is optimized.

Softness is fixed at 0.2.
Non-obedience is fixed at 0.
}
\details{
This function uses \code{\link{SimpleRSAModelUttKLDiv_3paramsFTF}}.
}
