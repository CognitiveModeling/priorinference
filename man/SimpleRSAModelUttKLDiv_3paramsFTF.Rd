% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SRSA_UttChoiceOptimization_FeatureTypeFocus.R
\name{SimpleRSAModelUttKLDiv_3paramsFTF}
\alias{SimpleRSAModelUttKLDiv_3paramsFTF}
\title{FeatureTypeFocus: Main cost function for three parameter optimization for the utterance choice experiments.
Optimizing softness, non-obedience and klValueFactor.}
\usage{
SimpleRSAModelUttKLDiv_3paramsFTF(data, par1, par2, par3)
}
\arguments{
\item{data}{A matrix with data rows.

column structure: \code{[1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]}

\strong{1:OC1} Object 1. A value between 1 and 27.

\strong{2:OC2} Object 2. A value between 1 and 27.

\strong{3:OC3} Object 3. A value between 1 and 27.

\strong{4:numUttOptions} The number of valid utterances in the scene.

\strong{7-X:TurkerSliderValues} These columns contain the participants' slider values.}

\item{par1}{\describe{
\item{softness parameter}{A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).

Value reflects how categorical the listener's preferences are:

\strong{0:} The listener always picks her preferred object.

If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.

\strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}
}}

\item{par2}{\describe{
\item{non-obedience parameter}{Determines the extent to which the instruction of the speaker is obeyed by the listener.

(0 = full obedience, infinity = full instruction ignorance).

\strong{Example:}

\strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.

\strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
}}

\item{par3}{\describe{
\item{klValueFactor}{A parameter that can be negative, 0 or positive:
\describe{
\item{\strong{zero}}{Don't care about learning about the feature preferences of the listener}
\item{\strong{positive}}{Care about learning about the feature preferences of the listener}
\item{\strong{negative}}{Trying to pick non-ambiguous utterances}
}}}}
}
\value{
Minimized Kullback-Leibler divergence and the optimal parameters.
}
\description{
Simple RSA

Actual RSA model Kullback-Leibler divergence determination for the utterance choice experiments.

Softness, non-obedience and klValueFactor are optimized.
}
\details{
This function uses \code{\link{getSimpleBestInfGainUttPreferencesFTF}}.

This function is used in
\code{\link{SimpleRSAModelUttKLDivFTFParamA}},

\code{\link{SimpleRSAModelUttKLDivFTFParamB}},

\code{\link{SimpleRSAModelUttKLDivFTFParamK}},

\code{\link{SimpleRSAModelUttKLDivFTFParamBK}},

\code{\link{SimpleRSAModelUttKLDivFTFParamAK}},

\code{\link{SimpleRSAModelUttKLDivFTFParamAK.2}},

\code{\link{SimpleRSAModelUttKLDivFTFParamA.2}},

\code{\link{SimpleRSAModelUttKLDivFTFParamB.2}},

\code{\link{SimpleRSAModelUttKLDivFTFParamK.2.2}},

\code{\link{SimpleRSAModelUttKLDivFTFParamK.2.0}},

\code{\link{SimpleRSAModelUttKLDivFTFParamBK.2}},

\code{\link{SimpleRSAModelUttKLDivFTFParamAK.2}},

\code{\link{SimpleRSAModelUttKLDivFTFParamAB}},

\code{\link{SimpleRSAModelUttKLDivFTFParamABK}}.
}
