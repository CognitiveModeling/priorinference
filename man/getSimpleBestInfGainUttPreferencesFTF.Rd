% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SRSA_UttChoiceOptimization_FeatureTypeFocus.R
\name{getSimpleBestInfGainUttPreferencesFTF}
\alias{getSimpleBestInfGainUttPreferencesFTF}
\title{Feature Type Focus:
Get the utterances for the speaker's best information gain}
\usage{
getSimpleBestInfGainUttPreferencesFTF(
  currentObjects,
  featureTypeFocus,
  softPrefValue,
  notObeyInst,
  klValueFactor
)
}
\arguments{
\item{currentObjects}{A vector of three values in \code{{1,...,27}} specifying the target and the other two objects in the scene.

The target is the first object in the vector \code{(index = 1)}.}

\item{featureTypeFocus}{A value between 1-3. It specifies the feature the speaker is interested to know more about and is not allowed to utter.
(shape =  1, pattern =  2, color = 3).}

\item{softPrefValue}{A parameter value between \code{[0,infinity)} (The larger the value the higher the tendency towards uniform liking).

Value reflects how categorical the listener's preferences are:

\strong{0:} The listener always picks her preferred object.

If the listener prefers \emph{red} objects, she will always pick the \emph{red} object in the scene.

\strong{infinity:} It is as likely for the listener to pick \emph{green}, \emph{blue} or \emph{red} objects.}

\item{notObeyInst}{Determines the extent to which the instruction of the speaker is obeyed by the listener.

(0 = full obedience, infinity = full instruction ignorance).

\strong{Example:}

\strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.

\strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}

\item{klValueFactor}{A parameter that can be negative, 0 or positive:
\describe{
\item{zero}{Don't care about learning about feature preferences of the listener}
\item{positive}{Care about learning about feature preferences of the listener}
\item{negative}{Trying to pick non-ambiguous utterances}
}}
}
\value{
A vector containing the normalized probability over utterances given the listener's object preference priors.

The utterance with the highest probability is the one that maximizes the information gain for the speaker.

The vector has the same length as the validUtterancesFTF vector.
}
\description{
Simple RSA

Determines the optimal utterance for the best information gain.

These are based on the valid utterances determined from the current objects in the scene.

The inferred listener's object preferences are computed assuming the listener
picks a certain object and has certain object preferences.

Utterances of the feature type focus are filtered out (e.g. if you are interested in color preferences - so you are not allowed to utter colors!)
validUtterancesFTF correspond to all features present in the current objects except those of the featureTypeFocus!
}
\examples{
In the case of these objects being in a scene:

     [shape] [pattern] [color]
[1,] "cloud"  "solid" "blue"
[2,] "circle" "solid" "blue"
[3,] "square" "solid" "blue"

and these being the indices for the valid utterances:

[1] 1 2 3 4 7 (cloud, circle, square, solid, blue)

Since you are interested in shape (featureTypeFocus = 1) you are only
allowed to utter "solid" (index = 4) or "blue" (index = 7):

validUtterancesFTF:

[1] 4 7

Then uttering solid or blue would be best in order to gain
information about the shape preferences of the listener:
\donttest{getSimpleBestInfGainUttPreferencesFTF (currentObjects, featureTypeFocus,
softPrefValue, notObeyInst, klValueFactor)}

output:
[1] 0.0 0.0 0.0 0.5 0.5

You can see here that the indices with the highest probability, namely 4 and 5,
correspond to the indices in the validUtterance vector for the feature
values 4 (solid) (index = 4) and 7 (blue) (index = 5).

}
