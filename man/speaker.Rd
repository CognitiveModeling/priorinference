% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RSA_StratUtt.R
\name{speaker}
\alias{speaker}
\title{Speaker function}
\usage{
speaker(obj, listenerObjectPreferences, validUtterances, uttToObjProbs, alpha)
}
\arguments{
\item{obj}{A vector of three values in \code{{1,...,27}} specifying the target and the other two objects in the scene.

The target is the first object in the vector \code{(index = 1)}.}

\item{listenerObjectPreferences}{One of the rows of the list of preference priors
for all valid utterances based on the object in the scene.

The list has as many rows as the length of the validUtterances vector + 1.

Each row in the list contains a vector of length 3, as there are three objects in the scene.

The extra row is for the case of no feature preferences whatsoever, i.e. uniform prior over all three objects in the scene.}

\item{validUtterances}{A vector of utterances that correspond to all feature values present
in the current objects in the scene.

For example, it only makes sense to utter \emph{"red"} in a scene if there are \emph{red} objects present.}

\item{uttToObjProbs}{A matrix. The rows map each possible utterance that corresponds to each present feature value
of the current objects. The columns represent the three objects in the scene.

This reflects the obedience-parameter and which objects match the respective utterance.
The matrix shows the probability that a certain object is chosen following a certain utterance, that is valid in the scene.
The number of rows of the matrix match the length of the validUtterances vector.}

\item{alpha}{A parameter between 0 and 1.

Exponential scaling of the speaker choosing the utterance that maximizes the chance of the listener getting the target object right.}
}
\value{
P(utt | obj, listener's object preferences)

A vector of normalized probability for a certain utterance given object
and listener's object preferences.

The vector has the same length as the validUtterances vector.
}
\description{
Full-RSA

This function determines the utterances that increase the likelihood of the listener
picking the object the speaker is referring to.
}
\examples{
listenerObjectPreferences <- objectPreferenceSoftPriors[[3]]
\donttest{speaker(obj, listenerObjectPreferences, validUtterances, uttToObjProbs, alpha)}

output:
[1] 0.6 0.0 0.0 0.2 0.2

}
