% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllUtterancesAndObjects.R
\name{determineUttToObjectProbs}
\alias{determineUttToObjectProbs}
\title{Determine utterances to object probabilities}
\usage{
determineUttToObjectProbs(
  validUtterances,
  currentObjects,
  mapObjToUtt,
  notObeyInst
)
}
\arguments{
\item{validUtterances}{A vector of utterances that correspond to all feature values present
in the current objects in the scene.

For example, it only makes sense to utter \emph{"red"} in a scene if there are \emph{red} objects present.}

\item{currentObjects}{A vector of three values in range \code{{1,...,27}} specifying
the target and the other two objects in the scene.

The target is the first object in the vector \code{(index = 1)}.}

\item{mapObjToUtt}{A 3x3 matrix. Values in the matrix are in range \code{{1-9}}.

\strong{rows:} The current objects in the scene

\strong{columns:} Features of the objects}

\item{notObeyInst}{Determines the extent to which the instruction of the speaker is obeyed by the listener.

(0 = full obedience, infinity = full instruction ignorance).

\strong{Example:}

\strong{0:} Listener always picks \emph{red} objects following the utterance \emph{"red"}.

\strong{infinity:} Listener as likely to pick \emph{green, blue} or \emph{red} objects even if the utterance is \emph{"red"}.}
}
\value{
A matrix. The rows map each possible utterance that corresponds to each present feature value
of the current objects. The columns represent the three objects in the scene.

This reflects the obedience-parameter and which objects match the respective utterance.
The matrix shows the probability that a certain object is chosen following a certain utterance, that is valid in the scene.
The number of rows of the matrix match the length of the validUtterances vector.
}
\description{
This function determines maps the valid utterances to the corresponding objects in the scene.

It then determines the probability of choosing an object based on the valid utterances in the scene.
}
\details{
This function is only used in X9
}
\examples{
\donttest{determineUttToObjectProbs(validUtterances, currentObjects, mappedObjToUtt, notObeyInst)}

output:
     [,1] [,2] [,3]
[1,] 1.00 0.00 0.00
[2,] 0.00 1.00 0.00
[3,] 0.00 0.00 1.00
[4,] 0.33 0.33 0.33
[5,] 0.33 0.33 0.33

Example:

To see which objects are present in the scene run:
\donttest{allObjects[currentObjects,]}

output:

      shape   pattern  color
[1,] "cloud"  "solid" "blue"
[2,] "circle" "solid" "blue"
[3,] "square" "solid" "blue"


The columns correspond to the three objects in the scene:
A solid blue cloud, a solid blue circle and a solid blue square.
If the utterance is circle the probability of choosing circle is 1,
while the probability for the cloud and the square is 0.
If the utterance is blue the probability of choosing the cloud,
the circle or the square is 1/3, since all three objects are blue.
The same goes for the utterance solid.

}
