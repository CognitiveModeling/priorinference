#' Experiment 1 raw data (Inferring preferences)
#'
#' This file contains the raw data from prior inference experiment.
#'
#' @docType data
#'
#' @format A data frame with the following columns: 
#' \describe{
#'   \item{workerid}{Participant's ID}
#'   
#'   \item{item}{Unique ID of a stimulus}
#'  
#'   \item{slide_number}{Indicates trial order.}
#'   
#'   \item{language}{Participant self-reported native language}
#'   
#'   \item{pref1-pref6}{The feature preference of the listener 
#'   ('red things', 'clouds', 'striped things', etc.). 
#'   It is the corresponding property that the participants are rating.}
#'  
#'   \item{response1-response6}{The value to which the slider (1-6) was adjusted 
#'   by the participant. 
#'   The corresponding property that they are rating is stored under pref1-6. 
#'   Can be in range 0-1.
#'   The value indicates how much a listener likes objects with a particular property.
#'   Small numbers mean less liked, larger numbers mean property is preferred.}
#'   
#'   \item{target, obj2, obj3}{A three-digit code referring to the object that 
#'   the speaker intends to signal by the utterance (target), or to one of the 
#'   other two objects present in the scene (obj2, obj3).
#'   The first digit of the code refers to shape, the second digit to texture, 
#'   and the third digit to color of the object.
#'   
#'   \strong{1 Shape:} 1 cloud, 2 circle, 3 square
#'   
#'   \strong{2 Texture:} 1 solid, 2 striped, 3 polka-dotted
#'   
#'   \strong{3 Color:} 1 blue, 2 red, 3 green
#'   
#'   \strong{Example:} If the target is a solid blue square the code would be: 311.}
#'   
#'   \item{utterance}{Speaker’s utterance. A speaker chooses among the features present 
#'   in the scene. All possible utterances include: 'cloud', 'circle', 'square', 'solid', 
#'   'striped', 'polka-dotted', 'blue', 'red', 'green'.}
#'   
#'   \item{itemCode}{A six-digit code that indicates to which category the item belongs. 
#'   The categorization takes into account how many objects the utterance could possibly 
#'   refer to, and what properties the target object shares with other objects in 
#'   the scene. Each category consists of 3 tuples of 2 digits and refers to one feature, 
#'   where the first tuple always refers to the uttered feature, other features 
#'   are ordered from most ambiguous to less ambiguous. The more objects share the 
#'   feature value of the target object, the more ambiguous the feature is. 
#'   In the example below feature 1 is color since the utterance is 'blue', 
#'   feature 2 is pattern since the target object shares its pattern (solid) with 
#'   two other objects. Feature 3 is shape since the target object shares its shape 
#'   only with one other object. Feature order determines the order of tuples.
#'   
#'   The first digit in each tuple would then denote how many objects share the value 
#'   of the picked object for the corresponding feature. 
#'   We also reordered the objects so that the picked object would be the first. 
#'   In the experiment, the target object could take any place in the sequence.
#'   
#'   \strong{Example:} solid blue square, solid blue circle, solid green square.
#'   
#'   \strong{Ambiguity class:} 213222.
#'   
#'   \strong{Utterance: 'blue'}
#'   
#'   \strong{Target object:} blue solid square
#'   
#'   \strong{How to read code:}
#'   
#'   \strong{First tuple: 21 (color)}
#'   
#'   2: two objects (the target object + 1 more object) share color.
#'   
#'   1: the target object shares color with the second object
#'   
#'   \strong{Second tuple: 32 (pattern)}
#'   
#'   3: three objects share the pattern 'solid'.
#'   
#'   2: the target object shared the pattern with 2nd and 3rd objects
#'   
#'   \strong{Third tuple 22 (shape)}
#'   
#'   2: two objects (the target object + 1 more object) share shape.
#'   
#'   2: the target object shares shape with the third object. 
#'   
#'   The third object is not a possible competitor for being chosen, 
#'   since the utterance is 'blue', and the object is green.
#'   
#'   If a tuple starts with '3' or '1' the second digit would simply 
#'   denote whether the other objects have different values for that 
#'   feature or the same (See the second tuple above).
#'   }
#'   
#'   \item{ambiguous}{Refers to whether the utterance of the speaker is ambiguous or not. 
#'   
#'   \strong{Example:} In a scenario with a blue solid circle, 
#'   a blue striped square and a red polka-dotted cloud. 
#'   The utterance 'blue' is ambiguous.}
#' }
#' 
#' @keywords datasets
"X4_rawData"

#' Experiment 2 raw data (Epistemic utterance choice)
#'
#' This file contains the raw data from the utterance choice experiment.
#'
#' @docType data
#'
#' @format A data frame with the following columns: 
#' \describe{
#'   \item{workerid}{Participant's ID}
#'   
#'   \item{item}{Unique ID of a stimulus}
#'   
#'   \item{slide_number}{Indicates trial order.}
#'   
#'   \item{condition}{A six-digit code that infers to which category the item belongs. 
#'   In this experiment, it is only necessary to identify classes that have 
#'   potentially ambiguous utterances.}
#'   
#'   \item{language}{Participant self-reported native language}
#'   
#'   \item{pref1-pref9}{Utterance 1-9.}
#'   
#'   \item{response1- response9}{The value to which the slider (1-9) was adjusted 
#'   by the participant. Indicates how useful an utterance is for finding out 
#'   listener preferences. The corresponding property that they are rating is 
#'   stored under pref1-9. Can be in range 0-1.
#'   
#'   Small numbers mean less useful, larger numbers mean more useful.}
#'   
#'   \item{obj1, obj2, obj3}{A three-digit code referring to the object that the 
#'   speaker intends to signal by the utterance (obj1), or to one of the other 
#'   two objects present in the scene (obj2, obj3).
#'   
#'   The first digit of the code refers to shape, the second digit to texture, 
#'   and the third digit to color of the object.
#'   
#'   \strong{1 Shape:} 1 cloud, 2 circle, 3 square
#'   
#'   \strong{2 Texture:} 1 solid, 2 striped, 3 polka-dotted
#'   
#'   \strong{3 Color:} 1 blue, 2 red, 3 green
#'   
#'   \strong{Example:} If the target is a solid blue square the code would be: 311.}
#'   
#'   \item{ambiguous}{Marks whether a trial contains any ambiguous utterances.}
#'   
#'   \item{numFeatures}{The number of feature values present in a scene. 
#'   
#'   For example, if we have 3 red objects, all of them solid, 1 square, 1 cloud, 
#'   1 circle, then the number of features would be 5.
#'   
#'   Value in range 3-9.}
#' }
#' 
#' @keywords datasets
"X3_rawData"