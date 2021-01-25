# RSApackage - test

This package contains all the functions of the common code of the RSA model. This package was designed to analyze and model data for Experiments 1 and 2 in the paper Learning about others: Modeling social inference through ambiguity resolution (Achimova et al. (submitted)).

## To be able to use the RSApackage you need to follow these steps:
1. Install the package `devtools`.

Run the code below:
```
install.packages("devtools")
```

2. Run the command:
```
library("devtools")
```

3. Now you will be able to install the `RSApackage` by running:
```
install_github("haniaelkersh/rsa-publish-test")
```

Visit the [website](https://haniaelkersh.github.io/rsa-publish-test/index.html) to look at the documentation of the [functions](https://haniaelkersh.github.io/rsa-publish-test/reference/index.html).

## To replicate the experiments 1 and 2 from the paper open the matching manual and follow the steps.

### Experiment 1
#### _Inferring preferences_
The goal of Experiment 1 is to check the inferences of the pragmatic speaker having observed that a listener selects some objects in response to an utterance _u_. Is it possible to draw inferences about the most likely preferences the listener had when making her choice? Can this inference process be modeled by our RSA model—that is, by recursive Bayesian inference?

#### Task
Participants were presented with a series of reference game scenarios modeled after Figure 1 from (Frank & Goodman, 2012). Each scenario featured two people (simulated speaker and listener) and three objects. The sets of objects used could vary along three dimensions (shape, texture and color). The speaker produced a single-word utterance (e.g. “cloud”) to refer to one of the objects and the listener picked one of the objects in response. Experiment participants were told that the listener might have a preference for certain object features: For example, she might prefer clouds over squares, or red things over green things. The participants’ task was to infer those preferences by adjusting the sliders for each of the features after observing the speaker’s utterance and the listener’s object choice.

[click here to see the experiment](http://www.socsci.uci.edu/~gscontra/experiments/prior_inference/4-pilot-training/prior-inference.html)

### Experiment 2
#### _Epistemic utterance choice_
The goal of Experiment 2 is to check the predictions of the strategic utterance selection model. Given a set of potential referents _S_, will participants reason pragmatically about the anticipated potential epistemic utility of utterances $$u \in U$$ for inferring the listener’s preferences?

#### Task
Participants encountered a reference game scenario similar to Experiment 1. The task was to help the speaker choose an utterance that was most likely to reveal the listener’s shape, pattern or color preferences. The same sets of objects from Experiment 1, which could vary along three dimensions, were used.
Each trial featured a set of three objects. Participants adjusted sliders to indicate whether a single-feature utterance could help the speaker learn about the preferences of their listener. Potential utterances corresponded to the features of the objects present; depending on the number of unique features, participants adjusted between three and nine sliders.

[click here to see the experiment](http://www.socsci.uci.edu/~gscontra/experiments/prior_inference/3-pilot-utterance-choice/prior-inference.html)
