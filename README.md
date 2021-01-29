# prior inference R package 

This package contains all the functions of the common code of the prior inference and utterance choice models. It was designed to analyze and model data for Experiments 1 and 2 in the paper _Learning about others: Modeling social inference through ambiguity resolution_ (Asya Achimova, Gregory Scontras, Christian Stegemann-Philipps, Johannes Lohmann, Martin V. Butz (submitted)).

## To be able to use the `priorinference` package you need to follow these steps:
1. Install the package `devtools`.

Run the code below:
```
install.packages("devtools")
```

2. Run the command:
```
library("devtools")
```

3. Now you will be able to install the `priorinference` by running:
```
install_github("haniaelkersh/priorinference@HEAD")
```

Visit the [website](https://haniaelkersh.github.io/priorinference/index.html) to look at the documentation of the [functions](https://haniaelkersh.github.io/priorinference/reference/index.html).

You can also take a look at the pdf version of the manual [website](https://haniaelkersh.github.io/blob/main/docs/priorinference_manual.pdf)


## Description of the Experiments 1 and 2

### Experiment 1
#### _Inferring preferences_
The goal of Experiment 1 is to check the inferences of the pragmatic speaker having observed that a listener selects some objects in response to an utterance <img src="https://render.githubusercontent.com/render/math?math=u">. Is it possible to draw inferences about the most likely preferences the listener had when making her choice? Can this inference process be modeled by our RSA model—that is, by recursive Bayesian inference?

#### Task
Participants were presented with a series of reference game scenarios modeled after Figure 1 from (Frank & Goodman, 2012). Each scenario featured two people (simulated speaker and listener) and three objects. The sets of objects used could vary along three dimensions (shape, texture and color). The speaker produced a single-word utterance (e.g. “cloud”) to refer to one of the objects and the listener picked one of the objects in response. Experiment participants were told that the listener might have a preference for certain object features: For example, she might prefer clouds over squares, or red things over green things. The participants’ task was to infer those preferences by adjusting the sliders for each of the features after observing the speaker’s utterance and the listener’s object choice.

<img width="400" alt="preference-trial" src="https://user-images.githubusercontent.com/40029289/105725656-87b4c100-5f29-11eb-9668-2a167e99809c.png" >

[click here to see the experiment](http://www.socsci.uci.edu/~gscontra/experiments/prior_inference/4-pilot-training/prior-inference.html)

### Experiment 2
#### _Epistemic utterance choice_
The goal of Experiment 2 is to check the predictions of the strategic utterance selection model. Given a set of potential referents <img src="https://render.githubusercontent.com/render/math?math=S">, will participants reason pragmatically about the anticipated potential epistemic utility of utterances <img src="https://render.githubusercontent.com/render/math?math=u \in U"> for inferring the listener’s preferences?

#### Task
Participants encountered a reference game scenario similar to Experiment 1. The task was to help the speaker choose an utterance that was most likely to reveal the listener’s shape, pattern or color preferences. The same sets of objects from Experiment 1, which could vary along three dimensions, were used.
Each trial featured a set of three objects. Participants adjusted sliders to indicate whether a single-feature utterance could help the speaker learn about the preferences of their listener. Potential utterances corresponded to the features of the objects present; depending on the number of unique features, participants adjusted between three and nine sliders.


<img width="400" alt="utterance-choice-trial" src="https://user-images.githubusercontent.com/40029289/105725855-b59a0580-5f29-11eb-98b2-e1bd32635035.png">

[click here to see the experiment](http://www.socsci.uci.edu/~gscontra/experiments/prior_inference/3-pilot-utterance-choice/prior-inference.html)
