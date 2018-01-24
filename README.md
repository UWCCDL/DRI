# DRI Model

This is an ACT-R model designed to simulate the results of Patrick
J. Rice's DRI (Delayed Rule Inference) experiment.


## DRI Paradigm

In the DRI study, participants were asked to perform a simple version
of RITL paradigm, in which they were first given a specific rule ("Odd
/ Index") and then asked to respond to the parity of an input number
("7", which is _Odd_, and therefore requires a response with the
_Index_ finger).

Each trial's _rule_ could be of one of two types:
  * __Concrete__ rules, like "Odd / Index", which directl specify
  which finger (index or middle) to use to respond;
  * __Abstract__ rules, like "Odd / A", which require to map the
  location (left or right) a visual response cue ("A" or "B")) on the
  screen with the corresponding finger (index or middle).

In addition, the condition called for two different possible
_applications_ of the rule:
  * __Instructed__ application, where the parity of the number is the
  same as specified in the instructions (i.e., "7" for "Odd / Index")
  * __Inferred__ application, where the parity of the number is
  different, and therefore the response was not specified in the
  instructions (i.e., "8" for "Odd / Index", which requires responing
  with the _middle_ finger). 

### TMS

During the experiment, participants received high-frequency TMS (5
Pulses @ 10 Hz) on the dorsal premotor cortex (PMd) or on the vertex
(as a control condition). Stimulation could be adminsitered during the
encoding phase.

### Results

The results showed a very specific effect, with TMS causing a
significant delay in response times (~350ms) only when applied to _PMd_
during the _execution_ phase of _abstract_ rules with inferred
application.

No other effect was significant (see Data).

## The Model

The model follows a simple strategy.