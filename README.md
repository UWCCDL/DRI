# DRI Experiment, Data and Model

This repository contains the experimental data from Patrick
J. Rice's DRI (Delayed Rule Inference) experiment, together with the 
cognitive model designed to reproduce and explain the results.


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

The model was implemented in the ACT-R Cognitive architecture
(http://act-r.psy.cmu.edu/), version 7.5.

The model follows a simpe strategy. The timeline of the task can be
divided into three processing phases:

  1. The _Encoding_ phase_. This phase spans the moment from the onset
  of instructions on the screen to the moment the participant presses
  the button. During this phase, the instructions are committed to WM
  in a rather _amodal_ way, e.g. as a verbal code ("Even, Index").

  2. The _Preparation_ phase. During the preparation phase, the
  verbal instructions are used to prepare mental sensorimotor
  plans. The translation only happens for the specific motor command
  specified by the instructions. Thus, "Even, Index" is translated as
  an activation of an "index" motor chunk, which is placed in WM
  instead of the word "index".

  3. The _Execution_ phase. During the execution phase, the parity is
  checked. Two things can happen:

      3.1  If the parity is verified, then the prepared visuo/motor
      response is executed as planned.

	  3.2  If the parity is violated (__inferred__ instructions), then
      a new preparation phase is initiated. The new plan is then
      executed.
		 

The effect of TMS occurs only during the _Preparation_ phase for
symbolic rules. Thus, it does not show up during the _Encoding_ phase,
and shows up instead for the _Execution_ phase only whenever the
preparation needs to be re-done (__inferred__ trials).
