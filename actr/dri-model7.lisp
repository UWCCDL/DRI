;;;; =================================================================
;;;; DRI MODEL
;;;; -----------------------------------------------------------------
;;;; An ACT-R model for the DRI (Delayed Rule Inferral) experiment
;;;; by Patrick J. Rice.
;;;; -----------------------------------------------------------------
;;;;                            _____
;;;;                           (Start)
;;;; . . . . . . . . . . . . . .``|``. . . . . . . . . . . . . . . . .
;;;;                              |                     Encoding Phase
;;;;                           Encode
;;;;                        Instructions
;;;;                              |
;;;;                           Action
;;;;                          Concrete?
;;;;                        /           \
;;;;                      Yes            No
;;;;                     /                 \
;;;;             Prepare                     Prepare
;;;;            Concrete                     Abstract
;;;;            Response                     Response
;;;; . . . . . . . . . . \ . . . . . . . . / . . . . . . . . . . . . .
;;;;                      \               /            Execution Phase
;;;;                       Encode Stimulus  
;;;;                              |
;;;;                       Retrieve parity
;;;;                              |
;;;;                        Parity Match?
;;;;                       /             \
;;;;                    Yes               No
;;;;                   /                    \
;;;;           Action                         Action
;;;;          Concrete?                      Concrete?
;;;;         /         \                    /         \
;;;;      Yes           No               Yes           No
;;;;       |             |                |             |
;;;;       |             |            Re-Prepare    Re-Prepare
;;;;       |             |             Concrete      Abstract
;;;;       |             |             Response      Response
;;;;       |             |                |             |
;;;;       |             +--------------- | ----+-------+
;;;;       |                              |     |
;;;;       +------+-----------------------+     |
;;;;              |                             |
;;;; . . . . . .  | . . . . . . . . . . . . . . | . . . . . . . . . . .
;;;;              |                             |              Response
;;;;           Respond                         Find
;;;;           Planned                       Planned
;;;;            Finger                        Letter
;;;;              |                             |
;;;;              |                           Respond
;;;;              |                           Letter
;;;;              |                          Position
;;;;              |                             |
;;;;              +--------------+--------------+
;;;; . . . . . . . . . . . . .   |  . . . . . . . . . . . . . . . . . .
;;;;                             |                         End of Trial
;;;;                          (DONE)
;;;;                           ````
;;;; ------------------------------------------------------------------

(clear-all)
(define-model dri6

(sgp :auto-attend t         ; Automatic encoding
     :esc t                 ; Yes to subsymbolic (need spreading activation) 
     :blc 2                 ; Base level constant (any effect?)
     :epl nil               ; Disable Prod Compilation
     :er t                  ; Enable randomness
     :ans 0.1               ; Retrieval noise
     :imaginal-delay 0.083
     ;;:alpha 0.5
     :conflict-set-hook tms-selection-hook
     :motor-feature-prep-time 0.15
     )


(set-visloc-default screen-y lowest)

(chunk-type parity-fact number parity)

(chunk-type (dri-object (:include visual-object))
	    kind location)

(chunk-type wm rule action kind even odd left right)

(chunk-type trial step)

(chunk-type pair target alternative)  ;; Probably not needed any longer?

(chunk-type visuomotor-plan parity letter left right kind)  

(chunk-type motor-plan parity response kind)

(add-dm (even isa chunk) (odd isa chunk)
	(a isa chunk) (b isa chunk)
	(processing isa chunk) (respond isa chunk)
	(done isa chunk)
	(center isa chunk)

	;; Basic structures
	(stimulus isa chunk)
	(rule isa chunk)
	(option isa chunk)
	(target isa chunk)
	(action isa chunk)
	(instructions isa chunk)
	(replan isa chunk)
	(motor isa chunk)
	(visuomotor isa chunk)

	;; Parity
	(one-odd isa parity-fact
		 number 1 parity odd)
	(two-even isa parity-fact
		 number 2 parity even)
	(three-odd isa parity-fact
		 number 3 parity odd)
	(four-even isa parity-fact
		 number 4 parity even)
	(five-odd isa parity-fact
		 number 5 parity odd)
	(six-even isa parity-fact
		 number 6 parity even)
	(seven-odd isa parity-fact
		   number 7 parity odd)
	(eight-even isa parity-fact
		    number 8 parity even)
	(nine-odd isa parity-fact
		  number 9 parity odd)

	;;; Paired Alternatives

	(index-middle isa pair
		      target index
		      alternative middle)

	(middle-index isa pair
		      target middle
		      alternative index)

	(a-b isa pair
	     target a
	     alternative b)

	(b-a isa pair
	     target b
	     alternative a)
		  
	)  ; End of chunks in DM

;;; VISUAL PROCESSING

(p look-at-location
   "Attend whatever is in the visual-location" 
   ?visual>
     state free
   - buffer full
     
   ?visual-location>
     state free
     buffer full
     
   =visual-location>
==>
   +visual>
     isa move-attention
     screen-pos =visual-location

)

;;; ------------------------------------------------------------------
;;; RULE ENCODING PHASE
;;; ------------------------------------------------------------------
;;; The rule encoding phase is made of two simple operations:
;;; Attending the "rule" part of the instructions (i.e., the parity
;;; cue "EVEN" or "ODD") and attending the "action" part (which can
;;; be either a finger or a letter, depending on the condition
;;; ------------------------------------------------------------------

(p look-at-rule
   "Look at the rule part of the screen"
   ?visual>
   - state error
     state free

   ?manual>
     preparation free
     processor free
     execution free
     
   =visual>
     kind screen
     value rule
==>
   +visual-location>
     kind rule
)

(p encode-rule
   "Encode the rule"
   ?visual>
     state free
   
   ?imaginal>
     state free
     buffer empty

   =visual>
     kind rule
     value =RULE
==>
  +imaginal>
     isa wm
     kind instructions
     rule =RULE

  +goal>
     isa trial
     step processing
     
  =visual>
)

(p look-at-action
   "After encoding the rule, look at the action (i.e., 'Index' or 'A')" 
   ?visual>
     state free

   ?imaginal>
     state free
     
   =visual>
     kind rule

   =imaginal>
   - rule nil
     action nil
==>
   =imaginal>     
   +visual-location>
     kind action  
)


(p encode-action
   "Encode the action"
   ?visual>
     state free

   ?imaginal>
     state free

   =visual>
     kind action
     value =ACTION
    
   =imaginal>
   - rule nil
     action nil
==>
  =visual>

  *imaginal>
     isa wm
     action =ACTION
     )

;;; ------------------------------------------------------------------
;;; PREPARATION PHASE
;;; ------------------------------------------------------------------
;;; During the preparation phase, the commands are prepared. 
;;; ------------------------------------------------------------------

(p prepare-concrete-response
   "Prepares a plan to respond in case of a concrete rule"
   =imaginal>
     rule   =RULE
     action =ACTN
   - action A
   - action B  
   
   ?imaginal>
     state free

==>
   +imaginal>
     isa motor-plan
     kind motor
     parity =RULE
     response =ACTN
)   

(p prepare-abstract-response
   "Prepares a plan to respond in case of a concrete rule"
   =imaginal>
     rule   =RULE
     action =ACTN
   - action index
   - action middle
   
   ?imaginal>
     state free

 ==>
   +imaginal>
     isa visuomotor-plan
     kind visuomotor
     parity =RULE
     letter =ACTN
     left index
     right middle
)


;;; ------------------------------------------------------------------
;;; MOVE THROUGH INSTRUCTIONS
;;; ------------------------------------------------------------------

(p move-on
   "Presses a key with the left hand to proceed after preparing the rule"
   =visual>
     kind action   

   =imaginal>
   - parity nil  

   ?visual>
     state free
   
   ?imaginal>
     state free

   ?manual>
     preparation free
     processor free
     execution free

==>
   =imaginal>     

   -visual-location>

   +visual>
     isa clear

   +manual>
     isa punch
     hand left
     finger index
)

;;; ------------------------------------------------------------------
;;; TARGET PROCESSING
;;; ------------------------------------------------------------------
;;; During target processing, we first establish the parity of a
;;; target. If the parity is consistent with what expected, we proceed
;;; with the prepared command. If not, we need to re-prepare the
;;; motor command.
;;; ------------------------------------------------------------------


(p retrieve-parity
   "When looking at a target number, retrieve its parity"
   ?visual>
     state free

   ?imaginal>
     state free
     buffer full
     
   ?retrieval>
     state free
     buffer empty
   
   =visual>
     kind target
     value =NUM

==>
  =visual>

  +retrieval>
    isa parity-fact
    number =NUM
)


(p parity-verified
   "If the parity matches the instructions, proceed to response"
   ?imaginal>
     state free

   =imaginal>
     parity =PARITY
   
   =goal>
     step processing
   
   =visual>
     kind target
     
   =retrieval>
     isa parity-fact
     parity =PARITY
==>
   =imaginal>     

   +goal>
     step respond
)

(p parity-not-verified
   "If the parity is not verified, redo the preparation"
   ?imaginal>
     state free

   =imaginal>
     parity =PARITY
   
   =goal>
     step processing
   
   =visual>
     kind target
     
   =retrieval>
     isa parity-fact
   - parity =PARITY
==>
   =imaginal>

   *goal>
     step replan
)


;;; ------------------------------------------------------------------
;;; RE-PREPARATION
;;; ------------------------------------------------------------------
;;; Reprepares the imaginal buffer if parity does not match
;;; ------------------------------------------------------------------


;;; REPLANNING CONCRETE RESPONSES ------------------------------------

(p remap-finger
   "Finds the opposite finger when parity is violated" 
   ?imaginal>
     state free

   ?retrieval>
     state free
     buffer empty

   =imaginal>
     response =FINGER
     kind motor
   
   =goal>
     step replan
   
==>
   +retrieval>
     isa pair
     target =FINGER
     
   =imaginal>
     
   =goal>
)

(p replan-concrete-response
   ?imaginal>
     state free

   ?retrieval>
     state free

   =imaginal>
     response =FINGER
     kind motor
   
   =goal>
     step replan
   
   =retrieval>
     isa pair
     target =FINGER
     alternative =NEW
==>
   *imaginal>
     response =NEW
   
   +goal>
     step respond  
)

;;; REPLANNING ABSTRACT RESPONSES ------------------------------------

(p remap-letter
   ?imaginal>
     state free

   ?retrieval>
     state free
     buffer empty

   =imaginal>
     letter =LETTER
     kind visuomotor
   
   =goal>
     step replan
   
==>
   +retrieval>
     isa pair
     target =LETTER
   =imaginal>     
   =goal>
)

(p replan-abstract-response
   ?imaginal>
     state free

   ?retrieval>
     state free

   =imaginal>
     letter =LETTER
     kind visuomotor
   
   =goal>
     step replan
   
   =retrieval>
     isa pair
     target =LETTER
     alternative =NEW
==>
   *imaginal>
     letter =NEW
   
   +goal>
     step respond  
)

;;; ------------------------------------------------------------------
;;; RESPONSE
;;; ------------------------------------------------------------------
;;; In this model, the responses have to be prepared before hand in
;;; the imaginal buffer
;;; ------------------------------------------------------------------

;;; RESPONSE for CONCRETE CONDITION

(p respond-concrete
   "Responds to concrete trial by copying the imaginal slot" 
   =goal>
     step respond
   
   =imaginal>
     kind motor
     response =RESP

   ?manual>
     preparation free
     processor free
     execution free

   ?visual>
     state free
   
   ?imaginal>
     state free  
==>
   -visual-location>

   +visual>
     isa clear
   
   +manual>
     isa punch
     hand right
     finger =RESP
)

;;; RESPONSE for ABSTRACT (visuomotor) CONDITION

(p find-letter
   "Scans the display looking for the letter stored in the imaginal plan"
   =imaginal>
     letter =SYMBOL

   =goal>
     step respond  
     
   ?visual>
     state free
   
   ?imaginal>
     state free  
==>
   =imaginal>
   +visual-location>
     kind option
     value =SYMBOL
)


(p respond-letter
   "When the letter is found, respond with index or middle finger depending on position"
   =goal>
     step respond

   =visual>
     kind option
     location =LOC
    
   =imaginal>
     =LOC =RESP  

  ?manual>
     preparation free
     processor free
     execution free

   ?visual>
     state free  
==>
   -visual-location>

   +visual>
     isa clear

   +manual>
     isa punch
     hand right
     finger =RESP
)

;;; DONE

(p done
   ?visual>
   - state error
     state free

   =visual>
     kind screen
     value done
==>
   !stop!
)

(sdp (a-b :base-level 5)
     (b-a :base-level 5)
     (index-middle :base-level 5)
     (middle-index :base-level 5))

) ;; end of model
