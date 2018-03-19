;;;; =================================================================
;;;; DRI
;;;; -----------------------------------------------------------------
;;;; An ACT-R model for the DRI experiment by Patrick J. Rice.
;;;; -----------------------------------------------------------------
;;;; General idea is to have different models
;;;; 
;;;; Implementing TMS:
;;;; TMS is implemented by adding 500ms to the execution time
;;;; of any production that could be targeted. Productions encode
;;;; control, so the assumption is legit.
;;;; (Potentially, all of the prods could be tested!!!).
;;;;
;;;; to do:
;;;; Add a DM phase, in which the model decides whether it is the case
;;;; to respond or to invert.
;;;; ------------------------------------------------------------------

(clear-all)
(define-model dri

(sgp :auto-attend t           ; Automatic encoding
     :esc t                   ; Yes to subsymbolic (need spreading activation) 
     :visual-activation 2     ; Source of activation
     :imaginal-activation 2   ; Source of activation
     :mas 2                   ; Max spreading activation
     )


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

	;; Basic structures
	(stimulus isa chunk)
	(rule isa chunk)
	(screen isa chunk)
	(pause isa chunk)
	(option isa chunk)
	(target isa chunk)
	(action isa chunk)
	(instructions isa chunk)
	(can-proceed isa chunk)
	(mist-infer isa chunk)

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

	;;; Alternatives
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
		  
	)

;;; VISUAL PROCESSING

(p look-at-screen
   "Looks at the screen if nothing to process" 
   ?visual>
     state free
     buffer empty
   ?visual-location>
     state free
     buffer empty
==>
   +visual-location>
     kind screen
)

(p recover-from-visual-change
   "If the visual scene changes abrubtly, re-encode the screen"
   ?visual>
     error t
==>
   +visual-location>
     kind screen
)

;;; RULE ENCODING PHASE

(p look-at-rule
   "Look at the rule part of the screen"
   ?visual>
   - state error
     state free

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

;;; This is the part where the model should rehearse

(p cleanup-wm
   "After encoding, cleanup WM and retrieve the instructions"
   ?visual>
     state free

   ?imaginal>
     state free

   =visual>
     kind action
     value =ACTION
    
   =imaginal>
   - rule nil
   - action nil
==>
  =visual>
  )


(p retrieve-instructions
   "After letting instructions decay from WM, retrieve them"
   ?visual>
     state free

   ?imaginal>
     state free
     buffer empty

   ?retrieval>
     state free
   
   =visual>
     kind action
     value =ACTION
    
==>
   +retrieval>
   - rule nil
   =visual>
)



;;; ------------------------------------------------------------------
;;; PREPARATION PHASE
;;; ------------------------------------------------------------------
;;; During the preparation phase, the commands are prepared.
;;; ------------------------------------------------------------------

(p prepare-concrete-response
   "Prepares a plan to respond in case of a concrete rule"
   =retrieval>
     rule   =RULE
     action =ACTN
   - action A
   - action B  

   =visual>
     kind action
   
   ?imaginal>
     state free
     buffer empty
==>
   =visual>  
   +imaginal>
     isa motor-plan
     kind motor
     parity =RULE
     response =ACTN
)   

(p prepare-abstract-response
   "Prepares a plan to respond in case of a concrete rule"
   =retrieval>
     rule   =RULE
     action =ACTN
   - action index
   - action middle

   =visual>
     kind action
   
   ?imaginal>
     state free
     buffer empty
 ==>
   =visual>  
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
   =imaginal>
   - parity nil

   =visual>
     kind action   
   
   ?imaginal>
     state free

   ?manual>
     preparation free
     processor free
     execution free

==>
   -visual>     
   =imaginal>
   +manual>
     isa punch
     hand right
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

(p look-at-target
   ?visual>
     state free

   ?imaginal>
     state free
     buffer full
     
   =visual>
     value stimulus

==>

   +visual-location>
     kind target
)


(p retrieve-parity
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
   ?imaginal>
     state free
     buffer full    ;; Just need to know we have a chunk.

   =goal>
     step processing
   
   =visual>
     kind target
     
   =retrieval>
     isa parity-fact
     parity =PARITY
==>
   =goal>
     step respond
)

;;; ------------------------------------------------------------------
;;; REPLAN
;;; ------------------------------------------------------------------
;;; Reprepares the imaginal buffer if parity does not match
;;; ------------------------------------------------------------------

(p parity-not-verified
   =imaginal>
     rule =PARITY
     action =ACT
     alternative =OTHER

   =goal>
     step processing
   
   =visual>
     kind target
     
   =retrieval>
     isa parity-fact
   - parity =PARITY
==>
   =imaginal>
     action =OTHER
   =goal>
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
   =goal>
     step respond
   
   =imaginal>
     kind motor
     response =RESP

   ?manual>
     preparation free
     processor free
     execution free

==>
   +manual>
     isa punch
     hand right
     finger =RESP
)

;;; RESPONSE for ABSTRACT (visuomotor) CONDITION

(p find-letter
   =imaginal>
     letter =SYMBOL

   =goal>
     step respond  
     
   ?visual>
     state free
==>
   =imaginal>
   +visual-location>
     kind option
     value =SYMBOL
)


(p respond-letter
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

==>
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

) ;; end of model
