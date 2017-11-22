;;;; =================================================================
;;;; DRI
;;;; -----------------------------------------------------------------
;;;; An ACT-R model for the DRI experiment by Patrick J. Rice.
;;;; -----------------------------------------------------------------
;;;; General idea is to have different models
;;;; Different models:
;;;;


(clear-all)
(define-model dri

(sgp :auto-attend t)

(chunk-type parity-fact number parity)
(chunk-type (dri-stimulus (:include visual-object))
			  number left right)
(add-dm (even isa chunk) (odd isa chunk)
	(a isa chunk) (b isa chunk)
	(one isa chunk) (two isa chunk)
	(three isa chunk) (four isa chunk)
	(five isa chunk) (six isa chunk)
	(seven isa chunk) (eight isa chunk)
	(nine isa chunk)
	(one-odd isa parity-fact
		 number one parity odd)
	(two-even isa parity-fact
		 number two parity even)
	(three-odd isa parity-fact
		 number three parity odd)
	(four-even isa parity-fact
		 number four parity even)
	(five-odd isa parity-fact
		 number five parity odd)
	(six-even isa parity-fact
		 number six parity even)
	(seven-odd isa parity-fact
		   number seven parity odd)
	(eight-even isa parity-fact
		    number eight parity even)
	(nine-odd isa parity-fact
		  number nine parity odd))

(add-dm     (stim1 isa dri-stimulus
	       width 400
	       height 300
	       number nine
	       left a
	       right b))

;;; RESPONSE MONKEY

(p look-at-screen
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

(p look-at-target
   ?visual>
     state free

   =visual>
     value stimulus
==>
   +visual-location>
     kind target
)

(p look-at-rule
   ?visual>
     state free

   =visual>
     value rule
==>
   +visual-location>
     value target
)

(p response-monkey
   ?visual>
     buffer full

  =visual>
   - kind screen

   ?manual>
     preparation free
     processor free
     execution free

==>
   +manual>
     isa punch
     hand right
     finger index
)


;;; ---- End dummy

;; Checking parity

(p can-proceed
   =visual>
     number =NUM
   =retrieval>
     number =NUM
     parity =PAR
     target =TRGT
   =imaginal>
     target =PAR
   ==>
   *imaginal>
     target =TRGT
)


(p must-infer
   =visual>
     number =NUM
   =retrieval>
     number =NUM
     parity =PAR
   =imaginal>
   - target =PAR
   ==>
   @retrieval> =imaginal
   +imaginal>
     type rule
     kind inferred
     side1 nil
     target1 nil)
     
    

;; Process of inferring a rule

(p reverse-parity
   =retrieval>
     type rule
     parity1 =S
     target1 =T
   =imaginal>
     kind inferred
     parity1 nil
==>)
        
	       
(p respond
   ?imaginal>
     state free
   ?manual>
     preparation free
     execution free
   =imaginal>
     response =R
==>
   +manual>
     cmd punch
     finger =R
     )
); end of model
