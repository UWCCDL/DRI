;;; ==================================================================
;;; SIMON-DEVICE.LISP
;;; ==================================================================
;;; (c) 2017, Andrea Stocco, University of Washington
;;;           stocco@uw.edu
;;; ==================================================================
;;; A class that provide an ACT-R GUI interface for a modified
;;; version of the DRI Task.
;;; ==================================================================


;; If using Swank on Emacs; otherwise, nil
(defparameter *using-swank* t)

;;; ----------------------------------------------------------------
;;; ACT-R Functions
;;; ----------------------------------------------------------------

(defun act-r-loaded? ()
  "Checks whether ACTR is loaded"
  (member :ACT-R *features*))


(defun dri-reload ()
  (reload)
  (install-device (make-instance 'dri-task))
  (init (current-device))
  (proc-display))

;; ---------------------------------------------------------------- ;;
;; Some utilities
;; ---------------------------------------------------------------- ;;

(defun pick (lst)
  "Picks up an element from a list"
  (when  (listp lst)
    (elt lst (random (length lst)))))


(defun scramble (lst &optional (sofar nil))
  "Scrambles a list of different elements"
  (if (null lst)
      sofar
    (let ((picked (pick lst)))
      (scramble (remove picked lst) (cons picked sofar)))))

(defun scramble* (lst)
  "Scrambles any list of objects"
  (let ((l (length lst))
        (pos nil))
    (dotimes (i l)
      (push i pos))
    (mapcar #'(lambda (x) (elt lst x)) (scramble pos))))

(defun mean (&rest nums)
  (when (every #'numberp nums)
    (/ (reduce #'+ nums)
       (length nums))))


(defun divide-into-pairs (lst &optional (partial nil) (open nil))
  "Recursively divides a list into pairs"
  (cond ((null lst)
	 (append partial open))
	((= (length (car open)) 2)
	 (divide-into-pairs (rest lst)
			    (append partial open)
			    (list (list (first lst)))))
	((= (length (car open)) 1)
	 (divide-into-pairs (rest lst)
			    partial
			    (list (list (caar open)
					(first lst)))))
	(t
	 (divide-into-pairs (rest lst)
			    partial
			    (list (list (first lst)))))))


;;; ------------------------------------------------------------------
;;; SIMON TASK DATA STRUCTURES
;;; ------------------------------------------------------------------

(defparameter *example-rule* '(odd a))

(defparameter *example-stimulus* '(9 (b a)))

(defparameter *letters* '(a b))

(defparameter *fingers* '(index middle))

(defparameter *parity* '(odd even))

(defun letter? (ltr)
  (member ltr *letters*))

(defun finger? (fin)
  (member fin *fingers*))

(defun parity? (prt)
  (member prt *parity*))

(defun dri-rule? (lst)
  (and (consp lst)
       (= 2 (length lst))
       (parity? (first lst))
       (or (letter? (second lst))
	   (finger? (second lst)))))

(defun rule-parity (rule)
  (when (dri-rule? rule)
    (first rule)))

(defun rule-action (rule)
  (when (dri-rule? rule)
    (second rule)))

(defun rule-type (rule)
  (when (dri-rule? rule)
    (if (letter? (rule-action rule))
	'symbolic
	'concrete)))

(defun dri-stimulus? (stm)
  (and (consp stm)
       (= 2 (length stm))
       (= 2 (length (second stm)))
       (numberp (first stm))
       (or (every #'letter? (second stm))
	   (every #'finger? (second stm)))))

(defun stimulus-target (stm)
  (when (dri-stimulus? stm)
    (first stm)))

(defun stimulus-options (stm)
  (when (dri-stimulus? stm)
    (second stm)))

(defun stimulus-type (stim rule)
  (let ((parity (if (equalp (rule-parity rule) 'odd)
		    'oddp
		    'evenp)))
    (if (funcall parity (stimulus-target stim))
	'instructed
	'inferred)))
	      

(defparameter *response-mappings* '((0 . index) (1 . middle)))

(defparameter *key-finger-mappings* '((#\j . index) (#\k . middle))
  "Key/Finger mappings")

(defun stimulus-correct-response (stim rule)
  (let* ((parity (rule-parity rule))
	 (rtype (rule-type rule))
	 (num (stimulus-target stim))
	 (func (if (equalp parity 'odd)
		   'oddp
		   'evenp)))
    (if (funcall func num)
	(if (equalp rtype 'symbolic)
	    (cdr (assoc (position (rule-action rule)
				  (stimulus-options stim))
			*response-mappings*))
	    (rule-action rule))
	(if (equalp rtype 'symbolic)
	    (cdr (assoc (position (car (remove (rule-action rule)
					       (stimulus-options stim)
					       :test #'equalp))
				  (stimulus-options stim))
			*response-mappings*))
	    (car (remove (rule-action rule) *fingers*))))))


(defun make-dri-trial (rule stim)
  (let ((trial (list rule stim 0 0 0 0 nil nil nil nil)))
    (set-trial-correct-response trial (stimulus-correct-response stim rule))
    (set-trial-rule-type trial (rule-type rule))
    (set-trial-stimulus-type trial (stimulus-type stim rule))
    trial))

(defun trial-rule (trial)
  (nth 0 trial))

(defun set-trial-rule (trial rule)
  (when (dri-rule? rule)
    (setf (nth 0 trial) rule)))
  
(defun trial-stimulus (trial)
  (nth 1 trial))

(defun set-trial-stimulus (trial stimulus)
  (when (dri-stimulus? stimulus)
    (setf (nth 1 trial) stimulus)))

(defun trial-rule-onset-time (trial)
  (nth 2 trial))

(defun set-trial-rule-onset-time (trial tme)
  (setf (nth 2 trial) tme))

(defun trial-rule-response-time (trial)
  (nth 3 trial))

(defun set-trial-rule-response-time (trial tme)
  (setf (nth 3 trial) tme))

(defun trial-stimulus-onset-time (trial)
  (nth 4 trial))

(defun set-trial-stimulus-onset-time (trial tme)
  (setf (nth 4 trial) tme))

(defun trial-stimulus-response-time (trial)
  (nth 5 trial))

(defun set-trial-stimulus-response-time (trial tme)
  (setf (nth 5 trial) tme))

(defun trial-correct-response (trial)
  (nth 6 trial))

(defun set-trial-correct-response (trial response)
  (setf (nth 6 trial) response))

(defun trial-actual-response (trial)
  (nth 7 trial))

(defun set-trial-actual-response (trial response)
  (setf (nth 7 trial) response))

(defun trial-rule-type (trial)
  (nth 8 trial))

(defun set-trial-rule-type (trial typ)
  (setf (nth 8 trial) typ))

(defun trial-stimulus-type (trial)
  (nth 9 trial))

(defun set-trial-stimulus-type (trial typ)
  (setf (nth 9 trial) typ))

;;; STIM GENERATION AND ANALYSIS

(defun trial-stimulus-response-latency (trial)
  (- (trial-stimulus-response-time trial)
     (trial-stimulus-onset-time trial)))

(defun trial-rule-response-latency (trial)
  (- (trial-rule-response-time trial)
     (trial-rule-onset-time trial)))


(defun trial-accuracy (trial)
  (if (equal (trial-correct-response trial)
	     (trial-actual-response trial))
      1
      0)) 

(defparameter *targets* '(1 2 3 4 6 7 8 9))

(defun generate-rules ()
  (let ((partials nil))
    (dolist (parity '(odd even) partials)
      (dolist (targets '((a b) (index middle)))
	(dolist (target targets)
	  (push (list parity target) partials))))))

(defun generate-stimuli ()
  (let ((partials nil))
    (dolist (j *targets* partials)
      (dolist (options '((a b) (b a)))
	(push (list j options) partials)))))

(defun generate-trials ()
  (let ((trials nil))
    (dolist (rule (generate-rules) trials)
      (dolist (stim (generate-stimuli))
	(push (make-dri-trial rule stim) trials)))))
  

;;;  DRI Task object ------------------------------------------- ;;;


(defclass dri-task ()
  ((phase :accessor task-phase
	  :initform nil)
   (index :accessor index
	  :initform nil)
   (trials :accessor trials
	   :initform (generate-trials))
   (current-trial :accessor current-trial
		  :initform nil)
   (experiment-log :accessor experiment-log
		   :initform nil))
  (:documentation "A manager for the DRI task"))

(defmethod init ((task dri-task))
  "Initializes the DRI task manager"
  (unless (null (trials task))
    (setf (index task) 0)
    (setf (experiment-log task) nil)
    (setf (trials task) (scramble* (trials task)))
    (setf (current-trial task)
	  (nth (index task) (trials task)))
    (setf (task-phase task) 'rule)))


(defmethod respond ((task dri-task) key)
  "Records a response in the DRI task"
  (unless (null (current-trial task))
    (let* ((trial (current-trial task))
	   (phase (task-phase task))
	   (response (cdr (assoc key *key-finger-mappings*))))
      ;;(format t "Oila~%")
      (cond ((equal phase 'stimulus)
	     
	     (set-trial-actual-response trial response)
	     (when (act-r-loaded?)
	       (set-trial-stimulus-response-time (current-trial task)
						 (mp-time))
	       ;;(if (= 1 (trial-accuracy (current-trial task)))
	       ;;   (trigger-reward 1)
	       ;; (trigger-reward -1))
	       (schedule-event-relative 0 #'next :params (list task))))
	    ((equal phase 'rule)
	     (when (act-r-loaded?)
	       (schedule-event-relative 0 #'next :params (list task))))))))
            

(defmethod next ((task dri-task))
  "Moves to the next step in a DRI timeline"
  (cond ((equal (task-phase task) 'rule)
	 (setf (task-phase task) 'stimulus)
	 (when (act-r-loaded?)
	   (set-trial-rule-response-time (current-trial task)
					 (mp-time))
	   (set-trial-stimulus-onset-time (current-trial task)
					  (mp-time))
	   (start-tms)))
	
	((equal (task-phase task) 'stimulus)
	 (setf (task-phase task) 'pause)
	 (push (current-trial task) (experiment-log task))
	 (setf (current-trial task) nil)
	 (when (act-r-loaded?)
	   (schedule-event-relative 1 'next :params (list task))))
				    
	((equal (task-phase task) 'pause)
	 (incf (index task))
	 (cond ((>= (index task) (length (trials task)))
		(setf (task-phase task) 'done))
	       (t
		(setf (task-phase task) 'rule)
		(setf (current-trial task) (nth (index task)
						(trials task)))
		(when (act-r-loaded?)
		  (set-trial-rule-onset-time (current-trial task)
					     (mp-time)))))))
  (when (act-r-loaded?) 
    (schedule-event-relative 0 'proc-display :params nil)))

;;; ------------------------------------------------------------------
;;; ACT-R DEVICE INTERFACE
;;; ------------------------------------------------------------------
;;; These functions turn the Simon-Task class into an ACT-R device
;;; ------------------------------------------------------------------

(defmethod device-handle-keypress ((task dri-task) key)
  "Converts the key into a symbol and passes it on to the task manager"
  ;;(respond task (intern (string-capitalize (format nil "~a" key)))))
  (respond task key))

			   
(defmethod device-handle-click ((task dri-task))
  "Does nothing"
  (declare (ignore task))
  nil)

(defmethod device-move-cursor-to ((task dri-task) pos)
  "Does nothing"
  (declare (ignore task))
  nil)

(defmethod get-mouse-coordinates ((task dri-task))
  "Does nothing"
  (declare (ignore task))
  (vector 0 0))

(defmethod cursor-to-vis-loc ((task dri-task))
  "Does nothing"
  (declare (ignore task))
  nil)

(defparameter *screen-width* 400)

(defparameter *screen-height* 300)

(defparameter *char-width* 20)

(defparameter *char-height* 20)

(defun symbol-length (sym)
  (when (symbolp sym)
    (length (format nil "~A" sym))))

(defmethod build-vis-locs-for ((task dri-task) vismod)
  (let ((results nil)
	(phase (task-phase task)))
    (cond ((equalp (task-phase task) 'stimulus)
	   (setf results (build-vis-locs-for-stimulus
			  (trial-stimulus (current-trial task)))))
				     
	  ((equalp (task-phase task) 'rule)
	   (setf results (build-vis-locs-for-rule
			  (trial-rule (current-trial task))))))
    #|
    (push `(isa visual-location
		kind screen
		value ,phase
		color black
		screen-x 0
		screen-y 0
		height 400 
		width 400)
	  results) |#
    (define-chunks-fct results)))

(defun build-vis-locs-for-stimulus (stimulus)
  (let* ((results nil)
	 (target (stimulus-target stimulus))
	 (options (stimulus-options stimulus)))
    (push  `(isa visual-location 
		 kind target
		 value ,target
		 color black
		 ;;screen-x ,(round (/ (- *screen-width* *char-width*) 2))
		 ;;screen-y ,(round (/ (- *screen-height* *char-height*) 2))
		 ;;height ,*char-height*
		 ;;width ,*char-width*
		 screen-x ,(round (/ *screen-width* 3))
		 screen-y ,(round (/ *screen-height* 3))
		 height ,(round (/ *screen-height* 3))
		 width ,(round (/ *screen-width* 3))
		 )
	   results)
    
    (let ((n (1+ (length options))))
      (dolist (option options results)
	(push  `(isa visual-location 
		     kind option
		     value ,option
		     color black
		     screen-x ,(round (+ (* (1+ (position option options))
					    (/ *screen-width* n))
					 (* -1 (/ *char-width* 2))))
		     screen-y ,(round (* (/ *screen-height* 3) 2))
		     height ,*char-height*
		     width ,*char-width*)
	       results)))))

(defun build-vis-locs-for-rule (rule)
  (let* ((results nil))
    (let ((n (1+ (length rule)))
	  (roles '(rule action))) 
      
      (dolist (object (reverse (pairlis rule roles)) results)
	;;(format t "~A~%" object)
	(push  `(isa visual-location 
		     kind ,(cdr object)
		     value ,(car object)
		     color black
		     screen-x ,(round (/ (- *screen-width*
					    (* *char-width*
					       (symbol-length (car object))))
					 2))
		     screen-y ,(round (- (* (1+ (position (car object) rule))
					    (/ *screen-height* n))
					 (/  *char-height* 2)))
		     height ,*char-height*
		     width ,*char-width*)
	       results)))))


(defmethod vis-loc-to-obj ((task dri-task) vis-loc)
  "Transforms a visual-loc into a visual object"
  (let ((new-chunk (first (define-chunks-fct 
			      `((isa dri-object))))))
    (fill-default-vis-obj-slots new-chunk vis-loc)
    (set-chunk-slot-value-fct new-chunk 'kind
			      (chunk-slot-value-fct vis-loc 'kind))
    (let* ((x (chunk-slot-value-fct vis-loc 'screen-x))
	   (left (/ *screen-width* 3))
	   (center (/ *screen-width* 2))
	   (right (* left 2))
	   (pos nil))
      
      (cond ((< (abs (- left x)) *char-width*)
	     (setf pos 'left))
	    ((< (abs (- right x)) *char-width*)
	     (setf pos 'right))
	    ((< (abs (- center x)) *char-width*)
	     (setf pos 'center)))
      
      (set-chunk-slot-value-fct new-chunk 'location pos))
    new-chunk))

#|
(defmethod vis-loc-to-obj ((stimulus list) vis-loc)
  "Transforms a stimulus into a visual object"
  (first (define-chunks-fct 
	     `((isa simon-stimulus
		    kind simon-stimulus 
		    ,@stimulus
		    )))))

(defmethod vis-loc-to-obj ((phase symbol) vis-loc)
  "Transforms a stimulus into a visual object"
  (first (define-chunks-fct 
	     `((isa simon-screen
		    kind simon-screen 
		    value ,phase
		    )))))

|#

;;; ------------------------------------------------------------------
;;; DATA COLLECTION AND STATS
;;; ------------------------------------------------------------------

(defun summarize-trial (trial)
  "Summarizes a trial into a list of values"
  (list (trial-rule trial)
	(trial-stimulus trial)
	(trial-rule-type trial)
	(trial-stimulus-type trial)
	(trial-rule-response-latency trial)
	(trial-stimulus-response-latency trial)
	(trial-accuracy trial)))

(defparameter *col-names* '("Rule" "Stimulus" "RuleType" "StimulusType"
			    "EncodingRT" "ExecutionRT" "Accuracy")
  "List of column names in log CSV file")

(defun write-csv (table filename)
  "Writes a list of trials (formatted as in 'extract-results') in a CSV file"
  (with-open-file (out filename
		       :direction :output
		       :if-exists :append
		       :if-does-not-exist :create)
    (dolist (row table)
      (format out "~{~a~^,~}~%" row))))

(defun save-log (&optional (filename "model.txt"))
  (let* ((log (mapcar #'summarize-trial
		      (reverse (experiment-log (current-device)))))
	 (table (push *col-names* log)))
    (write-csv table filename)))

(defun analyze-log (log)
  "Analyzes the log of a single run"
  (let* ((incong (remove-if #'trial-congruent? log))
	 (cong (remove-if-not #'trial-congruent? log))
	 (correct-incong (remove-if-not #'(lambda (x) (= (trial-accuracy x) 1))
					incong))
	 (correct-cong (remove-if-not #'(lambda (x) (= (trial-accuracy x) 1))
					cong)))
    
    (if (or (null correct-incong)
	    (null correct-cong))
	;; If we don't have enough trials, return NA
	;;'((:congruent :na) (:incongruent :na))
	'(:na :na :na :na)
	;; Otherwise, compute accuracies and RTs (on correct trials)
	(let* ((cong-acc (apply #'mean (mapcar #'trial-accuracy cong)))
	       (incong-acc (apply #'mean (mapcar #'trial-accuracy incong)))
	       (cong-rt (apply #'mean (mapcar #'trial-rt correct-cong)))
	       (incong-rt (apply #'mean (mapcar #'trial-rt correct-incong))))
	  (list cong-acc cong-rt incong-acc incong-rt)))))


(defun result? (lst)
  "A list is a result IFF it's made of at least eight numbers"
  (and (>= (length lst) 8)
       (every #'(lambda (x) (or (numberp x)
				(keywordp x)))
	      lst)))

(defun average-results (results)
  "Averages values across a list of results"
  (if (every #'result? results)
    (let* ((meanres nil)
	   (n (length (first results))))
      (dotimes (i n (reverse meanres))
	(let ((avg
	       (float
		(apply 'mean
		       (remove-if-not #'numberp
				      (mapcar #'(lambda (x) (nth i x))
					      results))))))
	  (push avg meanres))))
    (progn
      (format t "   Not every row is a result")
      (setf *results* results))))


