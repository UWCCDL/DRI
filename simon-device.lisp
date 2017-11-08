;;; ==================================================================
;;; SIMON-DEVICE.LISP
;;; ==================================================================
;;; (c) 2016, Andrea Stocco, University of Washington
;;;           stocco@uw.edu
;;; ==================================================================
;;; A class that provide an ACT-R GUI interface for a modified
;;; version of the Simon Task.
;;; ==================================================================


;; If using Swank on Emacs; otherwise, nil
(defparameter *using-swank* t)

;;; ----------------------------------------------------------------
;;; ACT-R Functions
;;; ----------------------------------------------------------------

(defun act-r-loaded? ()
  "Cheap hack to check whether ACTR is loaded"
  (and (fboundp 'run-n-events)
       (fboundp 'start-environment)))

;;; ------------------------------------------------------------------
;;; Modified action selection
;;; ------------------------------------------------------------------
;;; This is the code that modifies the action selection system in
;;; ACT-R, making it more consistent with the real basal ganglia.
;;; (see Stocco, "A biologically-plausible action selection system
;;; for cognitive architectures", submitted, Cog. Sci.)
;;; ------------------------------------------------------------------

(defparameter *d1* 1 "Dopamine D1 receptor density")

(defparameter *d2* 1 "Dopamine D2 receptor density")

(defparameter *bias* 10 "Processing bias")

(defparameter *error-penalty* -1)

(defun bg-reward-hook (production reward time)
  (declare (ignore time))
  (let* ((pname (symbol-name production))
	 (start (subseq pname 0 4)))

    (cond ((string-equal start "PROC")
	   (* *d1* reward))
	  ((string-equal start "DONT")
	   (* *d2* reward))
	  (t
	   0.0))))

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

(defparameter *default-simon-rule* '(shape . ((circle . left) (square . right))))

(defparameter *responses* '((f . left) (j . right)))

(defparameter *default-simon-congruent-stimuli* '((shape circle location left)
						 (shape square location right)))

(defparameter *default-simon-incongruent-stimuli* '((shape circle location right)
						    (shape square location left)))


(defun simon-stimulus? (lst)
  (and (consp lst)
       (evenp (length lst))))

(defun stimulus-correct-response (stimulus &optional (rule *default-simon-rule*))
  (when (simon-stimulus? stimulus)
    (let* ((dimension (first rule))
	   (value (second (assoc dimension (divide-into-pairs stimulus)))))
      (rest (assoc value (rest rule))))))

(defun stimulus-congruent? (stimulus)
  (let ((pos (second (assoc 'location (divide-into-pairs stimulus)))))
    (when (equalp pos (stimulus-correct-response stimulus))
      t)))


(defun stimulus-incongruent? (stimulus)
  (let ((pos (second (assoc 'location (divide-into-pairs stimulus)))))
    (unless (equalp pos (stimulus-correct-response stimulus))
      t)))

(defun stimulus-type (stimulus)
  (if (stimulus-congruent? stimulus)
    'congruent
    'incongruent))

(defun make-simon-trial (stim)
  (let ((trial (list stim 0 0 (stimulus-correct-response stim) nil nil)))
    (set-trial-type trial (stimulus-type stim))
    trial))

(defun trial-stimulus (trial)
  (nth 0 trial))

(defun set-trial-stimulus (trial stimulus)
  (when (simon-stimulus? stimulus)
    (setf (nth 0 trial) stimulus)))

(defun trial-onset-time (trial)
  (nth 1 trial))

(defun set-trial-onset-time (trial tme)
  (setf (nth 1 trial) tme))

(defun trial-response-time (trial)
  (nth 2 trial))

(defun set-trial-response-time (trial tme)
  (setf (nth 2 trial) tme))

(defun trial-correct-response (trial)
  (nth 3 trial))

(defun set-trial-correct-response (trial response)
  (setf (nth 3 trial) response))

(defun trial-actual-response (trial)
  (nth 4 trial))

(defun set-trial-actual-response (trial response)
  (setf (nth 4 trial) response))

(defun trial-type (trial)
  (nth 5 trial))

(defun set-trial-type (trial typ)
  (setf (nth 5 trial) typ))

(defun trial-congruent? (trial)
  (equalp (trial-type trial) 'congruent))

(defun generate-stimuli ()
  (let ((result nil))
    (dolist (stimulus *default-simon-congruent-stimuli*)
      (dotimes (i 75)
	(push (copy-seq stimulus) result)))
    (dolist (stimulus *default-simon-incongruent-stimuli*)
      (dotimes (i 25)
	(push (copy-seq stimulus) result)))
    result))

(defun generate-trials (stim-list)
  (mapcar #'make-simon-trial stim-list))

(defun trial-rt (trial)
  (- (trial-response-time trial)
     (trial-onset-time trial)))

(defun trial-accuracy (trial)
  (if (equal (trial-correct-response trial)
	     (trial-actual-response trial))
      1
      0)) 

;;;  Simon Task object ------------------------------------------- ;;;

(defclass simon-task ()
  ((phase :accessor task-phase
	  :initform nil)
   (index :accessor index
	  :initform nil)
   (trials :accessor trials
	   :initform (generate-trials (generate-stimuli)))
   (current-trial :accessor current-trial
		  :initform nil)
   (experiment-log :accessor experiment-log
		   :initform nil))
  (:documentation "A manager for the PSS task"))

(defmethod init ((task simon-task))
  "Initializes the PSS task manager"
  (unless (null (trials task))
    (setf (index task) 0)
    (setf (experiment-log task) nil)
    (setf (trials task) (scramble* (trials task)))
    (setf (current-trial task)
	  (nth (index task) (trials task)))
    (setf (task-phase task) 'stimulus)))


(defmethod respond ((task simon-task) key)
  "Records a response in the PSS task"
  (unless (null (current-trial task))
    (let* ((trial (current-trial task))
	   (response (cdr (assoc key *responses*))))
      (set-trial-actual-response trial response)
      (when (act-r-loaded?)
	(set-trial-response-time (current-trial task)
				 (mp-time))
	(if (= 1 (trial-accuracy (current-trial task)))
	    (trigger-reward 1)
	    (trigger-reward -1))
	(schedule-event-relative 0 #'next :params (list task))))))
            

(defmethod next ((task simon-task))
  "Moves to the next step in a Simon Task timeline"
  (cond ((equal (task-phase task) 'stimulus)
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
		(setf (task-phase task) 'stimulus)
		(setf (current-trial task) (nth (index task)
						(trials task)))
		(when (act-r-loaded?)
		  (set-trial-onset-time (current-trial task)
					(mp-time)))))))
  (when (act-r-loaded?) 
    (schedule-event-relative 0 'proc-display :params nil)))

;;; ------------------------------------------------------------------
;;; ACT-R DEVICE INTERFACE
;;; ------------------------------------------------------------------
;;; These functions turn the Simon-Task class into an ACT-R device
;;; ------------------------------------------------------------------

(defmethod device-handle-keypress ((task simon-task) key)
  "Converts the key into a symbol and passes it on to the task manager"
  (respond task (intern (string-capitalize (format nil "~a" key)))))

			   
(defmethod device-handle-click ((task simon-task))
  "Does nothing"
  (declare (ignore task))
  nil)

(defmethod device-move-cursor-to ((task simon-task) pos)
  "Does nothing"
  (declare (ignore task))
  nil)


(defmethod get-mouse-coordinates ((task simon-task))
  "Does nothing"
  (declare (ignore task))
  (vector 0 0))

(defmethod cursor-to-vis-loc ((task simon-task))
  "Does nothing"
  (declare (ignore task))
  nil)

(defmethod build-vis-locs-for ((task simon-task) vismod)
  (if (equalp (task-phase task) 'stimulus)
      (build-vis-locs-for (trial-stimulus (current-trial task))
			  vismod)
      (build-vis-locs-for (task-phase task)
			  vismod)))

(defmethod build-vis-locs-for ((trial list) vismod)
  (let ((results nil))
    (push  `(isa simon-stimulus-location 
		 kind simon-stimulus
		 value stimulus
		 color black
		 screen-x 0
		 screen-y 0
		 height 400 
		 width 400
		 ,@trial)
	   results)
    (define-chunks-fct results)))

(defmethod build-vis-locs-for ((phase symbol) vismod)
  (let ((results nil))
    (push  `(isa simon-stimulus-location 
		 kind screen
		 value ,phase
		 color black
		 screen-x 0
		 screen-y 0
		 height 400 
		 width 400)
	   results)
    (define-chunks-fct results)))


(defmethod vis-loc-to-obj ((task simon-task) vis-loc)
  "Transforms a visual-loc into a visual object"
  (let ((new-chunk nil)
	(phase (task-phase task))
	(stimulus (trial-stimulus (current-trial task))))
    (if (equal phase 'stimulus)
	(setf new-chunk (vis-loc-to-obj stimulus vis-loc))
	(setf new-chunk (vis-loc-to-obj phase vis-loc)))
    (fill-default-vis-obj-slots new-chunk vis-loc)
    new-chunk))


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

;;; ------------------------------------------------------------------
;;; DATA COLLECTION AND STATS
;;; ------------------------------------------------------------------

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
  "A list is a result IFF it's made of at least four numbers"
  (and (>= (length lst) 4)
       (every #'(lambda (x) (or (numberp x)
				(keywordp x)))
	      lst)))

(defun result-congruent-accuracy (res)
  (nth 0 res))


(defun result-congruent-rt (res)
  (nth 1 res))


(defun result-incongruent-accuracy (res)
  (nth 2 res))


(defun result-incongruent-rt (res)
  (nth 3 res))

	  
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
      (format t "   Not every results is a result")
      (setf *results* results))))


(defparameter *results* nil)
