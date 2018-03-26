;;; ------------------------------------------------------------------
;;; ACT-R IMPLEMENTATION OF TMS
;;; ------------------------------------------------------------------

;;; Redefines the original query-buffer function in ACT-R to make it
;;; compatible with a TMS function
;;;

;;(defparameter *original-query-buffer* #'query-buffer)


;;; Redefines query-buffer to handle TMS
;;;
;(defun query-buffer (buffer-name queries-list-or-spec)
;  (when (and *tms-method1*
;	     *tms-start-time*
;	     (< (- (mp-time) *tms-start-time) *tms-train-duration*))
;    
;         <return override value>
;       (funcall original buffer-name queries-list-or-spec))))

(defparameter *tms-verbose* t
  "A parameter to print output during the trace") 

(defparameter *tms-start-time* nil
  "The beginning of a TMS pulse train, if any")

(defparameter *tms-train-duration* 0.5
  "Duration of the TMS high-frequency train (in secs)")

(defparameter *tms-chunk* nil
  "The chunk in a buffer targeted by TMS*")

(defun tms-selection-hook (set)
  "When a production targeted by TMS is affected, increase its action time :AT"  
  (when (and *tms-method2*
	     (member *tms-method2-target* set))
    (when *tms-start-time*
      (let* ((delta (- (mp-time) *tms-start-time*))
	     (remaining-action-time (- 0.5 delta)))
	(spp-fct (list *tms-method2-target*
		       :at
		       (+ 0.05
			  remaining-action-time))))))
  nil)

(defparameter *tms-method1* t
  "Type 1 Family of TMS interface -- disables buffers")

(defparameter *tms-method1-target* 'imaginal
  "The buffer targeted by PMd TMS")

(defparameter *tms-method2* nil
  "Type 2 Family of TMS interface -- disables buffers")

(defparameter *tms-method2-target* 'look-at-target
  "The buffer targeted by PMd TMS")

;;(defparameter

(defun start-tms (&optional (duration *tms-train-duration*))
  "Initiates a simulated TMS pulse"
  (when (act-r-loaded?)
    (when *tms-verbose*
      (format t "TMS train started at ~A~%" (mp-time)))
    (setf *tms-start-time* (mp-time))
    (cond (*tms-method1*
	   (setf *tms-chunk*
		 (first (no-output (buffer-chunk-fct (list *tms-method1-target*)))))
	   (set-buffer-failure *tms-method1-target*
			       :ignore-if-full t
			       :requested nil)
	   (schedule-event-relative duration 'stop-tms))
	  (*tms-method2*
	   ;; (let ((production-list (no-output (pp))))
	   ;;   (when (member *tms-method2-target* production-list)
	   ;;     (spp-fct (list *tms-method2-target* :at *tms-train-duration*)))))
	   (schedule-event-relative duration 'stop-tms))
	   
	  (t
	   nil))))

(defun stop-tms ()
  "Initiates a simulated TMS pulse"
  (when (act-r-loaded?)
    (when *tms-verbose*
      (format t "TMS Stopped at ~A~%" (mp-time)))
    (setf *tms-start-time* nil)
    (when (act-r-loaded?)
      (cond (*tms-method1*
	     (clear-buffer *tms-method1-target*)
	     (when *tms-chunk*
	       (set-buffer-chunk *tms-method1-target* *tms-chunk*)))

	    (*tms-method2*
	     ;; Restore default action-time for target production
	     (let ((production-list (no-output (pp))))
	       (when (member *tms-method2-target* production-list)
		 (spp-fct (list *tms-method2-target* :at 0.05)))))
	    (t
	     nil)))))
