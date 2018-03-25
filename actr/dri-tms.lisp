;;; ------------------------------------------------------------------
;;; ACT-R IMPLEMENTATION OF TMS
;;; ------------------------------------------------------------------

(defparameter *tms-train-duration* 0.5
  "Duration of the TMS high-frequency train (in secs)")

(defparameter *tms-method1* nil
  "Type 1 Family of TMS interface -- disables buffers")

(defparameter *tms-method1-target* 'imaginal
  "The buffer targeted by PMd TMS")

(defparameter *tms-method2* nil
  "Type 2 Family of TMS interface -- disables buffers")

(defparameter *tms-method2-target* 'retrieval
  "The buffer targeted by PMd TMS")

(defun start-tms (&optional (duration *tms-train-duration*))
  "Initiates a simulated TMS pulse"
  (when (act-r-loaded?)
    (cond (*tms-method1*
	   (set-buffer-failure *tms-method1-target*
			       :ignore-if-full t
			       :requested nil)
	   (schedule-event-relative duration 'stop-tms))
	  
	  (t
	   nil))))

(defun stop-tms ()
  "Initiates a simulated TMS pulse"
  (when (act-r-loaded?)
    (cond (*tms-method1*
	   (clear-buffer *tms-method1-target*))
	  (t
	   nil))))
