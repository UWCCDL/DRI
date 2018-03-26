(defparameter *buffers* '(visual visual-location retrieval imaginal goal manual)
  "List of ACT-R buffers used by the DRI model")

(defun simulate-tms-method1 (&optional (verbose t))
  "Simulates the effects of TMS on every buffer used by the model"
  (dolist (buffer *buffers*)
    (when verbose
      (format t "... Simulating TMS on buffer ~A~%" buffer))
    (dri-reload)
    (sgp :v nil) ;; silences the trace
    (setf *tms-method1* t)
    (setf *tms-method2* nil)
    (setf *tms-method1-target* buffer)
    (run 1000 :real-time nil)
    
    (let ((filename
	   (string-downcase
	    (format nil
		    "~A-tms1-~A.txt"
		    (current-model)
		    buffer))))
      (save-log filename))))


(defun simulate-tms-method2 (&optional (verbose t))
  "Simulates the effects of TMS on every buffer used by the model"
  (dri-reload)
  (let ((tmsv *tms-verbose*))
    (setf *tms-verbose* nil)
    (dolist (production (no-output (pp)))
      (when verbose
	(format t "... Simulating TMS on production ~A~%" production))
      
      (dri-reload)
      (sgp :v nil) ;; silences the trace
      (setf *tms-start-time* nil)
      (setf *tms-method1* nil)
      (setf *tms-method2* t)
      (setf *tms-method2-target* production)
      (run 1000 :real-time nil)
    
      (let ((filename
	     (string-downcase
	      (format nil
		      "~A-tms2-~A.txt"
		      (current-model)
		      production))))
	(save-log filename)))
    (setf *tms-verbose* tmsv)))
