;; show when garbage collection is running
;; (setq garbage-collection-messages t)

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold (* 1024 1024 1024))

;; When idle for 10 sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 10 t
                       (lambda ()
                         (k-time (garbage-collect)))))
