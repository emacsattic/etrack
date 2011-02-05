;;; check-config.scm --- check FILE and summarize, or signal error

;; Copyright (C) 2004-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (check-config file)     ; init=#f
  (catch 'bad-config
         (lambda ()
           (configure (read-config-file file)))
         (lambda args
           (format (current-error-port) "ERROR: ~A~%" (cadr args))
           (error (car args))))
  (fso "sockdir: ~A~%" *sockdir*)
  (fso "database: ~A~%" *db*)
  (fso "attributes:~{ ~A~}~%" *attributes*)
  (fso "attcodes:~{ ~A~}~%" *attcodes*)
  (fso "~A user-defined queries:~%" (*queries* #:count))
  (fso "~{- ~A~%~}" (map car (*queries*))))

;;; check-config.scm ends here
