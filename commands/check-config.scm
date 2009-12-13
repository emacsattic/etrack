;;; check-config.scm --- check FILE and summarize, or signal error

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (check-config file)     ; init=#f
  (catch 'bad-config
         (lambda ()
           (configure (read-config-file file)))
         (lambda args
           (format (current-error-port) "ERROR: ~A\n" (cadr args))
           (error (car args))))
  (fso "database: ~A\n" *db*)
  (fso "attributes:~{ ~A~}\n" *attributes*)
  (fso "attcodes:~{ ~A~}\n" *attcodes*)
  (fso "~A user-defined queries:\n" (*queries* #:count))
  (fso "~{- ~A\n~}" (map car (*queries*))))

;;; check-config.scm ends here
