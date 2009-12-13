;;; reset.scm --- drop and create the db tables

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (reset)                 ; init=#t
  (let ((drop-result #f) (create-result #f))
    ;; Do it this way to ensure evaluation order.
    (set! drop-result (M #:drop))
    (set! create-result (M #:create))
    (fso "reset result: (drop ~A create ~A)\n"
         drop-result create-result)))

;;; reset.scm ends here
