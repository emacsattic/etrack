;;; update-row.scm --- update ROW with new DATA

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (update-row pair)       ; init=#t "(row . data)"
  (let ((row (car pair))
        (data (cdr pair)))
    (let ((x (cadr data)))              ; amount
      (and (string? x) (set! x (string->number x)))
      (set-car! (cdr data) (inexact->exact (* 100 x))))
    (fso "~A\n" (UPDCOL (DK #:e-user-fields) data `(= ,row i)))))

;;; update-row.scm ends here
