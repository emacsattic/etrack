;;; insert.scm --- insert new data LIST into the table

;; Copyright (C) 2004-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (insert ls)             ; init=#t
  (let ((res (M #:insert-alist
                (map (lambda (field proc raw)
                       (cons field (proc raw)))
                     (DK #:e-user-fields)
                     (list identity
                           (lambda (x) (inexact->exact (* 100 x)))
                           identity
                           (lambda (x) (if (eq? 'nil x) '() x)))
                     ls))))
    (set! last-insert-i (one-value `(currval ,(DK #:iseq))))
    (fso "~A ~@[~A~]~%" res last-insert-i)))

;;; insert.scm ends here
