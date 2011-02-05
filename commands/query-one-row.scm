;;; query-one-row.scm --- display ROW

;; Copyright (C) 2004-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (query-one-row row)     ; init=#t
  (>>table (M #:select sel:* #:where `(= i ,row)))
  (fso "EOQ~%~%"))

;;; query-one-row.scm ends here
