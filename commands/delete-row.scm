;;; delete-row.scm --- delete ROW from the table

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (delete-row row)        ; init=#t
  (fso "~A\n" (M #:delete-rows `(= i ,row))))

;;; delete-row.scm ends here
