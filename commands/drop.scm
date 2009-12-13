;;; drop.scm --- drop the expenses table

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (drop)                  ; init=#t
  (fso "drop result: ~A\n" (M #:drop)))

;;; drop.scm ends here
