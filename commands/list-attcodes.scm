;;; list-attcodes.scm --- list attcodes and associated attributes

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (list-attcodes)         ; init=#t
  (fso "~:{~A\t~A\n~}" (map list *attcodes* *attributes*)))

;;; list-attcodes.scm ends here
