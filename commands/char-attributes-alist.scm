;;; char-attributes-alist.scm --- display attributes keyed by char number

;; Copyright (C) 2004-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (char-attributes-alist) ; init=#t
  (fso "~S~%" (map (lambda (att)
                     (let ((s (symbol->string att)))
                       (cons (char->integer (string-ref s 0)) s)))
                   *attributes*)))

;;; char-attributes-alist.scm ends here
