;;; query-two-rows.scm --- query two rows

;; Copyright (C) 2004-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (query-two-rows pair)   ; init=#t "(ROW-1 . ROW-2)"
  (let ((p (cond ((pair? pair) pair)
                 ((string? pair) (with-input-from-string pair read))
                 (else (error "bad pair:" pair)))))
    (>>table (M #:select (cons
                          ;; hardcode this since that's the only caller ATM
                          `(#f "prune" (if (= i ,(car p)) " (1) " " (2) "))
                          sel:*:raw)
                #:where `(or (= i ,(car p))
                             (= i ,(cdr p)))))
    (fso "~%EOQ~%~%")))

;;; query-two-rows.scm ends here
