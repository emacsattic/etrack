;;; list-queries.scm --- list queries in two columns

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (list-queries)          ; init=#t
  (let ((mid (inexact->exact (/ (*queries* #:count) 2)))
        (out (lambda (n s) (fso "~2,'0D  -  ~26A" n s))))
    (let loop ((i1 1)        (c1 (*queries*))
               (i2 (1+ mid)) (c2 (list-cdr-ref (*queries*) mid)))
      (or (null? c1)
          (> i1 mid)
          (let ((last? (null? c2)))
            (out i1 (caar c1))
            (or last? (out i2 (caar c2)))
            (newline)
            (loop (1+ i1) (cdr c1) (1+ i2) (or last? (cdr c2)))))))
  (fso "~A\n" (*queries* #:count))
  (fso "EOL\n"))

;;; list-queries.scm ends here
