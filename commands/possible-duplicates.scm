;;; possible-duplicates.scm --- display single list of possible duplicates

;; Copyright (C) 2004-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (possible-duplicates)   ; init=#t
  (let loop ((all (let* ((result (M #:select '(i date amount)
                                    #:order-by o:date))
                         (acc (list #f))
                         (tp acc))
                    (for-each-tuple (lambda ls
                                      (set-cdr! tp (list ls))
                                      (set! tp (cdr tp)))
                                    result)
                    (cdr acc)))
             (dups (list)))
    (if (null? all)
        (begin
          (fso "~A~%" (if (null? dups)
                          "[]"          ; for --etrack-cmd:prune-duplicates
                          dups))
          dups)                         ; rv
        (loop (cdr all)
              (let ((head (car all)))
                (cond ((find-if (lambda (x)
                                  (equal? (cdr x) (cdr head)))
                                (cdr all))
                       => (lambda (x)
                            (list* (string->number (car head))
                                   (string->number (car x))
                                   dups)))
                      (else dups)))))))

;;; possible-duplicates.scm ends here
