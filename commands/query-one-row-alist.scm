;;; query-one-row-alist.scm --- display ROW as an Emacs-friendly alist

;; Copyright (C) 2004-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack)
  #:use-module ((database postgres) #:select (pg-result? pg-ntuples))
  #:use-module ((database postgres-resx) #:select (result->object-alists)))

(define-command (query-one-row-alist row) ; init=#t
  (let* ((defs-no-i (cdr (DK #:edefs)))
         (res (M #:select (map col-defs:column-name defs-no-i)
                 #:where `(= i ,row))))
    (fso "~S~%"
         (if (and (pg-result? res)
                  (< 0 (pg-ntuples res)))
             (let* ((alist (car (result->object-alists
                                 res
                                 (col-defs:objectifiers defs-no-i))))
                    (date (car alist)))
               (set-cdr! date (strftime "%Y-%m-%d" (localtime (cdr date))))
               alist)
             'nil))))

;;; query-one-row-alist.scm ends here
