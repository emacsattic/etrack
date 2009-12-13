;;; templates-list.scm --- display single list of all templates

;; Copyright (C) 2006-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack)
  #:use-module ((database postgres-types) #:select (dbcoltype-lookup
                                                    dbcoltype:objectifier))
  #:use-module ((database postgres-resx) #:select (result->object-rows)))

(define-command (templates-list)        ; init=#t
  (define (->null-list s)
    (if (string-null? s) '() s))
  (let ((boss (pgtable-worker CONN (DK #:tname) (DK #:tdefs)))
        (sel `((name
                . ,identity)
               ((#f "date" (to_char date "YYYY-MM-DD"))
                . ,->null-list)
               ((#f "amount" (ltrim ,(w/c 5 'amount) " "))
                . ,->null-list)
               (attcode
                . ,->null-list)
               (details
                . ,(dbcoltype:objectifier (dbcoltype-lookup 'text[]))))))
    ;; Some values are allowed to be NULL, so we can't use `boss'
    ;; methods #:tuples-result->alists or #:tuples-result->object-alist.
    ;; However, we can use `result->object-rows' directly.
    (fso "~S\n" (result->object-rows (boss #:select (map car sel)
                                           #:order-by '((< name)))
                                     (map cdr sel)))))

;;; possible-duplicates.scm ends here
