;;; change-attcode.scm --- OLDNEW is one arg of two adjacent chars

;; Copyright (C) 2004-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack)
  #:use-module ((database postgres-table) #:select (sql-pre)))

(define-command (change-attcode oldnew) ; init=#t
  (and (symbol? oldnew) (set! oldnew (symbol->string oldnew)))
  (let ((old (substring oldnew 0 1))
        (new (substring oldnew 1 2)))
    (fso "~A~%"
         (UPDCOL '(attcode)
                 (list
                  ;; eventually, we want to use:
                  ;;+ `(overlay attcode
                  ;;+           #:PLACING ,new
                  ;;+           #:FROM (position ,old #:IN attcode))
                  ;; but for now we stick with:
                  (sql-pre
                   (fs "overlay (attcode PLACING '~A' FROM ~A)"
                       new (fs "position ('~A' IN attcode)" old))))
                 (simple-query-code->pexp old)))))

;;; change-attcode.scm ends here
