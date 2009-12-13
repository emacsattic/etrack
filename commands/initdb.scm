;;; initdb.scm --- initialize database

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (initdb)                ; init=#f
  (let* ((prelim (read-etrack-config))
         (db (or (assq-ref prelim 'database)
                 (error "no database specified in config")))
         (sys-cmd (fs "createdb ~A" db)))
    (fso "~A\n" sys-cmd)
    (or (= 0 (system sys-cmd))
        (error "createdb failed for db:" db)))

  ;; ok to init now that the db exists
  (init!)

  (let ((blurb "creating tables ..."))
    (define (do-gingerly m . rest)
      (let ((res (apply m rest)))
        (or (eq? 'PGRES_COMMAND_OK (pg-result-status res))
            (error (pg-result-error-message res)))))
    (fso "~A\n" blurb)
    (let ((meta-M (pgtable-worker CONN (DK #:metaname) (DK #:metadefs))))
      (do-gingerly meta-M #:create)
      (do-gingerly meta-M #:insert-values
                   "*db-design-version*" (DK #:design-version)))
    (do-gingerly (pgtable-worker CONN (DK #:tname) (DK #:tdefs)) #:create)
    (do-gingerly M #:create)
    (fso "~A done\n" blurb)))

;;; initdb.scm ends here
