;;; upgrade-db.scm --- fix old misunderstandings upward-compatibly

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack)
  #:use-module ((database postgres) #:select (pg-set-notice-out!
                                              pg-result-status
                                              pg-result-error-message
                                              pg-error-message))
  #:use-module ((database postgres-qcons) #:select (sql-quote
                                                    make-comma-separated-tree
                                                    sql-command<-trees
                                                    parse+make-SELECT-tree))
  #:use-module ((database postgres-table) #:select (pgtable-worker))
  #:use-module ((database postgres-meta) #:select (defs-from-psql)))

(define (sql-tree first . rest)
  (cond ((pair? first) (let ((from-specified? (eq? #:from (car rest))))
                         (apply parse+make-SELECT-tree #t first
                                #:from (if from-specified?
                                           (cadr rest)
                                           (list (string->symbol (car rest))))
                                ((if from-specified? cddr cdr) rest))))
        ((keyword? first) (case first
                            ((#:alt) (list #:ALTER #:TABLE rest))
                            ((#:ins) (list #:INSERT #:INTO rest))
                            ((#:crt) (list #:CREATE #:TABLE rest))
                            ((#:drt) (list #:DROP #:TABLE rest))
                            ((#:cols) (make-comma-separated-tree
                                       identity rest #t))
                            ((#:vals) (make-comma-separated-tree
                                       sql-quote rest #t))
                            (else (cons first rest))))
        (else (error "badness:" (cons first rest)))))

(define METAINFO #f)

(define EDO
  (let ((notices '()))
    (define (n-out! s)
      (set! notices (cons s notices)))
    ;; rv
    (lambda (cmd . args)
      (cond ((eq?  0 cmd) (pg-set-notice-out! CONN n-out!))
            ((eq? #f cmd) (set! notices '()))
            ((eq? #t cmd) (for-each display (reverse notices)))
            (else         (Cfexec "~A" (sql-command<-trees
                                        (apply sql-tree cmd args))))))))

(define (TOK? res)
  (cond ((eq? 'PGRES_TUPLES_OK (pg-result-status res)))
        (else
         (display (pg-result-error-message res))
         (newline)
         #f)))

(define (ETOK? . args)
  (TOK? (apply EDO args)))

(define (COK? res)
  (cond ((eq? 'PGRES_COMMAND_OK (pg-result-status res)))
        (else
         (display (pg-error-message CONN))
         (newline)
         #f)))

(define (ECOK? . args)
  (COK? (apply EDO args)))

(define (EFATAL? . args)
  (eq? 'PGRES_FATAL_ERROR (pg-result-status (apply EDO args))))

(define *upgrades* '())

(define-macro (define-upgrade name check-form . upgrade-forms)
  `(set! *upgrades*
         (assq-set! *upgrades*
                    ,name
                    (list (lambda () ,check-form)
                          (lambda () (and ,@upgrade-forms))))))

(define-upgrade #:has-attcode-not-catcode
    (and (ETOK? '(attcode) (DK #:ename) #:limit 1)
         (EFATAL? '(catcode) (DK #:ename) #:limit 1))
  (ECOK? #:alt (DK #:ename) #:RENAME #:COLUMN 'catcode #:TO 'attcode))

(define-upgrade #:has-etrackmetainfo-table
    (TOK? (METAINFO #:select #t))
  (COK? (METAINFO #:create))
  (COK? (METAINFO #:insert-values "*db-design-version*" (DK #:design-version))))

(define-upgrade #:has-templates-table
    (TOK? ((pgtable-worker CONN (DK #:tname) (DK #:tdefs)) #:select #t))
  (COK? ((pgtable-worker CONN (DK #:tname) (DK #:tdefs)) #:create)))

(define-upgrade #:uses-integer-for-amount
    (let ((def (assoc 'amount (defs-from-psql #t *db* (DK #:ename)))))
      (eq? 'integer (col-defs:type-name def)))
  (let ((old (fs "old_~A" (DK #:ename))))
    (return-it #t (EDO #:drt old))
    (ECOK? #:alt (DK #:ename) #:RENAME #:TO old)
    (ECOK? #:alt old #:DROP #:COLUMN 'i)
    (or (EFATAL? '(1) (DK #:iseq))
        (ECOK? #:DROP #:SEQUENCE (DK #:iseq)))
    (COK? (M #:create))
    (ECOK? #:ins (DK #:ename)
           (apply sql-tree #:cols (DK #:e-user-fields))
           (sql-tree (map (lambda (field)
                            (if (eq? 'amount field)
                                '(:: integer (* 100 amount))
                                field))
                          (DK #:e-user-fields))
                     old))
    (ECOK? #:drt old)))

(define-upgrade #:columns-declared-NOT-NULL
    (let* ((cols (lambda (relation)
                   (EDO '(("name" . a.attname))
                        #:from
                        '((c . pg_class) (a . pg_attribute) (t . pg_type))
                        #:where
                        `(and (= c.relname ,relation)
                              (> a.attnum 0)
                              (= a.attrelid c.oid)
                              (= a.atttypid t.oid)
                              a.attnotnull)
                        #:order-by
                        '((< a.attnum)))))
           (count (lambda (expected relation)
                    (let ((res (cols relation)))
                      (and (TOK? res) (= expected (pg-ntuples res)))))))
      (and (count 4 (DK #:ename))
           (count 2 (METAINFO #:k #:table-name))))
  (let ((alter! (lambda (relation)
                  (lambda (column)
                    (ECOK? #:alt relation #:ALTER #:COLUMN column
                           #:SET #:NOT #:NULL)))))
    (and (and-map (alter! (DK #:ename)) '(i date amount attcode))
         (and-map (alter! (METAINFO #:k #:table-name)) '(key value)))))

(define (check-and-upgrade name)
  (let* ((entry (assq-ref *upgrades* name))
         (check (car entry))
         (upgrade (cadr entry)))
    (cond ((check)
           (fso "ok: ~A\n" name))
          ((upgrade)
           (if (check)
               (fso "ok-after-upgrade: ~A\n" name)
               (error "upgrade ok but check failed:" name)))
          (else
           (error "upgrade failed:" name)))))

(define-command (upgrade-db)            ; init=#t
  (EDO 0)
  (set! METAINFO (pgtable-worker CONN (DK #:metaname) (DK #:metadefs)))
  (for-each check-and-upgrade '(#:has-attcode-not-catcode
                                #:has-etrackmetainfo-table
                                #:has-templates-table
                                #:uses-integer-for-amount
                                #:columns-declared-NOT-NULL))
  (COK? (METAINFO #:update-col '(value) (list (DK #:design-version))
                  '(= key "*db-design-version*"))))

;;; ttn-sez: (put 'define-upgrade 'scheme-indent-function 2)
;;; upgrade-db.scm ends here
