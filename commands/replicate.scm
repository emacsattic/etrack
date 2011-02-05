;;; replicate.scm --- copy database to DIR

;; Copyright (C) 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack)
  #:use-module ((database postgres) #:select (pg-get-copy-data
                                              pg-put-copy-data
                                              pg-put-copy-end))
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module ((ttn-do zzz filesystem) #:select (dir-exists?)))

(define (cok? res)
  (eq? 'PGRES_COMMAND_OK (pg-result-status res)))

(define (badness res)
  (fso "~A~%" (pg-result-error-message res))
  #f)

(define (c-chk res)
  (or (cok? res)
      (badness res)))

(define (create-database! dir)
  (let* ((conn (pg-connectdb (fs "host=~A dbname=template1" dir)))
         (rv (c-chk (pg-exec conn (fs "CREATE DATABASE ~A" *db*)))))
    (pg-finish conn)
    rv))

(define (copy-tables! conn cexec)
  (let ((stash (tmpfile)))

    (define (one! name defs)

      (define (start-copy c)
        (let* ((out? (eq? CONN c))
               (res (pg-exec c (fs "COPY ~A ~A" name
                                   (if out?
                                       "TO STDOUT"
                                       "FROM STDIN")))))
          (or (eq? (symbol-append 'PGRES_COPY_ (if out? 'OUT 'IN))
                   (pg-result-status res))
              (badness res))))

      (define (stall where)
        (fso "~A stall...~%" where)
        (sleep 1))

      (define (c-badness c)
        (fso "badness: ~A" (pg-error-message c))
        #f)

      (define (rewind!)
        (seek stash 0 SEEK_SET))

      (define (discard-after!)
        (truncate-file stash)
        #t)

      (fso "copying: ~A~%" name)
      (and (c-chk ((pgtable-worker conn name defs) #:create))
           ;; get
           (start-copy CONN)
           (let loop ()
             (case (pg-get-copy-data CONN stash)
               ((0) (stall 'get) (loop))
               ((-1) (discard-after!))
               ((-2) (c-badness CONN))
               (else (loop))))
           ;; spew / put
           (rewind!)
           (start-copy conn)
           (let loop ()
             (let ((line (read-line stash 'concat)))
               (or (eof-object? line)
                   (case (pg-put-copy-data conn line)
                     ((1) (fso "|\t~A" line) (loop))
                     ((0) (stall 'put) (loop))
                     (else (c-badness conn))))))
           (let loop ()
             (case (pg-put-copy-end conn)
               ((1) 'ok-done)
               ((0) (stall 'end) (loop))
               (else (c-badness conn))))
           ;; reset stash
           (rewind!)
           (discard-after!)
           #t))

    (define (copy-sequences!)

      (define (tok?-1 res)
        (and (eq? 'PGRES_TUPLES_OK (pg-result-status res))
             (pg-getvalue res 0 0)))

      (let* ((seq "expenses_i_seq")
             (res (Cfexec "SELECT nextval ('~S');" seq)))
        (cond ((tok?-1 res)
               => (lambda (cur)
                    (fso "copying: ~A (nextval = ~A)~%" seq cur)
                    (let ((now (cexec "SELECT setval ('~S', ~A, false);"
                                      seq cur)))
                      (or (tok?-1 now)
                          (badness now)))))

              (else
               (badness res)))))

    ;; do it!
    (and (and-map (lambda (pair)
                    (one! (DK (car pair)) (DK (cdr pair))))
                  (DK #:all-tables))
         (copy-sequences!))))

(define-command (replicate dir)         ; init=#t
  (cond ((dir-exists? dir)
         (fso "ERROR: directory already exists: ~A~%" dir)
         (all-done #f)))
  (let ((cm (cluster-mangler 'new dir)))
    (and (cm #:daemon-up)
         (or (create-database! dir)
             #t)
         (let ((conn (pg-connectdb (fs "host=~A dbname=~A" dir *db*))))
           (define (cexec s . args)
             (pg-exec conn (apply fs s args)))
           (and (copy-tables! conn cexec)
                (begin (pg-finish conn)
                       #t)))
         (cm #:daemon-down))))

;;; replicate.scm ends here
