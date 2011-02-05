;;; etrack.scm --- expense tracking backend

;; Copyright (C) 2001-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Commentary:

;; Env var ETRACK_CONFIG must be set to the configuration filename.

;;; Code:

(define-module (etrack)
  #:use-module ((ttn-do zzz ciabattone) #:select (cluster-mangler))
  #:use-module ((ice-9 accumulate) #:select (accumulator/one-only
                                             accumulator/counting))
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((ice-9 common-list) #:select (set-difference
                                              find-if
                                              pick
                                              pick-mappings))
  #:use-module ((srfi srfi-13) #:select (string-tokenize))
  #:use-module ((srfi srfi-14) #:select (char-set
                                         char-set-complement))
  #:use-module ((database tmpfile) #:select (tmpfile))
  #:use-module ((database postgres-qcons) #:select (sql-quote
                                                    sql-pre))
  #:use-module ((database postgres-resx) #:select (for-each-tuple))
  #:use-module ((database postgres-resdisp) #:select (display-result))
  #:use-module ((database postgres-table) #:select (pgtable-worker
                                                    compile-outspec))
  #:use-module ((database postgres) #:select (pg-connectdb
                                              pg-finish
                                              pg-exec
                                              pg-result-status
                                              pg-result-error-message
                                              pg-getvalue
                                              pg-ntuples))
  #:use-module ((database postgres-col-defs) #:prefix col-defs:))

(define *ETRACK-VERSION* "|VERSION|")
(define *ETRACK-DATA* "/home/ttn/build/etrack")

(define *client-encoding* #f)           ; string

(define *sockdir* #f)                   ; string

(define *name* #f)                      ; string
(define *db* #f)                        ; string
(define *attributes* #f)                ; list of symbols
(define *attcodes* #f)                  ; list of strings
(define *attcodes-char* #f)             ; list of chars

(define *drill-down-attributes* '()) ; consulted by `init!'

(define *by-month-query* #f)

(define *queries* (accumulator/counting #:count))

;; database design constants

(define DK
  (let ((ht (make-hash-table #:test eq?)))
    (define (? key) (hashq-ref  ht key))
    (define (! k v) (hashq-set! ht k v))
    ;; metainfo -- see also info node "(etrack)DB"
    (! #:design-version "4")
    (! #:metaname "etrackmetainfo")
    (! #:metadefs '((key   text "PRIMARY KEY")
                    (value text "NOT NULL")))
    ;; the original table
    (! #:ename "expenses")
    (! #:edefs '((i       serial)
                 (date    timestamp "WITH TIME ZONE" "NOT NULL")
                 (amount  integer "NOT NULL")
                 (attcode text "NOT NULL")
                 (details text[])))
    (! #:e-user-fields (map col-defs:column-name
                            ;; i not a "user field"
                            (cdr (? #:edefs))))
    (! #:iseq "expenses_i_seq")
    ;; common outspec elements
    (! #:o/date '(#f "date" (to_char date "YYYY-MM-DD")))
    ;; templates
    (! #:tname "templates")
    (! #:tdefs '((name    text "PRIMARY KEY")
                 ;; rest similar to table `expenses' but allowing NULL
                 (date    timestamp "WITH TIME ZONE")
                 (amount  integer)
                 (attcode text)
                 (details text[])))
    ;; compendium
    (! #:all-tables (map (lambda (nick)
                           (define (kw-append suffix)
                             (symbol->keyword (symbol-append nick suffix)))
                           (cons (kw-append 'name)
                                 (kw-append 'defs)))
                         '(meta e t)))
    ;; rv
    ?))

;; procs set by `init!'

(define M #f)
(define last-insert-i #f)

;; connection set by `init!'

(define CONN #f)

;; common {di,ab}stractions

(define (fs s . args)
  (apply format #f s args))

(define (fso s . args)
  (apply format #t s args))

(define *repl?* #f)                     ; command `repl' sets this
(define-macro (fso/norepl s . args)
  `(or *repl?* (fso ,s ,@args)))

(define (Cfexec . args)
  (pg-exec CONN (apply fs args)))

;; queries

(define (non-drill-down-attributes)
  (set-difference *attributes* *drill-down-attributes*))

(define (attribute->attcode attribute)
  (substring (symbol->string attribute) 0 1))

(define (mk-outspec x)
  (compile-outspec x (DK #:edefs)))

(define (w/c lead x)
  `(to_char (/ (:: float ,x) 100)
            ,(string-append (make-string lead #\9) "D99")))

(define sel:*:raw `(i
                    ,(DK #:o/date)
                    (#f "   amount" ,(w/c 5 'amount))
                    (#f      "attr" attcode)
                    details))

(define sel:* (mk-outspec sel:*:raw))

(define outspec:sum `((#f "       sum" ,(w/c 6 '(sum amount)))))
(define sel:sum (mk-outspec outspec:sum))

(define o:date '((< date)))

(define (mspec->having-clause date-trunc mspec)
  (or (string-null? mspec)
      (let* ((comma (string-index mspec #\,))
             (from  (if comma (substring mspec 0 comma) mspec))
             (to    (if comma
                        (substring mspec (1+ comma) (string-length mspec))
                        from))
             (year  (strftime "%Y" (localtime (current-time))))
             (day1  (lambda (s)
                      `(:: date ,(fs "~A-01" s)))))
        (or (string-index from #\-) (set! from (fs "~A-~A" year from)))
        (or (string-index to   #\-) (set! to   (fs "~A-~A" year to)))
        `(and (>= ,date-trunc ,(day1 from))
              (<= ,date-trunc ,(day1 to))))))

(define (define-query descr select-thunk)
  (*queries* (list descr select-thunk)))

(define (annotate:histogram max-width start lines)
  (let ((line-suffix (make-object-property))
        (line-value (make-object-property))
        (hw 0) (htot 0)                 ; high water / high total
        (lw 0) (ltot 0))                ; low  water / low  total
    (for-each (lambda (line)
                (let ((tok (with-input-from-string (substring line start)
                             (lambda () (read)))))
                  (cond ((number? tok)
                         (if (> 0 tok)
                             (begin
                               (set! lw (min lw tok))
                               (set! ltot (+ ltot tok)))
                             (begin
                               (set! hw (max hw tok))
                               (set! htot (+ htot tok))))
                         (set! (line-value line) tok))
                        ((eq? 'sum tok)
                         (set! (line-suffix line) "   pct")))))
              lines)
    (let* ((range (max 1 (- hw lw)))
           (prefix (- (* (/ lw range) max-width))))
      (map (lambda (line)
             (cond ((line-suffix line)
                    => (lambda (s)
                         (fs "~A~A" line s)))
                   ((line-value line)
                    => (lambda (n)
                         (let* ((fraction (/ n (if (> 0 n) (- ltot) htot)))
                                (w (* (/ n range) max-width))
                                (w+ (if (> 0 n) 0
                                        (inexact->exact (ceiling w))))
                                (w- (if (> n 0) 0
                                        (inexact->exact (floor (- w))))))
                           (fs "~A| ~V,,,A~V,,,'#A ~3D ~V,,,'#@A"
                               line
                               (- prefix w-) ""
                               w- ""
                               (inexact->exact (* fraction 100))
                               w+ ""))))
                   (else
                    (fs "~A~V,,,'-@A" line (+ 9 max-width) ""))))
           lines))))

(define (split-on-proc char)
  (let ((comp (char-set-complement (char-set char))))
    (lambda (string)
      (string-tokenize string comp))))

(define split-on-nl (split-on-proc #\nl))

(define (>>table res . options)
  (define (>>)
    (if (= 0 (pg-ntuples res))
        (display "(no data)\n")
        (display-result res 'fat-h-only)))
  (cond ((memq 'histogram options)
         => (lambda (ls)
              (fso "~{~A~%~}"
                   (annotate:histogram
                    (cadr ls) (caddr ls)
                    (split-on-nl (with-output-to-string >>))))))
        (else (>>))))

(define (define-double-query desc where-clause)
  (define-query desc
    (lambda ()
      (>>table (M #:select sel:* #:where where-clause #:order-by o:date))
      (let* ((date-trunc '(date_trunc "month" date))
             (outspec (mk-outspec
                       (cons `(#f "month" (to_char ,date-trunc "YYYY-MM"))
                             outspec:sum))))
        (set! *by-month-query*
              (lambda (mspec)
                (M #:select outspec
                   #:where where-clause
                   #:group-by (list date-trunc)
                   #:having (list (mspec->having-clause date-trunc mspec))))))
      (M #:select sel:sum #:where where-clause))))

(define (bad-config-error . args)
  (throw 'bad-config args))

(define (simple-query-code->pexp qc)
  (let ((acc (accumulator/one-only)))
    (let loop ((qc (string->list qc)))
      (define (sube!/next n op c)
        (acc `(,op attcode ,(make-string 1 c)))
        (loop n))
      (or (null? qc)
          (if (char=? #\! (car qc))
              (let ((ac (cadr qc)))
                (or (char=? #\! ac)
                    (memq ac *attcodes-char*)
                    (bad-config-error "bad simple-query code:" ac))
                (sube!/next (cddr qc) '!~ ac))
              (sube!/next (cdr qc) '~ (car qc)))))
    `(and ,@(acc))))

(define (define-simple-query description query-code)
  (let ((qc (cond ((string? query-code) query-code)
                  ((symbol? query-code) (symbol->string query-code))
                  (else (bad-config-error "bad simple-query code:"
                                          query-code)))))
    ;;(fso "defining simple query: ~A~%" description)
    (define-double-query description (simple-query-code->pexp qc))))

(define (define-drill-down-queries attribute tags)
  (let ((bad bad-config-error))
    (or (memq attribute *attributes*) (bad "bad attribute:" attribute))
    (or (list? tags)                  (bad "not a list:" tags))
    (or (and-map string? tags)        (bad "not a list of strings:" tags)))
  (set! *drill-down-attributes* (cons attribute *drill-down-attributes*))
  (let ((ac (attribute->attcode attribute)))
    (define-simple-query (symbol->string attribute) ac)
    (for-each (lambda (tag)
                (let ((description (fs "~A / ~A" attribute tag)))
                  ;;(fso "defining drill-down query: ~A~%" description)
                  (define-double-query description
                    `(and ,(simple-query-code->pexp ac)
                          (or (~ details[1] ,tag)
                              (~ details[2] ,tag))))))
              tags)))

;; support for custom queries (todo: move up and use internally)

(define (one-row-table-query labels vals)
  (M #:select (map (lambda (label val)
                     (list #f (fs "~A" label) val))
                   labels vals)
     #:limit 1))

(define (one-value expr . query-args)
  (pg-getvalue (apply M #:select `((#f #f ,expr)) query-args) 0 0))

;; mogrification

(define (UPDCOL cols data where-condition)
  (M #:update-col cols
     (map (lambda (col x)
            (if (equal? '(details nil) (list col x))
                '()
                x))
          cols data)
     where-condition))

;; configuration

(define (configure conf)
  (let* ((cq-acc #f)                    ; custom query accumulation
         (bad bad-config-error)
         (check (lambda (key handle)
                  (cond ((assq-ref conf key) => handle)
                        (else (bad "missing or invalid key:" key))))))
    (or (and-map (lambda (x) (and (pair? x) (symbol? (car x)))) conf)
        (bad "configuration not an alist:" conf))
    ;; ‘sockdir’ is optional for now; in the future it will be required.
    (and=> (assq-ref conf 'sockdir) (lambda (v) (set! *sockdir* v)))
    (or (string>? "2011-05-11" (strftime "%Y-%m-%d" (gmtime (current-time))))
        (error "missing key: sockdir"))
    (check 'name       (lambda (v) (set! *name* v)))
    (check 'database   (lambda (v) (set! *db* (symbol->string v))))
    (check 'attributes (lambda (v)
                         (or (list? v) (bad "attributes not a list:" v))
                         (set! *attributes* v)))
    (let ((acc (accumulator/one-only)))
      (for-each (lambda (a)
                  (or (symbol? a) (bad "attribute not a symbol:" a))
                  (let ((ac (attribute->attcode a)))
                    (and (member ac (acc)) (bad "attribute code clash:" a))
                    (acc ac)))
                *attributes*)
      (set! *attcodes* (acc)))
    (set! *attcodes-char* (map (lambda (s) (string-ref s 0)) *attcodes*))
    (set! conf (reverse!
                (pick (lambda (x)
                        (not (memq (car x)
                                   '(quote sockdir database table
                                           attributes name))))
                      conf)))
    (let ((spurious (pick-mappings (lambda (x)
                                     (and (not (eq? 'query (car x)))
                                          (car x)))
                                   conf)))
      (or (null? spurious) (apply bad "spurious keys:" spurious)))
    ;; queries only from here on
    (set! conf (map cdr conf))
    (for-each (lambda (query)
                (case (car query)
                  ((simple)
                   (apply define-simple-query (cdr query)))
                  ((simple-list)
                   (let loop ((more (cdr query)))
                     (or (null? more)
                         (begin
                           (define-simple-query (car more) (cadr more))
                           (loop (cddr more))))))
                  ((drill-down)
                   (apply define-drill-down-queries (cdr query)))
                  ((drill-down-list)
                   (let loop ((more (cdr query)))
                     (or (null? more)
                         (begin
                           (define-drill-down-queries (car more) (cadr more))
                           (loop (cddr more))))))
                  ((custom)
                   (or cq-acc (begin
                                (set! cq-acc (tmpfile))
                                (write '(define-module (etrack)) cq-acc)
                                (newline cq-acc)))
                   (format cq-acc "~S~%" `(define-query ,@(cdr query))))))
              conf)
    (and cq-acc (seek cq-acc 0 SEEK_SET)
         (let loop ((form (read cq-acc)))
           (or (eof-object? form)
               (begin (eval form)
                      (loop (read cq-acc)))))))
  conf)

(define (read-config-file file)
  (let ((acc (accumulator/one-only))
        (p (open-input-file file)))
    (let loop ((form (read p)))
      (or (eof-object? form)
          (begin (acc form)
                 (loop (read p)))))
    (acc)))

(define (read-etrack-config)
  (read-config-file
   (or (getenv "ETRACK_CONFIG")
       (bad-config-error "need to set env var ETRACK_CONFIG"))))

;; the exiting continuation

(define all-done #f)

;; command dispatch

(define *ALL* '((--help #f #f "display this message and exit succesfully")
                ;; Add command specs here.
                ))

(define ACT! (make-object-property))

(define-macro (define-command sig . body)
  `(set! (ACT! ',(car sig)) (lambda ,(cdr sig) ,@body)))

(define (require-command cmd)
  (or (ACT! cmd)
      (load-from-path (fs "/home/ttn/build/etrack/commands/~A.scm" cmd))))

(define init!
  (let ((init-promise
         (delay
           (begin
             (configure (read-etrack-config))
             (and *sockdir*
                  ;; If someone else owns the socket directory, don't
                  ;; even try bringing up the daemon.  If the following
                  ;; ‘pg-connectdb’ fails, user can go yell at admin.
                  (access? *sockdir* (logior R_OK W_OK))
                  (or ((cluster-mangler #f *sockdir*) #:daemon-up)
                      (error "no daemon for:" *sockdir*)))
             (set! CONN (pg-connectdb (fs "~Adbname=~A"
                                          (cond (*sockdir*
                                                 (fs "host=~A "
                                                     *sockdir*))
                                                (else ""))
                                          *db*)))
             (set! M (pgtable-worker CONN (DK #:ename) (DK #:edefs)))
             (and *client-encoding*
                  (Cfexec "SET SESSION CLIENT_ENCODING TO '~A';"
                          *client-encoding*))
             ;; backward compatibility
             (set! select (lambda args (apply M #:select args)))

             ;; standard queries follow

             (for-each (lambda (attribute)
                         (define-simple-query (symbol->string attribute)
                           (attribute->attcode attribute)))
                       (non-drill-down-attributes))

             (define-query "(possible duplicates)"
               (lambda ()
                 (let ((cmd 'possible-duplicates))
                   (require-command cmd)
                   (M #:select sel:*
                      #:where `(or ,@(map (lambda (i)
                                            `(= i ,i))
                                          ((ACT! cmd))))
                      #:order-by o:date))))))))
    ;; rv
    (lambda ()
      (force init-promise))))

(define (load-and-dispatch cl)
  (let* ((cmd (string->symbol (cadr cl)))
         (spec (or (assq-ref *ALL* cmd)
                   (error "command not found:" cmd)))
         (args (and (car spec)
                    (or (and (pair? (cddr cl)) (cddr cl))
                        (error "missing args for command:" cmd))))
         (proc (begin (require-command cmd)
                      (ACT! cmd))))
    (if (cadr spec) (init!))
    (or (if args
            (apply proc args)
            (proc))
        (all-done #f))))

(define (process-command cmd)
  (case (car cmd)
    ((quit)
     (all-done #t))
    (else
     (load-and-dispatch (cons #f cmd)))))

(define (usage available-commands)
  (fso "~%~:{~A~@[ ~:@(~A~)~]~*~30T -- ~A~%~}" available-commands))

(define (main cl)
  (cond ((and (< 2 (length cl))
              (or (string=? "--client-encoding" (list-ref cl 1))
                  (string=? "-E" (list-ref cl 1))))
         (set! *client-encoding* (list-ref cl 2))
         (set-cdr! cl (cdddr cl))))
  (cond ((or (= 1 (length cl))
             (string=? "--help" (list-ref cl 1)))
         (fso "Usage: etrack -b [-E CLIENT-ENCODING] CMD [ARG]~%")
         (fso "where CMD is one of:")
         (usage *ALL*))
        (else
         (load-and-dispatch cl))))

(exit (let ((ev (call-with-current-continuation
                 (lambda (cc)
                   (set! all-done cc)
                   (main (command-line))))))
        (and CONN (pg-finish CONN))
        ev))

;;; etrack.scm ends here
