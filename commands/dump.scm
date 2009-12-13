;;; dump.scm --- dump to FILE based on date or id

;; Copyright (C) 2007-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack)
  #:use-module ((database postgres) #:select (pg-ntuples pg-nfields)))

(define-command (dump args)             ; init=#t "(FILE TYPE SPEC)"

  (define (where/date spec)
    (define (expand j k)
      (let ((piece (substring spec j k))
            (rv (string-copy "YYYY-01-01")))
        (do ((i 0 (1+ i)))
            ((= i (string-length piece)))
          (string-set! rv i (string-ref piece i)))
        rv))
    (define (exact x)
      (format #t "(= ~S)" spec)
      `(= ,spec ,x))
    (define (the which)
      (one-value `(to_char (,which date) "YYYY-MM-DD")))
    (let ((len (string-length spec)))
      (cond ((string-index spec #\:)
             => (lambda (cut)
                  (let ((b (if (= 0 cut)
                               (the 'min)
                               (expand 0 cut)))
                        (e (if (= (1- len) cut)
                               (the 'max)
                               (expand (1+ cut) len))))
                    (format #t "(~S . ~S)" b e)
                    `(and (<= ,b date) (<= date ,e)))))
            ((= 10 len)
             (exact '(to_char date "YYYY-MM-DD")))
            ((= 7 len)
             (exact '(to_char date "YYYY-MM")))
            ((= 4 len)
             (exact '(to_char date "YYYY")))
            (else
             (display '())
             '(:: bool #f)))))

  (define (where/id ls)
    (display '(-))
    `(in/set i ,@ls))

  (define (tabsep-spew result outfile)
    (fso "(~A . ~A)" (pg-ntuples result) (pg-nfields result))
    (let ((rows (M #:tuples-result->rows result))
          (p (open-output-file outfile)))
      (for-each (lambda (row)
                  (display (list-ref row 0) p)
                  (for-each (lambda (n)
                              (display "\t" p)
                              (display (list-ref row n) p))
                            '(1 2 3))
                  (for-each (lambda (d)
                              (display "\t" p)
                              (display d p))
                            (list-ref row 4))
                  (newline p))
                rows)
      (close-port p)))

  (and (string? args)
       (set! args (with-input-from-string args read)))
  (or (and (pair? args) (< 2 (length args)))
      (error "bad args:" args))
  (apply-to-args
   args (lambda (filename type spec)
          (fso "(")
          (tabsep-spew (M #:select (mk-outspec
                                    `(i
                                      ,(DK #:o/date)
                                      (#f "amount" ,(w/c 6 'amount))
                                      attcode
                                      details))
                          #:where (case type
                                    ((date) (where/date spec))
                                    ((id) (where/id spec))
                                    (else (error "bad type:" type)))
                          #:order-by o:date)
                       filename)
          (fso "~A)\n" (stat:size (stat filename))))))

;;; dump.scm ends here
