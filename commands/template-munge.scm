;;; template-munge.scm --- add/del/mod a template by NAME

;; Copyright (C) 2006-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define tM #f)

(define-command (template-munge command) ; init=#t "(OP NAME [ARGS...])"
  ;; COMMAND is a list whose car is used to dispatch:
  ;; - (add NAME)
  ;; - (del NAME)
  ;; - (mod NAME FIELD1 NEWVALUE1 ...)
  (set! tM (or tM (pgtable-worker CONN (DK #:tname) (DK #:tdefs))))
  (and (string? command)
       (set! command (with-input-from-string command read)))
  (let* ((args (cdr command))
         (name (car args))
         (rest (cdr args)))
    (define (report! res)
      (fso "~A" res)
      (or (eq? 'PGRES_COMMAND_OK (pg-result-status res))
          (fso " (~A)" (pg-result-error-message res)))
      (newline))
    (case (car command)
      ((add)
       (fso/norepl "adding: ~S~%" name)
       (report! (tM #:insert-alist `((name . ,name)))))
      ((del)
       (fso/norepl "deleting: ~S~%" name)
       (report! (tM #:delete-rows `(= name ,name))))
      ((mod)
       (fso/norepl "modifiying: ~S ~S~%" name rest)
       (let loop ((ls rest) (cols '()) (data '()))
         (cond ((not (null? ls))
                (loop (cddr ls) (cons (car ls) cols) (cons (cadr ls) data)))
               (else
                (report! (tM #:update-col cols
                             (map (lambda (v)
                                    (case v
                                      ((nil) #:NULL)
                                      (else v)))
                                  data)
                             `(= name ,name)))))))
      (else
       (error "badness: unrecognized command:" command)))))

;;; template-munge.scm ends here
