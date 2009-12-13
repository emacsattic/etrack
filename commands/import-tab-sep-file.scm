;;; import-tab-sep-file.scm --- import tab-separated data from FILE

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack)
  #:use-module ((ice-9 gap-buffer) #:select (make-gap-buffer gb->lines))
  #:use-module ((srfi srfi-13) #:select (string-trim-both)))

(define-command (import-tab-sep-file file) ; init=#t
  (and (symbol? file) (set! file (symbol->string file)))
  (let* ((p (open-input-file file))
         (buf (or (false-if-exception (make-gap-buffer p))
                  (error "invalid file")))
         (/tab/ (split-on-proc #\tab))
         (/comma/ (split-on-proc #\,)))
    (close-port p)
    ;; todo: separate into two phases (validate / insert)
    (for-each (lambda (line)
                (let ((toks (/tab/ line)))
                  (cond ((string-null? line))
                        ((= 4 (length toks))
                         (apply
                          (lambda (date float-amount ac raw-details)
                            (let ((data (list date
                                              (inexact->exact
                                               (* 100 (string->number
                                                       float-amount)))
                                              ac
                                              (map string-trim-both
                                                   (/comma/ raw-details)))))
                              (fso "insert: ~S => " data)
                              (process-command `(insert ,data))))
                          toks))
                        (else (fso "unrecognized: ~A\n" line)))))
              (gb->lines buf))))

;;; import-tab-sep-file.scm ends here
