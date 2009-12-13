;;; shell.scm --- like repl but w/ prompt and no parens required

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack)
  #:use-module ((ice-9 rdelim) #:select (read-line)))

(define-command (shell)                 ; init=#t
  (fso "Enter q to quit, h for help.\n")
  (error-catching-loop
   (lambda ()
     (repl (lambda (port)
             (fso "(etrack) ")
             (flush-all-ports)
             (let ((line (read-line port)))
               (and (eof-object? line) (begin (newline) (quit)))
               (and (string=? "q" line) (quit))
               (cond ((string-null? line)
                      '(do-nothing))
                     ((string=? "h" line)
                      (usage (cdr *ALL*))
                      '(do-nothing))
                     (else
                      (with-input-from-string (fs "(~A)" line)
                        (lambda () (read)))))))
           process-command
           identity))))

;;; shell.scm ends here
