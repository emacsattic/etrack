;;; repl.scm --- for use primarily by the emacs front end

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (repl)                  ; init=#t
  (set! *repl?* #t)
  (error-catching-loop
   (lambda () (repl read process-command identity))))

;;; repl.scm ends here
